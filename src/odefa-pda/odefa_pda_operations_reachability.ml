(**
  A module which defines a reachability operation over a PDA.
*)

open Batteries;;

open Odefa_cache_utils;;
open Odefa_utils;;

open Odefa_pda_types;;

let logger = Odefa_logger.make_logger "Odefa_pda_operations";;

(**
  A comparator for PDA transitions.
*)
let transition_comparator_for
      pda
      (in_state1,input_option1,pop_option1,out_state1,pushes1)
      (in_state2,input_option2,pop_option2,out_state2,pushes2) =
  chain_compare pda.pda_compare_states in_state1 in_state2 @@
  chain_compare (Option.compare ~cmp:pda.pda_compare_input_symbols)
    input_option1 input_option2 @@
  chain_compare (Option.compare ~cmp:pda.pda_compare_stack_symbols)
    pop_option1 pop_option2 @@
  chain_compare pda.pda_compare_states out_state1 out_state2 @@
  List.compare pda.pda_compare_stack_symbols pushes1 pushes2
;;

(** The type of nodes in the summarization graph used in
    {reachable_goal_states}. *)
type 'a summary_graph_node =
  | State_node of 'a
  | Local_node of int
;;

let summary_graph_node_comparator state_comparator node1 node2 =
  match node1,node2 with
    | State_node(s1),State_node(s2) -> state_comparator s1 s2
    | State_node(_),Local_node(_) -> -1
    | Local_node(_),State_node(_) -> 1
    | Local_node(n1),Local_node(n2) -> compare n1 n2
;;

(** The type of node annotations in the summarization graph used in
    {reachable_goal_states}. *)
type 'c summary_graph_operation =
  | Push of 'c
  | Pop of 'c
  | Nop
;;

let summary_graph_operation_comparator stack_symbol_comparator op1 op2 =
  match op1,op2 with
    | Push(s1),Push(s2) -> stack_symbol_comparator s1 s2
    | Push(_),Pop(_) -> -1
    | Push(_),Nop -> -1
    | Pop(_),Push(_) -> 1
    | Pop(s1),Pop(s2) -> stack_symbol_comparator s1 s2
    | Pop(_),Nop -> -1
    | Nop,Push(_) -> 1
    | Nop,Pop(_) -> 1
    | Nop,Nop -> 0
;;

(**
   A function to determine the reachable goal states of a PDA.
*)
let reachable_goal_states
      (type state_t)
      (type input_symbol_t)
      (type stack_symbol_t)
      (pda : (state_t,input_symbol_t,stack_symbol_t) pda)
      : state_t Enum.t = 
  logger `debug "Beginning reachable goal state analysis for PDA.";
  (*
    Our strategy is to reduce the PDA to a directed graph describing individual
    stack operations and then summarize that graph by closing over its
    structure. For instance, the transition
      (S1) ---- pop k1, input a, push [k2,k3] ---> (S2)
    would be rendered in the graph as
      (S1) -- pop k1 --> (#1) -- push k2 --> (#2) -- push k3 --> (S2)
    where #1 and #2 are freshly-selected nodes.  Note that the "input a" part
    of the transition is discarded; this is part of the reason that this
    reachability test is possible.  After constructing this stack operation
    graph, we perform closure according to the following rules:
      1. (A) -- push k1 --> (B) -- pop k1 --> (C) ===> (A) -- nop --> (C)
      2. (A) -- op --> (B) -- nop --> (C) ===> (A) -- op --> (C)
      3. (A) -- nop --> (B) -- op --> (C) ===> (A) -- op --> (C)
      4. (A) -- nop --> (B) -- nop --> (C) ===> (A) -- nop --> (C)
    Once transitive closure is complete, any edge of the form
      (Start) -- pop initial_stack_symbol --> (X)
    indicates that (X) is an accepting clause for the PDA.  We should then
    report the state backed by (X) as reachable.
  *)
  
  (*
    As mentioned above, we'll need fresh nodes for the summarization.
  *)
  let uid_counter = ref 0 in
  let next_uid () =
    let x = !uid_counter in
    uid_counter := x + 1;
    x
  in
  
  (*
    We'll keep the graph as a mapping from source node to pairs between target
    node and the stack operation which gets us there.  To do this, we need
    appropriate comparison functions and similar tools.
  *)
  let compare_nodes = summary_graph_node_comparator pda.pda_compare_states in
  let compare_ops =
    summary_graph_operation_comparator pda.pda_compare_stack_symbols
  in 
  let compare_targets =
    Tuple.Tuple2.compare ~cmp1:compare_nodes ~cmp2:compare_ops
  in
  
  let empty_graph () = Map.PMap.create compare_nodes in
  let graph_find_all from_node graph =
    if Map.PMap.mem from_node graph
    then Map.PMap.find from_node graph
    else Set.PSet.create compare_targets
  in
  let graph_add from_node to_node operation graph =
    let old_set = graph_find_all from_node graph in
    Map.PMap.add from_node (Set.PSet.add (to_node,operation) old_set) graph
  in
  let graph_edge_enum graph =
    Map.PMap.enum graph
    |> Enum.map
        (fun (k,vs) ->
          Set.PSet.enum vs
          |> Enum.map (fun v -> (k,v))
        )
    |> Enum.concat
  in
  let graph_size graph =
    Map.PMap.enum graph
    |> Enum.map
        (fun (_,s) -> Set.PSet.cardinal s)
    |> Enum.fold (+) 0
  in
      
  (*
    Determine the initial set of summarization edges from the PDA's transitions.
  *)
  let initial_graph =
    pda.pda_enumerate_transitions ()
      |> Enum.map
          (fun (in_state, _, pop_option, out_state, pushes) ->
            let operations =
              let push_operations =
                List.map (fun x -> Push x) @@ List.rev pushes
              in
              match pop_option with
                | None -> push_operations
                | Some(x) -> (Pop x) :: push_operations
            in
            let rec make_path from_node to_node ops =
              match ops with
                | [] -> [(from_node, to_node, Nop)]
                | [op] -> [(from_node, to_node, op)]
                | op::ops' ->
                    let middle_node = Local_node(next_uid()) in
                    (from_node, middle_node, op) ::
                      make_path middle_node to_node ops' 
            in
            List.enum @@
              make_path (State_node in_state) (State_node out_state) operations
          )
      |> Enum.concat
      |> Enum.fold
        (fun g (from_node, to_node, op) ->
          graph_add from_node to_node op g
        )
        (empty_graph ())
  in
  logger `debug @@ "PDA reachable goal state analysis: " ^
    (string_of_int @@ graph_size initial_graph) ^ " initial edges";
  
  (*
    Define a function to perform a single step of graph closure.  This function
    yields new edges discovered by closure.
  *)
  let close1 graph =
    graph_edge_enum graph
    |> Enum.map
        (fun (in1,(out1,op1)) ->
          graph_find_all out1 graph
          |> Set.PSet.enum
          |> Enum.filter_map
              (fun (out2,op2) ->
                let operation_option =
                  match op1,op2 with
                    | Nop,Nop -> Some Nop
                    | Nop,_ -> Some op2
                    | _,Nop -> Some op1
                    | Push x,Pop x' ->
                      if pda.pda_compare_stack_symbols x x' <> 0
                      then None
                      else Some Nop
                    | _ -> None
                in
                Option.bind operation_option (fun op -> Some (in1,out2,op))
              )
        )
    |> Enum.concat
    |> Enum.fold
        (fun g (in_state,out_state,op) ->
          graph_add in_state out_state op g
        ) graph
  in
  
  (*
    Performs a full transitive closure over a set of edges.
  *)
  let rec close_fully graph =
    let graph' = close1 graph in
    if graph_size graph <> graph_size graph'
    then
      begin
        logger `debug @@ "PDA reachable goal state analysis: " ^
          (string_of_int @@ graph_size graph') ^ " edges so far";
        close_fully graph'
      end
    else
      begin
        logger `debug @@ "PDA reachable goal state analysis: " ^
          (string_of_int @@ graph_size graph') ^ " edges in total";
        graph'
      end
  in
  
  (* Now actually get the transitive closure. *)
  let closed_graph = close_fully initial_graph in
  
  (* And then filter it for the answers we wanted. *)
  let answer =
    closed_graph
    |> graph_edge_enum
    |> Enum.filter_map
        (function
          | (State_node(s),(State_node(s'),Pop(k))) ->
            if pda.pda_compare_states s pda.pda_initial_state = 0 &&
               pda.pda_compare_stack_symbols k pda.pda_initial_stack_symbol = 0
            then
              (*
                Then this is an edge from the initial state to another state via
                a single pop of the initial stack symbol.  Thus, this is an
                accepting state of the PDA.
              *)
              Some s'
            else
              None
          | _ -> None
        )
  in
  logger `debug "Completed reachable goal state analysis for PDA."; answer
;;
