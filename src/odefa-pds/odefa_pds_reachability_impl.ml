open Batteries;;

open Odefa_utils;;

open Odefa_pds;;
open Odefa_pds_reachability;;

let logger = Odefa_logger.make_logger "Odefa_pds_reachability_impl";;

module Make : Pds_reachability = functor (P : Pds) ->
struct
  (** The type of nodes in the summarization graph used in
      {reachable_goal_states}. *)
  type node =
    | State_node of P.state
    | Local_node of int
  ;;

  module Node_order =
  struct
    type t = node
    let compare node1 node2 =
      match node1,node2 with
        | State_node(s1),State_node(s2) -> P.State_order.compare s1 s2
        | State_node(_),Local_node(_) -> -1
        | Local_node(_),State_node(_) -> 1
        | Local_node(n1),Local_node(n2) -> compare n1 n2
  end;;
  
  (** The type of node annotations in the summarization graph used in
      {reachable_goal_states}. *)
  type edge_symbol =
    | Push of P.symbol
    | Pop of P.symbol
    | Nop
  ;;

  module Edge_symbol_order =
  struct
    type t = edge_symbol;;
    let compare op1 op2 =
      match op1,op2 with
        | Push(s1),Push(s2) -> P.Symbol_order.compare s1 s2
        | Push(_),Pop(_) -> -1
        | Push(_),Nop -> -1
        | Pop(_),Push(_) -> 1
        | Pop(s1),Pop(s2) -> P.Symbol_order.compare s1 s2
        | Pop(_),Nop -> -1
        | Nop,Push(_) -> 1
        | Nop,Pop(_) -> 1
        | Nop,Nop -> 0
    ;;
  end;;

  module Value_order =
  struct
    type t = node * edge_symbol;;
    let compare = Tuple.Tuple2.compare ~cmp1:Node_order.compare
                                       ~cmp2:Edge_symbol_order.compare
  end;;

  module Edge_map = Odefa_multimap.Make(Node_order)(Value_order);;
  
  type pds_reachability_analysis = Analysis of Edge_map.t;;
  
  let analyze_pds pds =
    logger `debug "Beginning reachable goal state analysis for PDS.";
    (*
      Our strategy is to reduce the PDS to a directed graph describing
      individual stack operations and then summarize that graph by closing over
      its structure. For instance, the transition
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
      indicates that (X) is an accepting clause for the PDS.  We should then
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
      node and the stack operation which gets us there.  Determine the initial
      set of summarization edges from the PDS's transitions.
    *)
    let initial_graph =
      P.transitions_of_pds pds
      |> Enum.fold
          (fun graph (in_state, pop_option, out_state, pushes) ->
            let operations =
              let push_operations =
                List.map (fun x -> Push x) @@ List.rev pushes
              in
              match pop_option with
                | None -> push_operations
                | Some(x) -> (Pop x) :: push_operations
            in
            let rec make_path from_node to_node ops m =
              match ops with
                | [] -> Edge_map.add from_node (to_node, Nop) m
                | [op] -> Edge_map.add from_node (to_node, op) m
                | op::ops' ->
                    let middle_node = Local_node(next_uid()) in
                    let m' = Edge_map.add from_node (middle_node, op) m in
                    make_path middle_node to_node ops' m'
            in
            make_path
              (State_node in_state)
              (State_node out_state)
              operations
              graph
          )
          Edge_map.empty
    in
    logger `debug @@ "PDS reachable goal state analysis: " ^
      (string_of_int @@ Edge_map.num_values initial_graph) ^ " initial edges";
    
    (*
      Define a function to perform a single step of graph closure.  This function
      yields new edges discovered by closure.
    *)
    let close1 graph =
      Edge_map.enum graph
      |> Enum.map
          (fun (in1,(out1,op1)) ->
            Edge_map.find out1 graph
            |> Enum.filter_map
                (fun (out2,op2) ->
                  let operation_option =
                    match op1,op2 with
                      | Nop,Nop -> Some Nop
                      | Nop,_ -> Some op2
                      | _,Nop -> Some op1
                      | Push x,Pop x' ->
                        if P.Symbol_order.compare x x' <> 0
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
            Edge_map.add in_state (out_state,op) g
          ) graph
    in
    
    (*
      Performs a full transitive closure over a set of edges.
    *)
    let rec close_fully graph =
      let graph' = close1 graph in
      if Edge_map.num_values graph <> Edge_map.num_values graph'
      then
        begin
          logger `debug @@ "PDS reachable goal state analysis: " ^
            (string_of_int @@ Edge_map.num_values graph') ^ " edges so far";
          close_fully graph'
        end
      else
        begin
          logger `debug @@ "PDS reachable goal state analysis: " ^
            (string_of_int @@ Edge_map.num_values graph') ^ " edges in total";
          graph'
        end
    in
    
    (* Now actually get the transitive closure. *)
    let closed_graph = close_fully initial_graph in
    
    Analysis closed_graph
  ;;

  let reachable_from
        (Analysis graph)
        initial_state
        initial_symbol =
    (* Filter the graph for the answers we wanted. *)
    let answer =
      graph
      |> Edge_map.enum
      |> Enum.filter_map
          (function
            | (State_node(s),(State_node(s'),Pop(k))) ->
              if P.State_order.compare s initial_state = 0 &&
                 P.Symbol_order.compare k initial_symbol = 0
              then
                (*
                  Then this is an edge from the initial state to another state via
                  a single pop of the initial stack symbol.  Thus, this is an
                  accepting state of the PDS.
                *)
                Some s'
              else
                None
            | _ -> None
          )
    in
    logger `debug "Completed reachable goal state analysis for PDS."; answer
  ;;

  let analyze_rpds rpds =
    let pds_reachability_analysis = analyze_pds @@ P.pds_of_rpds rpds in
    let (state,symbol) = P.root_of_rpds rpds in
    reachable_from pds_reachability_analysis state symbol
  ;;
end;;
