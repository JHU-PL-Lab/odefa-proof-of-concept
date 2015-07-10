open Batteries;;

open Odefa_utils;;

open Odefa_misa_monoid;;
open Odefa_misa;;
open Odefa_misa_reachability;;

let logger = Odefa_logger.make_logger "Odefa_monoid_reachability_impl";;

module Make : Misa_reachability = functor (M : Misa) ->
struct
  (** The type of nodes in the summarization graph used in
      {reachable_goal_states}. *)
  type node =
    | State_node of M.state
    | Local_node of int
  ;;

  module Node_order =
  struct
    type t = node
    let compare node1 node2 =
      match node1,node2 with
      | State_node(s1),State_node(s2) -> M.State_order.compare s1 s2
      | State_node(_),Local_node(_) -> -1
      | Local_node(_),State_node(_) -> 1
      | Local_node(n1),Local_node(n2) -> compare n1 n2
  end;;

  type stack_symbol =
    | Push of M.symbol
    | Pop of M.symbol
    | Nop
  ;;

  let compare_stack_symbol op1 op2 =
    match op1,op2 with
    | Push(s1),Push(s2) -> M.Symbol_order.compare s1 s2
    | Push(_),Pop(_) -> -1
    | Push(_),Nop -> -1
    | Pop(_),Push(_) -> 1
    | Pop(s1),Pop(s2) -> M.Symbol_order.compare s1 s2
    | Pop(_),Nop -> -1
    | Nop,Push(_) -> 1
    | Nop,Pop(_) -> 1
    | Nop,Nop -> 0
  ;;

  (** The type of node annotations in the summarization graph used in
      {reachable_goal_states}. *)
  type edge_symbol = Edge_symbol of stack_symbol * M.recognized;;

  module Edge_symbol_order =
  struct
    type t = edge_symbol;;
    let compare (Edge_symbol(k1,r1)) (Edge_symbol(k2,r2)) =
      chain_compare compare_stack_symbol k1 k2 @@
        M.Recognized_order.compare r1 r2
    ;;
  end;;

  module Value_order =
  struct
    type t = node * edge_symbol;;
    let compare =
      Tuple.Tuple2.compare
        ~cmp1:Node_order.compare
        ~cmp2:Edge_symbol_order.compare
  end;;

  module Edge_map = Odefa_multimap.Make(Node_order)(Value_order);;

  (**
     The data structure representing a reachability analysis.  The maps are, in
     order, the forward and backward edges of the reachability graph.
  *)
  type umisa_analysis = Analysis of Edge_map.t * Edge_map.t;;

  let empty_analysis = Analysis(Edge_map.empty, Edge_map.empty);;

  let edge_enum (Analysis(forward,_)) = Edge_map.enum forward;;

  let edges_from state (Analysis(forward,_)) = Edge_map.find state forward;;

  let edges_to state (Analysis(_,backward)) = Edge_map.find state backward;;

  let add_edge (from_state, to_state, edge_symbol) (Analysis(forward,backward)) =
    let forward' = Edge_map.add from_state (to_state, edge_symbol) forward in
    let backward' = Edge_map.add to_state (from_state, edge_symbol) backward in
    Analysis(forward', backward')
  ;;

  let has_edge (from_state, to_state, edge_symbol) (Analysis(forward,_)) =
    Edge_map.mem from_state (to_state, edge_symbol) forward
  ;;

  let analyze_umisa umisa =
    logger `debug "Beginning reachable goal state analysis for MISA.";
    (*
      Our strategy is to reduce the MISA to a directed graph describing
      individual stack operations and then summarize that graph by closing over
      its structure. For instance, the transition
        (S1) ---- pop k1, input a, push [k2,k3] ---> (S2)
      would be rendered in the graph as
        (S1) -- pop k1 --> (#1) -- push k2 --> (#2) -- input a, push k3 --> (S2)
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
      The initial edges derived from the PDS.
    *)
    let initial_edges =
      M.transitions_of_umisa umisa
      |> Enum.map
        (fun (in_state, input_option, pop_option, out_state, pushes) ->
           let operations =
             let push_operations =
               List.map (fun x -> Push x) @@ List.rev pushes
             in
             match pop_option with
             | None -> push_operations
             | Some(x) -> (Pop x) :: push_operations
           in
           let last_recognized =
            Option.map_default M.inject M.Recognized_monoid.zero input_option in
           let rec make_edges from_node to_node ops =
             match ops with
             | [] -> [(from_node, to_node, Edge_symbol(Nop,last_recognized))]
             | [op] -> [(from_node, to_node, Edge_symbol(op,last_recognized))]
             | op::ops' ->
               let middle_node = Local_node(next_uid()) in
               let edge_symbol = Edge_symbol(op,M.Recognized_monoid.zero) in
               (from_node, middle_node, edge_symbol)::
               (make_edges middle_node to_node ops')
           in
           make_edges
             (State_node in_state)
             (State_node out_state)
             operations
           |> List.enum
        )
      |> Enum.concat
      |> List.of_enum
    in
    logger `debug @@ "PDS reachable goal state analysis: " ^
                     (string_of_int @@ List.length initial_edges) ^ " initial edges";

    (*
      A function to determine the transitive closure of two operations.
    *)
    let join_ops op1 op2 =
      match op1,op2 with
      | Nop,Nop -> Some Nop
      | Nop,_ -> Some op2
      | _,Nop -> Some op1
      | Push x,Pop x' ->
        if M.Symbol_order.compare x x' <> 0
        then None
        else Some Nop
      | _ -> None
    in
    
    (*
      A function to determine the transitive closure of edge symbols.
    *)
    let join_edge_symbols (Edge_symbol(op1,r1)) (Edge_symbol(op2,r2)) =
      let op = join_ops op1 op2 in
      Option.bind op
        (fun op' -> Some(Edge_symbol(op', M.Recognized_monoid.plus r1 r2)))
    in

    (*
      Define a function which, given a reachability analysis graph and an edge,
      determines all of the edges which may be transitively closed by adding
      that edge to the graph.
    *)
    let derive_edges_from graph (from_state, to_state, edge_symbol) =
      let successor_edge_closure =
        edges_from to_state graph
        |> Enum.filter_map
          (fun (successor_state, edge_symbol') ->
             let edge_symbol'' = join_edge_symbols edge_symbol edge_symbol' in
             Option.bind edge_symbol''
              (fun edge_symbol ->
                Some(from_state, successor_state, edge_symbol))
          )
      in
      let predecessor_edge_closure =
        edges_to from_state graph
        |> Enum.filter_map
          (fun (predecessor_state, edge_symbol') ->
             let edge_symbol'' = join_edge_symbols edge_symbol' edge_symbol in
             Option.bind edge_symbol''
              (fun edge_symbol ->
                Some(predecessor_state, to_state, edge_symbol))
          )
      in
      Enum.append successor_edge_closure predecessor_edge_closure
      |> Enum.filter (fun edge -> not @@ has_edge edge graph)
      |> List.of_enum
    in

    (*
      A function to compute the transitive closure of the reachability graph via
      a work stack strategy.

      Each closure rule described above is pairwise: only two edges are
      involved.  We utilize this by maintaining a collection of "new" edges
      distinct from the "old" edges; only the latter appear in the graph.  As
      each "new" edge is added to the graph, closure is performed on it with
      respect only to those edges currently in the graph.  In this way, each
      edge will be compared to each other edge pairwise at most once.

      To avoid concatenation, our work queue is a list of lists; this way, each
      new collection of edges can be prepended to the existing queue.
    *)
    let rec graph_closure graph work =
      match work with
      | [] -> graph
      | []::work' -> graph_closure graph work'
      | (edge::edges')::work' ->
        let new_edges = derive_edges_from graph edge in
        let graph' = add_edge edge graph in
        graph_closure graph' @@ new_edges::(edges'::work')
    in

    (* All we have to do now is close an empty graph with the initial edges. *)
    graph_closure empty_analysis [initial_edges]
  ;;

  (* TODO: something to prevent reachable_from from performing the full closure
           with all of the recognized symbols. *)

  let reachable_from
      analysis
      initial_state
      initial_symbol =
    (* Filter the graph for the answers we wanted. *)
    let answer =
      edges_from (State_node(initial_state)) analysis
      |> Enum.filter_map
        (function
          | (State_node(s'),Edge_symbol(Pop(k),_)) ->
            if M.Symbol_order.compare k initial_symbol = 0
            then
                (*
                  Then this is an edge from the initial state to another state
                  via a single pop of the initial stack symbol.  Thus, this is
                  an accepting state of the MISA.
                *)
              Some s'
            else
              None
          | _ -> None
        )
    in
    logger `debug "Completed reachable goal state analysis for PDS."; answer
  ;;

  (* TODO: something to generate these paths lazily so even unbounded sets of
           valid paths may be enumerated *)

  let valid_paths
      analysis
      initial_state
      initial_symbol =
    (* Filter the graph for the answers we wanted. *)
    let answer =
      edges_from (State_node(initial_state)) analysis
      |> Enum.filter_map
        (function
          | (State_node(s'),Edge_symbol(Pop(k),recognized)) ->
            if M.Symbol_order.compare k initial_symbol = 0
            then
                (*
                  Then this is an edge from the initial state to another state
                  via a single pop of the initial stack symbol.  Thus, this is
                  an accepting state of the MISA.
                *)
              Some (s',recognized)
            else
              None
          | _ -> None
        )
    in
    logger `debug "Completed reachable goal state analysis for PDS."; answer
  ;;

  let analyze_rmisa rmisa =
    let umisa_reachability_analysis = analyze_umisa @@ M.umisa_of_rmisa rmisa in
    let (state,symbol) = M.root_of_rmisa rmisa in
    reachable_from umisa_reachability_analysis state symbol
  ;;
end;;
