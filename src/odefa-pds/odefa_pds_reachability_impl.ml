open Batteries;;

open Odefa_string_utils;;
open Odefa_utils;;

open Odefa_pds;;
open Odefa_pds_reachability;;

let logger = Odefa_logger.make_logger "Odefa_pds_reachability_impl";;

module Make : Pds_reachability = functor (P : Pds) ->
struct
  module P = P;;
  
  type state = P.state;;
  let equal_state s1 s2 = P.State_order.compare s1 s2 = 0;;
  let compare_state = P.State_order.compare;;
  type symbol = P.symbol;;
  let equal_symbol s1 s2 = P.Symbol_order.compare s1 s2 = 0;;
  type edge_symbol = (state,symbol) pds_action;;

  module Edge_symbol_order =
  struct
    type t = edge_symbol;;
    let compare =
      compare_pds_action P.State_order.compare P.Symbol_order.compare
    ;;
  end;;
  let compare_edge_symbol = Edge_symbol_order.compare;;

  (** The type of node annotations in the summarization graph used in
      {reachable_goal_states}. *)
  let equal_edge_symbol s1 s2 = Edge_symbol_order.compare s1 s2 = 0
  ;;

  (** The type of nodes in the summarization graph used in
      {reachable_goal_states}. *)
  type node =
    | State_node of state
    | Local_node of edge_symbol list * node
    [@@deriving eq,ord]
  ;;

  module Node_order =
  struct
    type t = node
    let compare = compare_node
  end;;

  module Value_order =
  struct
    type t = node * edge_symbol * bool;;
    let compare = Tuple.Tuple3.compare ~cmp1:Node_order.compare
        ~cmp2:Edge_symbol_order.compare ~cmp3:compare
  end;;

  module Edge_map = Odefa_multimap.Make(Node_order)(Value_order);;

  (**
     The data structure representing a reachability analysis.  The maps are, in
     order, the forward and backward edges of the reachability graph.
  *)
  type analysis = Analysis of Edge_map.t * Edge_map.t;;

  let empty_analysis = Analysis(Edge_map.empty, Edge_map.empty);;

  type analysis_node = node;;
  type analysis_action = edge_symbol;;

  let pp_analysis_action = pp_pds_action P.pp_state P.pp_symbol;;
  
  let rec pp_analysis_node node =
    match node with
    | State_node(s) -> P.pp_state s
    | Local_node(upcoming,node') ->
      pretty_list pp_analysis_action upcoming ^ " ; " ^
      pp_analysis_node node'
  ;;

  let edge_enum (Analysis(forward,_)) = Edge_map.enum forward;;

  let edges_of_analysis (Analysis(forward,_)) =
    (* (analysis_node * analysis_action * analysis_node) Enum.t *)
    Edge_map.enum forward
    |> Enum.map
      (fun (source, (target, edge, from_closure)) ->
        (source, edge, target, from_closure))
  ;;

  let edges_from state (Analysis(forward,_)) = Edge_map.find state forward;;

  let edges_to state (Analysis(_,backward)) = Edge_map.find state backward;;

  let add_edge (from_state, to_state, op, from_closure) (Analysis(forward,backward)) =
    let forward' = Edge_map.add from_state (to_state, op, from_closure) forward in
    let backward' = Edge_map.add to_state (from_state, op, from_closure) backward in
    Analysis(forward', backward')
  ;;

  let has_edge (from_state, to_state, op) (Analysis(forward,_)) =
    Edge_map.mem from_state (to_state, op, true) forward ||
    Edge_map.mem from_state (to_state, op, false) forward
  ;;

  let rec make_edges from_node to_node ops =
    match ops with
    | [] -> [(from_node, to_node, Nop)]
    | [op] -> [(from_node, to_node, op)]
    | op::ops' ->
      let middle_node = Local_node(ops', to_node) in
      (from_node, middle_node, op)::
      (make_edges middle_node to_node ops')
  ;;

  let convert_pds_to_edges pds =
    let transition_edges =
      P.transitions_of_pds pds
      |> Enum.map
        (fun (in_state, actions, out_state) ->
           make_edges
             (State_node in_state)
             (State_node out_state)
             actions
           |> List.enum
        )
      |> Enum.concat
      |> Enum.map (fun (a,b,c) -> (a,b,c,false))
    in
    let starting_edges =
      P.starters_of_pds pds
      |> Enum.map
        (fun (state, symbol) ->
          let target_state = State_node(state) in
          let starting_state = Local_node([Push symbol], target_state) in
          (starting_state, target_state, Push symbol, false)
        )
    in
    List.of_enum @@ Enum.append starting_edges transition_edges
  ;;

  let perform_closure initial_edges =
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
        5. (A) -- push k1 --> (B) -- push k2 --> (C) ===>
           (A) -- push k2 --> (D) -- push k1 --> (C) [if the condition allows]
        6. (A) -- jump(B) --> (C) ===> (A) -- nop --> (B)
      Once transitive closure is complete, any edge of the form
        ([push initial_stack_symbol], Start) -- nop --> (X)
      indicates that (X) is an accepting clause for the PDS.  We should then
      report the state backed by (X) as reachable.
    *)

    (*
      A function to determine the transitive closure of two edges by their
      operations.
    *)
    let join_ops op1 op2 =
      let reduction_step =
        match op1,op2 with
        | Nop,Nop -> Enum.singleton [Nop]
        | Nop,_ -> Enum.singleton [op2]
        | _,Nop -> Enum.singleton [op1]
        | Push x,Pop x' ->
          if P.Symbol_order.compare x x' <> 0
          then Enum.empty ()
          else Enum.singleton [Nop]
        | _ -> Enum.empty ()
      in
      let swap_step =
        if P.legal_action_swaps op1 op2
        then
          Enum.singleton [op2;op1]
        else
          Enum.empty ()
      in
      Enum.append reduction_step swap_step
    in

    (*
      Define a function which, given a reachability analysis graph and an edge,
      determines all of the edges which may be transitively closed by adding
      that edge to the graph.
    *)
    let derive_edges_from graph (from_state, to_state, op) =
      let successor_edge_closure =
        edges_from to_state graph
        |> Enum.map
          (fun (successor_state, op', _) ->
            join_ops op op'
            |> Enum.map
              (fun op'' -> from_state, successor_state, op'')
          )
        |> Enum.concat
      in
      let predecessor_edge_closure =
        edges_to from_state graph
        |> Enum.map
          (fun (predecessor_state, op', _) ->
            join_ops op' op
            |> Enum.map
              (fun op'' -> predecessor_state, to_state, op'')
          )
        |> Enum.concat
      in
      let jump_edge_closure =
        match op with
        | Jump(jump_state) ->
          Enum.singleton (from_state, State_node(jump_state), [])
        | _ ->
          Enum.empty()
      in
      Enum.concat (List.enum [ successor_edge_closure
                             ; predecessor_edge_closure
                             ; jump_edge_closure ])
      |> Enum.map
        (fun (source,target,ops) -> List.enum @@ make_edges source target ops)
      |> Enum.concat
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
      | (((from_node,to_node,op,_) as edge)::edges')::work' ->
        let new_edges = derive_edges_from graph (from_node,to_node,op) in
        let new_edges' = List.map (fun (a,b,c) -> (a,b,c,true)) new_edges in
        let graph' = add_edge edge graph in
        graph_closure graph' @@ new_edges'::(edges'::work')
    in
    graph_closure empty_analysis [initial_edges]
  ;;

  let analyze_pds pds =
    logger `debug "Beginning reachable goal state analysis for PDS.";
    
    (*
      The initial edges derived from the PDS.
    *)
    let initial_edges = convert_pds_to_edges pds in
    logger `debug @@ "PDS reachable goal state analysis: " ^
                     (string_of_int @@ List.length initial_edges) ^ " initial edges";
    

    (* All we have to do now is close an empty graph with the initial edges. *)
    perform_closure initial_edges
  ;;

  let reachable_from
      analysis
      initial_state
      initial_symbol =
    let in_state =
      Local_node([Push initial_symbol], State_node(initial_state))
    in
    let answer =
      edges_from in_state analysis
      |> Enum.filter_map
        (function
          | (State_node(s'),Nop,_) -> Some s'
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
