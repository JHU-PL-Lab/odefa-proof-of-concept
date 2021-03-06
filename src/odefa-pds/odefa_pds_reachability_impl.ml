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
    type t = edge_symbol * node * bool;;
    let compare =
      Tuple.Tuple3.compare
          ~cmp1:Edge_symbol_order.compare
          ~cmp2:Node_order.compare
          ~cmp3:compare
  end;;

  type edge = Edge of node * edge_symbol * node * bool;;
  module Edge_ord =
  struct
    type t = edge;;
    let compare (Edge(source1,op1,target1,fc1))
                (Edge(source2,op2,target2,fc2)) =
      (Tuple.Tuple4.compare
          ~cmp1:compare_node
          ~cmp2:compare_edge_symbol
          ~cmp3:compare_node
          ~cmp4:compare)
        (source1,op1,target1,fc1) (source2,op2,target2,fc2)
      ;;
  end;;
  module Edge_set = Set.Make(Edge_ord);;
  
  module Edge_map = Odefa_multimap.Make(Node_order)(Value_order);;

  (**
     The data structure representing a reachability analysis.  The maps are, in
     order, the forward and backward edges of the reachability graph.
  *)
  type analysis = Analysis of Edge_map.t * Edge_map.t;;

  let empty_analysis = Analysis(Edge_map.empty, Edge_map.empty);;

  let equal_analysis (Analysis(em1,_)) (Analysis(em2,_)) =
    Edge_map.Ord.compare em1 em2 = 0
  ;;

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

  let edges_of_analysis (Analysis(forward,_)) =
    (* (analysis_node * analysis_action * analysis_node) Enum.t *)
    Edge_map.enum forward
    |> Enum.map
      (fun (source, (edge, target, from_closure)) ->
        Edge(source, edge, target, from_closure))
  ;;

  let edges_from state (Analysis(forward,_)) = Edge_map.find state forward;;

  let edges_to state (Analysis(_,backward)) = Edge_map.find state backward;;

  let add_edge (from_state, op, to_state, from_closure) (Analysis(forward,backward)) =
    let forward' = Edge_map.add from_state (op, to_state, from_closure) forward in
    let backward' = Edge_map.add to_state (op, from_state, from_closure) backward in
    Analysis(forward', backward')
  ;;

  let has_edge (from_state, op, to_state) (Analysis(forward,_)) =
    Edge_map.mem from_state (op, to_state, true) forward ||
    Edge_map.mem from_state (op, to_state, false) forward
  ;;

  let pp_edge (Edge(from_state,op,to_state,_)) =
    pp_analysis_node from_state ^ " -- " ^ pp_analysis_action op ^
      " --> " ^ pp_analysis_node to_state
  ;;

  let pp_analysis analysis =
    let body =
      analysis
      |> edges_of_analysis
      |> Enum.fold
        (fun a edge -> a ^ "  " ^ pp_edge edge ^ "\n")
        ""
    in
    "{\n" ^ body ^ "}"
  ;;

  let rec make_edges from_node to_node ops =
    match ops with
    | [] -> [(from_node, Nop, to_node)]
    | [op] -> [(from_node, op, to_node)]
    | op::ops' ->
      let middle_node = Local_node(ops', to_node) in
      (from_node, op, middle_node)::
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
          (starting_state, Push symbol, target_state, false)
        )
    in
    List.of_enum @@ Enum.append starting_edges transition_edges
  ;;

  let perform_closure initial_edges =
    let initial_edge_set =
      initial_edges
      |> List.enum
      |> Enum.map (fun (a,b,c,d) -> Edge(a,b,c,d))
      |> Edge_set.of_enum
    in
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
    let closure_step_push_pop from_node op1 op2 to_node =
      match op1,op2 with
      | (Push k1,Pop k2) when equal_symbol k1 k2 ->
        Enum.singleton (from_node, Nop, to_node, true)
      | _ ->
        Enum.empty ()
    in
    let closure_step_nop_any from_node op1 op2 to_node =
      match op1 with
      | Nop ->
        Enum.singleton (from_node, op2, to_node, true)
      | _ ->
        Enum.empty ()
    in
    let closure_step_any_nop from_node op1 op2 to_node =
      match op2 with
      | Nop ->
        Enum.singleton (from_node, op1, to_node, true)
      | _ ->
        Enum.empty ()
    in
    let closure_step_swap from_node op1 op2 to_node =
      begin
        if P.legal_action_swaps op1 op2
        then List.enum (make_edges from_node to_node [op2;op1])
        else Enum.empty ()
      end
      |> Enum.map
        (fun (a,b,c) -> (a,b,c,true))
    in
    let closure_step_jump from_node op1 _ _ =
      match op1 with
      | Jump(jump_state) ->
        Enum.singleton (from_node, Nop, State_node(jump_state), true)
      | _ ->
        Enum.empty ()
    in
    let closure_steps =
      [ closure_step_push_pop
      ; closure_step_nop_any
      ; closure_step_any_nop
      ; closure_step_swap
      ; closure_step_jump
      ]
    in
    let rec perform_closure graph work_remaining =
      if Edge_set.is_empty work_remaining then graph
      else
        let (work, work_remaining') = Edge_set.pop work_remaining in
        let Edge(from_node,op,to_node,from_closure) = work in
        let successor_results =
          edges_from to_node graph
          |> Enum.map
            (fun (op',to_node',_) ->
              closure_steps
              |> List.enum
              |> Enum.map (fun f -> f from_node op op' to_node')
              |> Enum.concat
            )
          |> Enum.concat
        in
        let predecessor_results =
          edges_to from_node graph
          |> Enum.map
            (fun (op',from_node',_) ->
              closure_steps
              |> List.enum
              |> Enum.map (fun f -> f from_node' op' op to_node)
              |> Enum.concat
            )
          |> Enum.concat
        in
        let new_edges =
          Enum.append successor_results predecessor_results
          |> Enum.filter
            (fun (from_node,op,to_node,from_closure) ->
              not @@ has_edge (from_node,op,to_node) graph
            )
          |> Enum.map (fun (from_node,op,to_node,from_closure) ->
                        Edge(from_node,op,to_node,from_closure))
        in
        let work_remaining'' = Edge_set.union work_remaining' @@
                               Edge_set.of_enum new_edges
        in
        let graph' = add_edge (from_node, op, to_node, from_closure) graph in
        perform_closure graph' work_remaining''
    in
    perform_closure empty_analysis initial_edge_set
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
          | (Nop,State_node(s'),_) -> Some s'
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
