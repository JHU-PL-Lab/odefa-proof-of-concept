open Batteries;;

open Odefa_string_utils;;
open Odefa_utils;;

open Odefa_pds;;
open Odefa_pds_reachability;;

let logger = Odefa_logger.make_logger "Odefa_pds_reachability_impl";;

module Make : Pds_reachability = functor (P : Pds) ->
struct
  module P = P;;
  
  (** The type of node annotations in the summarization graph used in
      {reachable_goal_states}. *)
  type edge_symbol =
    | Push of P.symbol
    | Pop of P.symbol
    | Nop
  ;;

  (** The type of nodes in the summarization graph used in
      {reachable_goal_states}. *)
  type node =
    | State_node of P.state
    | Local_node of edge_symbol list * node
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

  module Node_order =
  struct
    type t = node
    let compare node1 node2 =
      match node1,node2 with
      | State_node(s1),State_node(s2) -> P.State_order.compare s1 s2
      | State_node(_),Local_node(_) -> -1
      | Local_node(_),State_node(_) -> 1
      | Local_node(l1,n1),Local_node(l2,n2) ->
        let c =
          Enum.compare Edge_symbol_order.compare (List.enum l1) (List.enum l2)
        in
        if c <> 0 then c else
          compare n1 n2
  end;;

  module Value_order =
  struct
    type t = node * edge_symbol;;
    let compare = Tuple.Tuple2.compare ~cmp1:Node_order.compare
        ~cmp2:Edge_symbol_order.compare
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

  let pp_analysis_action action =
    match action with
    | Push symbol -> "push " ^ P.pp_symbol symbol
    | Pop symbol -> "pop " ^ P.pp_symbol symbol
    | Nop -> "nop"
  ;;
  
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
      (fun (source, (target, edge)) -> (source, edge, target))
  ;;

  let edges_from state (Analysis(forward,_)) = Edge_map.find state forward;;

  let edges_to state (Analysis(_,backward)) = Edge_map.find state backward;;

  let add_edge (from_state, to_state, op) (Analysis(forward,backward)) =
    let forward' = Edge_map.add from_state (to_state, op) forward in
    let backward' = Edge_map.add to_state (from_state, op) backward in
    Analysis(forward', backward')
  ;;

  let has_edge (from_state, to_state, op) (Analysis(forward,_)) =
    Edge_map.mem from_state (to_state, op) forward
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
        5. (A) -- push k1 --> (B) -- push k2 --> (C) ===>
           (A) -- push k2 --> (B) -- push k1 --> (C)
      Once transitive closure is complete, any edge of the form
        (Start) -- pop initial_stack_symbol --> (X)
      indicates that (X) is an accepting clause for the PDS.  We should then
      report the state backed by (X) as reachable.
    *)

    (*
      The initial edges derived from the PDS.
    *)
    let initial_edges =
      P.transitions_of_pds pds
      |> Enum.map
        (fun (in_state, pops, out_state, pushes) ->
           let operations =
             let push_operations =
               List.map (fun x -> Push x) @@ List.rev pushes
             in
             let pop_operations =
               List.map (fun x -> Pop x) @@ List.rev pops
             in
             (pop_operations @ push_operations)
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
      A function to determine the transitive closure of two edges by their
      operations.
    *)
    let join_ops op1 op2 =
      match op1,op2 with
      | Nop,Nop -> Some [Nop]
      | Nop,_ -> Some [op2]
      | _,Nop -> Some [op1]
      | Push x,Pop x' ->
        if P.Symbol_order.compare x x' <> 0
        then None
        else Some [Nop]
      | Push x,Push x' ->
        if P.legal_symbol_swaps x x'
        then Some [Push x';Push x]
        else None
      | _ -> None
    in

    (*
      Define a function which, given a reachability analysis graph and an edge,
      determines all of the edges which may be transitively closed by adding
      that edge to the graph.
    *)
    let derive_edges_from graph (from_state, to_state, op) =
      let successor_edge_closure =
        edges_from to_state graph
        |> Enum.filter_map
          (fun (successor_state, op') ->
             let op'' = join_ops op op' in
             Option.bind op'' (fun op -> Some(from_state, successor_state, op))
          )
      in
      let predecessor_edge_closure =
        edges_to from_state graph
        |> Enum.filter_map
          (fun (predecessor_state, op') ->
             let op'' = join_ops op' op in
             Option.bind op'' (fun op -> Some(predecessor_state, to_state, op))
          )
      in
      Enum.append successor_edge_closure predecessor_edge_closure
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
      | (edge::edges')::work' ->
        let new_edges = derive_edges_from graph edge in
        let graph' = add_edge edge graph in
        graph_closure graph' @@ new_edges::(edges'::work')
    in

    (* All we have to do now is close an empty graph with the initial edges. *)
    graph_closure empty_analysis [initial_edges]
  ;;

  let reachable_from
      analysis
      initial_state
      initial_symbol =
    (* Filter the graph for the answers we wanted. *)
    let answer =
      edges_from (State_node(initial_state)) analysis
      |> Enum.filter_map
        (function
          | (State_node(s'),Pop(k)) ->
            if P.Symbol_order.compare k initial_symbol = 0
            then
                (*
                  Then this is an edge from the initial state to another state
                  via a single pop of the initial stack symbol.  Thus, this is
                  an accepting state of the PDS.
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
