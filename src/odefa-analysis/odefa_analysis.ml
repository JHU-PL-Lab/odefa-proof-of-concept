open Batteries;;

open Odefa_analysis_context_stack;;
open Odefa_analysis_dot;;
open Odefa_analysis_graph;;
open Odefa_analysis_lookup;;
open Odefa_analysis_utils;;
open Odefa_ast;;
open Odefa_ast_pretty;;
open Odefa_pds;;
open Odefa_pds_dot;;
open Odefa_pds_reachability;;
open Odefa_cache_utils;;
open Odefa_string_utils;;
open Odefa_utils;;

let logger = Odefa_logger.make_logger "Odefa_analysis";;

let rv (Expr(cls)) =
  let Clause(x,_) = List.last cls in x
;;

let wire
    (Graph(edges) as g)
    cl
    (Function_value(x_param,(Expr(body_cls) as body)))
    x_arg
    x_ret =
  let in_clause = Enter_clause(x_param, x_arg, cl) in
  let out_clause = Exit_clause(x_ret, rv body, cl) in
  let body_edges = acls_to_edges @@ cls_to_acls body_cls in
  let with_inner_wire_edges =
    Edge_set.add (Edge(in_clause, Annotated_clause(List.first body_cls))) @@
    Edge_set.add (Edge(Annotated_clause(List.last body_cls), out_clause)) @@
    body_edges
  in
  let outer_input_wire_edges =
    Enum.map (fun acl -> Edge(acl,in_clause)) @@ preds g (Annotated_clause cl)
  in
  let outer_output_wire_edges =
    Enum.map (fun acl -> Edge(out_clause,acl)) @@ succs g (Annotated_clause cl)
  in
  Enum.fold (fun s e -> Edge_set.add e s) with_inner_wire_edges @@
  Enum.append outer_input_wire_edges outer_output_wire_edges
;;

let is_active g acl =
  let rec close_ancestors acls_found acls_to_explore =
    logger `debug @@
    "Closing ancestors of " ^ pretty_acl acl ^ ": " ^
    pretty_acls acls_found ^ "  /  " ^ pretty_acls acls_to_explore;
    let acls_found' = Annotated_clause_set.union acls_found acls_to_explore in
    let acls_to_explore' =
      acls_to_explore
      |> Annotated_clause_set.enum
      |> Enum.map (preds g)
      |> Enum.concat
      |> Enum.filter
        (fun acl -> match acl with
           | Annotated_clause(Clause(_,Var_body(_)))
           | Annotated_clause(Clause(_,Value_body(_)))
           | Annotated_clause(Clause(_,Projection_body(_)))
           | Enter_clause(_,_,_)
           | Exit_clause(_,_,_)
           | Start_clause
           | End_clause -> true
           | Annotated_clause(Clause(_,Appl_body(_,_)))
           | Annotated_clause(Clause(_,Conditional_body(_,_,_,_))) -> false)
      |> Enum.filter
        (fun acl -> not @@ Annotated_clause_set.mem acl acls_found)
      |> Annotated_clause_set.of_enum
    in
    if Annotated_clause_set.is_empty acls_to_explore' then
      acls_found'
    else 
      close_ancestors acls_found' acls_to_explore'
  in
  let ancestors_of_acl =
    close_ancestors Annotated_clause_set.empty @@
    Annotated_clause_set.singleton acl
  in
  logger `debug @@
  "Ancestors of " ^ pretty_acl acl ^ " are: " ^ pretty_acls ancestors_of_acl;
  Annotated_clause_set.mem Start_clause ancestors_of_acl
;;

module Make(S : Context_stack) =
struct
  type pds_state = State of annotated_clause * S.t;;

  module Analysis_pds = Odefa_pds_impl.Make(
    struct
      type state = pds_state
      type symbol = lookup_stack_operation
      
      module State_order =
      struct
        type t = state
        let compare (State(acl1,ctx1)) (State(acl2,ctx2)) =
          chain_compare Annotated_clause_ord.compare acl1 acl2 @@
            S.compare ctx1 ctx2
      end;;

      module Symbol_order =
      struct
        type t = symbol
        let compare = lookup_compare
      end;;
    end
  );;

  module Analysis_pds_reachability =
    Odefa_pds_reachability_impl.Make(Analysis_pds)
  ;;

  module Analysis_pds_dot = Odefa_pds_dot.Make(
    struct
      module P = Analysis_pds;;
      let pretty_state (State(acl,context_stack)) =
        pretty_acl acl ^ "@" ^ S.pretty context_stack
      ;;
      let pretty_symbol = pretty_lookup;;
    end
  );;

  type analysis_pda_transition =
    pds_state * lookup_stack_operation option *
    pds_state * lookup_stack_operation list
  ;;

  let pds_transitions_of_graph e ((Graph edges) as g)
      : analysis_pda_transition Enum.t =
    (* To perform lookup, we construct an appropriate PDA from the current
       graph and then analyze it for reachability: a reachable node represents
       a possible value of the variable. *)
    let all_vars = find_all_vars e in
    let all_projs = find_all_projection_labels e in
    let all_clauses = clauses_of_graph g in
    let enumerate_lookup_stack_values () =
      Enum.append
        (Enum.clone all_vars |> Enum.map (fun x -> Lookup_variable x))
        (Enum.clone all_projs |> Enum.map (fun l -> Lookup_projection l))
    in
    (* We will construct the PDA by enumerating the transitions which appear
       within it.  All transitions use epsilon input edges; only the stack
       operations control the PDA. *)
    enumerate_lookup_stack_values ()
    |> Enum.map
      (fun lookup_operation ->
         match lookup_operation with
         | Lookup_variable x ->
           (* At each flow transition, we need to react appropriately. *)
           edges
           |> Edge_set.enum
           |> Enum.map
             (fun (Edge(acl1,acl0)) ->
                (*
                  A utility function for handling cases in which the lookup
                  operation is unaffected by a clause.  This function assumes
                  that the context stack does not change.
                *)
                let edges_for_skip () =
                  (* PERF: Modify the data structure to allow this to be done in
                           a lookup-operation-agnostic way? *)
                  S.enumerate e
                  |> Enum.map
                    (fun context_stack ->
                       let from_state = State(acl0,context_stack) in
                       let to_state = State(acl1,context_stack) in
                       (from_state, Some lookup_operation,
                        to_state, [lookup_operation])
                    )
                in
                (* The transitions generated are based on the form of clause. *)
                match acl1 with
                | Annotated_clause(Clause(x',Value_body(v))) ->
                  if Var_order.compare x x' = 0
                  then
                    (* We found a value!  Pop the lookup frame. *)
                    S.enumerate e
                    |> Enum.map
                      (fun context_stack ->
                         let from_state = State(acl0,context_stack) in
                         let to_state = State(acl1,context_stack) in
                         (from_state, Some lookup_operation,
                          to_state, [])
                      )
                  else edges_for_skip ()
                | Annotated_clause(Clause(x',Var_body(x''))) ->
                  (* If we were looking for x', we should now look for x''. *)
                  if Var_order.compare x x' = 0
                  then
                    S.enumerate e
                    |> Enum.map
                      (fun context_stack ->
                         let from_state = State(acl0,context_stack) in
                         let to_state = State(acl1,context_stack) in
                         (from_state, Some lookup_operation,
                          to_state, [Lookup_variable x''])
                      )
                  else edges_for_skip ()
                | Annotated_clause(Clause(x', Projection_body(x'', l))) ->
                  if Var_order.compare x x' = 0
                  then
                    S.enumerate e
                    |> Enum.map
                      (fun context_stack ->
                         let from_state = State(acl0,context_stack) in
                         let to_state = State(acl1,context_stack) in
                         (from_state, Some lookup_operation,
                          to_state, [ Lookup_variable x''
                                    ; Lookup_projection l])
                      )
                  else
                    (* This projection doesn't affect us; move on. *)
                    edges_for_skip ()
                | Annotated_clause(Clause(x', Appl_body(_,_)))
                | Annotated_clause(Clause(x', Conditional_body(_,_,_,_))) ->
                  (* In this case, we should add an edge to skip backwards into
                     this node *only if* we're not looking for the variable it
                     defines.  If we *are* looking for that variable, a wiring
                     node will handle this case. *)
                  if Var_order.compare x x' = 0
                  then Enum.empty ()
                  else edges_for_skip ()
                | Start_clause ->
                  (* Nothing to do for edges moving back to the start
                     clause. *)
                  Enum.empty ()
                | End_clause ->
                  (* How did an edge wind up *after* the end clause? *)
                  raise @@ Invariant_failure "end clause has a successor!"
                | Enter_clause(x_param,x_arg,site) ->
                  (* This case covers both non-locals and arguments.  Which
                     one we use depends on whether the variable we are looking
                     for matches the parameter variable or not.  In any case,
                     we add no edges unless the top of the context matches
                     that of the call site. *)
                  S.enumerate e
                  |> Enum.filter_map
                    (fun context_stack ->
                       if not @@ S.is_top site context_stack
                       then None
                       else
                         let context_stack' = S.pop context_stack in
                         let from_state = State(acl0,context_stack) in
                         let to_state = State(acl1,context_stack') in
                         if Var_order.compare x x_param = 0
                         then
                           (* We're looking for the function's parameter.
                              Continue as in the variable clause case above,
                              but pop the call stack.  If we can't pop the
                              call stack, then we bail. *)
                           Some (from_state, Some lookup_operation,
                                 to_state, [Lookup_variable x_arg])
                         else
                           begin
                              (*
                                We're looking for a non-local variable.  The
                                manner in which we find the appropriate context
                                in which to start depends on what kind of clause
                                we came from.
                              *)
                             match site with
                             | Clause(_,Appl_body(x_func,_)) ->
                                (*
                                  We're looking for a non-local in a
                                  function.  To do this correctly, we need to
                                  pause our search for current variable and go
                                  find possible definitions of the function;
                                  our variable will be defined in that
                                  function's closure.
                                *)
                               Some (from_state, Some lookup_operation,
                                     to_state, [ Lookup_variable x_func
                                               ; lookup_operation ])
                             | Clause(_,Conditional_body(_,_,_,_)) ->
                                (*
                                  This is just like the situation above, except
                                  that functions in conditionals must be
                                  embedded directly in the clause.  This means
                                  that we're already in the closure of our
                                  variable, so we can just proceed in the outer
                                  context by looking for the same thing.
                                *)
                               Some (from_state, Some lookup_operation,
                                     to_state, [lookup_operation])
                             | _ ->
                               raise @@ Invariant_failure
                                 "non-application, non-conditional call site in wiring?"
                           end
                    )
                | Exit_clause(x_site, x_ret, site) ->
                  if Var_order.compare x x_site = 0
                  then
                    begin
                      (* We're looking up the variable defined by this call
                         site.  We need to push the site onto our context
                         stack and move into the function in question. *)
                      S.enumerate e
                      |> Enum.map
                        (fun context_stack ->
                           let context_stack' = S.push site context_stack in
                           let from_state = State(acl0,context_stack) in
                           let to_state = State(acl1,context_stack') in
                           (from_state, Some lookup_operation,
                            to_state, [Lookup_variable x_ret])
                        )
                    end
                  else
                    (* We're not looking for the value returned by this
                       invocation, so we should just skip the whole thing.
                       (This is actually necessary for correctness. *)
                    edges_for_skip ()
             )
           |> Enum.concat
         | Lookup_projection l ->
            (*
              If a projection is appropriate, then the transition is actually
              from each clause onto *itself*; that is, projection does not
              change the program point in the same way that variable lookup
              does.  This is a slight departure from the formalism, which
              partially couples projection and variable lookup.

              If the next action is a projection, then we've presumably just
              looked up the variable from which we want to project.  This means
              that we're at a clause defining a record value; if we're not, then
              we're stuck.
            *)
           all_clauses
           |> Annotated_clause_set.enum
           |> Enum.map
             (fun acl ->
                (* The only kind of clause from which we can project a label is
                   a proper record value clause with that label.  In other
                   cases, we're stuck. *)
                match acl with
                | Annotated_clause(Clause(_,Value_body(Value_record(
                    Proper_record_value(im)))))
                  when Ident_map.mem l im ->
                  S.enumerate e
                  |> Enum.map
                    (fun context_stack ->
                       let state = State(acl,context_stack) in
                       (state, Some lookup_operation,
                        state, [Lookup_variable (Ident_map.find l im)])
                    )
                | _ ->
                  Enum.empty ()
             )
           |> Enum.concat
      )
    |> Enum.concat
  ;;

  let lookup_reachability_cache_point :
      graph ->
      (unit -> Analysis_pds_reachability.analysis) ->
      Analysis_pds_reachability.analysis =
    create_single_point_cache ~cmp:graph_compare
  ;;

  (**
     Performs a lookup operation on a graph.
     @param e The expression being analyzed.
     @param g The graph so far.
     @param x The variable for which to find values.
     @param acl The annotated clause from which to start.
  *)
  let lookup e g x acl =
    (* Construct the actual PDA.  Use caching if possible. *)
    let pds_reachability_analysis =
      lookup_reachability_cache_point g @@
      fun () ->
        pds_transitions_of_graph e g
        |> Analysis_pds.create_pds
        |> Analysis_pds_reachability.analyze_pds
    in
    (* Extract the reachable values. *)
    let reachable_states =
      Analysis_pds_reachability.reachable_from
        pds_reachability_analysis
        (State(acl,S.empty))
        (Lookup_variable x)
    in
    reachable_states
    |> Enum.filter_map
      (fun (State(acl,_)) ->
         match acl with
         | Annotated_clause(Clause(_,Value_body(v))) -> Some v
         | _ -> None
      )
    |> Value_set.of_enum
  ;;

  (**
     Performs deductive closure on a given graph.
     @param e The expression being analyzed.
     @param init_g The graph to close.
  *)
  let perform_graph_closure e init_g =
    let step (Graph edges as g) =
      (* For each annotated clause appearing in the graph, determine what we
         can learn from it. *)
      let new_edges : Edge_set.t =
        clauses_of_graph g
        |> Annotated_clause_set.enum
        |> Enum.filter (is_active g)
        |> Enum.map (fun acl ->
            match acl with
            | Annotated_clause(Clause(x1,Appl_body(x2,x3)) as cl) ->
              logger `debug
                ("Learning edges from application: " ^ pretty_acl acl);
              (* Confirm that the argument has a concrete value backing
                 it (that is, that the argument isn't provably
                 divergent). *)
              if Value_set.is_empty @@ lookup e g x3 acl then
                Edge_set.empty
              else
                (* Wire each function in. *)
                lookup e g x2 acl
                |> Value_set.enum
                |> Enum.filter_map
                  (fun v ->
                     match v with
                     | Value_function(f) -> Some(wire g cl f x3 x1)
                     | _ -> None)
                |> Enum.fold Edge_set.union Edge_set.empty
            | Annotated_clause(
                Clause(x1,Conditional_body(x2,p,f1,f2)) as cl) ->
              logger `debug
                ("Learning edges from conditional: " ^ pretty_acl acl);
              (* Each argument either matches the pattern or does not.
                 We merely need to establish for each of these two cases
                 whether any argument appears; then, we expand the
                 appropriate function(s). *)
              let vs = lookup e g x2 acl in
              let (match_exists,antimatch_exists) =
                vs
                |> Value_set.enum
                |> Enum.fold
                  (fun (y,n) v -> let match_here = match v with
                       | Value_record(r) ->
                         begin
                           match r with
                           | Empty_record_value ->
                             begin
                               match p with
                               | Record_pattern(labels) ->
                                 Ident_set.is_empty labels
                             end
                           | Degenerate_record_value(labels)  ->
                             begin
                               match p with
                               | Record_pattern(labels') ->
                                 Ident_set.subset labels' labels
                             end
                           | Proper_record_value(entries) ->
                             begin
                               match p with
                               | Record_pattern(labels') ->
                                 Ident_set.subset labels' @@
                                 Ident_set.of_enum @@ Ident_map.keys entries
                             end
                         end
                       | Value_function(_) ->
                         false
                     in
                     if match_here then (true,n) else (y,true)
                  )
                  (false,false)
              in
              Edge_set.union
                ( if match_exists then
                    wire g cl f1 x2 x1
                  else
                    Edge_set.empty
                )
                ( if antimatch_exists then
                    wire g cl f2 x2 x1
                  else
                    Edge_set.empty
                )
            | _ ->
              Edge_set.empty
          )
        |> Enum.fold Edge_set.union Edge_set.empty
      in
      Graph (Edge_set.union edges new_edges)
    in
    let rec close g =
      logger `debug @@
      "Graph closure step begins: " ^ pretty_graph g;
      let g' = step g in
      logger `debug @@
      "Graph closure step result: " ^ pretty_graph g';
      logger `debug @@
      "Result is from " ^ pretty_graph g;
      if graph_equal g g'
      then (logger `debug "Graph closure completed!";
            logger `debug (
              "DOT file of resulting graph:\n" ^
              dot_string_of_graph g');
            logger `debug (
              "DOT file of graph's PDS:\n" ^
              Analysis_pds_dot.dot_string_of_pds
                (Analysis_pds.create_pds @@ pds_transitions_of_graph e g)
            );
            g')
      else (logger `debug "Graph closure continuing..."; close g')
    in
    close init_g
  ;;

  (**
     Tests a graph to determine if it is inconsistent.
     @param e The expression being analyzed.
     @param g The graph to test.
  *)
  let test_graph_inconsistency e g =
    clauses_of_graph g
    |> Annotated_clause_set.enum
    |> Enum.filter (is_active g)
    (* For each clause, determine if it triggers an inconsistency. *)
    |> Enum.exists (fun acl -> match acl with
        | Annotated_clause(Clause(_,Appl_body(x2,x3))) ->
          (* Application clauses are inconsistent if the
             called variable might not be a function. *)
          lookup e g x2 acl
          |> Value_set.enum
          |> Enum.exists (fun v -> match v with
              | Value_function(_) -> false
              | _ -> true)
        | Annotated_clause(Clause(_,Projection_body(x2,i))) ->
          (* Projection clauses are inconsistent if the projection subject is
             not a proper record or does not have the specified field. *)
          lookup e g x2 acl
          |> Value_set.enum
          |> Enum.exists (fun v -> match v with
              | Value_record(Proper_record_value(es)) ->
                not @@ Ident_map.mem i es
              | _ -> true)
        | _ ->
          (* No other clauses cause inconsistencies. *)
          false
      )
  ;;

  let becomes_stuck e =
    let g = graph_of_expr e in
    let g' = perform_graph_closure e g in
    test_graph_inconsistency e g'
  ;;
end;;
