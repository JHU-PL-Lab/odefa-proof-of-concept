open Batteries;;

open Odefa_analysis_context_stack;;
open Odefa_analysis_dot;;
open Odefa_analysis_graph;;
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
  
  type context_stack = S.t;;
  let equal_context_stack c1 c2 = S.compare c1 c2 = 0;;
  let compare_context_stack = S.compare;;

  type pds_state =
    | State of annotated_clause * context_stack
    [@@deriving eq,ord]
  ;;

  let pretty_state (State(acl,context_stack)) =
    pretty_acl acl ^ "@" ^ S.pretty context_stack
  ;;

  (** Types and definitions for the Odefa lookup stack. *)

  (** A lookup stack operation. *)
  type lookup_stack_operation =
    (** The operation which looks up a given variable in context. *)
    | Lookup_variable of var
    (** The operation which projects from the current variable and looks up the
        projection result. *)
    | Lookup_projection of ident
    (** The value found by a subordinate lookup.. *)
    | Lookup_value of value
    (** The operation which captures the variable of a subordinate lookup. *)
    | Lookup_capture
    [@@deriving eq,ord]
  ;;

  let lookup_compare = compare_lookup_stack_operation;;

  (** A pretty-printing function for lookup stack operations. *)
  let pretty_lookup op =
    match op with
    | Lookup_variable x -> pretty_var x
    | Lookup_projection l -> "." ^ pretty_ident l
    | Lookup_value (value) -> "value = `" ^ pretty_value value ^ "'"
    | Lookup_capture -> "CAPTURE"
  ;;

  module Analysis_pds = Odefa_pds_impl.Make(
    struct
      type state = pds_state
      type symbol = lookup_stack_operation
      
      module State_order =
      struct
        type t = state
        let compare = compare_pds_state
      end;;

      module Symbol_order =
      struct
        type t = symbol
        let compare = lookup_compare
      end;;

      let pp_state = pretty_state;;
      let pp_symbol = pretty_lookup;;

      let legal_action_swaps s1 s2 =
        match s1,s2 with
        | (Jump _, Push(Lookup_value _))
        | (Push(Lookup_value _), Jump _) -> true
        | _ -> false
      ;;
    end
  );;

  module Analysis_pds_reachability =
    Odefa_pds_reachability_impl.Make(Analysis_pds)
  ;;

  module Analysis_pds_dot = Odefa_pds_dot.Make(Analysis_pds_reachability);;

  type analysis_pda_transition =
    pds_state * (pds_state,lookup_stack_operation) pds_action list * pds_state
  ;;

  let pds_transitions_of_graph e ((Graph edges) as g)
      : analysis_pda_transition Enum.t =
    (* To perform lookup, we construct an appropriate PDA from the current
       graph and then analyze it for reachability: a reachable node represents
       a possible value of the variable. *)
    let all_vars = find_all_vars e in
    let all_values = find_all_values e in
    let all_projs = find_all_projection_labels e in
    let all_clauses = clauses_of_graph g in
    let enumerate_lookup_stack_values () =
      Enum.concat @@ List.enum
        [ Enum.clone all_vars |> Enum.map (fun x -> Lookup_variable x)
        ; Enum.clone all_projs |> Enum.map (fun l -> Lookup_projection l)
        ; Enum.clone all_values |> Enum.map (fun v -> Lookup_value v)
        ; Enum.singleton Lookup_capture
        ]
    in
    logger `trace
      ("All vars: " ^
        pretty_list pretty_var (List.of_enum @@ Enum.clone all_vars));
    (* We will construct the PDA by enumerating the transitions which appear
       within it.  All transitions use epsilon input edges; only the stack
       operations control the PDA. *)
    let all_lookup_operations =
      List.of_enum @@ enumerate_lookup_stack_values ()
    in
    let all_edges =
      edges
      |> Edge_set.enum
      |> List.of_enum
    in
    List.cartesian_product all_lookup_operations all_edges
    |> List.enum
    |> Enum.map
      (fun (lookup_operation, Edge(acl1,acl0)) ->
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
               (from_state, [Pop lookup_operation; Push lookup_operation],
                to_state)
            )
        in
        (* Match lookup operation and acl1 simultaneously -- the following cases
           mix and match which of them seem to be more important, so it doesn't
           make sense to pick one over the other. *)
        match (lookup_operation, acl1) with
        | (Lookup_variable x, Annotated_clause(Clause(x',Value_body(v)))) ->
          if Var_order.compare x x' = 0
          then
            (* We found a value!  Pop the lookup frame. *)
            S.enumerate e
            |> Enum.map
              (fun context_stack ->
                 let from_state = State(acl0,context_stack) in
                 let to_state = State(acl1,context_stack) in
                 (from_state, [Pop lookup_operation], to_state)
              )
          else edges_for_skip ()
        | (Lookup_variable x, Annotated_clause(Clause(x',Var_body(x'')))) ->
          (* If we were looking for x', we should now look for x''. *)
          if Var_order.compare x x' = 0
          then
            S.enumerate e
            |> Enum.map
              (fun context_stack ->
                 let from_state = State(acl0,context_stack) in
                 let to_state = State(acl1,context_stack) in
                 (from_state,
                  [Pop lookup_operation; Push (Lookup_variable x'')],
                  to_state)
              )
          else edges_for_skip ()
        | (Lookup_variable x, Annotated_clause(Clause(x', Projection_body(x'', l)))) ->
          if Var_order.compare x x' = 0
          then
            S.enumerate e
            |> Enum.map
              (fun context_stack ->
                 let from_state = State(acl0,context_stack) in
                 let to_state = State(acl1,context_stack) in
                 (from_state,
                  [ Pop lookup_operation
                  ; Push (Lookup_projection l)
                  ; Push (Lookup_variable x'')],
                  to_state)
              )
          else
            (* This projection doesn't affect us; move on. *)
            edges_for_skip ()
        | (Lookup_variable x, Annotated_clause(Clause(x', Appl_body(_,_))))
        | (Lookup_variable x, Annotated_clause(Clause(x', Conditional_body(_,_,_,_)))) ->
          (* In this case, we should add an edge to skip backwards into
             this node *only if* we're not looking for the variable it
             defines.  If we *are* looking for that variable, a wiring
             node will handle this case. *)
          if Var_order.compare x x' = 0
          then Enum.empty ()
          else edges_for_skip ()
        | (_, Start_clause) ->
          (* Nothing to do for edges moving back to the start
             clause. *)
          Enum.empty ()
        | (_, End_clause) ->
          (* How did an edge wind up *after* the end clause? *)
          raise @@ Invariant_failure "end clause has a successor!"
        | (Lookup_variable x, Enter_clause(x_param,x_arg,site)) ->
          (* This case covers both non-locals and arguments.  Which
             one we use depends on whether the variable we are looking
             for matches the parameter variable or not.  In any case,
             we add no edges unless the top of the context matches
             that of the call site. *)
          S.enumerate e
          |> Enum.filter_map
            (fun context_stack ->
              match site with
              | Clause(_, Appl_body(x_func,_)) ->
                if not @@ S.is_top site context_stack
                then None
                else
                  let context_stack' = S.pop context_stack in
                  let from_state = State(acl0, context_stack) in
                  let to_state = State(acl1, context_stack') in
                  if Var_order.compare x x_param = 0
                  then
                    (* We're looking for the function's parameter.
                       Continue as in the variable clause case above,
                       but pop the call stack.  If we can't pop the
                       call stack, then we bail. *)
                    Some (from_state,
                          [Pop lookup_operation; Push (Lookup_variable x_arg)],
                          to_state)
                  else
                    (*
                      We're looking for a non-local in a
                      function.  To do this correctly, we need to
                      pause our search for current variable and go
                      find possible definitions of the function;
                      our variable will be defined in that
                      function's closure.
                    *)
                    Some (from_state,
                          [Pop lookup_operation
                          ;Push lookup_operation
                          ;Push (Lookup_variable x_func)],
                          to_state)
              | Clause(_, Conditional_body _) ->
                let from_state = State(acl0, context_stack) in
                let to_state = State(acl1, context_stack) in
                if Var_order.compare x x_param = 0
                then
                  Some (from_state,
                        [Pop lookup_operation; Push (Lookup_variable x_arg)],
                        to_state)
                else
                  Some (from_state, [], to_state)
              | _ ->
                raise @@ Invariant_failure
                  "Unrecognized call site type in entrance wiring node"
            )
        | (_, Exit_clause(x_site, x_ret, site)) ->
            begin
              S.enumerate e
              |> Enum.map
                (fun context_stack ->
                   match site with
                   | Clause (_, Appl_body (function_variable, _)) ->
                     begin
                       match lookup_operation with
                       | Lookup_value (value) ->
                         begin
                           match value with
                           | Value_function (
                               Function_value (_, Expr (clauses))
                             ) ->
                             let Clause(return_variable, _) = List.last clauses in
                             (* We're looking up the variable defined by this
                                call site.  We need to push the site onto our
                                context stack and move into the function in
                                question. *)
                             if Var_order.compare x_ret return_variable = 0 then
                               let context_stack' = S.push site context_stack in
                               let from_state = State(acl0,context_stack) in
                               let to_state = State(acl1,context_stack') in
                               Enum.singleton
                                 (from_state,
                                  [ Pop lookup_operation
                                  ; Pop (Lookup_variable x_site)
                                  ; Push (Lookup_variable x_ret)],
                                  to_state)
                             else
                               Enum.empty ()
                           | _ ->
                             Enum.empty ()
                         end
                       | Lookup_variable x ->
                         if Var_order.compare x x_site = 0
                         then
                           let from_state = State(acl0,context_stack) in
                           let to_state = State(Annotated_clause (site),context_stack) in
                           Enum.singleton
                             (from_state,
                              [ Pop lookup_operation
                              ; Push lookup_operation
                              ; Jump from_state
                              ; Push (Lookup_capture)
                              ; Push (Lookup_variable function_variable)
                              ],
                              to_state)
                         else
                           (* We're not looking for the value returned by this
                              invocation, so we should just skip the whole thing.
                              (This is actually necessary for correctness. *)
                           edges_for_skip ()
                       | _ ->
                         Enum.empty ()
                     end
                   | Clause (_, Conditional_body _) ->
                     (* Conditionals don't need to use the call stack
                        because the _functions_ that represent the
                        match and anti-match branches can only be
                        called from one site, so their calls and
                        returns are automatically aligned. *)
                     begin
                       match lookup_operation with
                       | Lookup_variable x ->
                         if Var_order.compare x x_site = 0
                         then
                           let from_state = State(acl0,context_stack) in
                           let to_state = State(acl1,context_stack) in
                           Enum.singleton
                             (from_state,
                              [ Pop lookup_operation
                              ; Push (Lookup_variable x_ret)],
                              to_state)
                         else
                           edges_for_skip ()
                       | _ ->
                         Enum.empty ()
                     end
                   | _ ->
                     raise @@ Invariant_failure "Something other than a function call or a conditional showed up as a call site."
                )
              |> Enum.concat
            end
        | (Lookup_projection l, _) ->
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
                      (state,
                       [ Pop lookup_operation
                       ; Push (Lookup_variable (Ident_map.find l im))
                       ],
                       state)
                   )
               | _ ->
                 Enum.empty ()
            )
          |> Enum.concat
        | (Lookup_capture, Annotated_clause(Clause(_,Value_body v))) ->
          S.enumerate e
          |> Enum.map
            (fun context_stack ->
              let state = State(acl1,context_stack) in
              (state,
               [ Pop lookup_operation
               ; Push (Lookup_value v)
               ],
               state)
            )
        | (Lookup_capture, _)
        | (Lookup_value _, _) ->
          Enum.empty ()
      )
    |> Enum.concat
  ;;

  let lookup_reachability_cache_point :
      graph ->
      (unit -> Analysis_pds_reachability.analysis) ->
      Analysis_pds_reachability.analysis =
    create_single_point_cache ~cmp:compare_graph
  ;;

  let all_starting_points e g =
    let all_vars = find_all_vars e in
    let all_contexts = S.enumerate e in
    let all_clauses = clauses_of_graph g in
    let result =
      Enum.clone all_vars
      |> Enum.map
        (fun x ->
          Enum.clone all_contexts
          |> Enum.map
            (fun context ->
              all_clauses
              |> Annotated_clause_set.enum
              |> Enum.map
                (fun acl ->
                  let state = State(acl,context) in
                  let symbol = Lookup_variable x in
                  (state,symbol)
                )
            )
          |> Enum.concat
        )
      |> Enum.concat
    in
    result
  ;;

  (**
     Performs a lookup operation on a graph.
     @param e The expression being analyzed.
     @param g The graph so far.
     @param x The variable for which to find values.
     @param acl The annotated clause from which to start.
  *)
  let lookup e g x acl =
    let answer =
      (* Construct the actual PDA.  Use caching if possible. *)
      let pds_reachability_analysis =
        lookup_reachability_cache_point g @@
        fun () ->
          let transitions = pds_transitions_of_graph e g in
          Analysis_pds.create_pds transitions (all_starting_points e g)
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
    in
    logger `debug @@
      "Lookup of " ^ pretty_var x ^ " from " ^ pretty_acl acl ^
      " yields the following values: " ^
      (answer |> Value_set.enum |> Enum.map pretty_value |>
        concat_sep ", ");
    answer
  ;;

  (**
     Performs deductive closure on a given graph.
     @param e The expression being analyzed.
     @param init_g The graph to close.
  *)
  let perform_graph_closure e init_g =
    (* Some sanity-checking logs. *)
    S.enumerate e
    |> Enum.iter
      (fun s -> logger `debug @@ "Possible context stack: " ^ S.pretty s);
    (* The single-step graph closure function. *)
    let step (Graph edges as g) =
      (* A simple logging utillity. *)
      let log_new_edges new_edges =
        let distinct_edges = Edge_set.diff new_edges edges in
        logger `debug @@
          "Learned " ^ (string_of_int @@ Edge_set.cardinal new_edges) ^
          " edge(s), of which " ^
          (string_of_int @@ Edge_set.cardinal distinct_edges) ^ " is/are new";
        new_edges
      in
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
              log_new_edges @@
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
              log_new_edges @@
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
      if equal_graph g g'
      then (logger `debug "Graph closure completed!"; g')
      else (logger `debug "Graph closure continuing..."; close g')
    in
    close init_g
  ;;

  let pds_dot_string_of_graph e g =
    let transitions = pds_transitions_of_graph e g in
    let pds = Analysis_pds.create_pds transitions (all_starting_points e g) in
    let reachability = Analysis_pds_reachability.analyze_pds pds in
    Analysis_pds_dot.dot_string_of_pds_reachability reachability
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
