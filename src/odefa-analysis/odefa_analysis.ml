open Batteries;;

open Odefa_analysis_context_stack;;
open Odefa_analysis_dot;;
open Odefa_analysis_graph;;
open Odefa_analysis_lookup;;
open Odefa_analysis_utils;;
open Odefa_ast;;
open Odefa_ast_pretty;;
open Odefa_pda_dot;;
open Odefa_pda_generic;;
open Odefa_pda_operations;;
open Odefa_pda_types;;
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
  type pda_state = State of annotated_clause * S.t;;

  let compare_pda_states (State(acl1,s1)) (State(acl2,s2)) =
    let c = Annotated_clause_ord.compare acl1 acl2 in
    if c <> 0 then c else S.compare s1 s2
  ;;

  let pda_transitions_of_graph e (Graph edges) = 
    (* To perform lookup, we construct an appropriate PDA from the current
       graph and then analyze it for reachability: a reachable node represents
       a possible value of the variable. *)
    let all_vars = find_all_vars e in
    let all_projs = find_all_projection_labels e in
    let enumerate_lookup_stack_values () =
      Enum.clone all_vars
      |> Enum.map
          (fun x ->
            Enum.concat @@ List.enum @@
              [ Enum.singleton @@ Closure_lookup(x)
              ; Enum.clone all_projs
                |> Enum.map (fun l -> Projection(x,l))
              ]
          )
      |> Enum.concat
    in
    (* We will construct the PDA by enumerating the transitions which appear
       within it.  All transitions use epsilon input edges; only the stack
       operations control the PDA. *)
    edges
    |> Edge_set.enum
    |> Enum.map
      (fun (Edge(acl1,acl0)) ->
        match acl1 with
          | Annotated_clause(Clause(x,Value_body(v))) ->
            (* If we're looking for x, we've just found it.  Otherwise,
               keep moving. *)
            (* PERF: Some generalized notion of transition would probably
               help a lot here; we're creating transitions for every
               variable in the program. *)
            enumerate_lookup_stack_values ()
            |> Enum.map
              (fun stack_value ->
                match stack_value with
                  | Closure_lookup(x') ->
                    if x = x'
                    then
                      (* PERF: Should be able to do this in a
                               context-stack-agnostic way. *)
                      S.enumerate e
                      |> Enum.map
                        (fun context_stack ->
                          let from_state = State(acl0,context_stack) in
                          let to_state = State(acl1,context_stack) in
                          (from_state, None, Some stack_value,
                            to_state, [])
                        )
                    else
                      (* PERF: Should be able to do this in a
                               context-stack-agnostic way. *)
                      S.enumerate e
                      |> Enum.map
                        (fun context_stack ->
                          let from_state = State(acl0,context_stack) in
                          let to_state = State(acl1,context_stack) in
                          (from_state, None, Some stack_value,
                            to_state, [stack_value])
                        )
                  | Projection(x,l) ->
                    raise @@ Not_yet_implemented "projection in x=v"
              )
            |> Enum.concat
          | Annotated_clause(Clause(x,Var_body(x'))) ->
            (* If we're looking for x, we now need to look for x'. *)
            (* PERF: Some generalized notion of transition would probably
               help a lot here; we're creating transitions for every
               variable in the program. *)
            enumerate_lookup_stack_values ()
            |> Enum.map
              (fun stack_value ->
                match stack_value with
                  | Closure_lookup(x'') ->
                    if x = x''
                    then
                      (* PERF: Should be able to do this in a
                               context-stack-agnostic way. *)
                      S.enumerate e
                      |> Enum.map
                        (fun context_stack ->
                          let from_state = State(acl0,context_stack) in
                          let to_state = State(acl1,context_stack) in
                          (from_state, None, Some stack_value,
                            to_state, [Closure_lookup(x')])
                        )
                    else
                      (* PERF: Should be able to do this in a
                               context-stack-agnostic way. *)
                      S.enumerate e
                      |> Enum.map
                        (fun context_stack ->
                          let from_state = State(acl0,context_stack) in
                          let to_state = State(acl1,context_stack) in
                          (from_state, None, Some stack_value,
                            to_state, [stack_value])
                        )
                  | Projection(x,l) ->
                    raise @@ Not_yet_implemented "projection lookup in x=v"
              )
            |> Enum.concat
          | Annotated_clause(Clause(_,Projection_body(_,_))) ->
            raise @@ Not_yet_implemented "projection body in lookup"
          | Annotated_clause(Clause(_,Appl_body(_,_)))
          | Annotated_clause(Clause(_,Conditional_body(_,_,_,_))) ->
            (* Lookup does not move backward through non-trivial nodes. *)
            Enum.empty ()
          | Start_clause ->
            (* Nothing to do for edges moving back to the start clause. *)
            Enum.empty ()
          | End_clause ->
            (* How did an edge wind up *after* the end clause? *)
            raise @@ Invariant_failure "end clause has a successor!"
          | Enter_clause(x_param,x_arg,site) ->
            raise @@ Not_yet_implemented "enter clause in lookup"
          | Exit_clause(x_site_var,x_ret,site) ->
            raise @@ Not_yet_implemented "exit clause in lookup"
      )
    |> Enum.concat
  ;;

  (**
     Performs a lookup operation on a graph.
     @param e The expression being analyzed.
     @param g The graph so far.
     @param x The variable for which to find values.
     @param acl The annotated clause from which to start.
  *)
  let lookup e g x acl =
    (* Construct the actual PDA. *)
    let pda = enumerated_pda
                (pda_transitions_of_graph e g)
                (State(acl,S.empty))
                (Closure_lookup(x))
                compare_pda_states
                (fun _ _ -> 0)
                lookup_compare
    in
    (* Extract the reachable values. *)
    reachable_goal_states pda
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
              "DOT file of graph's PDA:\n" ^
              dot_string_of_pda
                ~transition_formatter:
                  (fun pop _ push ->
                    if "[" ^ pop ^ "]" = push
                    then "top:" ^ pop
                    else "-" ^ pop ^ "; +" ^ push
                  )
                (fun (State(acl,stk)) ->
                  "(" ^ pretty_acl acl ^ "," ^ S.pretty stk ^ ")")
                (fun x -> "")
                pretty_lookup
                (enumerated_pda
                  (pda_transitions_of_graph e g)
                  (State(End_clause,S.empty))
                  (Closure_lookup(rv e))
                  compare_pda_states
                  (fun _ _ -> 0)
                  lookup_compare
                )
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
