open Batteries;;

open Odefa_analysis_data;;
open Odefa_ast;;
open Odefa_ast_pretty;;
open Odefa_string_utils;;
open Odefa_utils;;

let logger = Odefa_logger.make_logger "Odefa_analysis";;

let rv (Expr(cls)) =
  let Clause(x,_) = List.last cls in x
  ;;

let cls_to_acls cls = List.map (fun cl -> Annotated_clause cl) cls;;

let rec acls_to_edges acls =
  match acls with
    | h1::h2::acls' -> Edge_set.add (Edge(h1,h2)) @@ acls_to_edges (h2::acls')
    | _ -> Edge_set.empty
  ;;

let preds (Graph(edges)) c0 =
  edges
    |> Edge_set.enum
    |> Enum.filter_map (fun (Edge(c1,c2)) -> if c2 = c0 then Some c1 else None)
  ;;

let succs (Graph(edges)) c0 =
  edges
    |> Edge_set.enum
    |> Enum.filter_map (fun (Edge(c1,c2)) -> if c1 = c0 then Some c2 else None)
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
                          | Enter_clause(_,_,_)
                          | Exit_clause(_,_,_)
                          | Start_clause
                          | End_clause -> true
                          | _ -> false)
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

let clauses_of_graph (Graph edges) =
  edges
    |> Edge_set.enum
    |> Enum.fold (fun s (Edge(acl1,acl2)) ->
                        Annotated_clause_set.add acl1 @@
                        Annotated_clause_set.add acl2 s)
                 Annotated_clause_set.empty
;;

let graph_of_expr (Expr cls) =
  let acls = cls_to_acls cls in
  let edges = acls_to_edges acls in
  let edges' = Edge_set.add (Edge(Start_clause, List.first acls)) @@
               Edge_set.add (Edge(List.last acls, End_clause)) @@ edges in
  Graph edges'
;;

module type Context_stack =
sig
  type t
  val compare : t -> t -> int
  val empty : t
  val push : clause -> t -> t
  val pop : t -> t
  val is_top : clause -> t -> bool
  val pretty : t -> string
end;;

module Make(S : Context_stack) =
struct
  type t = S.t;;

(*
  module Visit_set = Set.Make(
    struct
      type t = S.t * annotated_clause
      let compare = compare
    end
  );;
*)

  let lookup g init_x init_acl0 =
    let rec lookup' x acl0 lookups context : Value_set.t =
      Odefa_logger.bracket_log logger `debug
        ("lookup' (" ^ pretty_var x ^ ") (" ^ pretty_acl acl0 ^ ") " ^
          pretty_list pretty_lookup_task lookups ^ " " ^ S.pretty context)
        (fun vs -> concat_sep_delim "{" "}" ", " @@ Enum.map pretty_value @@
          Value_set.enum vs)
      @@ fun () ->
      (* For each predecessor of acl0, we need to look at the form of the
         clause. *)
      preds g acl0
        |> Enum.map
            (fun acl1 ->
              match acl1 with
                | Start_clause | End_clause ->
                    Value_set.empty
                | Annotated_clause(Clause(x_here,b)) ->
                    if x_here = x then
                      begin
                        match b with
                          | Value_body(v) ->
                              begin
                                match lookups with
                                  | [] ->
                                    (* Rule #1 applies: this is the answer! *)
                                    Value_set.singleton v
                                  | Function_lookup(x1) :: lookups_t ->
                                      begin
                                        match v with
                                          | Value_function(_) ->
                                              (* Rule #2 applies: we've found
                                                 the definition of the function
                                                 and can proceed looking for the
                                                 next variable in its
                                                 closure. *)
                                              lookup' x1 acl1 lookups_t context
                                          | _ ->
                                              (* We found a non-function value,
                                                 so the "function" has no
                                                 closure and we stop our lookup.
                                                 This can happen if the function
                                                 in question was a union type;
                                                 though this would generally
                                                 indicate a type error, that
                                                 will be caught later and should
                                                 not be handled here. *)
                                              Value_set.empty
                                      end
                              end
                          | Var_body(x') ->
                              (* This is a simple alias clause (as opposed to
                                 a wiring clause) that matches our variable.
                                 Rule #3 applies. *)
                              lookup' x' acl1 lookups context
                          | Appl_body(_,_) | Conditional_body(_,_,_,_) ->
                              (* We do not move backward through these nodes. *)
                              Value_set.empty
                      end
                    else
                      (* Rule #6 applies: this clause has nothing to do with
                         us, so move on. *)
                      lookup' x acl1 lookups context
                | Enter_clause(x_here,x_from,site) ->
                    if S.is_top site context then
                      begin
                        if x = x_here then
                          (* Rule #4 applies: we're looking for a function's
                             parameter, so defer to the argument at the call
                             site. *)
                          lookup' x_from acl1 lookups @@ S.pop context
                        else
                          let Clause(_,site_body) = site in
                          match site_body with
                            | Appl_body(x_f,_) ->
                                (* Rule #7 applies: we've reached the entry
                                   point of a function but we're not looking
                                   for its argument, so we must be looking for
                                   something defined in the function's closure.
                                   Go look up the function and then look up the
                                   variable where the function was defined. *)
                                lookup' x_f acl1
                                  ((Function_lookup x) :: lookups) @@
                                  S.pop context
                            | Conditional_body(_,_,_,_) ->
                                (* Rule #8 applies: we're looking for a
                                   non-local variable in a conditional clause.
                                   Because the functions of conditional clauses
                                   are defined immediately within the clause,
                                   we're already at its definition and so simply
                                   proceed by looking up that variable. *)
                                lookup' x acl1 lookups @@ S.pop context
                            | Value_body(_) | Var_body(_) ->
                                raise (Invariant_failure (
                                  "Discovered non-invocation clause as call site!"))
                      end
                    else
                      (* This entry point doesn't correspond to our context.
                         We can skip it. *)
                      Value_set.empty
                | Exit_clause(x_here,x_from,site) ->
                    if x_here = x then
                      (* Rule #5 applies: we're looking at a function's
                         return value, so defer to the function body. *)
                      lookup' x_from acl1 lookups @@ S.push site context
                    else
                      (* This exit point doesn't correspond to our lookup
                         variable.  We can skip the entire function body because
                         there's no way that it will affect the thing we are
                         trying to look up (due to lack of side effects!). *)
                      Value_set.empty
            )
        |> Enum.fold Value_set.union Value_set.empty
    in
    lookup' init_x init_acl0 [] S.empty
  ;;

  let perform_graph_closure init_g =
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
                    (* Confirm that the argument has a concrete value backing
                       it (that is, that the argument isn't provably
                       divergent). *)
                    if Value_set.is_empty @@ lookup g x3 acl then
                      Edge_set.empty
                    else
                      (* Wire each function in. *)
                      lookup g x2 acl
                        |> Value_set.enum
                        |> Enum.filter_map
                              (fun v ->
                                match v with
                                  | Value_function(f) -> Some(wire g cl f x3 x1)
                                  | _ -> None)
                        |> Enum.fold Edge_set.union Edge_set.empty
                | Annotated_clause(
                    Clause(x1,Conditional_body(x2,p,f1,f2)) as cl) ->
                    (* Each argument either matches the pattern or does not.
                       We merely need to establish for each of these two cases
                       whether any argument appears; then, we expand the
                       appropriate function(s). *)
                    let vs = lookup g x2 acl in
                    let (match_exists,antimatch_exists) =
                      vs
                        |> Value_set.enum
                        |> Enum.fold
                            (fun (y,n) v -> match v with
                              | Value_record(Record_value(labels)) ->
                                  begin
                                    match p with
                                      | Record_pattern(labels') ->
                                          if Ident_set.subset labels' labels
                                            then (true,n)
                                            else (y,true)
                                  end
                              | Value_function(_) ->
                                  (y,true)
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
        then (logger `debug "Graph closure completed!"; g')
        else (logger `debug "Graph closure continuing..."; close g')
    in
    close init_g
  ;;

  let test_graph_inconsistency g =
    clauses_of_graph g
      |> Annotated_clause_set.enum
      |> Enum.filter (is_active g)
          (* For each clause, determine if it triggers an inconsistency. *)
      |> Enum.exists (fun acl -> match acl with
                        | Annotated_clause(Clause(x1,Appl_body(x2,x3))) ->
                            (* Application clauses are inconsistent if the
                               called variable might not be a function. *)
                            lookup g x2 acl
                              |> Value_set.enum
                              |> Enum.exists (fun v -> match v with
                                    | Value_function(_) -> false
                                    | _ -> true)
                        | _ ->
                            (* No other clauses cause inconsistencies. *)
                            false
                     )
  ;;

  let becomes_stuck e =
    let g = graph_of_expr e in
    let g' = perform_graph_closure g in
    test_graph_inconsistency g'
  ;;
end;;
