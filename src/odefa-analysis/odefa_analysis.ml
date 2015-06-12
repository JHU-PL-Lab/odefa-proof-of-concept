open Batteries;;

open Odefa_analysis_data;;
open Odefa_ast;;
open Odefa_utils;;

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
    Edge_set.add (Edge(out_clause, Annotated_clause(List.last body_cls))) @@
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

module type Context_stack =
sig
  type s
  val empty : s
  val push : clause -> s -> s
  val pop : s -> s
  val is_top : clause -> s -> bool
end;;

module Make(S : Context_stack) =
struct
  type s = S.s;;
  
  let lookup g init_x init_acl0 =
    let rec lookup' x acl0 lookups context : Value_set.t =
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

end;;
