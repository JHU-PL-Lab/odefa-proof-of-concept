open Batteries;;

open Odefa_analysis_context_stack;;
open Odefa_analysis_dot;;
open Odefa_analysis_graph;;
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

module Make(S : Context_stack) =
struct
  type t = S.t;;

  type visit = Visit of var * annotated_clause * S.t;;

  module Visit_set = Set.Make(
    struct
      type t = visit
      let compare = compare
    end
    );;

  let lookup g init_x init_acl0 =
    raise @@ Not_yet_implemented "lookup"
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
              logger `debug
                ("Learning edges from application: " ^ pretty_acl acl);
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
              logger `debug
                ("Learning edges from conditional: " ^ pretty_acl acl);
              (* Each argument either matches the pattern or does not.
                 We merely need to establish for each of these two cases
                 whether any argument appears; then, we expand the
                 appropriate function(s). *)
              let vs = lookup g x2 acl in
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
            g')
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
        | Annotated_clause(Clause(_,Appl_body(x2,x3))) ->
          (* Application clauses are inconsistent if the
             called variable might not be a function. *)
          lookup g x2 acl
          |> Value_set.enum
          |> Enum.exists (fun v -> match v with
              | Value_function(_) -> false
              | _ -> true)
        | Annotated_clause(Clause(_,Projection_body(x2,i))) ->
          (* Projection clauses are inconsistent if the projection subject is
             not a proper record or does not have the specified field. *)
          lookup g x2 acl
          |> Value_set.enum
          |> Enum.exists (fun v -> match v with
              | Value_record(Proper_record_value(es)) -> not @@ Ident_map.mem i es
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
