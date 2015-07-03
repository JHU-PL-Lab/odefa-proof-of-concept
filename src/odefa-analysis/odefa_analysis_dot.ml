open Batteries;;

open Odefa_analysis_graph;;
open Odefa_ast;;
open Odefa_ast_pretty;;

(* Creates a string representing an Odefa analysis graph as a DOT file. *)
let dot_string_of_graph (Graph edges) =
  (* Determines the DOT identity of a clause. *)
  let dot_id_of_acl acl =
    let name =
      match acl with
      | Annotated_clause(Clause(Var(i,_),_)) -> pretty_ident i
      | Enter_clause(Var(i1,_),Var(i2,_),(Clause(Var(i,_),_))) ->
        pretty_ident i1 ^ "=" ^ pretty_ident i2 ^ "@" ^ pretty_ident i ^ "+"
      | Exit_clause(Var(i1,_),Var(i2,_),(Clause(Var(i,_),_))) ->
        pretty_ident i1 ^ "=" ^ pretty_ident i2 ^ "@" ^ pretty_ident i ^ "-"
      | Start_clause -> "start"
      | End_clause -> "end"
    in
    "\"" ^ name ^ "\""
  in
  (* Get the dot string form of each edge. *)
  let edge_commands =
    edges
    |> Edge_set.enum
    |> Enum.map
      (fun (Edge(acl1,acl2)) -> dot_id_of_acl acl1 ^ " -> " ^
                                dot_id_of_acl acl2)
  in
  (* Get the style commands for each node. *)
  let style_commands =
    edges
    |> Edge_set.enum
    |> Enum.fold
      (fun acc (Edge(acl1,acl2)) ->
         Annotated_clause_set.add acl1 @@ Annotated_clause_set.add acl2 acc)
      Annotated_clause_set.empty
    |> Annotated_clause_set.enum
    |> Enum.map
      (fun acl ->
         let style_code =
           match acl with
           | Annotated_clause(Clause(x,b)) ->
             begin
               match b with
               | Var_body(_) | Value_body(_) | Projection_body(_) ->
                 "style=filled,fillcolor=\"#44ff44\""
               | Appl_body(_,_) | Conditional_body(_,_,_,_) ->
                 "style=filled,fillcolor=\"gray\""
             end
           | Enter_clause(_) | Exit_clause(_) ->
             "style=filled,fillcolor=\"#ff8844\""
           | Start_clause | End_clause ->
             "style=filled,fillcolor=\"#44ff44\""
         in
         dot_id_of_acl acl ^ "[" ^ style_code ^ "]"
      )
  in
  (* Put the DOT file together. *)
  "strict digraph analysis {\n" ^
  "    rankdir=\"LR\"\n" ^
  ( Enum.append edge_commands style_commands
    |> Enum.map (fun s -> "    " ^ s ^ ";\n")
    |> Enum.fold (^) ""
  ) ^
  "}\n"
;;
