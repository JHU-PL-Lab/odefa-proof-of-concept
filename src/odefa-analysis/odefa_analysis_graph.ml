open Batteries;;

open Odefa_ast;;
open Odefa_ast_pretty;;
open Odefa_string_utils;;
open Odefa_utils;;

(* ****************** BASIC DATA STRUCTURES ****************** *)

module Value_ord =
struct
  type t = value
  let compare = compare
end;;

module Value_set = Set.Make(Value_ord);;

type annotated_clause =
  | Annotated_clause of clause
  | Enter_clause of var * var * clause
  | Exit_clause of var * var * clause
  | Start_clause
  | End_clause
;;

module Clause_ord =
struct
  type t = clause
  let compare = compare
end;;

module Clause_set = Set.Make(Clause_ord);;

module Annotated_clause_ord =
struct
  type t = annotated_clause
  let compare = compare
end;;

module Annotated_clause_set = Set.Make(Annotated_clause_ord);;

type edge =
  | Edge of annotated_clause * annotated_clause
;;

module Edge_ord =
struct
  type t = edge
  let compare = compare
end;;

module Edge_set = Set.Make(Edge_ord);;

type graph = Graph of Edge_set.t;;

let graph_equal (Graph es1) (Graph es2) =
  Edge_set.equal es1 es2
;;

(* ****************** GRAPH OPERATIONS ****************** *)

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

(* ****************** PRETTY PRINTING ****************** *)

let pretty_acl acl =
  match acl with
  | Annotated_clause(cl) -> pretty_clause cl
  | Enter_clause(x,x',Clause(x'',_)) ->
    pretty_var x ^ " = " ^ pretty_var x' ^ " @ " ^ pretty_var x'' ^ "+"
  | Exit_clause(x,x',Clause(x'',_)) ->
    pretty_var x ^ " = " ^ pretty_var x' ^ " @ " ^ pretty_var x'' ^ "-"
  | Start_clause -> "start"
  | End_clause -> "end"
;;

let pretty_acls acls =
  concat_sep_delim "{" "}" ", " @@ Enum.map pretty_acl @@
  Annotated_clause_set.enum acls
;;

let pretty_edge (Edge(acl1,acl2)) =
  pretty_acl acl1 ^ " --> " ^ pretty_acl acl2
;;

let pretty_edges edges =
  concat_sep_delim "{" "}" ", " @@ Enum.map pretty_edge @@ Edge_set.enum edges
;;

let pretty_graph (Graph edges) = pretty_edges edges;;
