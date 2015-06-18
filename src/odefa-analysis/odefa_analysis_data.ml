open Batteries;;

open Odefa_ast;;
open Odefa_ast_pretty;;
open Odefa_string_utils;;

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

type lookup_task =
  (* Indicates that we are now looking up a function.  When we are finished, we
     should resume by looking up this variable in the context where the function
     was defined. *)
  | Function_lookup of var
;;

let pretty_lookup_task t =
  match t with
    | Function_lookup(x) -> pretty_var x
  ;;

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
