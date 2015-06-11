open Batteries;;

open Odefa_ast;;

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

type lookup_task =
  (* Indicates that we are now looking up a function.  When we are finished, we
     should resume by looking up this variable in the context where the function
     was defined. *)
  | Function_lookup of var
;;
