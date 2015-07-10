open Batteries;;

open Odefa_ast;;
open Odefa_string_utils;;

let pretty_ident (Ident s) = s;;

let pretty_freshening_stack (Freshening_stack ids) =
  List.fold_left
    (fun acc -> fun i ->
       (* Since freshening stacks are stored in reverse, we reverse the string    *)
       (* here.                                                                   *)
       acc ^ "__" ^ pretty_ident i) "" ids
;;

let pretty_var (Var(i, mfs)) =
  match mfs with
  | None -> pretty_ident i ^ "$"
  | Some fs -> pretty_ident i ^ pretty_freshening_stack fs
;;

let pretty_record_value record_value =
  match record_value with
  | Empty_record_value -> "{}"
  | Degenerate_record_value(is) ->
    concat_sep_delim "{" "}" ", " @@ Enum.map pretty_ident @@
    Ident_set.enum is
  | Proper_record_value(es) ->
    concat_sep_delim "{" "}" ", " @@
    Enum.map (fun (i,x) -> pretty_ident i ^ " = " ^ pretty_var x) @@
    Ident_map.enum es
;;

let rec pretty_function_value (Function_value(x,e)) =
  pretty_var x ^ " -> { " ^ pretty_expr e ^ " }"

and pretty_value v =
  match v with
  | Value_record(r) -> pretty_record_value r
  | Value_function(f) -> pretty_function_value f

and pretty_clause_body b =
  match b with
  | Var_body(x) -> pretty_var x
  | Value_body(v) -> pretty_value v
  | Appl_body(x1,x2) -> pretty_var x1 ^ " " ^ pretty_var x2
  | Conditional_body(x,p,f1,f2) ->
    pretty_var x ^ " ~ " ^ pretty_pattern p ^ " ? " ^
    pretty_function_value f1 ^ " : " ^ pretty_function_value f2
  | Projection_body(x,i) -> pretty_var x ^ "." ^ pretty_ident i

and pretty_clause (Clause(x,b)) =
  pretty_var x ^ " = " ^ pretty_clause_body b

and pretty_expr (Expr(cls)) =
  concat_sep "; " @@ Enum.map pretty_clause @@ List.enum cls

and pretty_pattern p =
  match p with
  | Record_pattern(is) ->
    concat_sep_delim "{" "}" ", " @@ Enum.map pretty_ident @@
    Ident_set.enum is
;;

let brief_pretty_clause (Clause(x,_)) = pretty_var x;;
