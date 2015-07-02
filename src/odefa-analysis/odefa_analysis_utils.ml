(** A module containing utility functions for analyses. *)

open Batteries;;

open Odefa_ast;;

(** Obtain the set of all variables appearing within an expression. *)
let rec find_all_vars (Expr cls) =
  cls
  |> List.enum
  |> Enum.map
      (fun (Clause(x,r)) ->
        match r with
          | Value_body(_) -> Enum.singleton x
          | Var_body(x') -> List.enum [x;x']
          | Appl_body(x',x'') -> List.enum [x;x';x'']
          | Conditional_body(x,_,f1,f2) ->
            Enum.concat @@ List.enum @@
              [ Enum.singleton x
              ; find_all_vars_in_fn f1
              ; find_all_vars_in_fn f2
              ]
          | Projection_body(x',_) -> List.enum [x;x']
      )
  |> Enum.concat

and find_all_vars_in_fn (Function_value(x,e)) =
  Enum.append (Enum.singleton x) @@ find_all_vars e
;;

(** Retrieve the set of "context clauses" appearing anywhere within an
    expression.  These are clauses which may be pushed onto the context stack
    during analysis. *)
let rec extract_context_clauses (Expr cls) =
  let immediate =
    cls
    |> List.enum
    |> Enum.filter
      (function
        | Clause(_,Appl_body(_,_)) -> true
        | Clause(_,Conditional_body(_,_,_,_)) -> true
        | _ -> false)
  in
  let inner =
    cls
    |> List.enum
    |> Enum.filter_map
      (function
        | Clause(_,Value_body(Value_function(Function_value(_,e)))) ->
          Some (extract_context_clauses e)
        | _ -> None)
    |> Enum.concat
  in
  Enum.append immediate inner
;;
