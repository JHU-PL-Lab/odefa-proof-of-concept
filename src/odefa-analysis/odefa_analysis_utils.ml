(** A module containing utility functions for analyses. *)

open Batteries;;

open Odefa_ast;;

(* The clauses pushed onto a context stack are always invokable clauses
   appearing within original expression (provided here).  Begin by finding
   a set of those. *)
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
