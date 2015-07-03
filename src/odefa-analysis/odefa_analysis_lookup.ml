(** A module containing types and definitions for the Odefa lookup stack. *)

open Batteries;;

open Odefa_ast;;
open Odefa_ast_pretty;;

(** The grammar of lookup stack operations. *)
type lookup_stack_operation =
  (** An operation to look up a variable in the current closure.  This is used
      both for the initial lookup and for non-local variables. *)
  | Closure_lookup of var
  (** An operation to project a label from a variable's values. *)
  | Projection of var * ident
;;

(** A comparison for lookup stack operations. *)
let lookup_compare = compare;;

(** A pretty-printing function for lookup stack operations. *)
let pretty_lookup = function
  | Closure_lookup(x) -> "?" ^ pretty_var x
  | Projection(x,l) -> pretty_var x ^ "." ^ pretty_ident l
;;
