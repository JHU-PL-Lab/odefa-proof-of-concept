(** A module containing types and definitions for the Odefa lookup stack. *)

open Batteries;;

open Odefa_ast;;
open Odefa_ast_pretty;;

(** A lookup stack operation. *)
type lookup_stack_operation =
  (** The operation which looks up a given variable in context. *)
  | Lookup_variable of var
  (** The operation which projects from the current variable and looks up the
      projection result. *)
  | Lookup_projection of ident
;;

(** A comparison for lookup stack operations. *)
let lookup_compare = compare;;

(** A pretty-printing function for lookup stack operations. *)
let pretty_lookup op =
  match op with
    | Lookup_variable x -> pretty_var x
    | Lookup_projection l -> "." ^ pretty_ident l
;;
