(** A module containing types and definitions for the Odefa lookup stack. *)

open Batteries;;

open Odefa_ast;;
open Odefa_ast_pretty;;

(** The operation to perform after a lookup stack operation discovers the
    variable for which it is looking. *)
type lookup_stack_action =
  (** The non-action value which is taken by e.g. looking up a variable in
      closure. *)
  | No_action
  (** Indicates that, upon reaching a value, the value should be a record and
      we should continue lookup by projecting a label from it and finding
      values for that variable. *)
  | Projection_action of ident
;;

(** A lookup stack operation. *)
type lookup_stack_operation =
  | Lookup of var * lookup_stack_action
;;

(** A comparison for lookup stack operations. *)
let lookup_compare = compare;;

(** A pretty-printing function for lookup stack operations. *)
let pretty_lookup (Lookup(x,action)) =
  pretty_var x ^
  match action with
    | No_action -> ""
    | Projection_action l -> "." ^ pretty_ident l
;;
