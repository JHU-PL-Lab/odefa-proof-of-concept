(** This module contains an interface describing the behavior of a push-down
    system (PDS).  A PDS is similar to a PDA in which the input alphabet is
    empty.  This module makes use of terminology from
    "Pushdown Control-Flow Analysis of Higher-Order Programs", which uses PDS to
    defer to a transition mechanism and stack grammar and "rooted PDS" (RPDS) to
    refer to a PDS with a starting state and stack.
*)

open Batteries;;

type ('state,'symbol) pds_action =
  | Push of 'symbol
  | Pop of 'symbol
  | Nop
  | Jump of 'state
  [@@deriving eq,ord]
;;

let pp_pds_action pp_state pp_symbol action =  
  match action with
  | Push symbol -> "push " ^ pp_symbol symbol
  | Pop symbol -> "pop " ^ pp_symbol symbol
  | Nop -> "nop"
  | Jump state -> "jump(" ^ pp_state state ^ ")"
;;

module type Pds =
sig
  type pds_desc
  
  type state
  type symbol
  
  module State_order : Interfaces.OrderedType with type t = state
  module Symbol_order : Interfaces.OrderedType with type t = symbol
  
  type pds
  type rpds
  
  val legal_action_swaps :
    (state,symbol) pds_action -> (state,symbol) pds_action -> bool
  val pp_state : state -> string
  val pp_symbol : symbol -> string

  val create_pds : pds_desc -> (state * symbol) Enum.t -> pds
  val root_pds : pds -> state -> symbol -> rpds
  
  val transitions_of_pds :
    pds -> (state * (state,symbol) pds_action list * state) Enum.t
  val starters_of_pds :
    pds -> (state * symbol) Enum.t
  val root_of_rpds : rpds -> state * symbol
  val pds_of_rpds : rpds -> pds
end;;
