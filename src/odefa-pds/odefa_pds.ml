(** This module contains an interface describing the behavior of a push-down
    system (PDS).  A PDS is similar to a PDA in which the input alphabet is
    empty.  This module makes use of terminology from
    "Pushdown Control-Flow Analysis of Higher-Order Programs", which uses PDS to
    defer to a transition mechanism and stack grammar and "rooted PDS" (RPDS) to
    refer to a PDS with a starting state and stack.
*)

open Batteries;;

type 'a pds_action =
  | Push of 'a
  | Pop of 'a
  | Nop

let compare_pds_action compare_symbol a1 a2 =
  match a1,a2 with
  | Push(s1),Push(s2) -> compare_symbol s1 s2
  | Push(_),Pop(_) -> -1
  | Push(_),Nop -> -1
  | Pop(_),Push(_) -> 1
  | Pop(s1),Pop(s2) -> compare_symbol s1 s2
  | Pop(_),Nop -> -1
  | Nop,Push(_) -> 1
  | Nop,Pop(_) -> 1
  | Nop,Nop -> 0
;;

let pp_pds_action pp_symbol action =  
  match action with
  | Push symbol -> "push " ^ pp_symbol symbol
  | Pop symbol -> "pop " ^ pp_symbol symbol
  | Nop -> "nop"
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
  
  val legal_symbol_swaps : symbol -> symbol -> bool
  val pp_state : state -> string
  val pp_symbol : symbol -> string

  val create_pds : pds_desc -> pds
  val root_pds : pds -> state -> symbol -> rpds
  
  val transitions_of_pds :
    pds -> (state * symbol pds_action list * state) Enum.t
  val root_of_rpds : rpds -> state * symbol
  val pds_of_rpds : rpds -> pds
end;;
