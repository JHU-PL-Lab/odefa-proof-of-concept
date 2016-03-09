(** This module contains an interface describing the behavior of a push-down
    system (PDS).  A PDS is similar to a PDA in which the input alphabet is
    empty.  This module makes use of terminology from
    "Pushdown Control-Flow Analysis of Higher-Order Programs", which uses PDS to
    defer to a transition mechanism and stack grammar and "rooted PDS" (RPDS) to
    refer to a PDS with a starting state and stack.
*)

open Batteries;;

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

  val create_pds : pds_desc -> pds
  val root_pds : pds -> state -> symbol -> rpds
  
  val transitions_of_pds :
    pds -> (state * symbol list * state * symbol list) Enum.t
  val root_of_rpds : rpds -> state * symbol
  val pds_of_rpds : rpds -> pds
end;;
