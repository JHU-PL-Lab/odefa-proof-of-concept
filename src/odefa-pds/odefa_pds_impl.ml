(** An implementation of a PDS. *)

open Batteries;;

open Odefa_pds;;

module type Sig =
sig
  type state
  type symbol
  
  module State_order : Interfaces.OrderedType with type t = state
  module Symbol_order : Interfaces.OrderedType with type t = symbol
  
  val pp_state : state -> string
  val pp_symbol : symbol -> string
  
  val legal_symbol_swaps : symbol -> symbol -> bool
end;;

module Make(S : Sig) :
  Pds
    with type pds_desc =
                  (S.state * S.symbol list * S.state * S.symbol list) Enum.t
     and type state = S.state
     and type symbol = S.symbol
  =
struct
  type pds_desc = (S.state * S.symbol list * S.state * S.symbol list) Enum.t
  type state = S.state;;
  type symbol = S.symbol;;
  
  module State_order = S.State_order;;
  module Symbol_order = S.Symbol_order;;

  let pp_state = S.pp_state;;
  let pp_symbol = S.pp_symbol;;
  
  type pds = Pds of (state * symbol list * state * symbol list) Enum.t
  type rpds = Rooted_pds of pds * state * symbol
  
  let legal_symbol_swaps = S.legal_symbol_swaps;;
  
  let create_pds e = Pds e;;
  let root_pds pds st sy = Rooted_pds(pds,st,sy);;

  let transitions_of_pds (Pds e) = Enum.clone e;;
  let root_of_rpds (Rooted_pds(_,st,sy)) = (st,sy);;
  let pds_of_rpds (Rooted_pds(pds,_,_)) = pds;;
end;;
