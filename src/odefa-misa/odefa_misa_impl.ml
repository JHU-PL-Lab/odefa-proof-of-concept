(** An implementation of a PDS. *)

open Batteries;;

open Odefa_misa_monoid;;
open Odefa_misa;;

module type Sig =
sig
  type state
  type input
  type symbol
  type recognized
  
  module State_order : Interfaces.OrderedType with type t = state
  module Input_order : Interfaces.OrderedType with type t = input
  module Symbol_order : Interfaces.OrderedType with type t = symbol
  module Recognized_order : Interfaces.OrderedType with type t = recognized
  module Recognized_monoid : Monoid with type t = recognized
  
  val inject : input -> recognized
end;;

module Make(S : Sig) :
  Misa
    with type umisa_desc =
        (S.state * S.input option * S.symbol option *
          S.state * S.symbol list) Enum.t
     and type state = S.state
     and type input = S.input
     and type symbol = S.symbol
     and type recognized = S.recognized
  =
struct
  type umisa_desc = (S.state * S.input option * S.symbol option *
                     S.state * S.symbol list) Enum.t
  type state = S.state;;
  type input = S.input;;
  type symbol = S.symbol;;
  type recognized = S.recognized;;
  
  module State_order = S.State_order;;
  module Input_order = S.Input_order;;
  module Symbol_order = S.Symbol_order;;
  module Recognized_order = S.Recognized_order;;
  module Recognized_monoid = S.Recognized_monoid;;

  let inject = S.inject;;
  
  type umisa = Misa of (state * input option * symbol option *
                        state * symbol list) Enum.t
  type rmisa = Rooted_misa of umisa * state * symbol
  
  let create_umisa e = Misa e;;
  let root_umisa umisa st sy = Rooted_misa(umisa,st,sy);;

  let transitions_of_umisa (Misa e) = Enum.clone e;;
  let root_of_rmisa (Rooted_misa(_,st,sy)) = (st,sy);;
  let umisa_of_rmisa (Rooted_misa(umisa,_,_)) = umisa;;
end;;
