(** This module provides a functor which may be used in NFA canonicalization.
    The functor receives an NFA type, a generating data type, and a function
    from the latter onto the former.  The module provides a function to
    canonicalize the generating type: generating values are equated by the
    equivalence of their respective NFAs.  The canonicalizing function takes a
    generating value and returns the first generating value whose NFA is
    equivalent to the NFA of its argument. *)
    
open Batteries;;

open Hashtbl;;

open Odefa_nfa;;

module type Spec =
sig
  (** The type which is used to generate the NFAs. *)
  type t;;
  (** The state type of the NFAs. *)
  type state;;
  (** The symbol type of the NFAs. *)
  type symbol;;
  (** The generating function. *)
  val generate_nfa : t -> (state,symbol) nfa;;

  (** An ordering on the generating type. *)
  module Gen_ord : BatInterfaces.OrderedType with type t = t;;
  (** A hash on the generating type. *)
  module Gen_hash : Hashtbl.HashedType with type t = t;;
  (** An ordering on the state type. *)
  module State_ord : BatInterfaces.OrderedType with type t = state;;
  (** An ordering on the symbol type. *)
  module Symbol_ord : BatInterfaces.OrderedType with type t = symbol;;
end;;

module Make(S : Spec) =
struct
  module Gen_hashtbl = Hashtbl.Make(S.Gen_hash);;
  
  module Nfa_spec =
  struct
    type state = S.state
    type symbol = S.symbol
    module State_order = S.State_ord;;
    module Symbol_order = S.Symbol_ord;;
  end;;

  module Nfa = Odefa_nfa.Make(Nfa_spec);;

  let gen_answer_cache : S.t Gen_hashtbl.t = Gen_hashtbl.create 10;;

  let viewed_nfa_answers : ((S.state,S.symbol) nfa * S.t) list ref = ref [];;

  let canonicalize x =
    if Gen_hashtbl.mem gen_answer_cache x
    then Gen_hashtbl.find gen_answer_cache x
    else
      let nfa = S.generate_nfa x in
      let answer =
        !viewed_nfa_answers
          |> List.enum
          |> Enum.filter_map
              (fun (nfa',answer) ->
                if Odefa_nfa.are_equal nfa nfa'
                then Some answer
                else None)
          |> Enum.get
          |> Option.default x
      in
      viewed_nfa_answers := (nfa,answer) :: !viewed_nfa_answers;
      Gen_hashtbl.add gen_answer_cache x answer;
      answer
end;;
