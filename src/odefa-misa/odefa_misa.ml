(**
  This module contains an interface describing the behavior of a monoidal
  input stack automaton (MISA).  A MISA is a generalization of a PDA.  While a
  PDA recognizes lists formed from the input grammar, the form recognized by a
  MISA is determined by a monoid.  Formally, a PDA is described by:
  
    * S: A set of states.
    * I: A set of input symbols.
    * K: A set of stack symbols.
    * s: An initial state.
    * k: An initial stack symbol.
    * T(s,i,k,s',X): A transition relation, where X is a list drawn from K.

  The MISA adds to the above a monoid:
  
    * M: The set of recognizable elements.
    * m: The unit of the recognizable symbol set.
    * m1+m2=m3: A binary function closed under M.
    * f(i)=m': An injection function, where i is an element of I.

  A PDA is a specialization of a MISA in which the monoid is the free monoid
  over I.  A PDS (push-down system) equivalent to a MISA in which the monoid is
  defined as the trivial monoid over a singleton set.
  
  We use the term "rooted MISA" to refer to a MISA as described above.  An
  "unrooted MISA" is a structure as the above but without specifying an initial
  state or stack symbol.
*)

open Batteries;;
open Odefa_misa_monoid;;

module type Misa =
sig
  type umisa_desc
  
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
  
  type umisa
  type rmisa
  
  val create_umisa : umisa_desc -> umisa
  val root_umisa : umisa -> state -> symbol -> rmisa
  
  val transitions_of_umisa :
    umisa -> (state * input option * symbol option * state * symbol list) Enum.t
  val root_of_rmisa : rmisa -> state * symbol
  val umisa_of_rmisa : rmisa -> umisa
end;;
