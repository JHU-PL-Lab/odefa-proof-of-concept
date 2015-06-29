(**
  An NFA library for Odefa.  This library contains a general functor for
  generating operations over an NFA with nodes of any type.
*)

open Batteries;;

open Odefa_utils;;

(**
  The interface to which NFA specifications must conform.  An NFA module can
  be produced for each module meeting this specification.
*)
module type Spec =
sig
  (** The type of states in the NFA. *)
  type state;;
  (** The type of input symbols in the NFA. *)
  type symbol;;

  (** An OrderedType instance for states. *)
  module State_order : BatInterfaces.OrderedType with type t = state;;
  (** An OrderedType instance for symbols. *)
  module Symbol_order : BatInterfaces.OrderedType with type t = symbol;;
end;;

(** A general type for NFAs.  This type provides an interface to interact with
    various NFA implementations. *)
type ('state,'symbol) nfa =
  { nfa_initial_state : 'state
  ; nfa_is_accepting_state : 'state -> bool
  ; nfa_valid_symbols_from : 'state -> 'symbol Enum.t
  ; nfa_transition_from : 'state -> 'symbol -> 'state Enum.t
  ; nfa_state_comparator : 'state -> 'state -> int
  }
;;

(** A function used internally for addition to multi-maps. *)
let multi_map_add
      map_mem_fn
      map_find_fn
      map_add_fn
      set_add_fn
      set_singleton_fn
      key
      value
      map =
  if map_mem_fn key map
  then
    let old_values = map_find_fn key map in
    let new_values = set_add_fn value old_values in
    map_add_fn key new_values map
  else
    map_add_fn key (set_singleton_fn value) map
;;

(**
  A functor to produce an NFA module.
*)
module Make(S : Spec) =
struct
  type state = S.state;;
  type symbol = S.symbol;;

  (* ************ BASE NFA ************ *)
  (* Implementation of NFAs directly using the above state and symbol types. *)
  
  (** An ordering for transitions. *)
  module Transition_order =
  struct
    type t = state * symbol;;
    let compare (st1,sy1) (st2,sy2) =
      let c = S.State_order.compare st1 st2 in
      if c <> 0 then c else S.Symbol_order.compare sy1 sy2
    ;;
  end;;

  (** A data structure describing a set of NFA states. *)
  module State_set = Set.Make(S.State_order);;

  (** A data structure describing a set of NFA symbols. *)
  module Symbol_set = Set.Make(S.Symbol_order);;

  (** A data structure for transitions. *)
  module Transition_map = Map.Make(Transition_order);;

  (** A data structure tracking which symbols are valid in each state. *)
  module State_map = Map.Make(S.State_order);;

  let transition_map_add =
    multi_map_add
      Transition_map.mem Transition_map.find Transition_map.add
      State_set.add State_set.singleton
  ;;

  let state_map_add =
    multi_map_add
      State_map.mem State_map.find State_map.add
      Symbol_set.add Symbol_set.singleton
  ;;

  (** Creates a basic NFA from some prefabricated data structures. *)
  let create_internal
        (transitions : State_set.t Transition_map.t)
        (symbols_per_state : Symbol_set.t State_map.t)
        (initial_state : state)
        (accepting_states : State_set.t) =
    { nfa_initial_state = initial_state
    ; nfa_is_accepting_state =
        (fun st -> State_set.mem st accepting_states)
    ; nfa_valid_symbols_from =
        (fun st ->
          if State_map.mem st symbols_per_state
          then Symbol_set.enum @@ State_map.find st symbols_per_state
          else Enum.empty ()
        )
    ; nfa_transition_from =
        (fun st sy ->
          if Transition_map.mem (st,sy) transitions
          then State_set.enum @@ Transition_map.find (st,sy) transitions
          else Enum.empty ()
        )
    ; nfa_state_comparator = S.State_order.compare
    }
  ;;

  (** Creates a basic NFA from a set of transitions, an initial state, and
      some accepting states. *)
  let create
        (transitions : (state * symbol * state) Enum.t)
        (initial_state : state)
        (accepting_states : state Enum.t) =
    let (transition_map, symbols_per_state) =
      transitions
        |> Enum.fold
            (fun (transition_map,symbols_per_state) (st,sy,st') ->
              let new_transition_map =
                    transition_map_add (st,sy) st' transition_map
              in
              let new_symbols_per_state =
                    state_map_add st sy symbols_per_state
              in
              (new_transition_map, new_symbols_per_state) 
            )
            (Transition_map.empty, State_map.empty)
    in
    create_internal
      transition_map
      symbols_per_state
      initial_state
      (State_set.of_enum accepting_states)
  ;;
end;;

(** Calculates an NFA which recognizes all of the terms from the first
    argument which are not recognized by the second argument. *)
let minus (nfa1 : ('st1,'sy) nfa) (nfa2 : ('st2,'sy) nfa) =
  { nfa_initial_state = (nfa1.nfa_initial_state, nfa2.nfa_initial_state)
  ; nfa_is_accepting_state =
      (fun (st1,st2) ->
        nfa1.nfa_is_accepting_state st1 &&
          (not @@ nfa2.nfa_is_accepting_state st2)
      )
  ; nfa_valid_symbols_from =
      (fun (st1,st2) ->
        raise @@ Not_yet_implemented("nfa_valid_symbols_from for minus")
      )
  ; nfa_transition_from =
      (fun (st1,st2) sy ->
        raise @@ Not_yet_implemented("nfa_transitions_from for minus")
      )
  ; nfa_state_comparator =
      (fun (st1l,st1r) (st2l,st2r) ->
        let c = compare st1l st2l in
        if c <> 0 then compare st1r st2r else c
      )
  }
;;

(** Determines whether an NFA is empty. *)
let is_empty nfa =
  let rec loop exploration_stack visit_set =
    match exploration_stack with
      | [] -> true
      | next::exploration_stack' ->
        (* We have a state to explore. *)
        if nfa.nfa_is_accepting_state next
        then false
        else
          let exploration_stack'',visit_set' =
            nfa.nfa_valid_symbols_from next
            |> Enum.fold
                (fun acc sy ->
                  let new_states = nfa.nfa_transition_from next sy in
                  new_states
                  |> Enum.fold
                      (fun (exploration_stack_acc, visit_set_acc) new_state ->
                        if Set.PSet.mem new_state visit_set_acc
                        then (exploration_stack_acc, visit_set_acc)
                        else
                          ( new_state :: exploration_stack_acc
                          , Set.PSet.add new_state visit_set_acc
                          )
                      ) acc
                )
                (exploration_stack', visit_set)
          in
          loop exploration_stack'' visit_set'
  in
  loop [nfa.nfa_initial_state] (Set.PSet.create nfa.nfa_state_comparator)
;;

(** Determines whether two NFAs are equal in terms of the strings they
    accept. *)
let are_equal nfa1 nfa2 =
  is_empty (minus nfa1 nfa2) && is_empty (minus nfa2 nfa1)
;;
