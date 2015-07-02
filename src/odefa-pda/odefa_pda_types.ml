(**
  This module contains an interface to which PDAs are expected to conform.
  These PDAs accept by empty stack rather than by finish state.
*)

open Batteries;;

open Odefa_utils;;

type ('state,'input_symbol,'stack_symbol) pda =
  { pda_enumerate_states : unit -> 'state Enum.t
  ; pda_enumerate_input_symbols : unit -> 'input_symbol Enum.t
  ; pda_enumerate_transitions :
      unit -> ('state * 'input_symbol option * 'stack_symbol option *
               'state * 'stack_symbol list) Enum.t
  ; pda_transitions_by_state :
      'state -> ('input_symbol option * 'stack_symbol option *
                 'state * 'stack_symbol list) Enum.t
  ; pda_transition :
      'state -> 'input_symbol option -> 'stack_symbol option ->
          ('state * 'stack_symbol list) Enum.t
  ; pda_initial_state : 'state
  ; pda_initial_stack_symbol : 'stack_symbol
  ; pda_compare_states : 'state -> 'state -> int
  ; pda_compare_input_symbols : 'input_symbol -> 'input_symbol -> int
  ; pda_compare_stack_symbols : 'stack_symbol -> 'stack_symbol -> int
  }
;;
