(** Generic convenience constructors for PDAs. *)

open Batteries;;

open Odefa_pda_types;;
open Odefa_utils;;

(**
  Creates a PDA from an enumeration of states.  The provided enumeration is
  cloned, so its entire contents are buffered in memory for the lifetime of the
  PDA.  The state and symbol sets are inferred from those mentioned in the
  enumeration of transitions.
*)
let enumerated_pda :
          'state 'input_symbol 'stack_symbol.
          ('state * 'input_symbol option * 'stack_symbol option *
           'state * 'stack_symbol list) Enum.t ->
          'state ->
          'stack_symbol ->
          ('state -> 'state -> int) ->
          ('input_symbol -> 'input_symbol -> int) ->
          ('stack_symbol -> 'stack_symbol -> int) ->
          ('state,'input_symbol,'stack_symbol) pda =
  fun transitions initial_state initial_stack_symbol
        state_comparator input_symbol_comparator stack_symbol_comparator ->
  (* Create a polymorphic set containing the states of the PDA.  We buffer those
     states here so we can eliminate duplicates. *)
  let all_states =
    Enum.clone transitions
    |> Enum.fold
        (fun a (in_state, _, _, out_state, _) ->
          Set.PSet.add in_state @@ Set.PSet.add out_state a)
        (Set.PSet.create state_comparator)
  in
  (* Do the same for input symbols. *)
  let all_input_symbols =
    Enum.clone transitions
    |> Enum.fold
        (fun a (_, input_option, _, _, _) ->
          match input_option with
            | Some input -> Set.PSet.add input a
            | None -> a
        )
        (Set.PSet.create input_symbol_comparator)
  in
  (* Define the PDA itself. *)
  { pda_enumerate_states = (fun () -> Set.PSet.enum all_states)
  ; pda_enumerate_input_symbols = (fun () -> Set.PSet.enum all_input_symbols)
  ; pda_enumerate_transitions = (fun () -> Enum.clone transitions)
  ; pda_transitions_by_state = (fun state ->
      Enum.clone transitions
      |> Enum.filter_map
          (fun (in_state, input, pop_option, out_state, pushes) ->
            if state_comparator in_state state = 0
            then Some (input, pop_option, out_state, pushes)
            else None)
      )
  ; pda_transition = (fun state input top_of_stack ->
      Enum.clone transitions
      |> Enum.filter_map
          (fun (in_state, input_option, pop_option, out_state, pushes) ->
            if state_comparator in_state state <> 0
            then None
            else
              (* Determine if the required input is present (if necessary). *)
              Option.Monad.bind
              (match input_option with
                | None -> Some(false)
                | Some(input') ->
                  if input_symbol_comparator input input' <> 0
                  then Some(true)
                  else None)
              @@ function consumed_input ->
              (* Determine if the required stack is present (if necessary). *)
              Option.Monad.bind
              (match pop_option with
                | None -> Some(false)
                | Some(pop) ->
                  if stack_symbol_comparator pop top_of_stack <> 0
                  then Some(true)
                  else None)
              @@ function consumed_stack ->
              Some(out_state, consumed_input, consumed_stack, pushes)
          )
      )
  ; pda_initial_state = initial_state
  ; pda_initial_stack_symbol = initial_stack_symbol
  ; pda_compare_states = state_comparator
  ; pda_compare_input_symbols = input_symbol_comparator
  ; pda_compare_stack_symbols = stack_symbol_comparator
  }
;;
