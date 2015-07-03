(** A module containing a DOT file generator for PDAs. *)

open Batteries;;

open Odefa_pda_types;;
open Odefa_string_utils;;
open Odefa_utils;;

let dot_string_of_pda
      ?transition_formatter:
        (transition_formatter=fun x y z -> x ^ " ; " ^ y ^ " ; " ^ z) 
      string_of_state
      string_of_input
      string_of_stack
      pda
      : string =
  (* We store our dot file in a buffer as it grows. *)
  let buffer = Buffer.create 128 in
  let indent = ref 0 in
  let writeln s =
    (foreach (1 -- !indent) @@ fun _ ->
      Buffer.add_string buffer "    ");
    Buffer.add_string buffer s;
    Buffer.add_string buffer "\n"
  in
  (* We'll need to come up with names for the dot nodes. *)
  let state_cntr = ref 0 in
  let next_state () =
    let x = !state_cntr in
    state_cntr := x + 1;
    string_of_int x
  in
  (* Each state will be emitted as we encounter it; its dot name will be stored
     in a mapping. *)
  let state_map = ref @@ Map.empty in
  let dot_id_of_state s =
    let m = !state_map in
    if Map.mem s m
    then Map.find s m
    else
      let i = next_state () in
      state_map := Map.add s i m;
      writeln @@ i ^ "[label=\"" ^ string_of_state s ^ "\"];";
      i
  in
  (* Write the header. *)
  writeln "digraph pda {";
  incr indent;
  (* For each transition, write each of the elements. *)
  (foreach (pda.pda_enumerate_transitions ()) @@
    function (in_state, input_option, pop_option, out_state, pushes) ->
      let in_id = dot_id_of_state in_state in
      let out_id = dot_id_of_state out_state in
      (* TODO: edge annotations *)
      let pop_string = Option.map_default string_of_stack "-" pop_option in
      let input_string = Option.map_default string_of_input "-" input_option in
      let push_string = concat_sep_delim "[" "]" "," @@
                          Enum.map string_of_stack @@ List.enum pushes
      in
      writeln @@ in_id ^ " -> " ^ out_id ^ "[label=\"" ^
                  transition_formatter pop_string input_string push_string ^
                  "\"];";
  );
  (* Write the footer. *)
  decr indent;
  writeln "}";
  Buffer.contents buffer
;;