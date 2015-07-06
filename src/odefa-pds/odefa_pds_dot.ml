(** A module containing a DOT file generator for PDAs. *)

open Batteries;;

open Odefa_pds;;
open Odefa_string_utils;;
open Odefa_utils;;

module type Spec =
sig
  module P : Pds

  val pretty_state : P.state -> string
  val pretty_symbol : P.symbol -> string
end;;

module Make(S : Spec) =
struct
  let dot_string_of_pds
        ?transition_formatter:
          (transition_formatter=fun pop pushes -> pop ^ ";" ^ pushes)
        pds 
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
    (* Each state will be emitted as we encounter it; its dot name will be
       stored in a mapping. *)
    let state_map = ref @@ Map.empty in
    let dot_id_of_state s =
      let m = !state_map in
      if Map.mem s m
      then Map.find s m
      else
        let i = next_state () in
        state_map := Map.add s i m;
        writeln @@ i ^ "[label=\"" ^ S.pretty_state s ^ "\"];";
        i
    in
    (* Write the header. *)
    writeln "digraph pds {";
    incr indent;
    (* For each transition, write each of the elements. *)
    (foreach (S.P.transitions_of_pds pds) @@
      function (in_state, pop_option, out_state, pushes) ->
        let in_id = dot_id_of_state in_state in
        let out_id = dot_id_of_state out_state in
        (* TODO: edge annotations *)
        let pop_string = Option.map_default S.pretty_symbol "-" pop_option in
        let push_string = concat_sep_delim "[" "]" "," @@
                            Enum.map S.pretty_symbol @@ List.enum pushes
        in
        writeln @@ in_id ^ " -> " ^ out_id ^ "[label=\"" ^
                    transition_formatter pop_string push_string ^
                    "\"];";
    );
    (* Write the footer. *)
    decr indent;
    writeln "}";
    Buffer.contents buffer
  ;;
end;;
