(** A module containing a DOT file generator for PDAs. *)

open Batteries;;

open Odefa_pds;;
open Odefa_pds_reachability;;
open Odefa_string_utils;;
open Odefa_utils;;

module Make(R : Pds_reachability_sig) =
struct
  let dot_string_of_pds_reachability analysis : string =
    (* We store our dot file in a buffer as it grows. *)
    let buffer = Buffer.create 128 in
    let indent = ref 0 in
    let writeln s =
      (foreach (1 -- !indent) @@ fun _ ->
        Buffer.add_string buffer "    ");
      Buffer.add_string buffer s;
      Buffer.add_string buffer "\n"
    in
    (* Write the header. *)
    writeln "digraph pds {";
    incr indent;
    let seen_nodes =
      ref (Set.PSet.create (compare : string -> string -> int))
    in
    (* For each transition, write each of the elements. *)
    (foreach (R.edges_of_analysis analysis) @@
      function (R.Edge(in_node, action, out_node, from_closure)) ->
        let in_desc = R.pp_analysis_node in_node in
        let out_desc = R.pp_analysis_node out_node in
        let action_desc = R.pp_analysis_action action in
        
        let write_node_if_necessary color desc =
          if not @@ Set.PSet.mem desc !seen_nodes then
          begin
            seen_nodes := Set.PSet.add desc !seen_nodes;
            writeln @@ "\"" ^ desc ^ "\"[color=\"" ^ color ^ "\"];"
          end
        in
        
        let color = if from_closure then "blue" else "black" in
        
        write_node_if_necessary color in_desc;
        write_node_if_necessary color out_desc;
        
        writeln @@ "\"" ^ in_desc ^ "\" -> \"" ^ out_desc ^
                   "\"[label=\"" ^ action_desc ^
                   "\",fontcolor=\"" ^ color ^
                   "\",color=\"" ^ color ^ "\"];";
    );
    (* Write the footer. *)
    decr indent;
    writeln "}";
    Buffer.contents buffer
  ;;
end;;
