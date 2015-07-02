open Batteries;;

open Odefa_analysis_context_stack;;
open Odefa_analysis_graph;;
open Odefa_analysis_utils;;
open Odefa_ast;;
open Odefa_ast_pretty;;
open Odefa_string_utils;;

module Stack : Context_stack =
struct
  type t = S of clause option;;
  let compare = compare;;
  let empty = S(None);;
  let push c _ = S(Some(c));;
  let pop c = S(None);;
  let is_top c (S(c_option)) =
    match c_option with
      | Some c' -> c = c'
      | None -> true
  ;;
  let pretty = function
    | S(Some(c)) -> pretty_clause c ^ "|?"
    | S(None) -> "?"
  ;;
  let enumerate e : t Enum.t =
    extract_context_clauses e
    |> Enum.map (fun c -> S(Some c))
    |> Enum.append (Enum.singleton @@ S(None))
  ;;
end;;
