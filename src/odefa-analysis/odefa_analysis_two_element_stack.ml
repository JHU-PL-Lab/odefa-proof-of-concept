open Batteries;;

open Odefa_analysis_context_stack;;
open Odefa_analysis_graph;;
open Odefa_analysis_utils;;
open Odefa_ast;;
open Odefa_ast_pretty;;
open Odefa_string_utils;;

module Stack : Context_stack =
struct
  type t = S of clause list;;
  let compare = compare;;
  let empty = S([]);;
  let push c (S(lst)) =
    match lst with
      | [] -> S([c])
      | h::t -> S([c;h])
  ;;
  let pop (S(lst)) =
    match lst with
      | [] -> S([])
      | h::t -> S(t)
  ;;
  let is_top c (S(c_option)) =
    match c_option with
    | h::t -> c = h
    | [] -> true
  ;;
  let pretty (S(lst)) =
    let pfx = List.fold_left (fun a c -> a ^ pretty_clause c ^ "|") "" lst in
    pfx ^ "?"
  ;;
  let enumerate e : t Enum.t =
    let context_clauses = extract_context_clauses e in
    let context_clauses' = Enum.clone context_clauses in
    context_clauses
    |> Enum.map
        (fun c ->
          Enum.clone context_clauses'
          |> Enum.map (fun c' -> [c;c'])
          |> Enum.append (Enum.singleton [c])
        )
    |> Enum.concat
    |> Enum.append (Enum.singleton [])
    |> Enum.map (fun x -> S(x))
  ;;
end;;
