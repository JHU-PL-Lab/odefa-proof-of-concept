open Batteries;;

open Odefa_analysis;;
open Odefa_analysis_data;;
open Odefa_ast;;

module Stack : Context_stack =
struct
  type s = S of (clause list * Clause_set.t);;
  let empty = S([],Clause_set.empty);;
  let push c (S(c_list,c_set)) =
    if Clause_set.mem c c_set
      then S([c],Clause_set.singleton c)
      else S(c :: c_list, Clause_set.add c c_set)
  ;;
  let pop (S(c_list,c_set)) =
    match c_list with
      | [] -> empty
      | h::t -> S(t, Clause_set.remove h c_set)
  ;;
  let is_top c (S(c_list,c_set)) =
    match c_list with
      | [] -> true
      | h::t -> c = h
  ;;
end;;
