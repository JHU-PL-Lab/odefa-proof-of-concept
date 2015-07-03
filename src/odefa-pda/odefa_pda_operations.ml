(**
  An aliasing module to export the interesting parts of operations over PDAs.
*)

open Batteries;;

open Odefa_utils;;

let reachable_goal_states =
  Odefa_pda_operations_reachability.reachable_goal_states
;;