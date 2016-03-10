open Batteries;;

open OUnit2

open Odefa_string_utils;;

open Odefa_pds;;

module Test_pds = Odefa_pds_impl.Make(
  struct
    type state = int
    type symbol = string
    module State_order = struct
      type t = int
      let compare = compare
    end;;
    module Symbol_order = struct
      type t = string
      let compare = compare
    end;;
    let legal_action_swaps _ _ = false;;
    let pp_symbol x = x;;
    let pp_state = string_of_int;;
  end
);;

module Test_pds_reachability = Odefa_pds_reachability_impl.Make(Test_pds);;

module Test_swap_pds = Odefa_pds_impl.Make(
  struct
    type state = int
    type symbol = string
    module State_order = struct
      type t = int
      let compare = compare
    end;;
    module Symbol_order = struct
      type t = string
      let compare = compare
    end;;
    let legal_action_swaps a1 a2 =
      match a1,a2 with
      | (Push "a",Push "b") -> true
      | (Push "a",Jump _) -> true
      | _ -> false
    ;;
    let pp_symbol x = x;;
    let pp_state = string_of_int;;
  end
);;

module Test_swap_pds_reachability =
  Odefa_pds_reachability_impl.Make(Test_swap_pds)
;;

let jump_test _ =
  let pds = Test_pds.create_pds @@ List.enum
    [ (1, [Jump 10], 2)
    ; (2, [Pop "a"], 3)
    ; (10, [Pop "a"], 11)
    ]
  in
  let rpds = Test_pds.root_pds pds 1 "a" in
  let reachable_states =
    List.of_enum @@ Test_pds_reachability.analyze_rpds rpds
  in
  assert_equal [11] reachable_states
;;

let jump_intermediate_test _ =
  let pds = Test_pds.create_pds @@ List.enum
    [ (1, [Jump 10; Push "b"], 2)
    ; (2, [Pop "b"; Pop "a"], 3)
    ; (10, [Pop "a"], 11)
    ]
  in
  let rpds = Test_pds.root_pds pds 1 "a" in
  let reachable_states =
    List.of_enum @@ Test_pds_reachability.analyze_rpds rpds
  in
  assert_equal [11] reachable_states
;;

let pds_swap_test _ =
  let pds = Test_swap_pds.create_pds @@ List.enum
    [ (1, [Push "a"; Push "b"; Pop "a"; Pop "b"; Pop "c"], 2)
    ]
  in
  let rpds = Test_swap_pds.root_pds pds 1 "c" in
  let reachable_states =
    List.of_enum @@ Test_swap_pds_reachability.analyze_rpds rpds
  in
  assert_equal [2] reachable_states
;;

let pds_swap_jump_test _ =
  let pds = Test_swap_pds.create_pds @@ List.enum
    [ (1, [Push "a"; Jump 10; Pop "c"], 2)
    ; (10, [Pop "b"], 11)
    ]
  in
  let rpds = Test_swap_pds.root_pds pds 1 "b" in
  let reachable_states =
    List.of_enum @@ Test_swap_pds_reachability.analyze_rpds rpds
  in
  assert_equal [11] reachable_states
;;

let tests = "Odefa_pds" >::: [
    "jump_test" >:: jump_test
  ; "jump_intermediate_test" >:: jump_intermediate_test
  ; "pds_swap_test" >:: pds_swap_test
  ; "pds_swap_jump_test" >:: pds_swap_jump_test
  ]
;;
