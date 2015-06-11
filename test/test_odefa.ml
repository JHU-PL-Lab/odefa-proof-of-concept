open OUnit2

let all_tests =
  [ Test_odefa_utils.tests
  ; Test_odefa_files.tests
  ];;

run_test_tt_main ("Odefa" >::: all_tests);;
