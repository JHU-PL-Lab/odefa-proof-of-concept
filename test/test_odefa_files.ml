(**
   This test module will load a series of test files from the test sources
   directory and execute them one by one.

   Each file is expected to contain a comment describing the expected test
   result.  The comment should be of one of the following forms:

    - [EXPECT-EVALUATE] (which requires that the code evaluates to completion)
    - [EXPECT-STUCK] (which requires that the code gets stuck)
    - [EXPECT-TYPECHECKS] (which requires the analysis to prove that the code
                           does not get stuck)
    - [EXPECT-TYPECHECKS-AND-EVALUATES] (which requires the analysis to prove
                                         that the code does not get stuck and
                                         for it to evaluate to competion)
    - [EXPECT-TYPEFAILS] (which requires the analysis to conclude that the code
                          might get stuck)
*)

open Batteries;;
open OUnit2;;

open Odefa_ast_wellformedness;;
open Odefa_interpreter;;
open Odefa_parser;;
open Odefa_utils;;

exception File_test_creation_failure of string;;

type test_expectation =
  | Expect_evaluate
  | Expect_stuck
  | Expect_typechecks
  | Expect_typechecks_and_evaluates
  | Expect_typefails
  | Expect_ill_formed
;;

let parse_expectation str =
  match str with
  | "EXPECT-EVALUATE" -> Some(Expect_evaluate)
  | "EXPECT-STUCK" -> Some(Expect_stuck)
  | "EXPECT-TYPECHECKS" -> Some(Expect_typechecks)
  | "EXPECT-TYPECHECKS-AND-EVALUATES" -> Some(Expect_typechecks_and_evaluates)
  | "EXPECT-TYPEFAILS" -> Some(Expect_typefails)
  | "EXPECT-ILL-FORMED" -> Some(Expect_ill_formed)
  | _ -> None
;;

module Analysis = Odefa_analysis.Make(Odefa_analysis_single_element_stack.Stack);;

let make_test filename expectation =
  let test_name_expectation = match expectation with
    | Expect_evaluate ->
      "(should evaluate)"
    | Expect_stuck ->
      "(should get stuck)"
    | Expect_typechecks ->
      "(should typecheck)"
    | Expect_typechecks_and_evaluates ->
      "(should typecheck and evaluate)"
    | Expect_typefails ->
      "(should not typecheck)"
    | Expect_ill_formed ->
      "(should not be well-formed)"
  in
  let test_name = filename ^ ": " ^ test_name_expectation in
  (* Create the test in a thunk. *)
  let test_fun =
    fun _ ->
      (* Begin by parsing the file. *)
      let expr = File.with_file_in filename Odefa_parser.parse_odefa_program in
      (* Verify that it is well-formed. *)
      begin
        try
          check_wellformed_expr expr;
          begin
            match expectation with
                | Expect_ill_formed -> assert_failure @@ "No ill-formedness found."
                | _ -> ()
          end
        with Illformedness_found(illformednesses) ->
          begin
            match expectation with
                | Expect_ill_formed -> ()
                | _ -> assert_failure ("Ill-formedness program:" ^
                                       (illformednesses
                                        |> List.map pretty_illformedness
                                        |> String.join ", "))
          end
      end;
      (* Define some appropriate routines. *)
      let assert_evaluates () =
        try
          ignore @@ eval expr
        with Evaluation_failure(failure) ->
          assert_failure @@ "Evaluation became stuck: " ^ failure
      in
      let assert_typechecks status =
        let g = Odefa_analysis_graph.graph_of_expr expr in
        let g' = Analysis.perform_graph_closure expr g in
        if status
        then assert_bool "Analysis could not prove that this code does not get stuck"
            (not @@ Analysis.test_graph_inconsistency expr g')
        else assert_bool "Analysis did not conclude that this code might get stuck"
          @@ Analysis.test_graph_inconsistency expr g'
      in
      (* Now, based on our expectation, do the right thing. *)
      match expectation with
      | Expect_evaluate ->
        assert_evaluates ()
      | Expect_stuck ->
        begin
          try
            ignore (eval expr);
            assert_failure ("Evaluation completed")                
          with Evaluation_failure(failure) ->
            ()
        end
      | Expect_typechecks ->
        assert_typechecks true
      | Expect_typechecks_and_evaluates ->
        assert_typechecks true;
        assert_evaluates ()
      | Expect_typefails ->
        assert_typechecks false
      | Expect_ill_formed ->
        ()
  in
  OUnitTest.TestLabel(test_name, OUnitTest.TestCase(OUnitTest.Huge, test_fun))
;;

let make_test_from filename =
  let expectations =
    filename
    |> File.lines_of
    |> Enum.filter_map
      (fun str ->
         let str' = String.trim str in
         if String.starts_with str' "#"
         then
           let str'' = String.trim @@ String.tail str' 1 in
           parse_expectation str''
         else None
      )
    |> List.of_enum
  in
  match expectations with
  | [expectation] ->
    make_test filename expectation
  | [] ->
    raise (File_test_creation_failure(
        "Could not create test from file " ^ filename ^
        ": no expectation comment found."))
  | _ ->
    raise (File_test_creation_failure(
        "Could not create test from file " ^ filename ^
        ": multiple expectation comments found."))

let make_all_tests pathname =
  if Sys.file_exists pathname && Sys.is_directory pathname
  then
    Sys.files_of pathname
    |> Enum.map (fun f -> pathname ^ Filename.dir_sep ^ f)
    |> Enum.filter (fun f -> not @@ Sys.is_directory f)
    |> Enum.filter (fun f -> String.ends_with f ".odefa")
    |> Enum.map make_test_from
    |> List.of_enum
  else
    raise (File_test_creation_failure(
        "Test file directory " ^ pathname ^ " is missing"))
;;

let tests = "Test_odefa_files" >::: make_all_tests "test-sources";;
