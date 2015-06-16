open Batteries;;

open Odefa_ast_pretty;;
open Odefa_ast_wellformedness;;
open Odefa_interpreter;;

module Analysis = Odefa_analysis.Make(Odefa_analysis_nonrepeating_stack.Stack);;

exception Becomes_stuck;;

let toploop_operate e =
  print_string "\n";
  begin
    try
      (* Verify expression well-formedness. *)
      check_wellformed_expr e;
      (* Use analysis to detect potentially stuck programs. *)
      if Analysis.becomes_stuck e
        then raise Becomes_stuck
        else ();
      (* Evaluate. *)      
      let v,env = eval e in
      print_string (pretty_var v ^ " where "  ^ pretty_env env ^ "\n");
    with
      | Illformedness_found(ills) ->
          print_string "Provided expression is ill-formed:\n";
          List.iter
            (fun ill ->
              print_string @@ "   " ^ pretty_illformedness ill ^ "\n")
            ills
      | Becomes_stuck ->
          print_string "Program may become stuck at runtime.\n"
  end;
  print_string "\n";
  print_string "Please enter an expression to evaluate followed by \";;\".\n";
  print_string "\n";
  flush stdout
;;

let () =
  print_string "Odefa Toploop\n";
  print_string "--------------------\n";
  print_string "\n";
  print_string "Please enter an expression to evaluate followed by \";;\".\n";
  print_string "\n";
  flush stdout;
  Odefa_parser.parse_odefa_expressions IO.stdin
    |> LazyList.iter toploop_operate
;;
