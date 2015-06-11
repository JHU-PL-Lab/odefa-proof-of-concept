open Batteries;;

open Odefa_ast_pretty;;
open Odefa_ast_wellformedness;;
open Odefa_interpreter;;

let toploop_operate e =
  print_string "\n";
  begin
    try
      check_wellformed_expr e;
      let v,env = eval e in
      print_string (pretty_var v ^ " where "  ^ pretty_env env ^ "\n");
    with
      | Illformedness_found(ills) ->
          print_string "Provided expression is ill-formed:\n";
          List.iter
            (fun ill ->
              print_string @@ "   " ^ pretty_illformedness ill ^ "\n")
            ills
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
