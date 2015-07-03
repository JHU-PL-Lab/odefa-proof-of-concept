open Batteries;;

open Odefa_analysis_graph;;
open Odefa_ast;;
open Odefa_ast_pretty;;
open Odefa_ast_wellformedness;;
open Odefa_interpreter;;

module Analysis = Odefa_analysis.Make(Odefa_analysis_single_element_stack.Stack);;

exception Becomes_stuck;;

let toploop_operate e =
  print_string "\n";
  begin
    try
      (* Verify expression well-formedness. *)
      check_wellformed_expr e;
      (* Use analysis to detect potentially stuck programs. *)
      let g = graph_of_expr e in
      let g' = Analysis.perform_graph_closure e g in
      if Analysis.test_graph_inconsistency e g'
      then raise Becomes_stuck
      else
        begin
          (* Let's show the expected type of each top-level clause. *)
          let (Expr cls) = e in
          cls
          |> List.enum
          |> Enum.map (fun (Clause(x,_)) -> x)
          |> Enum.iter
            (fun x ->
               Analysis.lookup e g' x End_clause
               |> Value_set.enum
               |> Enum.iter
                 (fun v ->
                    print_endline @@
                    pretty_var x ^ " might be " ^ pretty_value v))
          ;
          print_endline "";
        end;
      (* Evaluate. *)      
      let x,env = eval e in
      print_string (pretty_var x ^ " where "  ^ pretty_env env ^ "\n");
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
