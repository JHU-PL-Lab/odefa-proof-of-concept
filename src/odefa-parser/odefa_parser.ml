(**
  A front-end for the parser library.
*)

open Batteries;;

open Odefa_ast;;
open Odefa_generated_lexer;;
open Odefa_generated_parser;;

let parse_odefa_expressions (input : IO.input) =
  let buf = Lexing.from_input input in
  let read_expr () =
    Odefa_generated_parser.delim_expr Odefa_generated_lexer.token buf
  in
  LazyList.from_while read_expr;;

let parse_odefa_program (input : IO.input) =
  let buf = Lexing.from_input input in
  Odefa_generated_parser.prog Odefa_generated_lexer.token buf
;;
