(**
   A front-end for the parser library.
*)

open Batteries;;

open Lexing;;

open Odefa_ast;;
open Odefa_generated_lexer;;
open Odefa_generated_parser;;

exception Parse_error of exn * int * int * string;;

let handle_parse_error buf f =
  try
    f ()
  with
  | exn ->
    let curr = buf.lex_curr_p in
    let line = curr.pos_lnum in
    let column = curr.pos_cnum - curr.pos_bol in
    let tok = lexeme buf in
    raise @@ Parse_error(exn,line,column,tok)
;;

let parse_odefa_expressions (input : IO.input) =
  let buf = Lexing.from_input input in
  let read_expr () =
    handle_parse_error buf @@ fun () ->
    Odefa_generated_parser.delim_expr Odefa_generated_lexer.token buf
  in
  LazyList.from_while read_expr;;

let parse_odefa_program (input : IO.input) =
  let buf = Lexing.from_input input in
  handle_parse_error buf @@ fun () ->
  Odefa_generated_parser.prog Odefa_generated_lexer.token buf
;;
