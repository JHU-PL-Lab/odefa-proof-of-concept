{
  open Odefa_parser_support;;
  open Odefa_generated_parser;;

  (* Because ocamllex is STUPID, we must increment the line counter
     ourselves. *)
  open Lexing
  let incr_lineno lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- { pos with
      pos_lnum = pos.pos_lnum + 1;
      pos_bol = pos.pos_cnum;
    }
}

let digit = ['0'-'9']
let alpha = ['a'-'z'] | ['A'-'Z']
let whitespace = [' ' '\t']
let newline = '\n'
let comment = '#' [^'\n']* '\n'

let ident_start = alpha
let ident_cont = alpha | digit

rule token = parse
  | eof                              { EOF }
  | comment                          { incr_lineno lexbuf; token lexbuf }
  | whitespace                       { token lexbuf }
  | newline                          { incr_lineno lexbuf; token lexbuf }
  | "{"                              { OPEN_BRACE }
  | "}"                              { CLOSE_BRACE }
  | ";"                              { SEMICOLON }
  | ","                              { COMMA }
  | "."                              { PERIOD }
  | "="                              { EQUALS }
  | "->"                             { ARROW }
  | "?"                              { QUESTION_MARK }
  | "~"                              { TILDE }
  | ":"                              { COLON }
  | "fun"                            { KEYWORD_FUN }
  | ident_start ident_cont* as s     { IDENTIFIER s }
  | ";;"                             { DOUBLE_SEMICOLON }
