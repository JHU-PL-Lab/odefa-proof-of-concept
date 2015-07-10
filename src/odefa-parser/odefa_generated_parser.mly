%{
open Odefa_ast;;
open Odefa_ast_uid;;
open Odefa_parser_support;;
open Odefa_source_origin;;
open Lexing;;

module List = BatList;;
module Map = BatMap;;
%}

%token <string> IDENTIFIER
%token EOF 
%token OPEN_BRACE 
%token CLOSE_BRACE 
%token OPEN_PAREN 
%token CLOSE_PAREN 
%token SEMICOLON
%token COMMA
%token PERIOD
%token EQUALS 
%token ARROW 
%token QUESTION_MARK 
%token TILDE 
%token COLON 
%token KEYWORD_FUN 
%token DOUBLE_SEMICOLON 

%start <Odefa_ast.expr> prog
%start <Odefa_ast.expr option> delim_expr

%%

prog:
  | expr EOF
      { $1 }
  ;
  
delim_expr:
  | EOF
      { None }
  | expr DOUBLE_SEMICOLON
      { Some($1) }
  | expr EOF
      { Some($1) }
  ;

expr:
  | separated_nonempty_trailing_list(SEMICOLON, clause)
      { Expr($1) }
  ;

clause:
  | variable EQUALS clause_body
      { Clause($1,$3) }
  ;

variable:
  | identifier
      { Var($1,None) }
  ;
  
identifier:
  | IDENTIFIER
      { Ident $1 }
  ;

clause_body:
  | value
      { Value_body($1) }
  | variable
      { Var_body($1) }
  | variable variable
      { Appl_body($1,$2) }
  | variable TILDE pattern QUESTION_MARK function_value COLON function_value
      { Conditional_body($1,$3,$5,$7) }
  | variable PERIOD identifier
      { Projection_body($1,$3) }
  ;

value:
  | record_value
      { Value_record($1) }
  | function_value
      { Value_function($1) }
  ;

record_value:
  | OPEN_BRACE CLOSE_BRACE
      { Empty_record_value }
  | OPEN_BRACE separated_nonempty_trailing_list(COMMA, identifier) CLOSE_BRACE
      { Degenerate_record_value(Ident_set.of_list $2) }
  | OPEN_BRACE separated_nonempty_trailing_list(COMMA, record_element)
        CLOSE_BRACE
      { Proper_record_value(Ident_map.of_enum @@ List.enum $2) }
  ;

record_element:
  | identifier EQUALS variable
      { ($1,$3) }
  ;
  
function_value:
  | KEYWORD_FUN variable ARROW OPEN_PAREN expr CLOSE_PAREN
      { Function_value($2,$5) }
  ;

pattern:
  | OPEN_BRACE separated_nonempty_trailing_list(COMMA, identifier) CLOSE_BRACE
      { Record_pattern(Ident_set.of_list $2) }
  | OPEN_BRACE CLOSE_BRACE
      { Record_pattern(Ident_set.empty) }
  ;

separated_nonempty_trailing_list(separator, rule):
  | nonempty_list(terminated(rule, separator))
      { $1 }
  | separated_nonempty_list(separator,rule)
      { $1 }
  ;
