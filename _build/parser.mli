
(* The type of tokens. *)

type token = 
  | WRITE
  | WHILE
  | VAR
  | TIMES
  | STRING of (string)
  | SEMICOLON
  | RIGHTPAREN
  | RIGHTBRACK
  | RIGHTBRACE
  | RETURN
  | READ
  | PLUS
  | NUMBER of (string)
  | NEWLINE
  | NEQ
  | MRBK
  | MOD
  | MINUS
  | LT
  | LEQ
  | LEFTPAREN
  | LEFTBRACK
  | LEFTBRACE
  | IN
  | IF
  | IDENTIFIER of (string)
  | GT
  | GEQ
  | FUNCTION
  | FOR
  | EQ
  | EP
  | EOF
  | END
  | ELSE
  | DOLLAR
  | DIVIDE
  | COMMA
  | COLON
  | BEGIN
  | ASSIGN

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val program: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.program)
