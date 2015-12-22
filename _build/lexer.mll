{
open Lexing
open Parser

(* current token line number *)
let line_num = ref 1

(* keyword -> token translation table *)
let keywords = [
    "begin", BEGIN; "end", END; "read", READ; "write", WRITE; "function", FUNCTION
]

exception Syntax_error of string

let syntax_error msg = raise (Syntax_error (msg ^ " on line " ^ (string_of_int !line_num)))

}

let blank = [' ' '\r' '\t']
let digit = ['0'-'9']
let int = '-'? digit digit*
let digits = digit*
let frac = '.' digit*
let exp = ['e' 'E']['-' '+']? digit+
let float = digit* frac? exp?
let number = (int | float)
let alpha = ['a'-'z' 'A'-'Z']
let iden = alpha (alpha | digit | '_')*

rule micro = parse
    | '='     { ASSIGN }
    | '+'      { PLUS }
    | '-'      { MINUS }
    | '*'      { TIMES }
    | ','      { COMMA }
    | ';'      { SEMICOLON }
    | '('      { LEFTPAREN }
    | ')'      { RIGHTPAREN }
    | '{'      { LEFTBRACE }
    | '}'      { RIGHTBRACE }
    | '['      { LEFTBRACK }
    | ']'      { RIGHTBRACK }
    | '/'      { DIVIDE }
    | '%'      { MOD }
    | '$'      { DOLLAR }
    | "#"      { comment lexbuf }           (* Comments *)
    | '"'      { read_string (Buffer.create 17) lexbuf}
    | "=="     { EQ }
    | "!="     { NEQ }
    | '<'      { LT }
    | "<="     { LEQ }
    | ">"      { GT }
    | ">="     { GEQ }
    | "if"     { IF }
    | "else"   { ELSE }
    | "for"    { FOR }
    | "in"     { IN }
    | "while"  { WHILE }
    | "return" { RETURN }
    | "mapReduceByKey" { MRBK }
    | "emptyPattern" { EP }
    | "var"     { VAR }
    | iden as i {
        (* try keywords if not found then it's identifier *)
        let l = String.lowercase i in
        try List.assoc l keywords
        with Not_found -> IDENTIFIER i   
    }
    | number as d { 
        (* parse number *)
        NUMBER  d
    }
    | '\n'     { incr line_num; micro lexbuf } (* counting new line characters *)
    | blank    { micro lexbuf } (* skipping blank characters *)
    | _        { syntax_error "couldn't identify the token" }
    | eof      { EOF } (* no more tokens *)
    and read_string buf =
        parse
        | '"'       { STRING (Buffer.contents buf) }
        | '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
        | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
        | '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf }
        | '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
        | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
        | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
        | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
        | [^ '"' '\\']+
           { Buffer.add_string buf (Lexing.lexeme lexbuf);
                read_string buf lexbuf
            }
        | _ { raise (Syntax_error ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
        | eof { raise (Syntax_error ("String is not terminated")) }

    and comment = parse
    '\n' { micro lexbuf }
    | _    { comment lexbuf }

