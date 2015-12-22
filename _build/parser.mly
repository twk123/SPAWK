%{

open Ast

%}

%token BEGIN END
%token <string> IDENTIFIER
%token <string> NUMBER
%token <string> STRING
%token READ WRITE FUNCTION
%token ASSIGN 
%token LEFTPAREN RIGHTPAREN  
%token LEFTBRACE RIGHTBRACE
%token COMMA SEMICOLON DOLLAR EP
%token LEFTBRACK
%token RIGHTBRACK
%token COLON
%token NEWLINE
%token PLUS MINUS TIMES DIVIDE
%token EQ NEQ LT LEQ GT GEQ MOD
%token RETURN IF ELSE FOR WHILE VAR IN MRBK
%token EOF

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE

%start program
%type <Ast.program> program

%%


program:
   EOF { [] }
 | padecl program {  $1 :: $2 }
 



padecl:
  LEFTBRACE stmt_list RIGHTBRACE { { pattern = "emptyPattern" ; actions = List.rev $2 } }
 | DIVIDE IDENTIFIER DIVIDE LEFTBRACE stmt_list RIGHTBRACE { { pattern = $2 ; actions = List.rev $5 } }


/* fdecl:
   FUNCTION IDENTIFIER LEFTPAREN formals_opt RIGHTPAREN LEFTBRACE stmt_list RIGHTBRACE
     { { fname = $2;
   formals = $4;
   body = List.rev $7 } } */

formals_opt:
    /* nothing */ { [] }
  | formal_list   { List.rev $1 }

formal_list:
    IDENTIFIER                   { [$1] }
  | formal_list COMMA IDENTIFIER { $3 :: $1 }


stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }

stmt:
    expr  { Expr($1) }
  | RETURN expr  { Return($2) }
  | LEFTBRACE stmt_list RIGHTBRACE { Block(List.rev $2) }
  | IF LEFTPAREN expr RIGHTPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
  | IF LEFTPAREN expr RIGHTPAREN stmt ELSE stmt    { If($3, $5, $7) }
  | FOR LEFTPAREN expr_opt SEMICOLON expr_opt SEMICOLON expr_opt RIGHTPAREN stmt
     { For($3, $5, $7, $9) }
  | FOR LEFTPAREN IDENTIFIER COMMA IDENTIFIER RIGHTPAREN IN IDENTIFIER stmt { ForIn($3,$5,$8,$9) }
  | WHILE LEFTPAREN expr RIGHTPAREN stmt { While($3, $5) }
  | FUNCTION IDENTIFIER LEFTPAREN actuals_opt RIGHTPAREN LEFTBRACE stmt_list RIGHTBRACE  { Def($2,$4,List.rev $7)}
  | IDENTIFIER ASSIGN MRBK LEFTPAREN actuals_opt RIGHTPAREN { Mrbk($1,$5) }
  

expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1 }

expr:
    NUMBER          { Number($1) }
  | STRING          { String($1) }
  | IDENTIFIER               { Id($1) }
  | expr PLUS   expr { Binop($1, Add,   $3) }
  | expr MINUS  expr { Binop($1, Sub,   $3) }
  | expr TIMES  expr { Binop($1, Mult,  $3) }
  | expr DIVIDE expr { Binop($1, Div,   $3) }
  | expr EQ     expr { Binop($1, Equal, $3) }
  | expr NEQ    expr { Binop($1, Neq,   $3) }
  | expr LT     expr { Binop($1, Less,  $3) }
  | expr LEQ    expr { Binop($1, Leq,   $3) }
  | expr GT     expr { Binop($1, Greater,  $3) }
  | expr GEQ    expr { Binop($1, Geq,   $3) }
  | expr MOD    expr { Binop($1, Mod,   $3) }
  | IDENTIFIER ASSIGN expr   { Assign($1, $3) }
  | IDENTIFIER LEFTBRACK expr RIGHTBRACK { Array($1,$3) }
  | IDENTIFIER LEFTPAREN actuals_opt RIGHTPAREN { Call($1, $3) }
  | LEFTPAREN expr COMMA expr RIGHTPAREN { KeyValue($2,$4) }
  | LEFTPAREN expr RIGHTPAREN { $2 } 

actuals_opt:
    /* nothing */ { [] }
  | actuals_list  { List.rev $1 }

actuals_list:
    expr                    { [$1] }
  | actuals_list COMMA expr { $3 :: $1 }

%%