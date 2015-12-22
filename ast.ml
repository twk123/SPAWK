type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq | Mod

type expr =
    Number of string
  | String of string
  | Id of string
  | Binop of expr * op * expr
  | Assign of string * expr
  | Call of string * expr list
  | PTuple of expr * expr list
  | Array of string * expr
  | KeyValue of expr * expr
  | Noexpr


type stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | ForIn of string * string * string * stmt
  | While of expr * stmt
  | Def of string * expr list * stmt list
  | Mrbk of string * expr list

type pat_act = {
     pattern : string;
     actions : stmt list;
}


type func_decl = {
    fname : string;
    formals : string list;
    body : stmt list;
  }

type program = pat_act list

let rec last = function
    | [] -> None
    | [x] -> Some x
    | _ :: t -> last t

let rec at k = function
    | [] -> None
    | h :: t -> if k = 1 then Some h else at (k-1) t

let rec string_of_expr = function
    String(s) -> s
  | Number(n) -> n
  | Id(s) -> s
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^
      (match o with
	Add -> "+" | Sub -> "-" | Mult -> "*" | Div -> "/"
      | Equal -> "==" | Neq -> "!="
      | Less -> "<" | Leq -> "<=" | Greater -> ">" | Geq -> ">=" | Mod -> "%") ^ " " ^
      string_of_expr e2
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | Array(v,e) -> v ^ "[" ^ string_of_expr e ^ "]"
  | PTuple(v, e) -> "(" ^ string_of_expr v ^ ", " ^ String.concat ", " (List.map string_of_expr e) ^ ")"
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | KeyValue(e1,e2) -> "(" ^ string_of_expr e1 ^ "," ^ string_of_expr e2 ^ ")"
  | Noexpr -> ""

let rec string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | For(e1, e2, e3, s) ->
      "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ") " ^ string_of_stmt s
  | ForIn(f,g,h,s) -> "for (" ^ f  ^ "," ^ g ^ ")" ^ " in " ^ h ^ ":\n   " ^ string_of_stmt s
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s
  | Def(f,el,sl) -> "def " ^ f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ "):\n    " 
                    ^ String.concat "" (List.map string_of_stmt sl) ^ "\n" 
  | Mrbk(a,sl) -> "flatMapStep = spawkData.flatMap(lambda x: x.split(' '))\n" ^
                    "mapStep = flatMapStep.map(" ^ 
                      (match sl with 
                        | hd :: tl -> string_of_expr hd) ^ ")\n" ^
                    "reduceStep = mapStep.reduceByKey(" ^ 
                      (match sl with 
                        | hd :: tl -> string_of_expr (List.hd tl)) ^ ")\n" ^
                    a ^ " = reduceStep.collect()\n"

let string_of_vdecl id = "int " ^ id ^ ";\n"

let string_of_fdecl fdecl =
  fdecl.fname ^ "(" ^ String.concat ", " fdecl.formals ^ ")\n{\n" ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_padecl padecl =
  padecl.pattern ^ "\n{\n" ^
  String.concat "" (List.map string_of_stmt padecl.actions) ^
  "}\n"

let string_of_program (patacts) =
  String.concat "\n" (List.map string_of_padecl patacts)
