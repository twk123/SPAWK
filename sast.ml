open Ast




let var_hash = Hashtbl.create 123456

let rec last = function
    | [] -> None
    | [x] -> Some x
    | _ :: t -> last t

let rec at k = function
    | [] -> None
    | h :: t -> if k = 1 then Some h else at (k-1) t

let rec convert_of_expr = function
    String(s) -> s
  | Number(n) -> n
  | Id(s) -> s 

let rec check_of_expr = function
    String(s) -> "String"
  | Number(n) -> "Number"
  | Id(s) -> "Indentifer"
  | Binop(e1, o, e2) ->
      (match o with
	     Add -> if (check_of_expr e1) == "Number" && (check_of_expr e2) == "Number" then "Add ok\n" else "Add error\n"
      | Sub -> if (check_of_expr e1) == "Number" &&  (check_of_expr e2) == "Number" then "Sub ok\n" else "Sub error\n"
      | Mult -> if (check_of_expr e1) == "Number" &&  (check_of_expr e2) == "Number" then "Mult ok\n" else "Mult error\n"
      | Div -> if (check_of_expr e1) == "Number" &&  (check_of_expr e2) == "Number" then "Div ok\n" else "Div error\n"
      | Equal -> if (check_of_expr e1) == "Number" &&  (check_of_expr e2) == "Number" then "Equal ok\n" else "Equal error\n"
      | Neq -> if (check_of_expr e1) == "Number" &&  (check_of_expr e2) == "Number" then "Neq ok\n" else "Neq error\n"
      | Less -> if (check_of_expr e1) == "Number" &&  (check_of_expr e2) == "Number" then "Less ok\n" else "Less error\n"
      | Leq -> if (check_of_expr e1) == "Number" &&  (check_of_expr e2) == "Number" then "Leq ok\n" else "Leq error\n"
      | Greater -> if (check_of_expr e1) == "Number" &&  (check_of_expr e2) == "Number" then "Greater ok\n" else "Greater error\n"
      | Geq -> if (check_of_expr e1) == "Number" &&  (check_of_expr e2) == "Number" then "Geq ok\n" else "Geq error\n"
      | Mod -> if (check_of_expr e1) == "Number" &&  (check_of_expr e2) == "Number" then "Mod ok\n" else "Mod error\n")
  | Assign(v, e) ->  v ^ "no assign check" 
  | Array(v,e) -> v ^ "no array check"
  | PTuple(v, e) -> "no check"
  | Call(f, el) -> f ^ "no call check"
  | KeyValue(e1,e2) -> "no kv check"
  | Noexpr -> ""

let rec check_of_stmt = function
    Block(stmts) -> "{\n" ^ String.concat "" (List.map check_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> check_of_expr expr 
  | Return(expr) -> check_of_expr expr
  | If(e, s, Block([])) -> check_of_expr e ; check_of_stmt s
  | If(e, s1, s2) ->  check_of_expr e ; check_of_stmt s1 ; check_of_stmt s2
  | For(e1, e2, e3, s) ->  check_of_expr e1 ; check_of_expr e2 ; check_of_expr e3 ; check_of_stmt s
  | ForIn(f,g,h,s) -> Hashtbl.add var_hash f "String" ; Hashtbl.add var_hash g "String" ; 
      (let rddVal = Hashtbl.find var_hash h in
        match rddVal with
           "RDD" -> "ForIn RDD ok\n" ^ check_of_stmt s
          | _ -> "ForIn error with: " ^ h)
  | While(e, s) -> "\n" ^ check_of_expr e  ^ check_of_stmt s
  | Def(f,el,sl) ->  "\n" ^ String.concat "" (List.map check_of_expr el) ^ "\n" ^ String.concat "" (List.map check_of_stmt sl)
  | Mrbk(a,sl) -> Hashtbl.add var_hash a "RDD" ; "check"
                    



let check_of_padecl padecl =
  padecl.pattern ^ "\n{\n" ^
  String.concat "" (List.map check_of_stmt padecl.actions) ^
  "}\n"

let check_of_program (patacts) =
  String.concat "\n" (List.map check_of_padecl patacts)
