open Ast


module StringMap = Map.Make(String)

(* Symbol table: Information about all the names in scope *)
type env = {
    function_index : int StringMap.t; (* Index for each function *)
    global_index   : int StringMap.t; (* "Address" for global variables *)
    local_index    : int StringMap.t; (* FP offset for args, locals *)
  }

exception Codegen_error of string

let codegen_error msg = raise (Codegen_error msg)

(* code generation *)
let chan = ref stdout
let dataFile = ref "string"
(* let var_hash = Hashtbl.create 123456 *)

let set_chan new_chan = chan := new_chan

let set_input new_chan = dataFile := new_chan

let gen v = output_string !chan v; output_string !chan "\n"



let generate_begin () = gen
("from __future__ import print_function
import sys
import re
from operator import add
from pyspark import SparkConf, SparkContext
dataFile = \"" ^ !dataFile ^ "\"  # Should be some file on your system
conf = (SparkConf()
         .setMaster(\"local\")
         .setAppName(\"spawk\")
         .set(\"spark.executor.memory\", \"1g\"))

sc = SparkContext(conf = conf)

")

let generate_end () = gen
"
sc.stop()
"

let generate_regexp i = gen 
("
spawkTempData = sc.textFile(dataFile).cache()
def parse_rrd_line(line):
    match = re.search(\'" ^ i ^ "\',line)
    if match is None:
        return ''
    return line

spawkData = (spawkTempData.map(parse_rrd_line).cache())
"
)

let generate_noregexp () = gen 
("
spawkData = sc.textFile(dataFile).cache()
"
)


let rec generate_of_expr = function
    String(s) -> s
  | Number(n) -> n
  | Id(s) -> s
  | Binop(e1, o, e2) ->
      generate_of_expr e1 ^ " " ^
      (match o with
    Add -> "+" | Sub -> "-" | Mult -> "*" | Div -> "/"
      | Equal -> "==" | Neq -> "!="
      | Less -> "<" | Leq -> "<=" | Greater -> ">" | Geq -> ">=" | Mod -> "%") ^ " " ^
      generate_of_expr e2
  | Assign(v, e) -> v ^ " = " ^ generate_of_expr e
  | Array(v, e) -> v ^ "[" ^ generate_of_expr e ^ "]"
  | PTuple(v, e) -> "(" ^ generate_of_expr v ^ ", " ^ String.concat ", " (List.map generate_of_expr e) ^ ")"
  | Call(f, el) -> f ^ "(" ^ String.concat ", " (List.map generate_of_expr el) ^ ")"
  | KeyValue(e1,e2) -> "(" ^ generate_of_expr e1 ^ "," ^ generate_of_expr e2 ^ ")"
  | Noexpr -> ""

let rec generate_of_stmt = function
    Block(stmts) ->
      "    " ^ String.concat " ; " (List.map generate_of_stmt stmts) ^ "\n"
  | Expr(expr) -> generate_of_expr expr 
  | Return(expr) -> "return " ^ generate_of_expr expr ^ "\n";
  | If(e, s, Block([])) -> "if (" ^ generate_of_expr e ^ ")\n" ^ generate_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ generate_of_expr e ^ ")\n" ^
      generate_of_stmt s1 ^ "else\n" ^ generate_of_stmt s2
  | For(e1, e2, e3, s) ->
      "for (" ^ generate_of_expr e1  ^ " ; " ^ generate_of_expr e2 ^ " ; " ^
      generate_of_expr e3  ^ ") " ^ generate_of_stmt s
  | ForIn(f,g,h,s) -> "for (" ^ f  ^ "," ^ g ^ ")" ^ " in " ^ h ^ ":\n   " ^ generate_of_stmt s
  | While(e, s) -> "while (" ^ generate_of_expr e ^ "):\n    " ^ generate_of_stmt s
  | Def(f,el,sl) -> "def " ^ f ^ "(" ^ String.concat ", " (List.map generate_of_expr el) ^ "):\n    " 
                    ^ String.concat "    " (List.map generate_of_stmt sl) ^ "\n" 
  | Mrbk(a,sl) -> "flatMapStep = spawkData.flatMap(lambda x: x.split(' '))\n" ^
                    "mapStep = flatMapStep.map(" ^ 
                      (match sl with 
                        | hd :: tl -> generate_of_expr hd) ^ ")\n" ^
                    "reduceStep = mapStep.reduceByKey(" ^ 
                      (match sl with 
                        | hd :: tl -> generate_of_expr (List.hd tl)) ^ ")\n" ^
                    a ^ " = reduceStep.collect()\n"

let generate_of_vdecl id = "int " ^ id ^ ";\n"

let generate_of_fdecl fdecl =
  fdecl.fname ^ "(" ^ String.concat ", " fdecl.formals ^ ")\n{\n" ^
  String.concat "" (List.map generate_of_stmt fdecl.body) ^
  "}\n"

let generate_actions actions = "\n" ^ String.concat "" (List.map generate_of_stmt actions) ^ "\n" 

let generate_of_padecl padecl =
  match padecl.pattern with
    | "emptyPattern" -> generate_noregexp () ; generate_actions padecl.actions
    | _ ->  generate_regexp padecl.pattern ; generate_actions padecl.actions


let generate_of_program (patacts) =
  gen(String.concat "\n" (List.map generate_of_padecl patacts))



(* val enum : int -> 'a list -> (int * 'a) list *)
let rec enum stride n = function
    [] -> []
  | hd::tl -> (n, hd) :: enum stride (n+stride) tl

(* val string_map_pairs StringMap 'a -> (int * 'a) list -> StringMap 'a *)
let string_map_pairs map pairs =
  List.fold_left (fun m (i, n) -> StringMap.add n i m) map pairs

(** Translate a program in AST form into a bytecode program.  Throw an
    exception if something is wrong, e.g., a reference to an unknown
    variable or function *)


let codegen p = 
        generate_begin (); generate_of_program p; generate_end ()
    

