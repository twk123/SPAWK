(* compiling *)
let compile f d = 
        let out = (Filename.chop_extension f) in
        let out_chan = open_out (out ^ ".py")
        and out_warn = open_out (out ^ ".warn")
        and lexbuf = Lexing.from_channel (open_in f) in
        try
            let program = Parser.program Lexer.micro lexbuf in
            let listing = Ast.string_of_program program
                in output_string out_warn listing ; output_string out_warn "\n";
            let slisting = Sast.check_of_program program
                in output_string out_warn slisting ; output_string out_warn "\n";
            Codegen.set_chan out_chan;
            Codegen.set_input d;
            Codegen.codegen program;
            close_out out_chan;
            close_out out_warn;
            (* ignore(Sys.command ("spark-submit " ^ out ^ ".py 2> spark.log")); *)
            ignore(Sys.command ("python -m py_compile " ^ out ^ ".py >> " ^ out ^ ".warn")) 
        with 
        | Codegen.Codegen_error s ->
            print_string s;
            print_string "\n";
            exit 1
        | Lexer.Syntax_error s ->
            print_string s;
            print_string "\n";
            exit 1

let help () = print_string "spawk.native <program file> <data file>\n"

let () = if Array.length Sys.argv < 2 then help ()
         else 
             let file = Array.get Sys.argv 1 in
                 let dataFile = Array.get Sys.argv 2 in
                 Format.printf "compiling %s\n" file;
                 Format.print_flush ();
                 compile file dataFile