
exception Error

let _eRR =
  Error

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

type _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  _menhir_token: token;
  mutable _menhir_error: bool
}

and _menhir_state = 
  | MenhirState105
  | MenhirState102
  | MenhirState101
  | MenhirState93
  | MenhirState88
  | MenhirState86
  | MenhirState84
  | MenhirState80
  | MenhirState74
  | MenhirState71
  | MenhirState70
  | MenhirState67
  | MenhirState62
  | MenhirState60
  | MenhirState58
  | MenhirState56
  | MenhirState53
  | MenhirState52
  | MenhirState50
  | MenhirState49
  | MenhirState45
  | MenhirState41
  | MenhirState38
  | MenhirState36
  | MenhirState31
  | MenhirState29
  | MenhirState27
  | MenhirState25
  | MenhirState23
  | MenhirState21
  | MenhirState19
  | MenhirState17
  | MenhirState15
  | MenhirState13
  | MenhirState11
  | MenhirState9
  | MenhirState7
  | MenhirState4
  | MenhirState2
  | MenhirState1
  | MenhirState0
  

open Ast


let rec _menhir_goto_actuals_list : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.expr list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    Printf.fprintf Pervasives.stderr "State 35:\n%!";
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COMMA ->
        Printf.fprintf Pervasives.stderr "Shifting (COMMA) to state 36\n%!";
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 36:\n%!";
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENTIFIER _v ->
            Printf.fprintf Pervasives.stderr "Shifting (IDENTIFIER) to state 8\n%!";
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
        | LEFTPAREN ->
            Printf.fprintf Pervasives.stderr "Shifting (LEFTPAREN) to state 7\n%!";
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | NUMBER _v ->
            Printf.fprintf Pervasives.stderr "Shifting (NUMBER) to state 6\n%!";
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
        | STRING _v ->
            Printf.fprintf Pervasives.stderr "Shifting (STRING) to state 5\n%!";
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState36)
    | RIGHTPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "Reducing production actuals_opt -> actuals_list \n%!";
        let (_menhir_stack, _menhir_s, _1) = _menhir_stack in
        let _v : (Ast.expr list) =                   ( List.rev _1 ) in
        _menhir_goto_actuals_opt _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run11 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    Printf.fprintf Pervasives.stderr "State 11:\n%!";
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENTIFIER _v ->
        Printf.fprintf Pervasives.stderr "Shifting (IDENTIFIER) to state 8\n%!";
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v
    | LEFTPAREN ->
        Printf.fprintf Pervasives.stderr "Shifting (LEFTPAREN) to state 7\n%!";
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | NUMBER _v ->
        Printf.fprintf Pervasives.stderr "Shifting (NUMBER) to state 6\n%!";
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v
    | STRING _v ->
        Printf.fprintf Pervasives.stderr "Shifting (STRING) to state 5\n%!";
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState11

and _menhir_run15 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    Printf.fprintf Pervasives.stderr "State 15:\n%!";
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENTIFIER _v ->
        Printf.fprintf Pervasives.stderr "Shifting (IDENTIFIER) to state 8\n%!";
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _v
    | LEFTPAREN ->
        Printf.fprintf Pervasives.stderr "Shifting (LEFTPAREN) to state 7\n%!";
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | NUMBER _v ->
        Printf.fprintf Pervasives.stderr "Shifting (NUMBER) to state 6\n%!";
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _v
    | STRING _v ->
        Printf.fprintf Pervasives.stderr "Shifting (STRING) to state 5\n%!";
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState15

and _menhir_run19 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    Printf.fprintf Pervasives.stderr "State 19:\n%!";
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENTIFIER _v ->
        Printf.fprintf Pervasives.stderr "Shifting (IDENTIFIER) to state 8\n%!";
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
    | LEFTPAREN ->
        Printf.fprintf Pervasives.stderr "Shifting (LEFTPAREN) to state 7\n%!";
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | NUMBER _v ->
        Printf.fprintf Pervasives.stderr "Shifting (NUMBER) to state 6\n%!";
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
    | STRING _v ->
        Printf.fprintf Pervasives.stderr "Shifting (STRING) to state 5\n%!";
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState19

and _menhir_run13 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    Printf.fprintf Pervasives.stderr "State 13:\n%!";
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENTIFIER _v ->
        Printf.fprintf Pervasives.stderr "Shifting (IDENTIFIER) to state 8\n%!";
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _v
    | LEFTPAREN ->
        Printf.fprintf Pervasives.stderr "Shifting (LEFTPAREN) to state 7\n%!";
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | NUMBER _v ->
        Printf.fprintf Pervasives.stderr "Shifting (NUMBER) to state 6\n%!";
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _v
    | STRING _v ->
        Printf.fprintf Pervasives.stderr "Shifting (STRING) to state 5\n%!";
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState13

and _menhir_run21 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    Printf.fprintf Pervasives.stderr "State 21:\n%!";
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENTIFIER _v ->
        Printf.fprintf Pervasives.stderr "Shifting (IDENTIFIER) to state 8\n%!";
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
    | LEFTPAREN ->
        Printf.fprintf Pervasives.stderr "Shifting (LEFTPAREN) to state 7\n%!";
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | NUMBER _v ->
        Printf.fprintf Pervasives.stderr "Shifting (NUMBER) to state 6\n%!";
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
    | STRING _v ->
        Printf.fprintf Pervasives.stderr "Shifting (STRING) to state 5\n%!";
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState21

and _menhir_run23 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    Printf.fprintf Pervasives.stderr "State 23:\n%!";
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENTIFIER _v ->
        Printf.fprintf Pervasives.stderr "Shifting (IDENTIFIER) to state 8\n%!";
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
    | LEFTPAREN ->
        Printf.fprintf Pervasives.stderr "Shifting (LEFTPAREN) to state 7\n%!";
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | NUMBER _v ->
        Printf.fprintf Pervasives.stderr "Shifting (NUMBER) to state 6\n%!";
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
    | STRING _v ->
        Printf.fprintf Pervasives.stderr "Shifting (STRING) to state 5\n%!";
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState23

and _menhir_run25 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    Printf.fprintf Pervasives.stderr "State 25:\n%!";
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENTIFIER _v ->
        Printf.fprintf Pervasives.stderr "Shifting (IDENTIFIER) to state 8\n%!";
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
    | LEFTPAREN ->
        Printf.fprintf Pervasives.stderr "Shifting (LEFTPAREN) to state 7\n%!";
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | NUMBER _v ->
        Printf.fprintf Pervasives.stderr "Shifting (NUMBER) to state 6\n%!";
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
    | STRING _v ->
        Printf.fprintf Pervasives.stderr "Shifting (STRING) to state 5\n%!";
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState25

and _menhir_run27 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    Printf.fprintf Pervasives.stderr "State 27:\n%!";
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENTIFIER _v ->
        Printf.fprintf Pervasives.stderr "Shifting (IDENTIFIER) to state 8\n%!";
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
    | LEFTPAREN ->
        Printf.fprintf Pervasives.stderr "Shifting (LEFTPAREN) to state 7\n%!";
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | NUMBER _v ->
        Printf.fprintf Pervasives.stderr "Shifting (NUMBER) to state 6\n%!";
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
    | STRING _v ->
        Printf.fprintf Pervasives.stderr "Shifting (STRING) to state 5\n%!";
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState27

and _menhir_run29 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    Printf.fprintf Pervasives.stderr "State 29:\n%!";
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENTIFIER _v ->
        Printf.fprintf Pervasives.stderr "Shifting (IDENTIFIER) to state 8\n%!";
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
    | LEFTPAREN ->
        Printf.fprintf Pervasives.stderr "Shifting (LEFTPAREN) to state 7\n%!";
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | NUMBER _v ->
        Printf.fprintf Pervasives.stderr "Shifting (NUMBER) to state 6\n%!";
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
    | STRING _v ->
        Printf.fprintf Pervasives.stderr "Shifting (STRING) to state 5\n%!";
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState29

and _menhir_run31 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    Printf.fprintf Pervasives.stderr "State 31:\n%!";
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENTIFIER _v ->
        Printf.fprintf Pervasives.stderr "Shifting (IDENTIFIER) to state 8\n%!";
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
    | LEFTPAREN ->
        Printf.fprintf Pervasives.stderr "Shifting (LEFTPAREN) to state 7\n%!";
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | NUMBER _v ->
        Printf.fprintf Pervasives.stderr "Shifting (NUMBER) to state 6\n%!";
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
    | STRING _v ->
        Printf.fprintf Pervasives.stderr "Shifting (STRING) to state 5\n%!";
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState31

and _menhir_run17 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    Printf.fprintf Pervasives.stderr "State 17:\n%!";
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENTIFIER _v ->
        Printf.fprintf Pervasives.stderr "Shifting (IDENTIFIER) to state 8\n%!";
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _v
    | LEFTPAREN ->
        Printf.fprintf Pervasives.stderr "Shifting (LEFTPAREN) to state 7\n%!";
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | NUMBER _v ->
        Printf.fprintf Pervasives.stderr "Shifting (NUMBER) to state 6\n%!";
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _v
    | STRING _v ->
        Printf.fprintf Pervasives.stderr "Shifting (STRING) to state 5\n%!";
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState17

and _menhir_goto_actuals_opt : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.expr list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState9 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 33:\n%!";
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RIGHTPAREN ->
            Printf.fprintf Pervasives.stderr "Shifting (RIGHTPAREN) to state 34\n%!";
            let _menhir_stack = Obj.magic _menhir_stack in
            Printf.fprintf Pervasives.stderr "State 34:\n%!";
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            Printf.fprintf Pervasives.stderr "Reducing production expr -> IDENTIFIER LEFTPAREN actuals_opt RIGHTPAREN \n%!";
            let ((_menhir_stack, _menhir_s, _1), _, _3) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _v : (Ast.expr) =                                                 ( Call(_1, _3) ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState62 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 63:\n%!";
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RIGHTPAREN ->
            Printf.fprintf Pervasives.stderr "Shifting (RIGHTPAREN) to state 64\n%!";
            let _menhir_stack = Obj.magic _menhir_stack in
            Printf.fprintf Pervasives.stderr "State 64:\n%!";
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            Printf.fprintf Pervasives.stderr "Reducing production stmt -> IDENTIFIER ASSIGN MRBK LEFTPAREN actuals_opt RIGHTPAREN \n%!";
            let (((_menhir_stack, _menhir_s, _1), _), _, _5) = _menhir_stack in
            let _6 = () in
            let _4 = () in
            let _3 = () in
            let _2 = () in
            let _v : (Ast.stmt) =                                                             ( Mrbk(_1,_5) ) in
            _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState67 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 68:\n%!";
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RIGHTPAREN ->
            Printf.fprintf Pervasives.stderr "Shifting (RIGHTPAREN) to state 69\n%!";
            let _menhir_stack = Obj.magic _menhir_stack in
            Printf.fprintf Pervasives.stderr "State 69:\n%!";
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LEFTBRACE ->
                Printf.fprintf Pervasives.stderr "Shifting (LEFTBRACE) to state 70\n%!";
                let _menhir_stack = Obj.magic _menhir_stack in
                Printf.fprintf Pervasives.stderr "State 70:\n%!";
                let _menhir_env = _menhir_discard _menhir_env in
                _menhir_reduce40 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | _ ->
                assert (not _menhir_env._menhir_error);
                Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_expr_opt : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState74 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 83:\n%!";
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | SEMICOLON ->
            Printf.fprintf Pervasives.stderr "Shifting (SEMICOLON) to state 84\n%!";
            let _menhir_stack = Obj.magic _menhir_stack in
            Printf.fprintf Pervasives.stderr "State 84:\n%!";
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | IDENTIFIER _v ->
                Printf.fprintf Pervasives.stderr "Shifting (IDENTIFIER) to state 8\n%!";
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
            | LEFTPAREN ->
                Printf.fprintf Pervasives.stderr "Shifting (LEFTPAREN) to state 7\n%!";
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState84
            | NUMBER _v ->
                Printf.fprintf Pervasives.stderr "Shifting (NUMBER) to state 6\n%!";
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
            | STRING _v ->
                Printf.fprintf Pervasives.stderr "Shifting (STRING) to state 5\n%!";
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
            | SEMICOLON ->
                _menhir_reduce24 _menhir_env (Obj.magic _menhir_stack) MenhirState84
            | _ ->
                assert (not _menhir_env._menhir_error);
                Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState84)
        | _ ->
            assert (not _menhir_env._menhir_error);
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState84 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 85:\n%!";
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | SEMICOLON ->
            Printf.fprintf Pervasives.stderr "Shifting (SEMICOLON) to state 86\n%!";
            let _menhir_stack = Obj.magic _menhir_stack in
            Printf.fprintf Pervasives.stderr "State 86:\n%!";
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | IDENTIFIER _v ->
                Printf.fprintf Pervasives.stderr "Shifting (IDENTIFIER) to state 8\n%!";
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v
            | LEFTPAREN ->
                Printf.fprintf Pervasives.stderr "Shifting (LEFTPAREN) to state 7\n%!";
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState86
            | NUMBER _v ->
                Printf.fprintf Pervasives.stderr "Shifting (NUMBER) to state 6\n%!";
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v
            | STRING _v ->
                Printf.fprintf Pervasives.stderr "Shifting (STRING) to state 5\n%!";
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v
            | RIGHTPAREN ->
                _menhir_reduce24 _menhir_env (Obj.magic _menhir_stack) MenhirState86
            | _ ->
                assert (not _menhir_env._menhir_error);
                Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState86)
        | _ ->
            assert (not _menhir_env._menhir_error);
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState86 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 87:\n%!";
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RIGHTPAREN ->
            Printf.fprintf Pervasives.stderr "Shifting (RIGHTPAREN) to state 88\n%!";
            let _menhir_stack = Obj.magic _menhir_stack in
            Printf.fprintf Pervasives.stderr "State 88:\n%!";
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | FOR ->
                Printf.fprintf Pervasives.stderr "Shifting (FOR) to state 73\n%!";
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState88
            | FUNCTION ->
                Printf.fprintf Pervasives.stderr "Shifting (FUNCTION) to state 65\n%!";
                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState88
            | IDENTIFIER _v ->
                Printf.fprintf Pervasives.stderr "Shifting (IDENTIFIER) to state 59\n%!";
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
            | IF ->
                Printf.fprintf Pervasives.stderr "Shifting (IF) to state 55\n%!";
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState88
            | LEFTBRACE ->
                Printf.fprintf Pervasives.stderr "Shifting (LEFTBRACE) to state 52\n%!";
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState88
            | LEFTPAREN ->
                Printf.fprintf Pervasives.stderr "Shifting (LEFTPAREN) to state 7\n%!";
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState88
            | NUMBER _v ->
                Printf.fprintf Pervasives.stderr "Shifting (NUMBER) to state 6\n%!";
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
            | RETURN ->
                Printf.fprintf Pervasives.stderr "Shifting (RETURN) to state 50\n%!";
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState88
            | STRING _v ->
                Printf.fprintf Pervasives.stderr "Shifting (STRING) to state 5\n%!";
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
            | WHILE ->
                Printf.fprintf Pervasives.stderr "Shifting (WHILE) to state 3\n%!";
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState88
            | _ ->
                assert (not _menhir_env._menhir_error);
                Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState88)
        | _ ->
            assert (not _menhir_env._menhir_error);
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState67 | MenhirState62 | MenhirState9 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 10:\n%!";
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            Printf.fprintf Pervasives.stderr "Shifting (DIVIDE) to state 17\n%!";
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            Printf.fprintf Pervasives.stderr "Shifting (EQ) to state 31\n%!";
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            Printf.fprintf Pervasives.stderr "Shifting (GEQ) to state 29\n%!";
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            Printf.fprintf Pervasives.stderr "Shifting (GT) to state 27\n%!";
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            Printf.fprintf Pervasives.stderr "Shifting (LEQ) to state 25\n%!";
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            Printf.fprintf Pervasives.stderr "Shifting (LT) to state 23\n%!";
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 21\n%!";
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            Printf.fprintf Pervasives.stderr "Shifting (MOD) to state 13\n%!";
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            Printf.fprintf Pervasives.stderr "Shifting (NEQ) to state 19\n%!";
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            Printf.fprintf Pervasives.stderr "Shifting (PLUS) to state 15\n%!";
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            Printf.fprintf Pervasives.stderr "Shifting (TIMES) to state 11\n%!";
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | RIGHTPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            Printf.fprintf Pervasives.stderr "Reducing production actuals_list -> expr \n%!";
            let (_menhir_stack, _menhir_s, _1) = _menhir_stack in
            let _v : (Ast.expr list) =                             ( [_1] ) in
            _menhir_goto_actuals_list _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState11 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 12:\n%!";
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | MOD ->
            Printf.fprintf Pervasives.stderr "Shifting (MOD) to state 13\n%!";
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | DIVIDE | ELSE | EQ | FOR | FUNCTION | GEQ | GT | IDENTIFIER _ | IF | LEFTBRACE | LEFTPAREN | LEQ | LT | MINUS | NEQ | NUMBER _ | PLUS | RETURN | RIGHTBRACE | RIGHTBRACK | RIGHTPAREN | SEMICOLON | STRING _ | TIMES | WHILE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            Printf.fprintf Pervasives.stderr "Reducing production expr -> expr TIMES expr \n%!";
            let ((_menhir_stack, _menhir_s, _1), _, _3) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.expr) =                      ( Binop(_1, Mult,  _3) ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState13 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 14:\n%!";
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            Printf.fprintf Pervasives.stderr "Shifting (DIVIDE) to state 17\n%!";
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            Printf.fprintf Pervasives.stderr "Shifting (EQ) to state 31\n%!";
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            Printf.fprintf Pervasives.stderr "Shifting (GEQ) to state 29\n%!";
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            Printf.fprintf Pervasives.stderr "Shifting (GT) to state 27\n%!";
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            Printf.fprintf Pervasives.stderr "Shifting (LEQ) to state 25\n%!";
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            Printf.fprintf Pervasives.stderr "Shifting (LT) to state 23\n%!";
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 21\n%!";
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            Printf.fprintf Pervasives.stderr "Shifting (MOD) to state 13\n%!";
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            Printf.fprintf Pervasives.stderr "Shifting (NEQ) to state 19\n%!";
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            Printf.fprintf Pervasives.stderr "Shifting (PLUS) to state 15\n%!";
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            Printf.fprintf Pervasives.stderr "Shifting (TIMES) to state 11\n%!";
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | ELSE | FOR | FUNCTION | IDENTIFIER _ | IF | LEFTBRACE | LEFTPAREN | NUMBER _ | RETURN | RIGHTBRACE | RIGHTBRACK | RIGHTPAREN | SEMICOLON | STRING _ | WHILE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            Printf.fprintf Pervasives.stderr "Reducing production expr -> expr MOD expr \n%!";
            let ((_menhir_stack, _menhir_s, _1), _, _3) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.expr) =                      ( Binop(_1, Mod,   _3) ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState15 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 16:\n%!";
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            Printf.fprintf Pervasives.stderr "Shifting (DIVIDE) to state 17\n%!";
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            Printf.fprintf Pervasives.stderr "Shifting (MOD) to state 13\n%!";
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            Printf.fprintf Pervasives.stderr "Shifting (TIMES) to state 11\n%!";
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | ELSE | EQ | FOR | FUNCTION | GEQ | GT | IDENTIFIER _ | IF | LEFTBRACE | LEFTPAREN | LEQ | LT | MINUS | NEQ | NUMBER _ | PLUS | RETURN | RIGHTBRACE | RIGHTBRACK | RIGHTPAREN | SEMICOLON | STRING _ | WHILE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            Printf.fprintf Pervasives.stderr "Reducing production expr -> expr PLUS expr \n%!";
            let ((_menhir_stack, _menhir_s, _1), _, _3) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.expr) =                      ( Binop(_1, Add,   _3) ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState17 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 18:\n%!";
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | MOD ->
            Printf.fprintf Pervasives.stderr "Shifting (MOD) to state 13\n%!";
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | DIVIDE | ELSE | EQ | FOR | FUNCTION | GEQ | GT | IDENTIFIER _ | IF | LEFTBRACE | LEFTPAREN | LEQ | LT | MINUS | NEQ | NUMBER _ | PLUS | RETURN | RIGHTBRACE | RIGHTBRACK | RIGHTPAREN | SEMICOLON | STRING _ | TIMES | WHILE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            Printf.fprintf Pervasives.stderr "Reducing production expr -> expr DIVIDE expr \n%!";
            let ((_menhir_stack, _menhir_s, _1), _, _3) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.expr) =                      ( Binop(_1, Div,   _3) ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState19 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 20:\n%!";
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            Printf.fprintf Pervasives.stderr "Shifting (DIVIDE) to state 17\n%!";
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            Printf.fprintf Pervasives.stderr "Shifting (GEQ) to state 29\n%!";
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            Printf.fprintf Pervasives.stderr "Shifting (GT) to state 27\n%!";
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            Printf.fprintf Pervasives.stderr "Shifting (LEQ) to state 25\n%!";
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            Printf.fprintf Pervasives.stderr "Shifting (LT) to state 23\n%!";
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 21\n%!";
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            Printf.fprintf Pervasives.stderr "Shifting (MOD) to state 13\n%!";
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            Printf.fprintf Pervasives.stderr "Shifting (PLUS) to state 15\n%!";
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            Printf.fprintf Pervasives.stderr "Shifting (TIMES) to state 11\n%!";
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | ELSE | EQ | FOR | FUNCTION | IDENTIFIER _ | IF | LEFTBRACE | LEFTPAREN | NEQ | NUMBER _ | RETURN | RIGHTBRACE | RIGHTBRACK | RIGHTPAREN | SEMICOLON | STRING _ | WHILE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            Printf.fprintf Pervasives.stderr "Reducing production expr -> expr NEQ expr \n%!";
            let ((_menhir_stack, _menhir_s, _1), _, _3) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.expr) =                      ( Binop(_1, Neq,   _3) ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState21 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 22:\n%!";
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            Printf.fprintf Pervasives.stderr "Shifting (DIVIDE) to state 17\n%!";
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            Printf.fprintf Pervasives.stderr "Shifting (MOD) to state 13\n%!";
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            Printf.fprintf Pervasives.stderr "Shifting (TIMES) to state 11\n%!";
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | ELSE | EQ | FOR | FUNCTION | GEQ | GT | IDENTIFIER _ | IF | LEFTBRACE | LEFTPAREN | LEQ | LT | MINUS | NEQ | NUMBER _ | PLUS | RETURN | RIGHTBRACE | RIGHTBRACK | RIGHTPAREN | SEMICOLON | STRING _ | WHILE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            Printf.fprintf Pervasives.stderr "Reducing production expr -> expr MINUS expr \n%!";
            let ((_menhir_stack, _menhir_s, _1), _, _3) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.expr) =                      ( Binop(_1, Sub,   _3) ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState23 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 24:\n%!";
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            Printf.fprintf Pervasives.stderr "Shifting (DIVIDE) to state 17\n%!";
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 21\n%!";
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            Printf.fprintf Pervasives.stderr "Shifting (MOD) to state 13\n%!";
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            Printf.fprintf Pervasives.stderr "Shifting (PLUS) to state 15\n%!";
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            Printf.fprintf Pervasives.stderr "Shifting (TIMES) to state 11\n%!";
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | ELSE | EQ | FOR | FUNCTION | GEQ | GT | IDENTIFIER _ | IF | LEFTBRACE | LEFTPAREN | LEQ | LT | NEQ | NUMBER _ | RETURN | RIGHTBRACE | RIGHTBRACK | RIGHTPAREN | SEMICOLON | STRING _ | WHILE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            Printf.fprintf Pervasives.stderr "Reducing production expr -> expr LT expr \n%!";
            let ((_menhir_stack, _menhir_s, _1), _, _3) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.expr) =                      ( Binop(_1, Less,  _3) ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState25 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 26:\n%!";
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            Printf.fprintf Pervasives.stderr "Shifting (DIVIDE) to state 17\n%!";
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 21\n%!";
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            Printf.fprintf Pervasives.stderr "Shifting (MOD) to state 13\n%!";
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            Printf.fprintf Pervasives.stderr "Shifting (PLUS) to state 15\n%!";
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            Printf.fprintf Pervasives.stderr "Shifting (TIMES) to state 11\n%!";
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | ELSE | EQ | FOR | FUNCTION | GEQ | GT | IDENTIFIER _ | IF | LEFTBRACE | LEFTPAREN | LEQ | LT | NEQ | NUMBER _ | RETURN | RIGHTBRACE | RIGHTBRACK | RIGHTPAREN | SEMICOLON | STRING _ | WHILE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            Printf.fprintf Pervasives.stderr "Reducing production expr -> expr LEQ expr \n%!";
            let ((_menhir_stack, _menhir_s, _1), _, _3) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.expr) =                      ( Binop(_1, Leq,   _3) ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState27 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 28:\n%!";
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            Printf.fprintf Pervasives.stderr "Shifting (DIVIDE) to state 17\n%!";
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 21\n%!";
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            Printf.fprintf Pervasives.stderr "Shifting (MOD) to state 13\n%!";
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            Printf.fprintf Pervasives.stderr "Shifting (PLUS) to state 15\n%!";
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            Printf.fprintf Pervasives.stderr "Shifting (TIMES) to state 11\n%!";
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | ELSE | EQ | FOR | FUNCTION | GEQ | GT | IDENTIFIER _ | IF | LEFTBRACE | LEFTPAREN | LEQ | LT | NEQ | NUMBER _ | RETURN | RIGHTBRACE | RIGHTBRACK | RIGHTPAREN | SEMICOLON | STRING _ | WHILE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            Printf.fprintf Pervasives.stderr "Reducing production expr -> expr GT expr \n%!";
            let ((_menhir_stack, _menhir_s, _1), _, _3) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.expr) =                      ( Binop(_1, Greater,  _3) ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState29 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 30:\n%!";
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            Printf.fprintf Pervasives.stderr "Shifting (DIVIDE) to state 17\n%!";
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 21\n%!";
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            Printf.fprintf Pervasives.stderr "Shifting (MOD) to state 13\n%!";
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            Printf.fprintf Pervasives.stderr "Shifting (PLUS) to state 15\n%!";
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            Printf.fprintf Pervasives.stderr "Shifting (TIMES) to state 11\n%!";
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | ELSE | EQ | FOR | FUNCTION | GEQ | GT | IDENTIFIER _ | IF | LEFTBRACE | LEFTPAREN | LEQ | LT | NEQ | NUMBER _ | RETURN | RIGHTBRACE | RIGHTBRACK | RIGHTPAREN | SEMICOLON | STRING _ | WHILE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            Printf.fprintf Pervasives.stderr "Reducing production expr -> expr GEQ expr \n%!";
            let ((_menhir_stack, _menhir_s, _1), _, _3) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.expr) =                      ( Binop(_1, Geq,   _3) ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 32:\n%!";
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            Printf.fprintf Pervasives.stderr "Shifting (DIVIDE) to state 17\n%!";
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            Printf.fprintf Pervasives.stderr "Shifting (GEQ) to state 29\n%!";
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            Printf.fprintf Pervasives.stderr "Shifting (GT) to state 27\n%!";
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            Printf.fprintf Pervasives.stderr "Shifting (LEQ) to state 25\n%!";
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            Printf.fprintf Pervasives.stderr "Shifting (LT) to state 23\n%!";
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 21\n%!";
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            Printf.fprintf Pervasives.stderr "Shifting (MOD) to state 13\n%!";
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            Printf.fprintf Pervasives.stderr "Shifting (PLUS) to state 15\n%!";
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            Printf.fprintf Pervasives.stderr "Shifting (TIMES) to state 11\n%!";
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | ELSE | EQ | FOR | FUNCTION | IDENTIFIER _ | IF | LEFTBRACE | LEFTPAREN | NEQ | NUMBER _ | RETURN | RIGHTBRACE | RIGHTBRACK | RIGHTPAREN | SEMICOLON | STRING _ | WHILE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            Printf.fprintf Pervasives.stderr "Reducing production expr -> expr EQ expr \n%!";
            let ((_menhir_stack, _menhir_s, _1), _, _3) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.expr) =                      ( Binop(_1, Equal, _3) ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState36 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 37:\n%!";
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            Printf.fprintf Pervasives.stderr "Shifting (DIVIDE) to state 17\n%!";
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            Printf.fprintf Pervasives.stderr "Shifting (EQ) to state 31\n%!";
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            Printf.fprintf Pervasives.stderr "Shifting (GEQ) to state 29\n%!";
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            Printf.fprintf Pervasives.stderr "Shifting (GT) to state 27\n%!";
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            Printf.fprintf Pervasives.stderr "Shifting (LEQ) to state 25\n%!";
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            Printf.fprintf Pervasives.stderr "Shifting (LT) to state 23\n%!";
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 21\n%!";
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            Printf.fprintf Pervasives.stderr "Shifting (MOD) to state 13\n%!";
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            Printf.fprintf Pervasives.stderr "Shifting (NEQ) to state 19\n%!";
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            Printf.fprintf Pervasives.stderr "Shifting (PLUS) to state 15\n%!";
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            Printf.fprintf Pervasives.stderr "Shifting (TIMES) to state 11\n%!";
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | RIGHTPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            Printf.fprintf Pervasives.stderr "Reducing production actuals_list -> actuals_list COMMA expr \n%!";
            let ((_menhir_stack, _menhir_s, _1), _, _3) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.expr list) =                             ( _3 :: _1 ) in
            _menhir_goto_actuals_list _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState38 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 39:\n%!";
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            Printf.fprintf Pervasives.stderr "Shifting (DIVIDE) to state 17\n%!";
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            Printf.fprintf Pervasives.stderr "Shifting (EQ) to state 31\n%!";
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            Printf.fprintf Pervasives.stderr "Shifting (GEQ) to state 29\n%!";
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            Printf.fprintf Pervasives.stderr "Shifting (GT) to state 27\n%!";
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            Printf.fprintf Pervasives.stderr "Shifting (LEQ) to state 25\n%!";
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            Printf.fprintf Pervasives.stderr "Shifting (LT) to state 23\n%!";
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 21\n%!";
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            Printf.fprintf Pervasives.stderr "Shifting (MOD) to state 13\n%!";
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            Printf.fprintf Pervasives.stderr "Shifting (NEQ) to state 19\n%!";
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            Printf.fprintf Pervasives.stderr "Shifting (PLUS) to state 15\n%!";
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack)
        | RIGHTBRACK ->
            Printf.fprintf Pervasives.stderr "Shifting (RIGHTBRACK) to state 40\n%!";
            let _menhir_stack = Obj.magic _menhir_stack in
            Printf.fprintf Pervasives.stderr "State 40:\n%!";
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            Printf.fprintf Pervasives.stderr "Reducing production expr -> IDENTIFIER LEFTBRACK expr RIGHTBRACK \n%!";
            let ((_menhir_stack, _menhir_s, _1), _, _3) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _v : (Ast.expr) =                                          ( Array(_1,_3) ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            Printf.fprintf Pervasives.stderr "Shifting (TIMES) to state 11\n%!";
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState60 | MenhirState41 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 42:\n%!";
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            Printf.fprintf Pervasives.stderr "Shifting (DIVIDE) to state 17\n%!";
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            Printf.fprintf Pervasives.stderr "Shifting (EQ) to state 31\n%!";
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            Printf.fprintf Pervasives.stderr "Shifting (GEQ) to state 29\n%!";
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            Printf.fprintf Pervasives.stderr "Shifting (GT) to state 27\n%!";
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            Printf.fprintf Pervasives.stderr "Shifting (LEQ) to state 25\n%!";
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            Printf.fprintf Pervasives.stderr "Shifting (LT) to state 23\n%!";
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 21\n%!";
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            Printf.fprintf Pervasives.stderr "Shifting (MOD) to state 13\n%!";
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            Printf.fprintf Pervasives.stderr "Shifting (NEQ) to state 19\n%!";
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            Printf.fprintf Pervasives.stderr "Shifting (PLUS) to state 15\n%!";
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            Printf.fprintf Pervasives.stderr "Shifting (TIMES) to state 11\n%!";
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | ELSE | FOR | FUNCTION | IDENTIFIER _ | IF | LEFTBRACE | LEFTPAREN | NUMBER _ | RETURN | RIGHTBRACE | RIGHTBRACK | RIGHTPAREN | SEMICOLON | STRING _ | WHILE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            Printf.fprintf Pervasives.stderr "Reducing production expr -> IDENTIFIER ASSIGN expr \n%!";
            let ((_menhir_stack, _menhir_s, _1), _, _3) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.expr) =                              ( Assign(_1, _3) ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState7 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 43:\n%!";
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            Printf.fprintf Pervasives.stderr "Shifting (COMMA) to state 45\n%!";
            let _menhir_stack = Obj.magic _menhir_stack in
            Printf.fprintf Pervasives.stderr "State 45:\n%!";
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | IDENTIFIER _v ->
                Printf.fprintf Pervasives.stderr "Shifting (IDENTIFIER) to state 8\n%!";
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
            | LEFTPAREN ->
                Printf.fprintf Pervasives.stderr "Shifting (LEFTPAREN) to state 7\n%!";
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState45
            | NUMBER _v ->
                Printf.fprintf Pervasives.stderr "Shifting (NUMBER) to state 6\n%!";
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
            | STRING _v ->
                Printf.fprintf Pervasives.stderr "Shifting (STRING) to state 5\n%!";
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState45)
        | DIVIDE ->
            Printf.fprintf Pervasives.stderr "Shifting (DIVIDE) to state 17\n%!";
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            Printf.fprintf Pervasives.stderr "Shifting (EQ) to state 31\n%!";
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            Printf.fprintf Pervasives.stderr "Shifting (GEQ) to state 29\n%!";
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            Printf.fprintf Pervasives.stderr "Shifting (GT) to state 27\n%!";
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            Printf.fprintf Pervasives.stderr "Shifting (LEQ) to state 25\n%!";
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            Printf.fprintf Pervasives.stderr "Shifting (LT) to state 23\n%!";
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 21\n%!";
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            Printf.fprintf Pervasives.stderr "Shifting (MOD) to state 13\n%!";
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            Printf.fprintf Pervasives.stderr "Shifting (NEQ) to state 19\n%!";
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            Printf.fprintf Pervasives.stderr "Shifting (PLUS) to state 15\n%!";
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack)
        | RIGHTPAREN ->
            Printf.fprintf Pervasives.stderr "Shifting (RIGHTPAREN) to state 44\n%!";
            let _menhir_stack = Obj.magic _menhir_stack in
            Printf.fprintf Pervasives.stderr "State 44:\n%!";
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            Printf.fprintf Pervasives.stderr "Reducing production expr -> LEFTPAREN expr RIGHTPAREN \n%!";
            let ((_menhir_stack, _menhir_s), _, _2) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.expr) =                               ( _2 ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            Printf.fprintf Pervasives.stderr "Shifting (TIMES) to state 11\n%!";
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState45 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 46:\n%!";
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            Printf.fprintf Pervasives.stderr "Shifting (DIVIDE) to state 17\n%!";
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            Printf.fprintf Pervasives.stderr "Shifting (EQ) to state 31\n%!";
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            Printf.fprintf Pervasives.stderr "Shifting (GEQ) to state 29\n%!";
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            Printf.fprintf Pervasives.stderr "Shifting (GT) to state 27\n%!";
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            Printf.fprintf Pervasives.stderr "Shifting (LEQ) to state 25\n%!";
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            Printf.fprintf Pervasives.stderr "Shifting (LT) to state 23\n%!";
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 21\n%!";
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            Printf.fprintf Pervasives.stderr "Shifting (MOD) to state 13\n%!";
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            Printf.fprintf Pervasives.stderr "Shifting (NEQ) to state 19\n%!";
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            Printf.fprintf Pervasives.stderr "Shifting (PLUS) to state 15\n%!";
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack)
        | RIGHTPAREN ->
            Printf.fprintf Pervasives.stderr "Shifting (RIGHTPAREN) to state 47\n%!";
            let _menhir_stack = Obj.magic _menhir_stack in
            Printf.fprintf Pervasives.stderr "State 47:\n%!";
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            Printf.fprintf Pervasives.stderr "Reducing production expr -> LEFTPAREN expr COMMA expr RIGHTPAREN \n%!";
            let (((_menhir_stack, _menhir_s), _, _2), _, _4) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.expr) =                                          ( KeyValue(_2,_4) ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            Printf.fprintf Pervasives.stderr "Shifting (TIMES) to state 11\n%!";
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState4 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 48:\n%!";
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            Printf.fprintf Pervasives.stderr "Shifting (DIVIDE) to state 17\n%!";
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            Printf.fprintf Pervasives.stderr "Shifting (EQ) to state 31\n%!";
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            Printf.fprintf Pervasives.stderr "Shifting (GEQ) to state 29\n%!";
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            Printf.fprintf Pervasives.stderr "Shifting (GT) to state 27\n%!";
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            Printf.fprintf Pervasives.stderr "Shifting (LEQ) to state 25\n%!";
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            Printf.fprintf Pervasives.stderr "Shifting (LT) to state 23\n%!";
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 21\n%!";
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            Printf.fprintf Pervasives.stderr "Shifting (MOD) to state 13\n%!";
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            Printf.fprintf Pervasives.stderr "Shifting (NEQ) to state 19\n%!";
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            Printf.fprintf Pervasives.stderr "Shifting (PLUS) to state 15\n%!";
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack)
        | RIGHTPAREN ->
            Printf.fprintf Pervasives.stderr "Shifting (RIGHTPAREN) to state 49\n%!";
            let _menhir_stack = Obj.magic _menhir_stack in
            Printf.fprintf Pervasives.stderr "State 49:\n%!";
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | FOR ->
                Printf.fprintf Pervasives.stderr "Shifting (FOR) to state 73\n%!";
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState49
            | FUNCTION ->
                Printf.fprintf Pervasives.stderr "Shifting (FUNCTION) to state 65\n%!";
                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState49
            | IDENTIFIER _v ->
                Printf.fprintf Pervasives.stderr "Shifting (IDENTIFIER) to state 59\n%!";
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
            | IF ->
                Printf.fprintf Pervasives.stderr "Shifting (IF) to state 55\n%!";
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState49
            | LEFTBRACE ->
                Printf.fprintf Pervasives.stderr "Shifting (LEFTBRACE) to state 52\n%!";
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState49
            | LEFTPAREN ->
                Printf.fprintf Pervasives.stderr "Shifting (LEFTPAREN) to state 7\n%!";
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState49
            | NUMBER _v ->
                Printf.fprintf Pervasives.stderr "Shifting (NUMBER) to state 6\n%!";
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
            | RETURN ->
                Printf.fprintf Pervasives.stderr "Shifting (RETURN) to state 50\n%!";
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState49
            | STRING _v ->
                Printf.fprintf Pervasives.stderr "Shifting (STRING) to state 5\n%!";
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
            | WHILE ->
                Printf.fprintf Pervasives.stderr "Shifting (WHILE) to state 3\n%!";
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState49
            | _ ->
                assert (not _menhir_env._menhir_error);
                Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState49)
        | TIMES ->
            Printf.fprintf Pervasives.stderr "Shifting (TIMES) to state 11\n%!";
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState50 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 51:\n%!";
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            Printf.fprintf Pervasives.stderr "Shifting (DIVIDE) to state 17\n%!";
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            Printf.fprintf Pervasives.stderr "Shifting (EQ) to state 31\n%!";
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            Printf.fprintf Pervasives.stderr "Shifting (GEQ) to state 29\n%!";
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            Printf.fprintf Pervasives.stderr "Shifting (GT) to state 27\n%!";
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            Printf.fprintf Pervasives.stderr "Shifting (LEQ) to state 25\n%!";
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            Printf.fprintf Pervasives.stderr "Shifting (LT) to state 23\n%!";
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 21\n%!";
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            Printf.fprintf Pervasives.stderr "Shifting (MOD) to state 13\n%!";
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            Printf.fprintf Pervasives.stderr "Shifting (NEQ) to state 19\n%!";
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            Printf.fprintf Pervasives.stderr "Shifting (PLUS) to state 15\n%!";
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            Printf.fprintf Pervasives.stderr "Shifting (TIMES) to state 11\n%!";
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack)
        | ELSE | FOR | FUNCTION | IDENTIFIER _ | IF | LEFTBRACE | LEFTPAREN | NUMBER _ | RETURN | RIGHTBRACE | STRING _ | WHILE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            Printf.fprintf Pervasives.stderr "Reducing production stmt -> RETURN expr \n%!";
            let ((_menhir_stack, _menhir_s), _, _2) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.stmt) =                  ( Return(_2) ) in
            _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState56 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 57:\n%!";
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            Printf.fprintf Pervasives.stderr "Shifting (DIVIDE) to state 17\n%!";
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            Printf.fprintf Pervasives.stderr "Shifting (EQ) to state 31\n%!";
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            Printf.fprintf Pervasives.stderr "Shifting (GEQ) to state 29\n%!";
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            Printf.fprintf Pervasives.stderr "Shifting (GT) to state 27\n%!";
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            Printf.fprintf Pervasives.stderr "Shifting (LEQ) to state 25\n%!";
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            Printf.fprintf Pervasives.stderr "Shifting (LT) to state 23\n%!";
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 21\n%!";
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            Printf.fprintf Pervasives.stderr "Shifting (MOD) to state 13\n%!";
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            Printf.fprintf Pervasives.stderr "Shifting (NEQ) to state 19\n%!";
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            Printf.fprintf Pervasives.stderr "Shifting (PLUS) to state 15\n%!";
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack)
        | RIGHTPAREN ->
            Printf.fprintf Pervasives.stderr "Shifting (RIGHTPAREN) to state 58\n%!";
            let _menhir_stack = Obj.magic _menhir_stack in
            Printf.fprintf Pervasives.stderr "State 58:\n%!";
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | FOR ->
                Printf.fprintf Pervasives.stderr "Shifting (FOR) to state 73\n%!";
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState58
            | FUNCTION ->
                Printf.fprintf Pervasives.stderr "Shifting (FUNCTION) to state 65\n%!";
                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState58
            | IDENTIFIER _v ->
                Printf.fprintf Pervasives.stderr "Shifting (IDENTIFIER) to state 59\n%!";
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
            | IF ->
                Printf.fprintf Pervasives.stderr "Shifting (IF) to state 55\n%!";
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState58
            | LEFTBRACE ->
                Printf.fprintf Pervasives.stderr "Shifting (LEFTBRACE) to state 52\n%!";
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState58
            | LEFTPAREN ->
                Printf.fprintf Pervasives.stderr "Shifting (LEFTPAREN) to state 7\n%!";
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState58
            | NUMBER _v ->
                Printf.fprintf Pervasives.stderr "Shifting (NUMBER) to state 6\n%!";
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
            | RETURN ->
                Printf.fprintf Pervasives.stderr "Shifting (RETURN) to state 50\n%!";
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState58
            | STRING _v ->
                Printf.fprintf Pervasives.stderr "Shifting (STRING) to state 5\n%!";
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
            | WHILE ->
                Printf.fprintf Pervasives.stderr "Shifting (WHILE) to state 3\n%!";
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState58
            | _ ->
                assert (not _menhir_env._menhir_error);
                Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState58)
        | TIMES ->
            Printf.fprintf Pervasives.stderr "Shifting (TIMES) to state 11\n%!";
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState102 | MenhirState2 | MenhirState49 | MenhirState53 | MenhirState58 | MenhirState93 | MenhirState71 | MenhirState88 | MenhirState80 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 82:\n%!";
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            Printf.fprintf Pervasives.stderr "Shifting (DIVIDE) to state 17\n%!";
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            Printf.fprintf Pervasives.stderr "Shifting (EQ) to state 31\n%!";
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            Printf.fprintf Pervasives.stderr "Shifting (GEQ) to state 29\n%!";
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            Printf.fprintf Pervasives.stderr "Shifting (GT) to state 27\n%!";
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            Printf.fprintf Pervasives.stderr "Shifting (LEQ) to state 25\n%!";
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            Printf.fprintf Pervasives.stderr "Shifting (LT) to state 23\n%!";
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 21\n%!";
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            Printf.fprintf Pervasives.stderr "Shifting (MOD) to state 13\n%!";
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            Printf.fprintf Pervasives.stderr "Shifting (NEQ) to state 19\n%!";
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            Printf.fprintf Pervasives.stderr "Shifting (PLUS) to state 15\n%!";
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            Printf.fprintf Pervasives.stderr "Shifting (TIMES) to state 11\n%!";
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack)
        | ELSE | FOR | FUNCTION | IDENTIFIER _ | IF | LEFTBRACE | LEFTPAREN | NUMBER _ | RETURN | RIGHTBRACE | STRING _ | WHILE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            Printf.fprintf Pervasives.stderr "Reducing production stmt -> expr \n%!";
            let (_menhir_stack, _menhir_s, _1) = _menhir_stack in
            let _v : (Ast.stmt) =           ( Expr(_1) ) in
            _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState74 | MenhirState84 | MenhirState86 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 90:\n%!";
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            Printf.fprintf Pervasives.stderr "Shifting (DIVIDE) to state 17\n%!";
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            Printf.fprintf Pervasives.stderr "Shifting (EQ) to state 31\n%!";
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            Printf.fprintf Pervasives.stderr "Shifting (GEQ) to state 29\n%!";
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            Printf.fprintf Pervasives.stderr "Shifting (GT) to state 27\n%!";
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            Printf.fprintf Pervasives.stderr "Shifting (LEQ) to state 25\n%!";
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            Printf.fprintf Pervasives.stderr "Shifting (LT) to state 23\n%!";
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 21\n%!";
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            Printf.fprintf Pervasives.stderr "Shifting (MOD) to state 13\n%!";
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            Printf.fprintf Pervasives.stderr "Shifting (NEQ) to state 19\n%!";
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            Printf.fprintf Pervasives.stderr "Shifting (PLUS) to state 15\n%!";
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            Printf.fprintf Pervasives.stderr "Shifting (TIMES) to state 11\n%!";
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack)
        | RIGHTPAREN | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            Printf.fprintf Pervasives.stderr "Reducing production expr_opt -> expr \n%!";
            let (_menhir_stack, _menhir_s, _1) = _menhir_stack in
            let _v : (Ast.expr) =                   ( _1 ) in
            _menhir_goto_expr_opt _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_reduce3 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    Printf.fprintf Pervasives.stderr "Reducing production actuals_opt -> \n%!";
    let _v : (Ast.expr list) =                   ( [] ) in
    _menhir_goto_actuals_opt _menhir_env _menhir_stack _menhir_s _v

and _menhir_run8 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    Printf.fprintf Pervasives.stderr "State 8:\n%!";
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ASSIGN ->
        Printf.fprintf Pervasives.stderr "Shifting (ASSIGN) to state 41\n%!";
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
    | LEFTBRACK ->
        Printf.fprintf Pervasives.stderr "Shifting (LEFTBRACK) to state 38\n%!";
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
    | LEFTPAREN ->
        Printf.fprintf Pervasives.stderr "Shifting (LEFTPAREN) to state 9\n%!";
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack)
    | COMMA | DIVIDE | ELSE | EQ | FOR | FUNCTION | GEQ | GT | IDENTIFIER _ | IF | LEFTBRACE | LEQ | LT | MINUS | MOD | NEQ | NUMBER _ | PLUS | RETURN | RIGHTBRACE | RIGHTBRACK | RIGHTPAREN | SEMICOLON | STRING _ | TIMES | WHILE ->
        _menhir_reduce7 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_reduce24 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    Printf.fprintf Pervasives.stderr "Reducing production expr_opt -> \n%!";
    let _v : (Ast.expr) =                   ( Noexpr ) in
    _menhir_goto_expr_opt _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce7 : _menhir_env -> 'ttv_tail * _menhir_state * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    Printf.fprintf Pervasives.stderr "Reducing production expr -> IDENTIFIER \n%!";
    let (_menhir_stack, _menhir_s, _1) = _menhir_stack in
    let _v : (Ast.expr) =                              ( Id(_1) ) in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run9 : _menhir_env -> 'ttv_tail * _menhir_state * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    Printf.fprintf Pervasives.stderr "State 9:\n%!";
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENTIFIER _v ->
        Printf.fprintf Pervasives.stderr "Shifting (IDENTIFIER) to state 8\n%!";
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
    | LEFTPAREN ->
        Printf.fprintf Pervasives.stderr "Shifting (LEFTPAREN) to state 7\n%!";
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | NUMBER _v ->
        Printf.fprintf Pervasives.stderr "Shifting (NUMBER) to state 6\n%!";
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
    | STRING _v ->
        Printf.fprintf Pervasives.stderr "Shifting (STRING) to state 5\n%!";
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
    | RIGHTPAREN ->
        _menhir_reduce3 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | _ ->
        assert (not _menhir_env._menhir_error);
        Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState9

and _menhir_run38 : _menhir_env -> 'ttv_tail * _menhir_state * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    Printf.fprintf Pervasives.stderr "State 38:\n%!";
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENTIFIER _v ->
        Printf.fprintf Pervasives.stderr "Shifting (IDENTIFIER) to state 8\n%!";
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
    | LEFTPAREN ->
        Printf.fprintf Pervasives.stderr "Shifting (LEFTPAREN) to state 7\n%!";
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | NUMBER _v ->
        Printf.fprintf Pervasives.stderr "Shifting (NUMBER) to state 6\n%!";
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
    | STRING _v ->
        Printf.fprintf Pervasives.stderr "Shifting (STRING) to state 5\n%!";
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState38

and _menhir_run41 : _menhir_env -> 'ttv_tail * _menhir_state * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    Printf.fprintf Pervasives.stderr "State 41:\n%!";
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENTIFIER _v ->
        Printf.fprintf Pervasives.stderr "Shifting (IDENTIFIER) to state 8\n%!";
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
    | LEFTPAREN ->
        Printf.fprintf Pervasives.stderr "Shifting (LEFTPAREN) to state 7\n%!";
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | NUMBER _v ->
        Printf.fprintf Pervasives.stderr "Shifting (NUMBER) to state 6\n%!";
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
    | STRING _v ->
        Printf.fprintf Pervasives.stderr "Shifting (STRING) to state 5\n%!";
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState41

and _menhir_goto_stmt : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.stmt) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState80 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 81:\n%!";
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "Reducing production stmt -> FOR LEFTPAREN IDENTIFIER COMMA IDENTIFIER RIGHTPAREN IN IDENTIFIER stmt \n%!";
        let (((((_menhir_stack, _menhir_s), _, _3), _5), _8), _, _9) = _menhir_stack in
        let _7 = () in
        let _6 = () in
        let _4 = () in
        let _2 = () in
        let _1 = () in
        let _v : (Ast.stmt) =                                                                             ( ForIn(_3,_5,_8,_9) ) in
        _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v
    | MenhirState88 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 89:\n%!";
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "Reducing production stmt -> FOR LEFTPAREN expr_opt SEMICOLON expr_opt SEMICOLON expr_opt RIGHTPAREN stmt \n%!";
        let (((((_menhir_stack, _menhir_s), _, _3), _, _5), _, _7), _, _9) = _menhir_stack in
        let _8 = () in
        let _6 = () in
        let _4 = () in
        let _2 = () in
        let _1 = () in
        let _v : (Ast.stmt) =      ( For(_3, _5, _7, _9) ) in
        _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v
    | MenhirState102 | MenhirState2 | MenhirState53 | MenhirState71 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 91:\n%!";
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "Reducing production stmt_list -> stmt_list stmt \n%!";
        let ((_menhir_stack, _menhir_s, _1), _, _2) = _menhir_stack in
        let _v : (Ast.stmt list) =                    ( _2 :: _1 ) in
        _menhir_goto_stmt_list _menhir_env _menhir_stack _menhir_s _v
    | MenhirState58 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 92:\n%!";
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ELSE ->
            Printf.fprintf Pervasives.stderr "Shifting (ELSE) to state 93\n%!";
            let _menhir_stack = Obj.magic _menhir_stack in
            Printf.fprintf Pervasives.stderr "State 93:\n%!";
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | FOR ->
                Printf.fprintf Pervasives.stderr "Shifting (FOR) to state 73\n%!";
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState93
            | FUNCTION ->
                Printf.fprintf Pervasives.stderr "Shifting (FUNCTION) to state 65\n%!";
                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState93
            | IDENTIFIER _v ->
                Printf.fprintf Pervasives.stderr "Shifting (IDENTIFIER) to state 59\n%!";
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v
            | IF ->
                Printf.fprintf Pervasives.stderr "Shifting (IF) to state 55\n%!";
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState93
            | LEFTBRACE ->
                Printf.fprintf Pervasives.stderr "Shifting (LEFTBRACE) to state 52\n%!";
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState93
            | LEFTPAREN ->
                Printf.fprintf Pervasives.stderr "Shifting (LEFTPAREN) to state 7\n%!";
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState93
            | NUMBER _v ->
                Printf.fprintf Pervasives.stderr "Shifting (NUMBER) to state 6\n%!";
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v
            | RETURN ->
                Printf.fprintf Pervasives.stderr "Shifting (RETURN) to state 50\n%!";
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState93
            | STRING _v ->
                Printf.fprintf Pervasives.stderr "Shifting (STRING) to state 5\n%!";
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v
            | WHILE ->
                Printf.fprintf Pervasives.stderr "Shifting (WHILE) to state 3\n%!";
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState93
            | _ ->
                assert (not _menhir_env._menhir_error);
                Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState93)
        | FOR | FUNCTION | IDENTIFIER _ | IF | LEFTBRACE | LEFTPAREN | NUMBER _ | RETURN | RIGHTBRACE | STRING _ | WHILE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            Printf.fprintf Pervasives.stderr "Reducing production stmt -> IF LEFTPAREN expr RIGHTPAREN stmt \n%!";
            let (((_menhir_stack, _menhir_s), _, _3), _, _5) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Ast.stmt) =                                                    ( If(_3, _5, Block([])) ) in
            _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState93 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 94:\n%!";
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "Reducing production stmt -> IF LEFTPAREN expr RIGHTPAREN stmt ELSE stmt \n%!";
        let ((((_menhir_stack, _menhir_s), _, _3), _, _5), _, _7) = _menhir_stack in
        let _6 = () in
        let _4 = () in
        let _2 = () in
        let _1 = () in
        let _v : (Ast.stmt) =                                                    ( If(_3, _5, _7) ) in
        _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v
    | MenhirState49 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 95:\n%!";
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "Reducing production stmt -> WHILE LEFTPAREN expr RIGHTPAREN stmt \n%!";
        let (((_menhir_stack, _menhir_s), _, _3), _, _5) = _menhir_stack in
        let _4 = () in
        let _2 = () in
        let _1 = () in
        let _v : (Ast.stmt) =                                          ( While(_3, _5) ) in
        _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run3 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    Printf.fprintf Pervasives.stderr "State 3:\n%!";
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LEFTPAREN ->
        Printf.fprintf Pervasives.stderr "Shifting (LEFTPAREN) to state 4\n%!";
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 4:\n%!";
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENTIFIER _v ->
            Printf.fprintf Pervasives.stderr "Shifting (IDENTIFIER) to state 8\n%!";
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
        | LEFTPAREN ->
            Printf.fprintf Pervasives.stderr "Shifting (LEFTPAREN) to state 7\n%!";
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState4
        | NUMBER _v ->
            Printf.fprintf Pervasives.stderr "Shifting (NUMBER) to state 6\n%!";
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
        | STRING _v ->
            Printf.fprintf Pervasives.stderr "Shifting (STRING) to state 5\n%!";
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState4)
    | _ ->
        assert (not _menhir_env._menhir_error);
        Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run5 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    Printf.fprintf Pervasives.stderr "State 5:\n%!";
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = _v in
    Printf.fprintf Pervasives.stderr "Reducing production expr -> STRING \n%!";
    let _v : (Ast.expr) =                     ( String(_1) ) in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_padecl : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.pat_act) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    Printf.fprintf Pervasives.stderr "State 105:\n%!";
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DIVIDE ->
        Printf.fprintf Pervasives.stderr "Shifting (DIVIDE) to state 98\n%!";
        _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | EOF ->
        Printf.fprintf Pervasives.stderr "Shifting (EOF) to state 97\n%!";
        _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | LEFTBRACE ->
        Printf.fprintf Pervasives.stderr "Shifting (LEFTBRACE) to state 1\n%!";
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | _ ->
        assert (not _menhir_env._menhir_error);
        Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState105

and _menhir_run50 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    Printf.fprintf Pervasives.stderr "State 50:\n%!";
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENTIFIER _v ->
        Printf.fprintf Pervasives.stderr "Shifting (IDENTIFIER) to state 8\n%!";
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
    | LEFTPAREN ->
        Printf.fprintf Pervasives.stderr "Shifting (LEFTPAREN) to state 7\n%!";
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | NUMBER _v ->
        Printf.fprintf Pervasives.stderr "Shifting (NUMBER) to state 6\n%!";
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
    | STRING _v ->
        Printf.fprintf Pervasives.stderr "Shifting (STRING) to state 5\n%!";
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState50

and _menhir_run6 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    Printf.fprintf Pervasives.stderr "State 6:\n%!";
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = _v in
    Printf.fprintf Pervasives.stderr "Reducing production expr -> NUMBER \n%!";
    let _v : (Ast.expr) =                     ( Number(_1) ) in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run7 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    Printf.fprintf Pervasives.stderr "State 7:\n%!";
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENTIFIER _v ->
        Printf.fprintf Pervasives.stderr "Shifting (IDENTIFIER) to state 8\n%!";
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
    | LEFTPAREN ->
        Printf.fprintf Pervasives.stderr "Shifting (LEFTPAREN) to state 7\n%!";
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | NUMBER _v ->
        Printf.fprintf Pervasives.stderr "Shifting (NUMBER) to state 6\n%!";
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
    | STRING _v ->
        Printf.fprintf Pervasives.stderr "Shifting (STRING) to state 5\n%!";
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState7

and _menhir_run52 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    Printf.fprintf Pervasives.stderr "State 52:\n%!";
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    _menhir_reduce40 _menhir_env (Obj.magic _menhir_stack) MenhirState52

and _menhir_run55 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    Printf.fprintf Pervasives.stderr "State 55:\n%!";
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LEFTPAREN ->
        Printf.fprintf Pervasives.stderr "Shifting (LEFTPAREN) to state 56\n%!";
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 56:\n%!";
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENTIFIER _v ->
            Printf.fprintf Pervasives.stderr "Shifting (IDENTIFIER) to state 8\n%!";
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
        | LEFTPAREN ->
            Printf.fprintf Pervasives.stderr "Shifting (LEFTPAREN) to state 7\n%!";
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | NUMBER _v ->
            Printf.fprintf Pervasives.stderr "Shifting (NUMBER) to state 6\n%!";
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
        | STRING _v ->
            Printf.fprintf Pervasives.stderr "Shifting (STRING) to state 5\n%!";
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState56)
    | _ ->
        assert (not _menhir_env._menhir_error);
        Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run59 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    Printf.fprintf Pervasives.stderr "State 59:\n%!";
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ASSIGN ->
        Printf.fprintf Pervasives.stderr "Shifting (ASSIGN) to state 60\n%!";
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 60:\n%!";
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENTIFIER _v ->
            Printf.fprintf Pervasives.stderr "Shifting (IDENTIFIER) to state 8\n%!";
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
        | LEFTPAREN ->
            Printf.fprintf Pervasives.stderr "Shifting (LEFTPAREN) to state 7\n%!";
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | MRBK ->
            Printf.fprintf Pervasives.stderr "Shifting (MRBK) to state 61\n%!";
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState60 in
            Printf.fprintf Pervasives.stderr "State 61:\n%!";
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LEFTPAREN ->
                Printf.fprintf Pervasives.stderr "Shifting (LEFTPAREN) to state 62\n%!";
                let _menhir_stack = Obj.magic _menhir_stack in
                Printf.fprintf Pervasives.stderr "State 62:\n%!";
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | IDENTIFIER _v ->
                    Printf.fprintf Pervasives.stderr "Shifting (IDENTIFIER) to state 8\n%!";
                    _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
                | LEFTPAREN ->
                    Printf.fprintf Pervasives.stderr "Shifting (LEFTPAREN) to state 7\n%!";
                    _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState62
                | NUMBER _v ->
                    Printf.fprintf Pervasives.stderr "Shifting (NUMBER) to state 6\n%!";
                    _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
                | STRING _v ->
                    Printf.fprintf Pervasives.stderr "Shifting (STRING) to state 5\n%!";
                    _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
                | RIGHTPAREN ->
                    _menhir_reduce3 _menhir_env (Obj.magic _menhir_stack) MenhirState62
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState62)
            | _ ->
                assert (not _menhir_env._menhir_error);
                Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | NUMBER _v ->
            Printf.fprintf Pervasives.stderr "Shifting (NUMBER) to state 6\n%!";
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
        | STRING _v ->
            Printf.fprintf Pervasives.stderr "Shifting (STRING) to state 5\n%!";
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState60)
    | LEFTBRACK ->
        Printf.fprintf Pervasives.stderr "Shifting (LEFTBRACK) to state 38\n%!";
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
    | LEFTPAREN ->
        Printf.fprintf Pervasives.stderr "Shifting (LEFTPAREN) to state 9\n%!";
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack)
    | DIVIDE | ELSE | EQ | FOR | FUNCTION | GEQ | GT | IDENTIFIER _ | IF | LEFTBRACE | LEQ | LT | MINUS | MOD | NEQ | NUMBER _ | PLUS | RETURN | RIGHTBRACE | STRING _ | TIMES | WHILE ->
        _menhir_reduce7 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run65 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    Printf.fprintf Pervasives.stderr "State 65:\n%!";
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENTIFIER _v ->
        Printf.fprintf Pervasives.stderr "Shifting (IDENTIFIER) to state 66\n%!";
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 66:\n%!";
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LEFTPAREN ->
            Printf.fprintf Pervasives.stderr "Shifting (LEFTPAREN) to state 67\n%!";
            let _menhir_stack = Obj.magic _menhir_stack in
            Printf.fprintf Pervasives.stderr "State 67:\n%!";
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | IDENTIFIER _v ->
                Printf.fprintf Pervasives.stderr "Shifting (IDENTIFIER) to state 8\n%!";
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
            | LEFTPAREN ->
                Printf.fprintf Pervasives.stderr "Shifting (LEFTPAREN) to state 7\n%!";
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | NUMBER _v ->
                Printf.fprintf Pervasives.stderr "Shifting (NUMBER) to state 6\n%!";
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
            | STRING _v ->
                Printf.fprintf Pervasives.stderr "Shifting (STRING) to state 5\n%!";
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
            | RIGHTPAREN ->
                _menhir_reduce3 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | _ ->
                assert (not _menhir_env._menhir_error);
                Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState67)
        | _ ->
            assert (not _menhir_env._menhir_error);
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run73 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    Printf.fprintf Pervasives.stderr "State 73:\n%!";
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LEFTPAREN ->
        Printf.fprintf Pervasives.stderr "Shifting (LEFTPAREN) to state 74\n%!";
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 74:\n%!";
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENTIFIER _v ->
            Printf.fprintf Pervasives.stderr "Shifting (IDENTIFIER) to state 75\n%!";
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState74 in
            Printf.fprintf Pervasives.stderr "State 75:\n%!";
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ASSIGN ->
                Printf.fprintf Pervasives.stderr "Shifting (ASSIGN) to state 41\n%!";
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
            | COMMA ->
                Printf.fprintf Pervasives.stderr "Shifting (COMMA) to state 76\n%!";
                let _menhir_stack = Obj.magic _menhir_stack in
                Printf.fprintf Pervasives.stderr "State 76:\n%!";
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | IDENTIFIER _v ->
                    Printf.fprintf Pervasives.stderr "Shifting (IDENTIFIER) to state 77\n%!";
                    let _menhir_stack = Obj.magic _menhir_stack in
                    Printf.fprintf Pervasives.stderr "State 77:\n%!";
                    let _menhir_stack = (_menhir_stack, _v) in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | RIGHTPAREN ->
                        Printf.fprintf Pervasives.stderr "Shifting (RIGHTPAREN) to state 78\n%!";
                        let _menhir_stack = Obj.magic _menhir_stack in
                        Printf.fprintf Pervasives.stderr "State 78:\n%!";
                        let _menhir_env = _menhir_discard _menhir_env in
                        let _tok = _menhir_env._menhir_token in
                        (match _tok with
                        | IN ->
                            Printf.fprintf Pervasives.stderr "Shifting (IN) to state 79\n%!";
                            let _menhir_stack = Obj.magic _menhir_stack in
                            Printf.fprintf Pervasives.stderr "State 79:\n%!";
                            let _menhir_env = _menhir_discard _menhir_env in
                            let _tok = _menhir_env._menhir_token in
                            (match _tok with
                            | IDENTIFIER _v ->
                                Printf.fprintf Pervasives.stderr "Shifting (IDENTIFIER) to state 80\n%!";
                                let _menhir_stack = Obj.magic _menhir_stack in
                                Printf.fprintf Pervasives.stderr "State 80:\n%!";
                                let _menhir_stack = (_menhir_stack, _v) in
                                let _menhir_env = _menhir_discard _menhir_env in
                                let _tok = _menhir_env._menhir_token in
                                (match _tok with
                                | FOR ->
                                    Printf.fprintf Pervasives.stderr "Shifting (FOR) to state 73\n%!";
                                    _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState80
                                | FUNCTION ->
                                    Printf.fprintf Pervasives.stderr "Shifting (FUNCTION) to state 65\n%!";
                                    _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState80
                                | IDENTIFIER _v ->
                                    Printf.fprintf Pervasives.stderr "Shifting (IDENTIFIER) to state 59\n%!";
                                    _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
                                | IF ->
                                    Printf.fprintf Pervasives.stderr "Shifting (IF) to state 55\n%!";
                                    _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState80
                                | LEFTBRACE ->
                                    Printf.fprintf Pervasives.stderr "Shifting (LEFTBRACE) to state 52\n%!";
                                    _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState80
                                | LEFTPAREN ->
                                    Printf.fprintf Pervasives.stderr "Shifting (LEFTPAREN) to state 7\n%!";
                                    _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState80
                                | NUMBER _v ->
                                    Printf.fprintf Pervasives.stderr "Shifting (NUMBER) to state 6\n%!";
                                    _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
                                | RETURN ->
                                    Printf.fprintf Pervasives.stderr "Shifting (RETURN) to state 50\n%!";
                                    _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState80
                                | STRING _v ->
                                    Printf.fprintf Pervasives.stderr "Shifting (STRING) to state 5\n%!";
                                    _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
                                | WHILE ->
                                    Printf.fprintf Pervasives.stderr "Shifting (WHILE) to state 3\n%!";
                                    _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState80
                                | _ ->
                                    assert (not _menhir_env._menhir_error);
                                    Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
                                    _menhir_env._menhir_error <- true;
                                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState80)
                            | _ ->
                                assert (not _menhir_env._menhir_error);
                                Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
                                _menhir_env._menhir_error <- true;
                                let _menhir_stack = Obj.magic _menhir_stack in
                                let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                        | _ ->
                            assert (not _menhir_env._menhir_error);
                            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
                            _menhir_env._menhir_error <- true;
                            let _menhir_stack = Obj.magic _menhir_stack in
                            let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
                        _menhir_env._menhir_error <- true;
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | LEFTBRACK ->
                Printf.fprintf Pervasives.stderr "Shifting (LEFTBRACK) to state 38\n%!";
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
            | LEFTPAREN ->
                Printf.fprintf Pervasives.stderr "Shifting (LEFTPAREN) to state 9\n%!";
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack)
            | DIVIDE | EQ | GEQ | GT | LEQ | LT | MINUS | MOD | NEQ | PLUS | SEMICOLON | TIMES ->
                _menhir_reduce7 _menhir_env (Obj.magic _menhir_stack)
            | _ ->
                assert (not _menhir_env._menhir_error);
                Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | LEFTPAREN ->
            Printf.fprintf Pervasives.stderr "Shifting (LEFTPAREN) to state 7\n%!";
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | NUMBER _v ->
            Printf.fprintf Pervasives.stderr "Shifting (NUMBER) to state 6\n%!";
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
        | STRING _v ->
            Printf.fprintf Pervasives.stderr "Shifting (STRING) to state 5\n%!";
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
        | SEMICOLON ->
            _menhir_reduce24 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | _ ->
            assert (not _menhir_env._menhir_error);
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState74)
    | _ ->
        assert (not _menhir_env._menhir_error);
        Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_goto_stmt_list : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.stmt list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState1 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 2:\n%!";
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | FOR ->
            Printf.fprintf Pervasives.stderr "Shifting (FOR) to state 73\n%!";
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | FUNCTION ->
            Printf.fprintf Pervasives.stderr "Shifting (FUNCTION) to state 65\n%!";
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | IDENTIFIER _v ->
            Printf.fprintf Pervasives.stderr "Shifting (IDENTIFIER) to state 59\n%!";
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
        | IF ->
            Printf.fprintf Pervasives.stderr "Shifting (IF) to state 55\n%!";
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | LEFTBRACE ->
            Printf.fprintf Pervasives.stderr "Shifting (LEFTBRACE) to state 52\n%!";
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | LEFTPAREN ->
            Printf.fprintf Pervasives.stderr "Shifting (LEFTPAREN) to state 7\n%!";
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | NUMBER _v ->
            Printf.fprintf Pervasives.stderr "Shifting (NUMBER) to state 6\n%!";
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
        | RETURN ->
            Printf.fprintf Pervasives.stderr "Shifting (RETURN) to state 50\n%!";
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | RIGHTBRACE ->
            Printf.fprintf Pervasives.stderr "Shifting (RIGHTBRACE) to state 96\n%!";
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState2 in
            Printf.fprintf Pervasives.stderr "State 96:\n%!";
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            Printf.fprintf Pervasives.stderr "Reducing production padecl -> LEFTBRACE stmt_list RIGHTBRACE \n%!";
            let ((_menhir_stack, _menhir_s), _, _2) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.pat_act) =                                  ( { pattern = "emptyPattern" ; actions = List.rev _2 } ) in
            _menhir_goto_padecl _menhir_env _menhir_stack _menhir_s _v
        | STRING _v ->
            Printf.fprintf Pervasives.stderr "Shifting (STRING) to state 5\n%!";
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
        | WHILE ->
            Printf.fprintf Pervasives.stderr "Shifting (WHILE) to state 3\n%!";
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | _ ->
            assert (not _menhir_env._menhir_error);
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState2)
    | MenhirState52 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 53:\n%!";
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | FOR ->
            Printf.fprintf Pervasives.stderr "Shifting (FOR) to state 73\n%!";
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | FUNCTION ->
            Printf.fprintf Pervasives.stderr "Shifting (FUNCTION) to state 65\n%!";
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | IDENTIFIER _v ->
            Printf.fprintf Pervasives.stderr "Shifting (IDENTIFIER) to state 59\n%!";
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
        | IF ->
            Printf.fprintf Pervasives.stderr "Shifting (IF) to state 55\n%!";
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | LEFTBRACE ->
            Printf.fprintf Pervasives.stderr "Shifting (LEFTBRACE) to state 52\n%!";
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | LEFTPAREN ->
            Printf.fprintf Pervasives.stderr "Shifting (LEFTPAREN) to state 7\n%!";
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | NUMBER _v ->
            Printf.fprintf Pervasives.stderr "Shifting (NUMBER) to state 6\n%!";
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
        | RETURN ->
            Printf.fprintf Pervasives.stderr "Shifting (RETURN) to state 50\n%!";
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | RIGHTBRACE ->
            Printf.fprintf Pervasives.stderr "Shifting (RIGHTBRACE) to state 54\n%!";
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState53 in
            Printf.fprintf Pervasives.stderr "State 54:\n%!";
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            Printf.fprintf Pervasives.stderr "Reducing production stmt -> LEFTBRACE stmt_list RIGHTBRACE \n%!";
            let ((_menhir_stack, _menhir_s), _, _2) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.stmt) =                                    ( Block(List.rev _2) ) in
            _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v
        | STRING _v ->
            Printf.fprintf Pervasives.stderr "Shifting (STRING) to state 5\n%!";
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
        | WHILE ->
            Printf.fprintf Pervasives.stderr "Shifting (WHILE) to state 3\n%!";
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | _ ->
            assert (not _menhir_env._menhir_error);
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState53)
    | MenhirState70 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 71:\n%!";
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | FOR ->
            Printf.fprintf Pervasives.stderr "Shifting (FOR) to state 73\n%!";
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | FUNCTION ->
            Printf.fprintf Pervasives.stderr "Shifting (FUNCTION) to state 65\n%!";
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | IDENTIFIER _v ->
            Printf.fprintf Pervasives.stderr "Shifting (IDENTIFIER) to state 59\n%!";
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
        | IF ->
            Printf.fprintf Pervasives.stderr "Shifting (IF) to state 55\n%!";
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | LEFTBRACE ->
            Printf.fprintf Pervasives.stderr "Shifting (LEFTBRACE) to state 52\n%!";
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | LEFTPAREN ->
            Printf.fprintf Pervasives.stderr "Shifting (LEFTPAREN) to state 7\n%!";
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | NUMBER _v ->
            Printf.fprintf Pervasives.stderr "Shifting (NUMBER) to state 6\n%!";
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
        | RETURN ->
            Printf.fprintf Pervasives.stderr "Shifting (RETURN) to state 50\n%!";
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | RIGHTBRACE ->
            Printf.fprintf Pervasives.stderr "Shifting (RIGHTBRACE) to state 72\n%!";
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState71 in
            Printf.fprintf Pervasives.stderr "State 72:\n%!";
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            Printf.fprintf Pervasives.stderr "Reducing production stmt -> FUNCTION IDENTIFIER LEFTPAREN actuals_opt RIGHTPAREN LEFTBRACE stmt_list RIGHTBRACE \n%!";
            let ((((_menhir_stack, _menhir_s), _2), _, _4), _, _7) = _menhir_stack in
            let _8 = () in
            let _6 = () in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.stmt) =                                                                                          ( Def(_2,_4,List.rev _7)) in
            _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v
        | STRING _v ->
            Printf.fprintf Pervasives.stderr "Shifting (STRING) to state 5\n%!";
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
        | WHILE ->
            Printf.fprintf Pervasives.stderr "Shifting (WHILE) to state 3\n%!";
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | _ ->
            assert (not _menhir_env._menhir_error);
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState71)
    | MenhirState101 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 102:\n%!";
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | FOR ->
            Printf.fprintf Pervasives.stderr "Shifting (FOR) to state 73\n%!";
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | FUNCTION ->
            Printf.fprintf Pervasives.stderr "Shifting (FUNCTION) to state 65\n%!";
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | IDENTIFIER _v ->
            Printf.fprintf Pervasives.stderr "Shifting (IDENTIFIER) to state 59\n%!";
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v
        | IF ->
            Printf.fprintf Pervasives.stderr "Shifting (IF) to state 55\n%!";
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | LEFTBRACE ->
            Printf.fprintf Pervasives.stderr "Shifting (LEFTBRACE) to state 52\n%!";
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | LEFTPAREN ->
            Printf.fprintf Pervasives.stderr "Shifting (LEFTPAREN) to state 7\n%!";
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | NUMBER _v ->
            Printf.fprintf Pervasives.stderr "Shifting (NUMBER) to state 6\n%!";
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v
        | RETURN ->
            Printf.fprintf Pervasives.stderr "Shifting (RETURN) to state 50\n%!";
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | RIGHTBRACE ->
            Printf.fprintf Pervasives.stderr "Shifting (RIGHTBRACE) to state 103\n%!";
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState102 in
            Printf.fprintf Pervasives.stderr "State 103:\n%!";
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            Printf.fprintf Pervasives.stderr "Reducing production padecl -> DIVIDE IDENTIFIER DIVIDE LEFTBRACE stmt_list RIGHTBRACE \n%!";
            let (((_menhir_stack, _menhir_s), _2), _, _5) = _menhir_stack in
            let _6 = () in
            let _4 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.pat_act) =                                                            ( { pattern = _2 ; actions = List.rev _5 } ) in
            _menhir_goto_padecl _menhir_env _menhir_stack _menhir_s _v
        | STRING _v ->
            Printf.fprintf Pervasives.stderr "Shifting (STRING) to state 5\n%!";
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v
        | WHILE ->
            Printf.fprintf Pervasives.stderr "Shifting (WHILE) to state 3\n%!";
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | _ ->
            assert (not _menhir_env._menhir_error);
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState102)
    | _ ->
        _menhir_fail ()

and _menhir_goto_program : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.program) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 104:\n%!";
        let _menhir_stack = Obj.magic _menhir_stack in
        let _1 = _v in
        Printf.fprintf Pervasives.stderr "Accepting\n%!";
        Obj.magic _1
    | MenhirState105 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 106:\n%!";
        let _menhir_stack = Obj.magic _menhir_stack in
        let _2 = _v in
        Printf.fprintf Pervasives.stderr "Reducing production program -> padecl program \n%!";
        let (_menhir_stack, _menhir_s, _1) = _menhir_stack in
        let _v : (Ast.program) =                   (  _1 :: _2 ) in
        _menhir_goto_program _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_reduce40 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    Printf.fprintf Pervasives.stderr "Reducing production stmt_list -> \n%!";
    let _v : (Ast.stmt list) =                    ( [] ) in
    _menhir_goto_stmt_list _menhir_env _menhir_stack _menhir_s _v

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState105 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState102 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState101 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState93 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState88 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState86 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState84 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState80 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, _), _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState74 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState71 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState70 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState67 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState62 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState60 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState58 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState56 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState53 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState52 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState50 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState49 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState45 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState41 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState38 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState36 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState29 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState27 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState25 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState23 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState21 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState19 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState17 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState15 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState13 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState11 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState9 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState7 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState4 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState2 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState1 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    Printf.fprintf Pervasives.stderr "State 1:\n%!";
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    _menhir_reduce40 _menhir_env (Obj.magic _menhir_stack) MenhirState1

and _menhir_run97 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    Printf.fprintf Pervasives.stderr "State 97:\n%!";
    let _menhir_stack = Obj.magic _menhir_stack in
    Printf.fprintf Pervasives.stderr "Reducing production program -> EOF \n%!";
    let _1 = () in
    let _v : (Ast.program) =        ( [] ) in
    _menhir_goto_program _menhir_env _menhir_stack _menhir_s _v

and _menhir_run98 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    Printf.fprintf Pervasives.stderr "State 98:\n%!";
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENTIFIER _v ->
        Printf.fprintf Pervasives.stderr "Shifting (IDENTIFIER) to state 99\n%!";
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 99:\n%!";
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            Printf.fprintf Pervasives.stderr "Shifting (DIVIDE) to state 100\n%!";
            let _menhir_stack = Obj.magic _menhir_stack in
            Printf.fprintf Pervasives.stderr "State 100:\n%!";
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LEFTBRACE ->
                Printf.fprintf Pervasives.stderr "Shifting (LEFTBRACE) to state 101\n%!";
                let _menhir_stack = Obj.magic _menhir_stack in
                Printf.fprintf Pervasives.stderr "State 101:\n%!";
                let _menhir_env = _menhir_discard _menhir_env in
                _menhir_reduce40 _menhir_env (Obj.magic _menhir_stack) MenhirState101
            | _ ->
                assert (not _menhir_env._menhir_error);
                Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_discard : _menhir_env -> _menhir_env =
  fun _menhir_env ->
    let lexer = _menhir_env._menhir_lexer in
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = lexer lexbuf in
    Printf.fprintf Pervasives.stderr "Lookahead token is now %s (%d-%d)\n%!" (match _tok with
    | ASSIGN ->
        "ASSIGN"
    | BEGIN ->
        "BEGIN"
    | COLON ->
        "COLON"
    | COMMA ->
        "COMMA"
    | DIVIDE ->
        "DIVIDE"
    | DOLLAR ->
        "DOLLAR"
    | ELSE ->
        "ELSE"
    | END ->
        "END"
    | EOF ->
        "EOF"
    | EP ->
        "EP"
    | EQ ->
        "EQ"
    | FOR ->
        "FOR"
    | FUNCTION ->
        "FUNCTION"
    | GEQ ->
        "GEQ"
    | GT ->
        "GT"
    | IDENTIFIER _ ->
        "IDENTIFIER"
    | IF ->
        "IF"
    | IN ->
        "IN"
    | LEFTBRACE ->
        "LEFTBRACE"
    | LEFTBRACK ->
        "LEFTBRACK"
    | LEFTPAREN ->
        "LEFTPAREN"
    | LEQ ->
        "LEQ"
    | LT ->
        "LT"
    | MINUS ->
        "MINUS"
    | MOD ->
        "MOD"
    | MRBK ->
        "MRBK"
    | NEQ ->
        "NEQ"
    | NEWLINE ->
        "NEWLINE"
    | NUMBER _ ->
        "NUMBER"
    | PLUS ->
        "PLUS"
    | READ ->
        "READ"
    | RETURN ->
        "RETURN"
    | RIGHTBRACE ->
        "RIGHTBRACE"
    | RIGHTBRACK ->
        "RIGHTBRACK"
    | RIGHTPAREN ->
        "RIGHTPAREN"
    | SEMICOLON ->
        "SEMICOLON"
    | STRING _ ->
        "STRING"
    | TIMES ->
        "TIMES"
    | VAR ->
        "VAR"
    | WHILE ->
        "WHILE"
    | WRITE ->
        "WRITE") lexbuf.Lexing.lex_start_p.Lexing.pos_cnum lexbuf.Lexing.lex_curr_p.Lexing.pos_cnum;
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    }

and program : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.program) =
  fun lexer lexbuf ->
    let _menhir_env = let _tok = Obj.magic () in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    } in
    Obj.magic (let _menhir_stack = () in
    Printf.fprintf Pervasives.stderr "State 0:\n%!";
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DIVIDE ->
        Printf.fprintf Pervasives.stderr "Shifting (DIVIDE) to state 98\n%!";
        _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | EOF ->
        Printf.fprintf Pervasives.stderr "Shifting (EOF) to state 97\n%!";
        _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | LEFTBRACE ->
        Printf.fprintf Pervasives.stderr "Shifting (LEFTBRACE) to state 1\n%!";
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)
    

