let ( let* ) = Result.bind

let rec expect (lexer : Lexer.t) (kind : Token.kind) :
    (Token.t * Lexer.t, Report.t) result =
  let* tt, lexer = Lexer.next lexer in
  if Token.equal_kind kind tt.kind then Ok (tt, lexer)
  else
    let msg = Printf.sprintf "expected '%s'" (Token.show_kind kind) in
    let report = Report.make tt.loc msg in
    Error report

and parse_ident (lexer : Lexer.t) :
    (Token.t * string * Lexer.t, Report.t) result =
  let* tt, lexer = Lexer.next lexer in
  match tt.kind with
  | Token.Ident i -> Ok (tt, i, lexer)
  | _ ->
      let msg = "expected identifier" in
      let report = Report.make tt.loc msg in
      Error report

and parse_call_params (lexer : Lexer.t) :
    (Ast.expr list * Lexer.t, Report.t) result =
  let* exp, lexer = parse_expr lexer in
  let rec aux lexer acc =
    let* tt, lexer = Lexer.next lexer in
    match tt.kind with
    | Token.Comma ->
        let* exp, lexer = parse_expr lexer in
        aux lexer (exp :: acc)
    | _ -> Ok (List.rev acc, lexer)
  in
  aux lexer [ exp ]

and parse_prim_expr (lexer : Lexer.t) : (Ast.expr * Lexer.t, Report.t) result =
  let* tt, lexer = Lexer.next lexer in
  let* kind, lexer =
    match tt.kind with
    | Token.Integer n -> Ok (Ast.Integer (Int64.of_string n), lexer)
    | Token.Ident i -> (
        let* tt, next_lexer = Lexer.next lexer in
        match tt.kind with
        | OParen ->
            let* params, lexer = parse_call_params next_lexer in
            Ok (Ast.Call (i, params), lexer)
        | _ -> Ok (Ast.Var i, lexer))
    | Token.OParen -> (
        let* exp, lexer = parse_expr lexer in
        let* tt, lexer = Lexer.next lexer in
        match tt.kind with
        | Token.CParen -> Ok (exp.kind, lexer)
        | _ ->
            let msg =
              Token.show_kind Token.CParen |> Printf.sprintf "expected '%s'"
            in
            let report = Report.make tt.loc msg in
            Error report)
    | _ ->
        let msg = "expected integer or identifier" in
        let report = Report.make tt.loc msg in
        Error report
  in
  let expr : Ast.expr = { kind; loc = tt.loc } in
  Ok (expr, lexer)

and parse_mul_expr (lexer : Lexer.t) : (Ast.expr * Lexer.t, Report.t) result =
  let* prim, lexer = parse_prim_expr lexer in
  let rec aux lexer acc =
    let* tt, next_lexer = Lexer.next lexer in
    match tt.kind with
    | Token.Mul ->
        let* prim, lexer = parse_prim_expr next_lexer in
        let expr : Ast.expr =
          { kind = Ast.BinExpr (Ast.Mul, acc, prim); loc = tt.loc }
        in
        aux lexer expr
    | Token.Div ->
        let* prim, lexer = parse_prim_expr next_lexer in
        let expr : Ast.expr =
          { kind = Ast.BinExpr (Ast.Div, acc, prim); loc = tt.loc }
        in
        aux lexer expr
    | Token.Mod ->
        let* prim, lexer = parse_prim_expr next_lexer in
        let expr : Ast.expr =
          { kind = Ast.BinExpr (Ast.Mod, acc, prim); loc = tt.loc }
        in
        aux lexer expr
    | _ -> Ok (acc, lexer)
  in
  aux lexer prim

and parse_add_expr (lexer : Lexer.t) : (Ast.expr * Lexer.t, Report.t) result =
  let* mul, lexer = parse_mul_expr lexer in
  let rec aux lexer acc =
    let* tt, next_lexer = Lexer.next lexer in
    match tt.kind with
    | Token.Plus ->
        let* mul, lexer = parse_mul_expr next_lexer in
        let expr : Ast.expr =
          { kind = Ast.BinExpr (Ast.Plus, acc, mul); loc = tt.loc }
        in
        aux lexer expr
    | Token.Minus ->
        let* mul, lexer = parse_mul_expr next_lexer in
        let expr : Ast.expr =
          { kind = Ast.BinExpr (Ast.Minus, acc, mul); loc = tt.loc }
        in
        aux lexer expr
    | _ -> Ok (acc, lexer)
  in
  aux lexer mul

and parse_rela_expr (lexer : Lexer.t) : (Ast.expr * Lexer.t, Report.t) result =
  let* add, lexer = parse_add_expr lexer in
  let rec aux lexer acc =
    let* tt, next_lexer = Lexer.next lexer in
    match tt.kind with
    | Token.Gt ->
        let* add, lexer = parse_add_expr next_lexer in
        let expr : Ast.expr =
          { kind = Ast.BinExpr (Ast.Gt, acc, add); loc = tt.loc }
        in
        aux lexer expr
    | Token.Lt ->
        let* add, lexer = parse_add_expr next_lexer in
        let expr : Ast.expr =
          { kind = Ast.BinExpr (Ast.Lt, acc, add); loc = tt.loc }
        in
        aux lexer expr
    | Token.Ge ->
        let* add, lexer = parse_add_expr next_lexer in
        let expr : Ast.expr =
          { kind = Ast.BinExpr (Ast.Ge, acc, add); loc = tt.loc }
        in
        aux lexer expr
    | Token.Le ->
        let* add, lexer = parse_add_expr next_lexer in
        let expr : Ast.expr =
          { kind = Ast.BinExpr (Ast.Le, acc, add); loc = tt.loc }
        in
        aux lexer expr
    | _ -> Ok (acc, lexer)
  in
  aux lexer add

and parse_equal_expr (lexer : Lexer.t) : (Ast.expr * Lexer.t, Report.t) result =
  let* rela, lexer = parse_rela_expr lexer in
  let rec aux lexer acc =
    let* tt, next_lexer = Lexer.next lexer in
    match tt.kind with
    | Token.Deq ->
        let* rela, lexer = parse_rela_expr next_lexer in
        let expr : Ast.expr =
          { kind = Ast.BinExpr (Ast.Eq, acc, rela); loc = tt.loc }
        in
        aux lexer expr
    | Token.Neq ->
        let* rela, lexer = parse_rela_expr next_lexer in
        let expr : Ast.expr =
          { kind = Ast.BinExpr (Ast.Neq, acc, rela); loc = tt.loc }
        in
        aux lexer expr
    | _ -> Ok (acc, lexer)
  in
  aux lexer rela

and parse_expr (lexer : Lexer.t) : (Ast.expr * Lexer.t, Report.t) result =
  parse_equal_expr lexer

and parse_stmt_block (lexer : Lexer.t) :
    (Ast.stmt list * Lexer.t, Report.t) result =
  let rec aux lexer acc =
    let* stmt, lexer = parse_stmt lexer in
    let* tt, next_lexer = Lexer.next lexer in
    match tt.kind with
    | Token.CBrack -> Ok (stmt :: acc |> List.rev, next_lexer)
    | _ -> aux lexer (stmt :: acc)
  in
  let* tt, lexer = Lexer.next lexer in
  match tt.kind with
  | Token.OBrack -> aux lexer []
  | _ ->
      let msg = Token.show_kind Token.OBrack |> Printf.sprintf "expected %s" in
      let report = Report.make tt.loc msg in
      Error report

and parse_stmt (lexer : Lexer.t) : (Ast.stmt * Lexer.t, Report.t) result =
  let* tt, next_lexer = Lexer.next lexer in
  match tt.kind with
  | Token.Print ->
      let* expr, lexer = parse_expr next_lexer in
      let* _, lexer = expect lexer Token.SemiColon in
      let stmt : Ast.stmt = { kind = Ast.Print expr; loc = tt.loc } in
      Ok (stmt, lexer)
  | Token.Var ->
      let* _, ident, lexer = parse_ident next_lexer in
      let* _, lexer = expect lexer Token.Eq in
      let* expr, lexer = parse_expr lexer in
      let* _, lexer = expect lexer SemiColon in
      let stmt : Ast.stmt =
        { kind = Ast.VarDecl (ident, expr); loc = tt.loc }
      in
      Ok (stmt, lexer)
  | Token.If ->
      let* cond, lexer = parse_expr next_lexer in
      let* then_block, lexer = parse_stmt_block lexer in
      let* tt, next_lexer = Lexer.next lexer in
      let* else_block, lexer =
        match tt.kind with
        | Token.Else ->
            let* block, lexer = parse_stmt_block next_lexer in
            Ok (Some block, lexer)
        | _ -> Ok (None, lexer)
      in
      let stmt : Ast.stmt =
        { kind = Ast.If (cond, then_block, else_block); loc = tt.loc }
      in
      Ok (stmt, lexer)
  | Token.For ->
      let* cond, lexer = parse_expr next_lexer in
      let* body, lexer = parse_stmt_block lexer in
      let stmt : Ast.stmt = { kind = Ast.For (cond, body); loc = tt.loc } in
      Ok (stmt, lexer)
  | Token.Ident i -> (
      let* tt, next_lexer = Lexer.next next_lexer in
      match tt.kind with
      | Token.Eq ->
          let* expr, lexer = parse_expr next_lexer in
          let* _, lexer = expect lexer Token.SemiColon in
          let stmt : Ast.stmt = { kind = Ast.Assign (i, expr); loc = tt.loc } in
          Ok (stmt, lexer)
      | _ ->
          let* expr, lexer = parse_expr lexer in
          let* _, lexer = expect lexer Token.SemiColon in
          let stmt : Ast.stmt = { kind = Ast.Expr expr; loc = tt.loc } in
          Ok (stmt, lexer))
  | Token.Break ->
      let* _, lexer = expect next_lexer Token.SemiColon in
      let stmt : Ast.stmt = { kind = Ast.Break; loc = tt.loc } in
      Ok (stmt, lexer)
  | Token.Continue ->
      let* _, lexer = expect next_lexer Token.SemiColon in
      let stmt : Ast.stmt = { kind = Ast.Continue; loc = tt.loc } in
      Ok (stmt, lexer)
  | _ ->
      let msg = "expected statement" in
      let report = Report.make tt.loc msg in
      Error report

and parse_fun_def_params (lexer : Lexer.t) :
    (Token.t list * Lexer.t, Report.t) result =
  let rec aux lexer acc =
    let* tt, next_lexer = Lexer.next lexer in
    match tt.kind with
    | Token.Comma ->
        let* ident, lexer = expect next_lexer (Token.Ident "") in
        aux lexer (ident :: acc)
    | _ -> Ok (List.rev acc, lexer)
  in
  let* tt, next_lexer = Lexer.next lexer in
  match tt.kind with
  | Token.Ident _ -> aux next_lexer [ tt ]
  | _ -> Ok ([], lexer)

and parse_fun_def (lexer : Lexer.t) : (Ast.fn_def * Lexer.t, Report.t) result =
  let* _, lexer = expect lexer Token.Fn in
  let* tti, lexer = expect lexer (Token.Ident "") in
  let* _, lexer = expect lexer Token.OParen in
  let* params, lexer = parse_fun_def_params lexer in
  let* _, lexer = expect lexer Token.CParen in
  let* _, lexer = expect lexer Token.Colon in
  let* tt_type, lexer = expect lexer (Token.Ident "") in
  let* body, lexer = parse_stmt_block lexer in
  let decl : Ast.fn_def =
    {
      name = Token.get_ident tti.kind;
      params = List.map (fun (t : Token.t) -> Token.get_ident t.kind) params;
      body;
      ret_type = Token.get_ident tt_type.kind;
      loc = tti.loc;
    }
  in
  Ok (decl, lexer)

and parse_program (lexer : Lexer.t) : (Ast.t, Report.t) result =
  let rec aux lexer tree =
    let* tt, next_lexer = Lexer.next lexer in
    match tt.kind with
    | Token.EOF -> Ok (tree, lexer)
    | _ ->
        let* t, lexer = parse_fun_def lexer in
        aux lexer (t :: tree)
  in
  let* tree, _ = aux lexer [] in
  Ok (List.rev tree)
