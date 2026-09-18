let ( let* ) = Result.bind

let rec expect (lexer : Lexer.t) (kind : Token.kind) :
    (Token.t * Lexer.t, string) result =
  let* tt, lexer = Lexer.next lexer in
  if Token.equal_kind kind tt.kind then Ok (tt, lexer)
  else Error (Printf.sprintf "expected '%s'" (Token.string_of_kind kind))

and parse_ident (lexer : Lexer.t) : (Token.t * string * Lexer.t, string) result
    =
  let* tt, lexer = Lexer.next lexer in
  match tt.kind with
  | Token.Ident i -> Ok (tt, i, lexer)
  | _ -> Error "expected identifier"

and parse_prim_expr (lexer : Lexer.t) : (Ast.expr * Lexer.t, string) result =
  let* tt, lexer = Lexer.next lexer in
  let* kind, lexer =
    match tt.kind with
    | Token.Integer n -> Ok (Ast.Integer (Int64.of_string n), lexer)
    | Token.Ident i -> Ok (Ast.Var i, lexer)
    | _ -> Error "expected integer or identifier"
  in
  let expr : Ast.expr = { kind; loc = tt.loc } in
  Ok (expr, lexer)

and parse_mul_expr (lexer : Lexer.t) : (Ast.expr * Lexer.t, string) result =
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

and parse_add_expr (lexer : Lexer.t) : (Ast.expr * Lexer.t, string) result =
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

and parse_expr (lexer : Lexer.t) : (Ast.expr * Lexer.t, string) result =
  parse_add_expr lexer

and parse_stmt_block (lexer : Lexer.t) :
    (Ast.stmt list * Lexer.t, string) result =
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
      Error (Token.string_of_kind Token.OBrack |> Printf.sprintf "expected %s")

and parse_stmt (lexer : Lexer.t) : (Ast.stmt * Lexer.t, string) result =
  let* tt, lexer = Lexer.next lexer in
  match tt.kind with
  | Token.Print ->
      let* expr, lexer = parse_expr lexer in
      let* _, lexer = expect lexer Token.SemiColon in
      let stmt : Ast.stmt = { kind = Ast.Print expr; loc = tt.loc } in
      Ok (stmt, lexer)
  | Token.Var ->
      let* _, ident, lexer = parse_ident lexer in
      let* _, lexer = expect lexer Token.Eq in
      let* expr, lexer = parse_expr lexer in
      let* _, lexer = expect lexer SemiColon in
      let stmt : Ast.stmt =
        { kind = Ast.VarDecl (ident, expr); loc = tt.loc }
      in
      Ok (stmt, lexer)
  | Token.If ->
      let* cond, lexer = parse_expr lexer in
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
      let* body, lexer = parse_stmt_block lexer in
      let stmt : Ast.stmt = { kind = Ast.For body; loc = tt.loc } in
      Ok (stmt, lexer)
  | _ -> Error "expected statement"

and parse_program (lexer : Lexer.t) : (Ast.t, string) result =
  let rec aux lexer tree =
    let* tt, next_lexer = Lexer.next lexer in
    match tt.kind with
    | Token.EOF -> Ok (tree, lexer)
    | _ ->
        let* t, lexer = parse_stmt lexer in
        aux lexer (t :: tree)
  in
  let* tree, _ = aux lexer [] in
  Ok (List.rev tree)
