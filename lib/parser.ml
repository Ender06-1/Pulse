let ( let* ) = Result.bind

let rec expect (lexer : Lexer.t) (token : Token.typ) :
    (Token.typ * Lexer.t, string) result =
  let* tt, lexer = Lexer.next lexer in
  if Token.equal token tt then Ok (tt, lexer)
  else Error (Printf.sprintf "expected '%s'" (Token.string_of_typ token))

and parse_ident (lexer : Lexer.t) : (string * Lexer.t, string) result =
  let* tt, lexer = Lexer.next lexer in
  match tt with
  | Token.Ident i -> Ok (i, lexer)
  | _ -> Error "expected identifier"

and parse_prim_expr (lexer : Lexer.t) : (Ast.expr * Lexer.t, string) result =
  let* tt, lexer = Lexer.next lexer in
  match tt with
  | Integer n -> Ok (Ast.Integer (Int64.of_string n), lexer)
  | Token.Ident i -> Ok (Ast.Var i, lexer)
  | _ -> Error "expected num"

and parse_mul_expr (lexer : Lexer.t) : (Ast.expr * Lexer.t, string) result =
  let* prim, lexer = parse_prim_expr lexer in
  let rec aux lexer acc =
    let* tt, next_lexer = Lexer.next lexer in
    match tt with
    | Mul ->
        let* exp, lexer = parse_prim_expr next_lexer in
        aux lexer (Ast.BinExpr (Ast.Mul, acc, exp))
    | Div ->
        let* exp, lexer = parse_prim_expr next_lexer in
        aux lexer (Ast.BinExpr (Ast.Div, acc, exp))
    | Mod ->
        let* exp, lexer = parse_prim_expr next_lexer in
        aux lexer (Ast.BinExpr (Ast.Mod, acc, exp))
    | _ -> Ok (acc, lexer)
  in
  aux lexer prim

and parse_add_expr (lexer : Lexer.t) : (Ast.expr * Lexer.t, string) result =
  let* mul, lexer = parse_mul_expr lexer in
  let rec aux lexer acc =
    let* tt, next_lexer = Lexer.next lexer in
    match tt with
    | Plus ->
        let* exp, lexer = parse_mul_expr next_lexer in
        aux lexer (Ast.BinExpr (Ast.Plus, acc, exp))
    | Minus ->
        let* exp, lexer = parse_mul_expr next_lexer in
        aux lexer (Ast.BinExpr (Ast.Minus, acc, exp))
    | _ -> Ok (acc, lexer)
  in
  aux lexer mul

and parse_expr (lexer : Lexer.t) : (Ast.expr * Lexer.t, string) result =
  parse_add_expr lexer

and parse_print_stmt (lexer : Lexer.t) : (Ast.stmt * Lexer.t, string) result =
  let* _, lexer = expect lexer Token.Print in
  let* expr, lexer = parse_expr lexer in
  let* _, lexer = expect lexer Token.SemiColon in
  Ok (Ast.Print expr, lexer)

and parse_var_stmt (lexer : Lexer.t) : (Ast.stmt * Lexer.t, string) result =
  let* _, lexer = expect lexer Token.Var in
  let* ident, lexer = parse_ident lexer in
  let* _, lexer = expect lexer Token.Eq in
  let* expr, lexer = parse_expr lexer in
  let* _, lexer = expect lexer SemiColon in
  Ok (Ast.VarDecl (ident, expr), lexer)

and parse_stmt (lexer : Lexer.t) : (Ast.stmt * Lexer.t, string) result =
  let* tt, _ = Lexer.next lexer in
  match tt with
  | Token.Print -> parse_print_stmt lexer
  | Token.Var -> parse_var_stmt lexer
  | _ -> Error "expected statement"

and parse_program (lexer : Lexer.t) : (Ast.t * Lexer.t, string) result =
  let rec aux lexer tree =
    let* tt, next_lexer = Lexer.next lexer in
    match tt with
    | Token.EOF -> Ok (tree, lexer)
    | _ ->
        let* t, lexer = parse_stmt lexer in
        aux lexer (t :: tree)
  in
  let* tree, lexer = aux lexer [] in
  Ok (List.rev tree, lexer)
