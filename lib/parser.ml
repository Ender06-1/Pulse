let ( let* ) = Result.bind

let rec parse_prim_expr (lexer : Lexer.t) : (Ast.t * Lexer.t, string) result =
  let* tt, lexer = Lexer.next lexer in
  match tt with
  | Integer n -> Ok (Ast.Integer (Int64.of_string n), lexer)
  | _ -> Error "expected num"

and parse_add_expr (lexer : Lexer.t) : (Ast.t * Lexer.t, string) result =
  let* prim, lexer = parse_prim_expr lexer in
  let rec aux (lexer : Lexer.t) (acc : Ast.t) =
    let* tt, next_lexer = Lexer.next lexer in
    match tt with
    | Plus ->
        let* exp, lexer = parse_prim_expr next_lexer in
        aux lexer (Ast.BinExpr (Ast.Plus, exp, acc))
    | Minus ->
        let* exp, lexer = parse_prim_expr next_lexer in
        aux lexer (Ast.BinExpr (Ast.Minus, exp, acc))
    | _ -> Ok (acc, lexer)
  in
  aux lexer prim

and parse_program (lexer : Lexer.t) : (Ast.t * Lexer.t, string) result =
  let* expr, lexer = parse_add_expr lexer in
  let* tt, lexer = Lexer.next lexer in
  match tt with EOF -> Ok (expr, lexer) | _ -> Error "expected EOF"
