(* Grammar *)
(* program = add_expr EOF ;*)
(* add_expr = prim_expr ( "+" add_expr )* ; *)
(* prim_expr = num *)

let ( let* ) = Result.bind

let rec parse_prim_expr (lexer : Lexer.t) : (Ast.t * Lexer.t, string) result =
  let* tt, lexer = Lexer.next lexer in
  match tt with
  | IntNum n -> Ok (Ast.IntNum (Int64.of_string n), lexer)
  | _ -> Error "expected num"

and parse_add_expr (lexer : Lexer.t) : (Ast.t * Lexer.t, string) result =
  let* prim, lexer = parse_prim_expr lexer in
  let* tt, lexer = Lexer.next lexer in
  match tt with
  | Plus ->
      let* exp, lexer = parse_add_expr lexer in
      Ok (Ast.BinExpr (Ast.Plus, prim, exp), lexer)
  | _ -> Ok (prim, lexer)

and parse_program (lexer : Lexer.t) : (Ast.t * Lexer.t, string) result =
  let* expr, lexer = parse_add_expr lexer in
  let* tt, lexer = Lexer.next lexer in
  match tt with EOF -> Ok (expr, lexer) | _ -> Error "expected EOF"
