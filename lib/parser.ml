let ( let* ) = Result.bind

let rec make_error (src_name : string) (line : int) (col : int) (msg : string) =
  src_name ^ ":" ^ string_of_int line ^ ":" ^ string_of_int col ^ ": " ^ msg

and consume (lexer : Lexer.t) (token_kind : Token.token_kind) :
    (Token.t * Lexer.t, string) result =
  let* t, next_lexer = Lexer.next lexer in
  if Token.equal_token_kind token_kind t.kind then Ok (t, next_lexer)
  else
    Error
      (make_error lexer.src_name lexer.line lexer.col
         "unexpected token, expecting "
      ^ Token.name_of_kind token_kind)

and parse_eof (lexer : Lexer.t) : (Token.t * Lexer.t, string) result =
  consume lexer Token.EOF

and parse_integer (lexer : Lexer.t) : (Ast.t * Lexer.t, string) result =
  let* t, next_lexer = consume lexer Token.INTEGER in
  match Int64.of_string_opt t.value with
  | None ->
      Error
        (make_error lexer.src_name lexer.line lexer.col "invalid integer format")
  | Some ret_val -> Ok (Ast.Return ret_val, next_lexer)

and parse (lexer : Lexer.t) : (Ast.t, string) result =
  let* tree, lexer = parse_integer lexer in
  let* _, _ = parse_eof lexer in
  Ok tree
