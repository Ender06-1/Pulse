type t = { input : string; pos : int }

let rec make (input : string) : t = { input; pos = 0 }
and advance (lexer : t) : t = { lexer with pos = lexer.pos + 1 }

and lex_integer (lexer : t) (acc : string) : Token.typ * t =
  match String.get lexer.input lexer.pos with
  | '0' .. '9' as c ->
      lex_integer (advance lexer) (String.make 1 c |> String.cat acc)
  | _ -> (Integer acc, lexer)

and next (lexer : t) : (Token.typ * t, string) result =
  if lexer.pos >= String.length lexer.input then Ok (EOF, lexer)
  else
    match String.get lexer.input lexer.pos with
    | '0' .. '9' -> Ok (lex_integer lexer "")
    | '+' -> Ok (Plus, advance lexer)
    | '-' -> Ok (Minus, advance lexer)
    | ' ' | '\n' -> advance lexer |> next
    | c -> Error (Printf.sprintf "unknown character '%c'" c)
