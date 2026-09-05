type t = { input : string; pos : int }

let rec make (input : string) : t = { input; pos = 0 }

and lex_int (lexer : t) (acc : string) : Token.typ * t =
  match String.get lexer.input lexer.pos with
  | '0' .. '9' as c ->
      lex_int
        { lexer with pos = lexer.pos + 1 }
        (String.make 1 c |> String.cat acc)
  | _ -> (IntNum acc, lexer)

and next (lexer : t) : (Token.typ * t, string) result =
  if lexer.pos >= String.length lexer.input then Ok (EOF, lexer)
  else
    match String.get lexer.input lexer.pos with
    | '0' .. '9' -> Ok (lex_int lexer "")
    | '+' -> Ok (Plus, { lexer with pos = lexer.pos + 1 })
    | ' ' | '\n' -> next { lexer with pos = lexer.pos + 1 }
    | _ -> Error "invalid character"
