type t = { src : string; src_name : string; pos : int; line : int; col : int }

let rec make (src_name : string) (src : string) : t =
  { src; src_name; pos = 0; line = 1; col = 1 }

and advance (lexer : t) : char option * t =
  let next_pos = lexer.pos + 1 in
  if next_pos >= String.length lexer.src then (None, lexer)
  else
    let c = String.get lexer.src lexer.pos in
    let lexer =
      if c = '\n' then
        { lexer with pos = next_pos; line = lexer.line + 1; col = 1 }
      else { lexer with pos = next_pos; col = lexer.col + 1 }
    in
    (Some c, lexer)

and lex_integer (lexer : t) : Token.t * t =
  let rec aux lexer =
    match advance lexer with Some '0' .. '9', lexer -> aux lexer | _ -> lexer
  in
  let end_lexer = aux lexer in
  let i = String.sub lexer.src lexer.pos (end_lexer.pos - lexer.pos) in
  (Token.make Token.INTEGER i lexer.line lexer.col, end_lexer)

and next (lexer : t) : (Token.t * t, string) result =
  match advance lexer with
  | None, _ -> Ok (Token.make Token.EOF "" lexer.line lexer.col, lexer)
  | Some c, _ -> (
      match c with
      | '0' .. '9' -> Ok (lex_integer lexer)
      | _ ->
          Error
            (lexer.src_name ^ ":" ^ string_of_int lexer.line ^ ":"
           ^ string_of_int lexer.col ^ ": unknown character"))
