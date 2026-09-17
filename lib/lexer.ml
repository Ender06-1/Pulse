type t = {
  input : string;
  pos : int;
}

let ( let* ) = Result.bind

let rec make (input : string) : t = { input; pos = 0 }

and advance (lexer : t) : (char * t) option =
  if lexer.pos >= String.length lexer.input then None
  else
    Some (String.get lexer.input lexer.pos, { lexer with pos = lexer.pos + 1 })

and is_ident_char c =
  match c with 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> true | _ -> false

and lex_integer (lexer : t) : Token.typ * t =
  let rec aux lexer acc =
    match advance lexer with
    | Some (c, next_lexer) -> (
        match c with
        | '0' .. '9' as c -> aux next_lexer (String.make 1 c |> String.cat acc)
        | _ -> (Token.Integer acc, lexer))
    | _ -> (Token.Integer acc, lexer)
  in
  aux lexer ""

and lex_identifier (lexer : t) : Token.typ * t =
  let rec aux lexer acc =
    match advance lexer with
    | Some (c, next_lexer) -> (
        match c with
        | c when is_ident_char c ->
            aux next_lexer (String.make 1 c |> String.cat acc)
        | _ -> (
            match Token.keyword_of_string_opt acc with
            | Some t -> (t, lexer)
            | _ -> (Token.Ident acc, lexer)))
    | _ -> (
        match Token.keyword_of_string_opt acc with
        | Some t -> (t, lexer)
        | _ -> (Token.Ident acc, lexer))
  in
  aux lexer ""

and next (lexer : t) : (Token.typ * t, string) result =
  match advance lexer with
  | Some (c, next_lexer) -> (
      match c with
      | ' ' | '\n' -> next next_lexer
      | '+' -> Ok (Plus, next_lexer)
      | '-' -> Ok (Minus, next_lexer)
      | '*' -> Ok (Mul, next_lexer)
      | '/' -> Ok (Div, next_lexer)
      | '%' -> Ok (Mod, next_lexer)
      | ';' -> Ok (SemiColon, next_lexer)
      | '=' -> Ok (Eq, next_lexer)
      | '{' -> Ok (OBrack, next_lexer)
      | '}' -> Ok (CBrack, next_lexer)
      | '0' .. '9' -> Ok (lex_integer lexer)
      | c when is_ident_char c -> Ok (lex_identifier lexer)
      | c -> Error (Printf.sprintf "unknown character '%c'" c))
  | _ -> Ok (EOF, lexer)
