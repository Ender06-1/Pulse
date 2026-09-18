type t = {
  input : string;
  pos : int;
  loc : Location.t;
}

let ( let* ) = Result.bind

let rec make (input : string) (file_path : string) : t =
  { input; pos = 0; loc = { line = 1; col = 1; file_path } }

and advance (lexer : t) : (char * t) option =
  if lexer.pos >= String.length lexer.input then None
  else
    let pos = lexer.pos + 1 and c = String.get lexer.input lexer.pos in
    if c = '\n' then
      Some
        ( c,
          {
            lexer with
            pos;
            loc = { lexer.loc with line = lexer.loc.line + 1; col = 1 };
          } )
    else
      Some
        (c, { lexer with pos; loc = { lexer.loc with col = lexer.loc.col + 1 } })

and is_ident_char c =
  match c with 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> true | _ -> false

and is_whitespace c = match c with ' ' | '\n' | '\t' -> true | _ -> false

and skip_whitespace (lexer : t) : t =
  match advance lexer with
  | Some (c, lexer) when is_whitespace c -> skip_whitespace lexer
  | _ -> lexer

and skip_comment (lexer : t) : t =
  match advance lexer with
  | Some ('\n', lexer) -> lexer
  | None -> lexer
  | Some (_, lexer) -> skip_comment lexer

and lex_integer (lexer : t) : Token.kind * Location.t * t =
  let rec aux lexer acc =
    match advance lexer with
    | Some (c, next_lexer) -> (
        match c with
        | '0' .. '9' as c -> aux next_lexer (String.make 1 c |> String.cat acc)
        | _ -> (Token.Integer acc, lexer))
    | _ -> (Token.Integer acc, lexer)
  in
  let kind, next_lexer = aux lexer "" in
  (kind, lexer.loc, next_lexer)

and lex_identifier (lexer : t) : Token.kind * Location.t * t =
  let rec aux lexer acc =
    match advance lexer with
    | Some (c, next_lexer) -> (
        match c with
        | c when is_ident_char c ->
            aux next_lexer (String.make 1 c |> String.cat acc)
        | _ -> (acc, lexer))
    | _ -> (acc, lexer)
  in
  let kind, next_lexer =
    let word, next_lexer = aux lexer "" in
    match Token.keyword_of_string_opt word with
    | Some t -> (t, next_lexer)
    | _ -> (Token.Ident word, next_lexer)
  in
  (kind, lexer.loc, next_lexer)

and next (lexer : t) : (Token.t * t, Report.t) result =
  let rec aux lexer =
    let lexer = skip_whitespace lexer in
    match advance lexer with
    | Some (c, next_lexer) -> (
        match c with
        | '#' -> skip_comment next_lexer |> aux
        | '+' -> Ok (Token.Plus, lexer.loc, next_lexer)
        | '-' -> Ok (Token.Minus, lexer.loc, next_lexer)
        | '*' -> Ok (Token.Mul, lexer.loc, next_lexer)
        | '/' -> Ok (Token.Div, lexer.loc, next_lexer)
        | '%' -> Ok (Token.Mod, lexer.loc, next_lexer)
        | ';' -> Ok (Token.SemiColon, lexer.loc, next_lexer)
        | '=' -> Ok (Token.Eq, lexer.loc, next_lexer)
        | '{' -> Ok (Token.OBrack, lexer.loc, next_lexer)
        | '}' -> Ok (Token.CBrack, lexer.loc, next_lexer)
        | '0' .. '9' -> Ok (lex_integer lexer)
        | c when is_ident_char c -> Ok (lex_identifier lexer)
        | c ->
            let msg = Printf.sprintf "unknown character '%c'" c in
            let report = Report.make lexer.loc msg in
            Error report)
    | _ -> Ok (EOF, lexer.loc, lexer)
  in
  let* kind, loc, lexer = aux lexer in
  let token : Token.t = { kind; loc } in
  Ok (token, lexer)
