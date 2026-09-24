type kind =
  (* Constants *)
  | Integer of string
  | Ident of string
  (* Keywords *)
  | Print
  | Var
  | If
  | Else
  | For
  | Break
  | Continue
  | Fn
  | Return
  (* Operators *)
  | Plus
  | Minus
  | Mul
  | Div
  | Mod
  | Eq
  | Deq
  | Neq
  | Gt
  | Lt
  | Ge
  | Le
  (* Ponctuators *)
  | SemiColon
  | Colon
  | OBrack
  | CBrack
  | OParen
  | CParen
  | Comma
  (* Misc *)
  | EOF

and t = {
  kind : kind;
  loc : Location.t;
}

let rec string_of_kind k =
  match k with
  (* Constants *)
  | Integer i -> Printf.sprintf "Integer(%s)" i
  | Ident i -> Printf.sprintf "Ident(%s)" i
  (* Keywords *)
  | Print -> "Print"
  | Var -> "Var"
  | If -> "If"
  | Else -> "Else"
  | For -> "For"
  | Break -> "Break"
  | Continue -> "Continue"
  | Fn -> "Fn"
  | Return -> "Return"
  (* Operators *)
  | Plus -> "Plus"
  | Minus -> "Minus"
  | Mul -> "Mul"
  | Div -> "Div"
  | Mod -> "Mod"
  | Eq -> "Equal"
  | Deq -> "DoubleEqual"
  | Neq -> "NotEqual"
  | Gt -> "GreaterThan"
  | Lt -> "LessThan"
  | Ge -> "GreaterEqual"
  | Le -> "LessEqual"
  (* Ponctuators *)
  | SemiColon -> "SemiColon"
  | Colon -> "Colon"
  | OBrack -> "OBrack"
  | CBrack -> "CBrack"
  | OParen -> "OParen"
  | CParen -> "CParen"
  | Comma -> "Comma"
  (* Misc *)
  | EOF -> "EOF"

and show_kind k =
  match k with
  (* Constants *)
  | Integer i -> i
  | Ident i -> i
  (* Keywords *)
  | Print -> "print"
  | Var -> "var"
  | If -> "if"
  | Else -> "else"
  | For -> "for"
  | Break -> "break"
  | Continue -> "continue"
  | Fn -> "fn"
  | Return -> "return"
  (* Operators *)
  | Plus -> "+"
  | Minus -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Eq -> "="
  | Deq -> "=="
  | Neq -> "!="
  | Gt -> ">"
  | Lt -> "<"
  | Ge -> ">="
  | Le -> "<="
  (* Ponctuators *)
  | SemiColon -> ";"
  | Colon -> ":"
  | OBrack -> "{"
  | CBrack -> "}"
  | OParen -> "("
  | CParen -> ")"
  | Comma -> ","
  (* Misc *)
  | EOF -> "EOF"

and to_string (t : t) : string =
  let kind_str = string_of_kind t.kind and loc_str = Location.to_string t.loc in
  Printf.sprintf "%s{%s}" kind_str loc_str

let rec keyword_of_string_opt (s : string) =
  match s with
  | "print" -> Some Print
  | "var" -> Some Var
  | "if" -> Some If
  | "else" -> Some Else
  | "for" -> Some For
  | "break" -> Some Break
  | "continue" -> Some Continue
  | "fn" -> Some Fn
  | "return" -> Some Return
  | _ -> None

and equal_kind (k1 : kind) (k2 : kind) : bool =
  match (k1, k2) with
  (* Constants *)
  | Integer _, Integer _ -> true
  | Ident _, Ident _ -> true
  (* Keywords *)
  | Print, Print -> true
  | Var, Var -> true
  | If, If -> true
  | Else, Else -> true
  | For, For -> true
  | Break, Break -> true
  | Continue, Continue -> true
  | Fn, Fn -> true
  | Return, Return -> true
  (* Operators *)
  | Plus, Plus -> true
  | Minus, Minus -> true
  | Mul, Mul -> true
  | Div, Div -> true
  | Mod, Mod -> true
  | Eq, Eq -> true
  | Deq, Deq -> true
  | Neq, Neq -> true
  | Gt, Gt -> true
  | Lt, Lt -> true
  | Ge, Ge -> true
  | Le, Le -> true
  (* Ponctuators *)
  | SemiColon, SemiColon -> true
  | Colon, Colon -> true
  | OBrack, OBrack -> true
  | CBrack, CBrack -> true
  | OParen, OParen -> true
  | CParen, CParen -> true
  | Comma, Comma -> true
  (* Misc *)
  | EOF, EOF -> true
  | _ -> false

and get_ident (t : kind) : string =
  assert (equal_kind t (Ident ""));
  match t with
  | Ident i -> i
  | _ -> failwith "Token.get_ident: not an identifier"
