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
  (* Operators *)
  | Plus
  | Minus
  | Mul
  | Div
  | Mod
  | Eq
  (* Ponctuators *)
  | SemiColon
  | OBrack
  | CBrack
  | OParen
  | CParen
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
  (* Operators *)
  | Plus -> "Plus"
  | Minus -> "Minus"
  | Mul -> "Mul"
  | Div -> "Div"
  | Mod -> "Mod"
  | Eq -> "Equal"
  (* Ponctuators *)
  | SemiColon -> "SemiColon"
  | OBrack -> "OBrack"
  | CBrack -> "CBrack"
  | OParen -> "OParen"
  | CParen -> "CParen"
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
  (* Operators *)
  | Plus -> "+"
  | Minus -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Eq -> "="
  (* Ponctuators *)
  | SemiColon -> ";"
  | OBrack -> "{"
  | CBrack -> "}"
  | OParen -> "("
  | CParen -> ")"
  (* Misc *)
  | EOF -> "EOF"

and to_string (t : t) : string =
  let kind_str = string_of_kind t.kind and loc_str = Location.to_string t.loc in
  Printf.sprintf "%s{%s}" kind_str loc_str

let keyword_of_string_opt (s : string) =
  match s with
  | "print" -> Some Print
  | "var" -> Some Var
  | "if" -> Some If
  | "else" -> Some Else
  | "for" -> Some For
  | _ -> None

and equal_kind (k1 : kind) (k2 : kind) : bool =
  match (k1, k2) with
  (* Constants *)
  | Integer x, Integer y -> x = y
  | Ident i, Ident j -> i = j
  (* Keywords *)
  | Print, Print -> true
  | Var, Var -> true
  | If, If -> true
  | Else, Else -> true
  | For, For -> true
  (* Operators *)
  | Plus, Plus -> true
  | Minus, Minus -> true
  | Mul, Mul -> true
  | Div, Div -> true
  | Mod, Mod -> true
  | Eq, Eq -> true
  (* Ponctuators *)
  | SemiColon, SemiColon -> true
  | OBrack, OBrack -> true
  | CBrack, CBrack -> true
  | OParen, OParen -> true
  | CParen, CParen -> true
  (* Misc *)
  | EOF, EOF -> true
  | _ -> false
