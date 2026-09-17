type typ =
  (* Constants *)
  | Integer of string
  | Ident of string
  (* Keywords *)
  | Print
  | Var
  | If
  | Else
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
  (* Misc *)
  | EOF

let string_of_typ t =
  match t with
  | Integer i -> "integer " ^ i
  | Ident i -> "ident " ^ i
  | Print -> "print"
  | Var -> "var"
  | If -> "if"
  | Else -> "else"
  | Plus -> "plus"
  | Minus -> "minus"
  | Mul -> "mul"
  | Div -> "div"
  | Mod -> "mod"
  | Eq -> "equal"
  | SemiColon -> "semicolon"
  | OBrack -> "open-bracket"
  | CBrack -> "close-bracket"
  | EOF -> "EOF"

and keyword_of_string_opt (s : string) =
  match s with
  | "print" -> Some Print
  | "var" -> Some Var
  | "if" -> Some If
  | "else" -> Some Else
  | _ -> None

and equal (t1 : typ) (t2 : typ) : bool =
  match (t1, t2) with
  (* Constants *)
  | Integer x, Integer y -> x = y
  | Ident i, Ident j -> i = j
  (* Keywords *)
  | Print, Print -> true
  | Var, Var -> true
  | If, If -> true
  | Else, Else -> true
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
  (* Misc *)
  | EOF, EOF -> true
  | _ -> false
