type typ =
  (* Constants *)
  | Integer of string
  | Ident of string
  (* Keywords *)
  | Print
  | Var
  (* Operators *)
  | Plus
  | Minus
  | Mul
  | Div
  | Mod
  | Eq
  (* Ponctuators *)
  | SemiColon
  (* Misc *)
  | EOF

let to_keyword (s : string) =
  match s with "print" -> Some Print | "var" -> Some Var | _ -> None

and equal (t1 : typ) (t2 : typ) : bool =
  match (t1, t2) with
  (* Constants *)
  | Integer x, Integer y -> x = y
  | Ident i, Ident j -> i = j
  (* Keywords *)
  | Print, Print -> true
  | Var, Var -> true
  (* Operators *)
  | Plus, Plus -> true
  | Minus, Minus -> true
  | Mul, Mul -> true
  | Div, Div -> true
  | Mod, Mod -> true
  | Eq, Eq -> true
  (* Ponctuators *)
  | SemiColon, SemiColon -> true
  (* Misc *)
  | EOF, EOF -> true
  | _ -> false

and string_of_typ t =
  match t with
  | Integer i -> i
  | Ident i -> i
  | Print -> "print"
  | Var -> "var"
  | Plus | Minus | Mul | Div | Mod -> "operator"
  | Eq -> "="
  | SemiColon -> ";"
  | EOF -> "end of file"
