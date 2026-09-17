type bin_op =
  | Plus
  | Minus
  | Mul
  | Div
  | Mod

type expr =
  | Integer of int64
  | BinExpr of bin_op * expr * expr
  | Var of string

type stmt =
  | Print of expr
  | VarDecl of string * expr
  | If of expr * stmt list * stmt list option

type t = stmt list

let rec string_of_binop (b : bin_op) : string =
  match b with
  | Plus -> "+"
  | Minus -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Mod -> "%"

and string_of_expr (e : expr) : string =
  match e with
  | Integer i -> Int64.to_string i
  | Var v -> v
  | BinExpr (op, l, r) ->
      let ls = string_of_expr l
      and rs = string_of_expr r
      and ops = string_of_binop op in
      Printf.sprintf "(BinExpr (%s %s %s))" ops ls rs

and string_of_stmt (s : stmt) : string =
  match s with
  | Print e ->
      let exps = string_of_expr e in
      Printf.sprintf "Print %s" exps
  | VarDecl (v, e) ->
      let exps = string_of_expr e in
      Printf.sprintf "VarDecl (%s, %s)" v exps
  | If (cond, the, els) ->
      let conds = string_of_expr cond
      and thes = List.map string_of_stmt the |> String.concat " "
      and elss =
        Option.map (List.map string_of_stmt) els
        |> Option.fold ~none:"" ~some:(String.concat " ")
      in
      Printf.sprintf "If (%s (%s) (%s))" conds thes elss

and to_string (tree : t) : string =
  List.map string_of_stmt tree |> String.concat "\n"
