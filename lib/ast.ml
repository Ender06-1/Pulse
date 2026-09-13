type bin_op = Plus | Minus | Mul | Div | Mod
type expr = Integer of int64 | BinExpr of bin_op * expr * expr | Var of string
type stmt = Print of expr | VarDecl of string * expr
type t = stmt list
