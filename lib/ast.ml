type bin_op = Plus | Minus
type t = Integer of int64 | BinExpr of bin_op * t * t
