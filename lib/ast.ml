type bin_op = Plus
type t = IntNum of int64 | BinExpr of bin_op * t * t
