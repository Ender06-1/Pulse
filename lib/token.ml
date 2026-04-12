type t = { kind : token_kind; value : string; line : int; col : int }
and token_kind = INTEGER | EOF [@@deriving show, eq]

let make (kind : token_kind) (value : string) (line : int) (col : int) : t =
  { kind; value; line; col }

and name_of_kind kind =
  match kind with INTEGER -> "integer" | EOF -> "end of file"
