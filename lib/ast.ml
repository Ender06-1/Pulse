type bin_op =
  | Plus
  | Minus
  | Mul
  | Div
  | Mod
  | Eq
  | Neq
  | Gt
  | Lt
  | Ge
  | Le

and expr_kind =
  | Integer of int64
  | BinExpr of bin_op * expr * expr
  | Var of string
  | Call of string * expr list

and expr = {
  kind : expr_kind;
  loc : Location.t;
}

and stmt_kind =
  | Print of expr
  | VarDecl of string * expr
  | If of expr * stmt list * stmt list option
  | For of expr * stmt list
  | Assign of string * expr
  | Break
  | Continue
  | Return of expr option
  | Expr of expr

and stmt = {
  kind : stmt_kind;
  loc : Location.t;
}

and fn_def = {
  name : string;
  params : string list;
  body : stmt list;
  ret_type : string;
  loc : Location.t;
}

and t = fn_def list

let rec pp_binop (fmt : Format.formatter) (b : bin_op) : unit =
  let binop_str =
    match b with
    | Plus -> "+"
    | Minus -> "-"
    | Mul -> "*"
    | Div -> "/"
    | Mod -> "%"
    | Eq -> "=="
    | Neq -> "!="
    | Gt -> ">"
    | Lt -> "<"
    | Ge -> ">="
    | Le -> "<="
  in
  Format.pp_print_string fmt binop_str

and pp_expr (fmt : Format.formatter) (e : expr) : unit = pp_expr_kind fmt e.kind

and pp_expr_kind (fmt : Format.formatter) (e : expr_kind) : unit =
  match e with
  | Integer i -> Format.fprintf fmt "%Ld" i
  | Var v -> Format.pp_print_string fmt v
  | BinExpr (op, l, r) ->
      Format.fprintf fmt "@[<hv 2>(BinExpr@ %a@ %a@ %a)@]" pp_binop op pp_expr l
        pp_expr r
  | Call (f, params) ->
      Format.fprintf fmt "@[<hv 2>(Call %s@ %a)@]" f
        Format.(pp_print_list ~pp_sep:pp_print_space pp_expr)
        params

and pp_stmt (fmt : Format.formatter) (s : stmt) : unit = pp_stmt_kind fmt s.kind

and pp_stmt_list (fmt : Format.formatter) (stmts : stmt list) : unit =
  Format.(pp_print_list ~pp_sep:pp_print_cut pp_stmt fmt stmts)

and pp_block (fmt : Format.formatter) (stmts : stmt list) : unit =
  Format.fprintf fmt "@[<v 2>(%a)@]" pp_stmt_list stmts

and pp_stmt_kind (fmt : Format.formatter) (s : stmt_kind) : unit =
  match s with
  | Print e -> Format.fprintf fmt "@[<hv 2>(Print@ %a)@]" pp_expr e
  | VarDecl (v, e) ->
      Format.fprintf fmt "@[<hv 2>(VarDecl@ %s@ %a)@]" v pp_expr e
  | If (cond, then_block, else_block_opt) ->
      let else_block = Option.value else_block_opt ~default:[] in
      Format.fprintf fmt "@[<v 2>(If %a@,%a@,%a)@]" pp_expr cond pp_block
        then_block pp_block else_block
  | For (cond, body) ->
      Format.fprintf fmt "@[<v 2>(For %a@,%a)@]" pp_expr cond pp_block body
  | Assign (i, e) -> Format.fprintf fmt "@[<hv 2>(Assign@ %s@ %a)@]" i pp_expr e
  | Break -> Format.pp_print_string fmt "(Break)"
  | Continue -> Format.pp_print_string fmt "(Continue)"
  | Return None -> Format.pp_print_string fmt "(Return)"
  | Return (Some e) -> Format.fprintf fmt "@[<hv 2>(Return@ %a)@]" pp_expr e
  | Expr e -> Format.fprintf fmt "@[<hv 2>(Expr@ %a)@]" pp_expr e

and pp_fn_def (fmt : Format.formatter) (f : fn_def) : unit =
  Format.fprintf fmt "@[<v 2>(Fn %s (%a) %s@,%a)@]" f.name
    Format.(pp_print_list ~pp_sep:pp_print_space pp_print_string)
    f.params f.ret_type pp_block f.body

and pp (fmt : Format.formatter) (tree : t) : unit =
  Format.fprintf fmt "@[<v>%a@]"
    Format.(pp_print_list ~pp_sep:pp_print_cut pp_fn_def)
    tree

and to_string (tree : t) : string = Format.asprintf "%a" pp tree
