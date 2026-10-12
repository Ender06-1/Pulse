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

and una_op =
  | Deref
  | Addr

and expr_kind =
  | IntegerLiteral of int64
  | Variable of string
  | UnaryExpr of una_op * expr
  | BinaryExpr of bin_op * expr * expr
  | Call of string * expr list
  | Null

and expr = {
  kind : expr_kind;
  loc : Location.t;
}

and lvalue_kind =
  | Ident of string
  | Deref of lvalue

and lvalue = {
  kind : lvalue_kind;
  loc : Location.t;
}

and stmt_kind =
  | Print of expr
  | VarDecl of string * expr
  | If of expr * stmt list * stmt list option
  | For of expr * stmt list
  | Assign of lvalue * expr
  | Break
  | Continue
  | Return of expr option
  | Expr of expr

and stmt = {
  kind : stmt_kind;
  loc : Location.t;
}

and type_str =
  | Name of string
  | Pointer of type_str

and fn_def = {
  name : string;
  params : (string * type_str) list;
  body : stmt list;
  ret_type : type_str;
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

and pp_unaryop (fmt : Format.formatter) (u : una_op) : unit =
  let unop_str = match u with Deref -> "*" | Addr -> "&" in
  Format.pp_print_string fmt unop_str

and pp_expr (fmt : Format.formatter) (e : expr) : unit = pp_expr_kind fmt e.kind

and pp_expr_kind (fmt : Format.formatter) (e : expr_kind) : unit =
  match e with
  | IntegerLiteral i -> Format.fprintf fmt "%Ld" i
  | Variable v -> Format.pp_print_string fmt v
  | UnaryExpr (op, e) ->
      Format.fprintf fmt "@[<hv 2>(UnaryExpr@ %a@ %a)@]" pp_unaryop op pp_expr e
  | BinaryExpr (op, l, r) ->
      Format.fprintf fmt "@[<hv 2>(BinaryExpr@ %a@ %a@ %a)@]" pp_binop op
        pp_expr l pp_expr r
  | Call (f, params) ->
      Format.fprintf fmt "@[<hv 2>(Call %s@ %a)@]" f
        Format.(pp_print_list ~pp_sep:pp_print_space pp_expr)
        params
  | Null -> Format.pp_print_string fmt "null"

and pp_lvalue_kind (fmt : Format.formatter) (lv : lvalue_kind) : unit =
  match lv with
  | Ident i -> Format.fprintf fmt "@[(LValue@ %s)@]" i
  | Deref d -> Format.fprintf fmt "@[(LValue@ *%a)@]" pp_lvalue d

and pp_lvalue (fmt : Format.formatter) (lv : lvalue) : unit =
  pp_lvalue_kind fmt lv.kind

and pp_stmt_list (fmt : Format.formatter) (stmts : stmt list) : unit =
  Format.(pp_print_list ~pp_sep:pp_print_cut pp_stmt fmt stmts)

and pp_block (fmt : Format.formatter) (stmts : stmt list) : unit =
  Format.fprintf fmt "@[<v 2>(%a)@]" pp_stmt_list stmts

and pp_stmt (fmt : Format.formatter) (s : stmt) : unit = pp_stmt_kind fmt s.kind

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
  | Assign (lv, e) ->
      Format.fprintf fmt "@[<hv 2>(Assign@ %a@ %a)@]" pp_lvalue lv pp_expr e
  | Break -> Format.pp_print_string fmt "(Break)"
  | Continue -> Format.pp_print_string fmt "(Continue)"
  | Return None -> Format.pp_print_string fmt "(Return)"
  | Return (Some e) -> Format.fprintf fmt "@[<hv 2>(Return@ %a)@]" pp_expr e
  | Expr e -> Format.fprintf fmt "@[<hv 2>(Expr@ %a)@]" pp_expr e

and pp_type_str (fmt : Format.formatter) (tr : type_str) : unit =
  let rec aux tr acc =
    match tr with
    | Name n -> Format.fprintf fmt "%s%s" acc n
    | Pointer tr ->
        let acc = Printf.sprintf "%s*" acc in
        aux tr acc
  in
  aux tr ""

and pp_fn_param (fmt : Format.formatter) (p : string * type_str) : unit =
  let name, tr = p in
  Format.fprintf fmt "%s: %a" name pp_type_str tr

and pp_fn_def (fmt : Format.formatter) (f : fn_def) : unit =
  Format.fprintf fmt "@[<v 2>(Fn %s (%a) %a@,%a)@]" f.name
    Format.(pp_print_list ~pp_sep:pp_print_space pp_fn_param)
    f.params pp_type_str f.ret_type pp_block f.body

and pp (fmt : Format.formatter) (tree : t) : unit =
  Format.fprintf fmt "@[<v>%a@]"
    Format.(pp_print_list ~pp_sep:pp_print_cut pp_fn_def)
    tree

and to_string (tree : t) : string = Format.asprintf "%a" pp tree
