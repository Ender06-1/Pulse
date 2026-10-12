type typ =
  | U64
  | Void
  | Pointer of typ

and fn_type = {
  params : (string * typ) list;
  ret_type : typ;
}

and bin_op =
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
  typ : typ;
}

and lvalue_kind =
  | Ident of string
  | Deref of lvalue

and lvalue = {
  kind : lvalue_kind;
  loc : Location.t;
  typ : typ;
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

and fn_def = {
  name : string;
  typ : fn_type;
  body : stmt list;
  loc : Location.t;
}

and t = fn_def list

let string_of_typ (t : typ) : string =
  let rec aux t acc =
    match t with
    | U64 -> Printf.sprintf "%su64" acc
    | Void -> Printf.sprintf "%svoid" acc
    | Pointer t ->
        let acc = String.cat acc "*" in
        aux t acc
  in
  aux t ""

let rec pp_typ (fmt : Format.formatter) (t : typ) : unit =
  let t_str = string_of_typ t in
  Format.pp_print_string fmt t_str

and pp_fn_type (fmt : Format.formatter) (ft : fn_type) : unit =
  let pp_param (fmt : Format.formatter) ((name, t) : string * typ) : unit =
    Format.fprintf fmt "%s: %a" name pp_typ t
  in
  Format.fprintf fmt "{(%a): %a}"
    (Format.pp_print_list ~pp_sep:Format.pp_print_space pp_param)
    ft.params pp_typ ft.ret_type

and pp_binop (fmt : Format.formatter) (b : bin_op) : unit =
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

and pp_expr (fmt : Format.formatter) (e : expr) : unit =
  Format.fprintf fmt "%a{%a}" pp_expr_kind e.kind pp_typ e.typ

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
  Format.fprintf fmt "%a{%a}" pp_lvalue_kind lv.kind pp_typ lv.typ

and pp_stmt_list (fmt : Format.formatter) (stmts : stmt list) : unit =
  Format.pp_print_list ~pp_sep:Format.pp_print_cut pp_stmt fmt stmts

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

and pp_fn_def (fmt : Format.formatter) (f : fn_def) : unit =
  Format.fprintf fmt "@[<v 2>(Fn %s %a@,%a)@]" f.name pp_fn_type f.typ pp_block
    f.body

and pp (fmt : Format.formatter) (tree : t) : unit =
  Format.fprintf fmt "@[<v>%a@]"
    (Format.pp_print_list ~pp_sep:Format.pp_print_cut pp_fn_def)
    tree

and to_string (tree : t) : string = Format.asprintf "%a" pp tree

module StringMap = Map.Make (String)

let to_binop (b : Ast.bin_op) : bin_op =
  match b with
  | Ast.Plus -> Plus
  | Ast.Minus -> Minus
  | Ast.Mul -> Mul
  | Ast.Div -> Div
  | Ast.Mod -> Mod
  | Ast.Eq -> Eq
  | Ast.Neq -> Neq
  | Ast.Gt -> Gt
  | Ast.Lt -> Lt
  | Ast.Ge -> Ge
  | Ast.Le -> Le

and to_unaop (u : Ast.una_op) : una_op =
  match u with Ast.Deref -> Deref | Ast.Addr -> Addr

module Context = struct
  type t = {
    fn_map : fn_type StringMap.t;
    cur_fn : fn_type option;
    var_scopes : typ StringMap.t list;
    is_main_defined : bool;
  }

  let empty : t =
    {
      fn_map = StringMap.empty;
      cur_fn = None;
      var_scopes = [ StringMap.empty ];
      is_main_defined = false;
    }

  let add_fn (name : string) (ft : fn_type) (ctx : t) : t =
    let fn_map = StringMap.add name ft ctx.fn_map in
    { ctx with fn_map }

  and get_fn_opt (name : string) (ctx : t) : fn_type option =
    StringMap.find_opt name ctx.fn_map

  and set_cur_fn (ft : fn_type option) (ctx : t) : t = { ctx with cur_fn = ft }

  and get_cur_fn (ctx : t) : fn_type =
    assert (Option.is_some ctx.cur_fn);
    Option.get ctx.cur_fn

  and push_var_scope (ctx : t) : t =
    { ctx with var_scopes = StringMap.empty :: ctx.var_scopes }

  and pop_var_scope (ctx : t) : t =
    assert (List.length ctx.var_scopes > 0);
    { ctx with var_scopes = List.tl ctx.var_scopes }

  and add_var (name : string) (typ : typ) (ctx : t) : t =
    assert (List.length ctx.var_scopes > 0);
    let s = List.hd ctx.var_scopes in
    let tl = List.tl ctx.var_scopes in
    let s = StringMap.add name typ s in
    { ctx with var_scopes = s :: tl }

  and get_var_opt (name : string) (ctx : t) : typ option =
    let rec aux scopes =
      match scopes with
      | [] -> None
      | s :: tl -> (
          match StringMap.find_opt name s with
          | Some t -> Some t
          | None -> aux tl)
    in
    aux ctx.var_scopes

  and set_main_defined (ctx : t) : t = { ctx with is_main_defined = true }
end

let ( let* ) = Result.bind

let typecheck (ast : Ast.t) : (t, Report.t) result =
  let rec is_compatible (t_expected : typ) (t_actual : typ) : bool =
    match (t_expected, t_actual) with
    | U64, U64 -> true
    | Void, Void -> true
    | Pointer t1, Pointer t2 -> is_compatible t1 t2
    | _ -> false
  and expect (t_expected : typ) (t_actual : typ) (l : Location.t) :
      (typ, Report.t) result =
    if is_compatible t_expected t_actual then Ok t_actual
    else
      let msg =
        string_of_typ t_expected |> Printf.sprintf "invalid type. Expected '%s'"
      in
      let report = Report.make l msg in
      Error report
  and expect_pointer (t_actual : typ) (l : Location.t) : (typ, Report.t) result
      =
    match t_actual with
    | Pointer _ -> Ok t_actual
    | _ ->
        let msg = "invalid type. Expected pointer" in
        let report = Report.make l msg in
        Error report
  and is_variable (e : expr) : bool =
    match e.kind with Variable _ -> true | _ -> false
  and deref_pointer (t : typ) : typ =
    match t with
    | Pointer p -> p
    | _ -> failwith "deref_pointer: type is not a pointer"
  and typecheck_call (c : Ast.expr) (fn : string) (params : Ast.expr list)
      (ctx : Context.t) : (expr, Report.t) result =
    let rec typecheck_params params ctx =
      let rec aux params acc =
        match params with
        | [] -> Ok (List.rev acc, ctx)
        | p :: tl ->
            let* p = typecheck_expr p ctx in
            aux tl (p :: acc)
      in
      aux params []
    in
    match Context.get_fn_opt fn ctx with
    | None ->
        let msg = Printf.sprintf "undefined function '%s'" fn in
        let report = Report.make c.loc msg in
        Error report
    | Some fn_type ->
        let* params, ctx = typecheck_params params ctx in
        let rec aux fn_params exp_params =
          match (fn_params, exp_params) with
          | [], [] -> Ok ()
          | [], _ ->
              let report = Report.make c.loc "too many parameters in call" in
              Error report
          | _, [] ->
              let report = Report.make c.loc "not enough parameters in call" in
              Error report
          | (_, fp) :: ftl, ep :: etl ->
              let* _ = expect fp ep.typ ep.loc in
              aux ftl etl
        in
        let* _ = aux fn_type.params params in
        let e : expr =
          { kind = Call (fn, params); loc = c.loc; typ = fn_type.ret_type }
        in
        Ok e
  and typecheck_expr (e : Ast.expr) (ctx : Context.t) : (expr, Report.t) result
      =
    match e.kind with
    | IntegerLiteral i ->
        let e : expr = { kind = IntegerLiteral i; loc = e.loc; typ = U64 } in
        Ok e
    | Variable v -> (
        match Context.get_var_opt v ctx with
        | None ->
            let msg = Printf.sprintf "undefined variable '%s'" v in
            let report = Report.make e.loc msg in
            Error report
        | Some typ ->
            let e : expr = { kind = Variable v; loc = e.loc; typ } in
            Ok e)
    | Call (fn, params) -> typecheck_call e fn params ctx
    | UnaryExpr (op, e_new) -> (
        let* e_new = typecheck_expr e_new ctx in
        let op = to_unaop op in
        match op with
        | Deref ->
            let* _ = expect_pointer e_new.typ e.loc in
            let sub_typ = deref_pointer e_new.typ in
            let e_una : expr =
              { kind = UnaryExpr (op, e_new); loc = e.loc; typ = sub_typ }
            in
            Ok e_una
        | Addr ->
            if is_variable e_new then
              let e_una : expr =
                {
                  kind = UnaryExpr (op, e_new);
                  loc = e.loc;
                  typ = Pointer e_new.typ;
                }
              in
              Ok e_una
            else
              let msg = "cannot take address of non-variable" in
              let report = Report.make e.loc msg in
              Error report)
    | BinaryExpr (op, l, r) ->
        let* l = typecheck_expr l ctx in
        let* r = typecheck_expr r ctx in
        let* _ = expect U64 l.typ l.loc in
        let* _ = expect U64 r.typ r.loc in
        let e : expr =
          { kind = BinaryExpr (to_binop op, l, r); loc = e.loc; typ = U64 }
        in
        Ok e
    | Null ->
        let e : expr = { kind = Null; loc = e.loc; typ = Pointer Void } in
        Ok e
  and typecheck_lvalue (lv : Ast.lvalue) (ctx : Context.t) :
      (lvalue, Report.t) result =
    match lv.kind with
    | Ident i -> (
        match Context.get_var_opt i ctx with
        | Some t ->
            let l : lvalue = { kind = Ident i; loc = lv.loc; typ = t } in
            Ok l
        | None ->
            let msg = Printf.sprintf "undefined variable '%s'" i in
            let report = Report.make lv.loc msg in
            Error report)
    | Deref d ->
        let* sub_lv = typecheck_lvalue d ctx in
        let l : lvalue =
          { kind = Deref sub_lv; loc = lv.loc; typ = deref_pointer sub_lv.typ }
        in
        Ok l
  and typecheck_stmt (s : Ast.stmt) (ctx : Context.t) :
      (stmt * Context.t, Report.t) result =
    match s.kind with
    | Print e ->
        let* e = typecheck_expr e ctx in
        let* _ = expect U64 e.typ e.loc in
        let s : stmt = { kind = Print e; loc = s.loc } in
        Ok (s, ctx)
    | VarDecl (v, e) ->
        let* e = typecheck_expr e ctx in
        let ctx = Context.add_var v e.typ ctx in
        let s : stmt = { kind = VarDecl (v, e); loc = s.loc } in
        Ok (s, ctx)
    | If (cond, then_block, else_block_opt) -> (
        let* cond = typecheck_expr cond ctx in
        let* _ = expect U64 cond.typ cond.loc in
        let ctx = Context.push_var_scope ctx in
        let* then_block, ctx = typecheck_stmt_list then_block ctx in
        let ctx = Context.pop_var_scope ctx in
        match else_block_opt with
        | None ->
            let s : stmt =
              { kind = If (cond, then_block, None); loc = s.loc }
            in
            Ok (s, ctx)
        | Some else_block ->
            let ctx = Context.push_var_scope ctx in
            let* else_block, ctx = typecheck_stmt_list else_block ctx in
            let ctx = Context.pop_var_scope ctx in
            let s : stmt =
              { kind = If (cond, then_block, Some else_block); loc = s.loc }
            in
            Ok (s, ctx))
    | For (cond, body) ->
        let* cond = typecheck_expr cond ctx in
        let* _ = expect U64 cond.typ cond.loc in
        let ctx = Context.push_var_scope ctx in
        let* body, ctx = typecheck_stmt_list body ctx in
        let ctx = Context.pop_var_scope ctx in
        let s : stmt = { kind = For (cond, body); loc = s.loc } in
        Ok (s, ctx)
    | Assign (lv, e) ->
        let* lv = typecheck_lvalue lv ctx in
        let* e = typecheck_expr e ctx in
        let* _ = expect lv.typ e.typ e.loc in
        let s : stmt = { kind = Assign (lv, e); loc = s.loc } in
        Ok (s, ctx)
    | Break ->
        let s : stmt = { kind = Break; loc = s.loc } in
        Ok (s, ctx)
    | Continue ->
        let s : stmt = { kind = Continue; loc = s.loc } in
        Ok (s, ctx)
    | Return e_opt ->
        let* e_opt =
          match e_opt with
          | None ->
              let* _ = expect (Context.get_cur_fn ctx).ret_type Void s.loc in
              Ok None
          | Some e ->
              let* e = typecheck_expr e ctx in
              let* _ = expect (Context.get_cur_fn ctx).ret_type e.typ e.loc in
              Ok (Some e)
        in
        let s : stmt = { kind = Return e_opt; loc = s.loc } in
        Ok (s, ctx)
    | Expr e ->
        let* e = typecheck_expr e ctx in
        let s : stmt = { kind = Expr e; loc = s.loc } in
        Ok (s, ctx)
  and typecheck_stmt_list (sl : Ast.stmt list) (ctx : Context.t) :
      (stmt list * Context.t, Report.t) result =
    let rec aux sl acc ctx =
      match sl with
      | [] -> Ok (List.rev acc, ctx)
      | s :: tl ->
          let* stmt, ctx = typecheck_stmt s ctx in
          aux tl (stmt :: acc) ctx
    in
    aux sl [] ctx
  and typecheck_type_str (t : Ast.type_str) (l : Location.t) :
      (typ, Report.t) result =
    match t with
    | Name "u64" -> Ok U64
    | Name "void" -> Ok Void
    | Pointer p ->
        let* t = typecheck_type_str p l in
        Ok (Pointer t)
    | Name n ->
        let msg = Printf.sprintf "unknown type '%s'" n in
        let report = Report.make l msg in
        Error report
  and typecheck_fn_decl (fn : Ast.fn_def) (ctx : Context.t) :
      (Context.t, Report.t) result =
    let* ret_type = typecheck_type_str fn.ret_type fn.loc in
    let* params =
      let rec aux params acc =
        match params with
        | [] -> Ok (List.rev acc)
        | (p, t) :: tl ->
            let* t = typecheck_type_str t fn.loc in
            aux tl ((p, t) :: acc)
      in
      aux fn.params []
    in
    let fn_type : fn_type = { params; ret_type } in
    let ctx = Context.add_fn fn.name fn_type ctx in
    let ctx = if fn.name = "main" then Context.set_main_defined ctx else ctx in
    Ok ctx
  and typecheck_fn_def (fn : Ast.fn_def) (ctx : Context.t) :
      (fn_def * Context.t, Report.t) result =
    let fn_type_opt = Context.get_fn_opt fn.name ctx in
    assert (Option.is_some fn_type_opt);
    let fn_type = Option.get fn_type_opt in
    let ctx = Context.set_cur_fn (Some fn_type) ctx in
    let ctx = Context.push_var_scope ctx in
    let ctx =
      List.fold_left
        (fun ctx (p, t) -> Context.add_var p t ctx)
        ctx fn_type.params
    in
    let* body, ctx = typecheck_stmt_list fn.body ctx in
    let ctx = Context.pop_var_scope ctx in
    let ctx = Context.set_cur_fn None ctx in
    let fn : fn_def = { name = fn.name; typ = fn_type; body; loc = fn.loc } in
    Ok (fn, ctx)
  and typecheck_fn_list (fl : Ast.fn_def list) (ctx : Context.t) :
      (fn_def list * Context.t, Report.t) result =
    let rec aux_decl fl ctx =
      match fl with
      | [] -> Ok ctx
      | fn :: tl ->
          let* ctx = typecheck_fn_decl fn ctx in
          aux_decl tl ctx
    and aux_def fl acc ctx =
      match fl with
      | [] -> Ok (List.rev acc, ctx)
      | fn :: tl ->
          let* fn, ctx = typecheck_fn_def fn ctx in
          aux_def tl (fn :: acc) ctx
    in
    let* ctx = aux_decl fl ctx in
    aux_def fl [] ctx
  in
  let* tree, ctx = typecheck_fn_list ast Context.empty in
  if ctx.is_main_defined then Ok tree
  else
    let msg = "no main function defined" in
    let report = Report.make { line = 1; col = 1; file_path = "" } msg in
    Error report
