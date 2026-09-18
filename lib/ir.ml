module Context = struct
  module StringMap = Map.Make (String)

  type t = {
    tmp_acc : int;
    label_acc : int;
    var_tmp_scopes : int StringMap.t list;
  }

  let empty : t =
    { tmp_acc = 0; label_acc = 0; var_tmp_scopes = [ StringMap.empty ] }

  and gen_tmp (ctx : t) : int * t =
    (ctx.tmp_acc, { ctx with tmp_acc = ctx.tmp_acc + 1 })

  and gen_label (ctx : t) : int * t =
    (ctx.label_acc, { ctx with label_acc = ctx.label_acc + 1 })

  and push_scope (ctx : t) : t =
    { ctx with var_tmp_scopes = StringMap.empty :: ctx.var_tmp_scopes }

  and pop_scope (ctx : t) : t =
    assert (List.length ctx.var_tmp_scopes > 0);
    { ctx with var_tmp_scopes = List.tl ctx.var_tmp_scopes }

  and add_var_tmp (name : string) (t : int) (ctx : t) : t =
    assert (List.length ctx.var_tmp_scopes > 0);
    let s = List.hd ctx.var_tmp_scopes in
    let s = StringMap.add name t s in
    { ctx with var_tmp_scopes = s :: List.tl ctx.var_tmp_scopes }

  and get_var_tmp (name : string) (ctx : t) : int =
    assert (List.length ctx.var_tmp_scopes > 0);
    let rec aux (scopes : int StringMap.t list) =
      match scopes with
      | [] -> failwith "Ir.Context.get_var_tmp: variable without tmp"
      | s :: tl -> (
          match StringMap.find_opt name s with Some t -> t | None -> aux tl)
    in
    aux ctx.var_tmp_scopes
end

type value =
  | Temp of int
  | Integer of int64

type instruction =
  | Print of value
  | Copy of int * value
  | Add of int * value * value
  | Sub of int * value * value
  | Mul of int * value * value
  | Div of int * value * value
  | Mod of int * value * value
  | Ceq of int * value * value

type terminator =
  | Jmp of int
  | Jnz of value * int * int
  | Halt

type block = {
  label : int;
  instrs : instruction list;
  term : terminator;
}

type cfg = block list

let ( let* ) = Result.bind

let cfg_add_insts (instrs : instruction list) (b : block) : block =
  { b with instrs = b.instrs @ instrs }

and cfg_add_block (b : block) (term : terminator) (cfg : cfg) : cfg =
  let b = { b with term } in
  cfg @ [ b ]

and cfg_make_block (label : int) : block =
  { label; instrs = []; term = Jmp (-1) }

let rec string_of_label (l : int) : string = Printf.sprintf "@%d" l
and string_of_tmp (t : int) : string = Printf.sprintf "%%%d" t

and string_of_value (v : value) : string =
  match v with Temp t -> string_of_tmp t | Integer i -> Int64.to_string i

and string_of_instruction (i : instruction) : string =
  let inst =
    match i with
    | Print v ->
        let vals = string_of_value v in
        Printf.sprintf "print %s" vals
    | Copy (t, value) ->
        let t_str = string_of_tmp t and value_str = string_of_value value in
        Printf.sprintf "%s = copy %s" t_str value_str
    | Add (t, l, r) ->
        let t_str = string_of_tmp t
        and l_str = string_of_value l
        and r_str = string_of_value r in
        Printf.sprintf "%s = add %s, %s" t_str l_str r_str
    | Sub (t, l, r) ->
        let t_str = string_of_tmp t
        and l_str = string_of_value l
        and r_str = string_of_value r in
        Printf.sprintf "%s = sub %s, %s" t_str l_str r_str
    | Mul (t, l, r) ->
        let t_str = string_of_tmp t
        and l_str = string_of_value l
        and r_str = string_of_value r in
        Printf.sprintf "%s = mul %s, %s" t_str l_str r_str
    | Div (t, l, r) ->
        let t_str = string_of_tmp t
        and l_str = string_of_value l
        and r_str = string_of_value r in
        Printf.sprintf "%s = div %s, %s" t_str l_str r_str
    | Mod (t, l, r) ->
        let t_str = string_of_tmp t
        and l_str = string_of_value l
        and r_str = string_of_value r in
        Printf.sprintf "%s = mod %s, %s" t_str l_str r_str
    | Ceq (t, l, r) ->
        let t_str = string_of_tmp t
        and l_str = string_of_value l
        and r_str = string_of_value r in
        Printf.sprintf "%s = ceq %s, %s" t_str l_str r_str
  in
  Printf.sprintf "  %s\n" inst

and string_of_terminator (t : terminator) : string =
  let inst =
    match t with
    | Jmp l -> string_of_label l |> Printf.sprintf "jmp %s"
    | Jnz (v, then_label, else_label) ->
        let v_str = string_of_value v
        and then_str = string_of_label then_label
        and else_str = string_of_label else_label in
        Printf.sprintf "jnz %s, %s, %s" v_str then_str else_str
    | Halt -> "hatl"
  in
  Printf.sprintf "  %s\n" inst

and string_of_block (b : block) : string =
  let insts = List.map string_of_instruction b.instrs |> String.concat ""
  and terms = string_of_terminator b.term
  and label_str = string_of_label b.label in
  Printf.sprintf "%s:\n%s%s" label_str insts terms

and string_of_cfg (g : cfg) : string =
  List.map string_of_block g |> String.concat ""

let rec flatten_expr (exp : Ast.expr) (cur_block : block) (ctx : Context.t) :
    value * block * Context.t =
  match exp.kind with
  | Ast.Integer i -> (Integer i, cur_block, ctx)
  | Ast.Var v ->
      let t = Context.get_var_tmp v ctx in
      (Temp t, cur_block, ctx)
  | Ast.BinExpr (op, lhs, rhs) -> (
      let lval, cur_block, ctx = flatten_expr lhs cur_block ctx in
      let rval, cur_block, ctx = flatten_expr rhs cur_block ctx in
      let t, ctx = Context.gen_tmp ctx in
      let tmp_val = Temp t in
      match op with
      | Ast.Plus ->
          (tmp_val, cfg_add_insts [ Add (t, lval, rval) ] cur_block, ctx)
      | Ast.Minus ->
          (tmp_val, cfg_add_insts [ Sub (t, lval, rval) ] cur_block, ctx)
      | Ast.Mul ->
          (tmp_val, cfg_add_insts [ Mul (t, lval, rval) ] cur_block, ctx)
      | Ast.Div ->
          (tmp_val, cfg_add_insts [ Div (t, lval, rval) ] cur_block, ctx)
      | Ast.Mod ->
          (tmp_val, cfg_add_insts [ Mod (t, lval, rval) ] cur_block, ctx))

and flatten_stmt_list (stmts : Ast.stmt list) (cur_block : block) (cfg : cfg)
    (ctx : Context.t) : block * cfg * Context.t =
  let rec aux stmts cur_block cfg ctx =
    match stmts with
    | [] -> (cur_block, cfg, ctx)
    | s :: tl ->
        let cur_block, cfg, ctx = flatten_stmt s cur_block cfg ctx in
        aux tl cur_block cfg ctx
  in
  aux stmts cur_block cfg ctx

and flatten_if (cond : Ast.expr) (then_stmts : Ast.stmt list)
    (cur_block : block) (cfg : cfg) (ctx : Context.t) : block * cfg * Context.t
    =
  let cond_value, cur_block, ctx = flatten_expr cond cur_block ctx in
  let cmp_t, ctx = Context.gen_tmp ctx in
  let cur_block =
    cfg_add_insts [ Ceq (cmp_t, cond_value, Integer 0L) ] cur_block
  in
  let then_label, ctx = Context.gen_label ctx in
  let end_label, ctx = Context.gen_label ctx in
  let cfg =
    cfg_add_block cur_block (Jnz (Temp cmp_t, end_label, then_label)) cfg
  in
  let then_block = cfg_make_block then_label in
  let ctx = Context.push_scope ctx in
  let then_block, cfg, ctx = flatten_stmt_list then_stmts then_block cfg ctx in
  let ctx = Context.pop_scope ctx in
  let cfg = cfg_add_block then_block (Jmp end_label) cfg in
  let end_block = cfg_make_block end_label in
  (end_block, cfg, ctx)

and flatten_if_else (cond : Ast.expr) (then_stmts : Ast.stmt list)
    (else_stmts : Ast.stmt list) (cur_block : block) (cfg : cfg)
    (ctx : Context.t) : block * cfg * Context.t =
  let cond_value, cur_block, ctx = flatten_expr cond cur_block ctx in
  let cmp_t, ctx = Context.gen_tmp ctx in
  let cur_block =
    cfg_add_insts [ Ceq (cmp_t, cond_value, Integer 0L) ] cur_block
  in
  let then_label, ctx = Context.gen_label ctx in
  let else_label, ctx = Context.gen_label ctx in
  let end_label, ctx = Context.gen_label ctx in
  let cfg =
    cfg_add_block cur_block (Jnz (Temp cmp_t, else_label, then_label)) cfg
  in
  let then_block = cfg_make_block then_label
  and else_block = cfg_make_block else_label in
  let ctx = Context.push_scope ctx in
  let then_block, cfg, ctx = flatten_stmt_list then_stmts then_block cfg ctx in
  let ctx = Context.pop_scope ctx in
  let cfg = cfg_add_block then_block (Jmp end_label) cfg in
  let ctx = Context.push_scope ctx in
  let else_block, cfg, ctx = flatten_stmt_list else_stmts else_block cfg ctx in
  let ctx = Context.pop_scope ctx in
  let cfg = cfg_add_block else_block (Jmp end_label) cfg in
  let end_block = cfg_make_block end_label in
  (end_block, cfg, ctx)

and flatten_stmt (stmt : Ast.stmt) (cur_block : block) (cfg : cfg)
    (ctx : Context.t) : block * cfg * Context.t =
  match stmt.kind with
  | Ast.Print exp ->
      let value, cur_block, ctx = flatten_expr exp cur_block ctx in
      (cfg_add_insts [ Print value ] cur_block, cfg, ctx)
  | Ast.VarDecl (v, exp) ->
      let value, cur_block, ctx = flatten_expr exp cur_block ctx in
      let t, ctx = Context.gen_tmp ctx in
      let ctx = Context.add_var_tmp v t ctx in
      (cfg_add_insts [ Copy (t, value) ] cur_block, cfg, ctx)
  | Ast.If (cond, then_stmts, else_stmts_opt) -> (
      match else_stmts_opt with
      | Some else_stmts ->
          flatten_if_else cond then_stmts else_stmts cur_block cfg ctx
      | None -> flatten_if cond then_stmts cur_block cfg ctx)
  | Ast.For body ->
      let body_label, ctx = Context.gen_label ctx in
      let cfg = cfg_add_block cur_block (Jmp body_label) cfg in
      let body_block = cfg_make_block body_label in
      let ctx = Context.push_scope ctx in
      let body_block, cfg, ctx = flatten_stmt_list body body_block cfg ctx in
      let ctx = Context.pop_scope ctx in
      let cfg = cfg_add_block body_block (Jmp body_label) cfg in
      let end_label, ctx = Context.gen_label ctx in
      let cur_block = cfg_make_block end_label in
      (cur_block, cfg, ctx)

and flatten (tree : Ast.t) : cfg =
  let ctx = Context.empty in
  let start_label, ctx = Context.gen_label ctx in
  let cfg = [] in
  let start_block = cfg_make_block start_label in
  let final_block, cfg, ctx = flatten_stmt_list tree start_block cfg ctx in
  cfg_add_block final_block Halt cfg
