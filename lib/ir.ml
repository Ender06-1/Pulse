module Context = struct
  type t = {
    tmp_acc : int;
    label_acc : int;
  }

  let empty : t = { tmp_acc = 0; label_acc = 0 }

  and gen_tmp (ctx : t) : string * t =
    (string_of_int ctx.tmp_acc, { ctx with tmp_acc = ctx.tmp_acc + 1 })

  and gen_label (ctx : t) : int * t =
    (ctx.label_acc, { ctx with label_acc = ctx.label_acc + 1 })
end

type variable =
  | Var of string
  | Tmp of string

type value =
  | Variable of variable
  | Integer of int64

type instruction =
  | Print of value
  | Copy of variable * value
  | Add of variable * value * value
  | Sub of variable * value * value
  | Mul of variable * value * value
  | Div of variable * value * value
  | Mod of variable * value * value
  | Cmp of variable * value * value

type label = int

type terminator =
  | Jmp of label
  | Jnz of value * label * label
  | Halt

type block = {
  label : label;
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

and cfg_make_block (label : label) : block =
  { label; instrs = []; term = Jmp (-1) }

let rec string_of_label (l : label) : string = Printf.sprintf "@%d" l

and string_of_variable (v : variable) : string =
  match v with Var v -> v | Tmp t -> Printf.sprintf "%%%s" t

and string_of_value (v : value) : string =
  match v with
  | Variable v -> string_of_variable v
  | Integer i -> Int64.to_string i

and string_of_instruction (i : instruction) : string =
  match i with
  | Print v ->
      let vals = string_of_value v in
      Printf.sprintf "print %s" vals
  | Copy (var, value) ->
      let var_str = string_of_variable var
      and value_str = string_of_value value in
      Printf.sprintf "%s = copy %s" var_str value_str
  | Add (v, l, r) ->
      let v_str = string_of_variable v
      and l_str = string_of_value l
      and r_str = string_of_value r in
      Printf.sprintf "%s = add %s, %s" v_str l_str r_str
  | Sub (v, l, r) ->
      let v_str = string_of_variable v
      and l_str = string_of_value l
      and r_str = string_of_value r in
      Printf.sprintf "%s = sub %s, %s" v_str l_str r_str
  | Mul (v, l, r) ->
      let v_str = string_of_variable v
      and l_str = string_of_value l
      and r_str = string_of_value r in
      Printf.sprintf "%s = mul %s, %s" v_str l_str r_str
  | Div (v, l, r) ->
      let v_str = string_of_variable v
      and l_str = string_of_value l
      and r_str = string_of_value r in
      Printf.sprintf "%s = div %s, %s" v_str l_str r_str
  | Mod (v, l, r) ->
      let v_str = string_of_variable v
      and l_str = string_of_value l
      and r_str = string_of_value r in
      Printf.sprintf "%s = mod %s, %s" v_str l_str r_str
  | Cmp (v, l, r) ->
      let v_str = string_of_variable v
      and l_str = string_of_value l
      and r_str = string_of_value r in
      Printf.sprintf "%s = cmp %s, %s" v_str l_str r_str

and string_of_terminator (t : terminator) : string =
  match t with
  | Jmp l -> string_of_label l |> Printf.sprintf "jmp %s"
  | Jnz (v, then_label, else_label) ->
      let v_str = string_of_value v
      and then_str = string_of_label then_label
      and else_str = string_of_label else_label in
      Printf.sprintf "jnz %s, %s, %s" v_str then_str else_str
  | Halt -> "hatl"

and string_of_block (b : block) : string =
  let insts =
    List.map (fun i -> string_of_instruction i |> String.cat "  ") b.instrs
    |> String.concat "\n"
  and terms = string_of_terminator b.term
  and label_str = string_of_label b.label in
  Printf.sprintf "%s:\n%s\n  %s" label_str insts terms

and string_of_cfg (g : cfg) : string =
  List.map string_of_block g |> String.concat "\n"

let rec flatten_expr (exp : Ast.expr) (cur_block : block) (ctx : Context.t) :
    value * block * Context.t =
  match exp with
  | Integer i -> (Integer i, cur_block, ctx)
  | Var v -> (Variable (Var v), cur_block, ctx)
  | BinExpr (op, lhs, rhs) -> (
      let lval, cur_block, ctx = flatten_expr lhs cur_block ctx in
      let rval, cur_block, ctx = flatten_expr rhs cur_block ctx in
      let t, ctx = Context.gen_tmp ctx in
      let tmp = Tmp t in
      let tmp_val = Variable tmp in
      match op with
      | Plus -> (tmp_val, cfg_add_insts [ Add (tmp, lval, rval) ] cur_block, ctx)
      | Minus ->
          (tmp_val, cfg_add_insts [ Sub (tmp, lval, rval) ] cur_block, ctx)
      | Mul -> (tmp_val, cfg_add_insts [ Mul (tmp, lval, rval) ] cur_block, ctx)
      | Div -> (tmp_val, cfg_add_insts [ Div (tmp, lval, rval) ] cur_block, ctx)
      | Mod -> (tmp_val, cfg_add_insts [ Mod (tmp, lval, rval) ] cur_block, ctx)
      )

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
  let cmp_tmp, ctx = Context.gen_tmp ctx in
  let cmp_var = Tmp cmp_tmp in
  let cur_block =
    cfg_add_insts [ Cmp (cmp_var, cond_value, Integer 0L) ] cur_block
  in
  let then_label, ctx = Context.gen_label ctx in
  let end_label, ctx = Context.gen_label ctx in
  let cfg =
    cfg_add_block cur_block (Jnz (Variable cmp_var, end_label, then_label)) cfg
  in
  let then_block = cfg_make_block then_label in
  let then_block, cfg, ctx = flatten_stmt_list then_stmts then_block cfg ctx in
  let cfg = cfg_add_block then_block (Jmp end_label) cfg in
  let end_block = cfg_make_block end_label in
  (end_block, cfg, ctx)

and flatten_if_else (cond : Ast.expr) (then_stmts : Ast.stmt list)
    (else_stmts : Ast.stmt list) (cur_block : block) (cfg : cfg)
    (ctx : Context.t) : block * cfg * Context.t =
  let cond_value, cur_block, ctx = flatten_expr cond cur_block ctx in
  let cmp_tmp, ctx = Context.gen_tmp ctx in
  let cmp_var = Tmp cmp_tmp in
  let cur_block =
    cfg_add_insts [ Cmp (cmp_var, cond_value, Integer 0L) ] cur_block
  in
  let then_label, ctx = Context.gen_label ctx in
  let else_label, ctx = Context.gen_label ctx in
  let end_label, ctx = Context.gen_label ctx in
  let cfg =
    cfg_add_block cur_block (Jnz (Variable cmp_var, else_label, then_label)) cfg
  in
  let then_block = cfg_make_block then_label
  and else_block = cfg_make_block else_label in
  let then_block, cfg, ctx = flatten_stmt_list then_stmts then_block cfg ctx in
  let cfg = cfg_add_block then_block (Jmp end_label) cfg in
  let else_block, cfg, ctx = flatten_stmt_list else_stmts else_block cfg ctx in
  let cfg = cfg_add_block else_block (Jmp end_label) cfg in
  let end_block = cfg_make_block end_label in
  (end_block, cfg, ctx)

and flatten_stmt (stmt : Ast.stmt) (cur_block : block) (cfg : cfg)
    (ctx : Context.t) : block * cfg * Context.t =
  match stmt with
  | Print exp ->
      let value, cur_block, ctx = flatten_expr exp cur_block ctx in
      (cfg_add_insts [ Print value ] cur_block, cfg, ctx)
  | VarDecl (v, exp) ->
      let value, cur_block, ctx = flatten_expr exp cur_block ctx in
      (cfg_add_insts [ Copy (Var v, value) ] cur_block, cfg, ctx)
  | If (cond, then_stmts, else_stmts_opt) -> (
      match else_stmts_opt with
      | Some else_stmts ->
          flatten_if_else cond then_stmts else_stmts cur_block cfg ctx
      | None -> flatten_if cond then_stmts cur_block cfg ctx)

and flatten (tree : Ast.t) : cfg =
  let ctx = Context.empty in
  let start_label, ctx = Context.gen_label ctx in
  let cfg = [] in
  let start_block = cfg_make_block start_label in
  let final_block, cfg, ctx = flatten_stmt_list tree start_block cfg ctx in
  cfg_add_block final_block Halt cfg
