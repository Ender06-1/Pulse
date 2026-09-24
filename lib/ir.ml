type value =
  | Temp of int
  | Integer of int64

and instruction =
  | Print of value
  | Copy of int * value
  | Add of int * value * value
  | Sub of int * value * value
  | Mul of int * value * value
  | Div of int * value * value
  | Mod of int * value * value
  | Ceq of int * value * value
  | Cne of int * value * value
  | Cgt of int * value * value
  | Clt of int * value * value
  | Cge of int * value * value
  | Cle of int * value * value
  | Call of int option * string * value list

and terminator =
  | Jmp of int
  | Jnz of value * int * int
  | Ret of value option
  | Halt

and block = {
  label : int;
  instrs : instruction list;
  term : terminator;
}

and fn = {
  name : string;
  params : int list;
  body : block list;
}

and t = fn list

let rec string_of_label (l : int) : string = Printf.sprintf "@%d" l
and string_of_tmp (t : int) : string = Printf.sprintf "%%%d" t

and string_of_value (v : value) : string =
  match v with Temp t -> string_of_tmp t | Integer i -> Int64.to_string i

and string_of_instruction (i : instruction) : string =
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
  | Cne (t, l, r) ->
      let t_str = string_of_tmp t
      and l_str = string_of_value l
      and r_str = string_of_value r in
      Printf.sprintf "%s = cne %s, %s" t_str l_str r_str
  | Cgt (t, l, r) ->
      let t_str = string_of_tmp t
      and l_str = string_of_value l
      and r_str = string_of_value r in
      Printf.sprintf "%s = cgt %s, %s" t_str l_str r_str
  | Clt (t, l, r) ->
      let t_str = string_of_tmp t
      and l_str = string_of_value l
      and r_str = string_of_value r in
      Printf.sprintf "%s = clt %s, %s" t_str l_str r_str
  | Cge (t, l, r) ->
      let t_str = string_of_tmp t
      and l_str = string_of_value l
      and r_str = string_of_value r in
      Printf.sprintf "%s = cge %s, %s" t_str l_str r_str
  | Cle (t, l, r) ->
      let t_str = string_of_tmp t
      and l_str = string_of_value l
      and r_str = string_of_value r in
      Printf.sprintf "%s = cle %s, %s" t_str l_str r_str
  | Call (t_opt, fn, params) -> (
      let param_str = List.map string_of_value params |> String.concat ", " in
      match t_opt with
      | None -> Printf.sprintf "call %s(%s)" fn param_str
      | Some t ->
          let t_str = string_of_tmp t in
          Printf.sprintf "%s = call %s(%s)" t_str fn param_str)

and string_of_terminator (t : terminator) : string =
  match t with
  | Jmp l -> string_of_label l |> Printf.sprintf "jmp %s"
  | Jnz (v, then_label, else_label) ->
      let v_str = string_of_value v
      and then_str = string_of_label then_label
      and else_str = string_of_label else_label in
      Printf.sprintf "jnz %s, %s, %s" v_str then_str else_str
  | Ret v_opt -> (
      match v_opt with
      | None -> "ret"
      | Some v ->
          let v_str = string_of_value v in
          Printf.sprintf "ret %s" v_str)
  | Halt -> "hatl"

and string_of_block (b : block) : string =
  let insts =
    List.map string_of_instruction b.instrs
    |> List.map (String.cat "  ")
    |> String.concat "\n"
  and term_str = string_of_terminator b.term
  and label_str = string_of_label b.label in
  Printf.sprintf "%s:\n%s\n  %s\n" label_str insts term_str

and string_of_block_list (bl : block list) : string =
  List.map string_of_block bl |> String.concat ""

and string_of_fn (f : fn) : string =
  let param_str = List.map string_of_int f.params |> String.concat ", "
  and body_str = string_of_block_list f.body in
  Printf.sprintf "fn %s(%s) {\n%s}" f.name param_str body_str

and string_of_fn_list (fl : fn list) : string =
  List.map string_of_fn fl |> String.concat "\n\n"

and to_string (program : t) : string = string_of_fn_list program

let ( let* ) = Result.bind

module Context = struct
  module StringMap = Map.Make (String)

  type t = {
    tmp_acc : int;
    label_acc : int;
    var_tmp_scopes : int StringMap.t list;
    label_stack : (int * int) list;
  }

  let empty : t =
    {
      tmp_acc = 0;
      label_acc = 0;
      var_tmp_scopes = [ StringMap.empty ];
      label_stack = [];
    }

  and to_string (ctx : t) : string =
    let label_stack_str =
      List.map (fun (l1, l2) -> Printf.sprintf "(%d, %d)" l1 l2) ctx.label_stack
      |> String.concat "; "
    in
    Printf.sprintf "Context{tmp_acc = %d; label_acc = %d; label_stack = [%s]}"
      ctx.tmp_acc ctx.label_acc label_stack_str

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

  and push_labels (l1 : int) (l2 : int) (ctx : t) : t =
    { ctx with label_stack = (l1, l2) :: ctx.label_stack }

  and get_labels (ctx : t) : (int * int) option =
    match ctx.label_stack with [] -> None | (l1, l2) :: tl -> Some (l1, l2)

  and pop_labels (ctx : t) : t =
    assert (not (List.is_empty ctx.label_stack));
    let label_stack = List.tl ctx.label_stack in
    { ctx with label_stack }
end

let cfg_add_insts (instrs : instruction list) (b : block) : block =
  { b with instrs = b.instrs @ instrs }

and cfg_add_block (b : block) (term : terminator) (cfg : block list) :
    block list =
  let b = { b with term } in
  cfg @ [ b ]

and cfg_make_block (label : int) : block =
  { label; instrs = []; term = Jmp (-1) }

let rec flatten_expr (exp : Typed_ast.expr) (cur_block : block)
    (ctx : Context.t) : value * block * Context.t =
  match exp.kind with
  | Typed_ast.Integer i -> (Integer i, cur_block, ctx)
  | Typed_ast.Var v ->
      let t = Context.get_var_tmp v ctx in
      (Temp t, cur_block, ctx)
  | Typed_ast.BinExpr (op, lhs, rhs) -> (
      let lval, cur_block, ctx = flatten_expr lhs cur_block ctx in
      let rval, cur_block, ctx = flatten_expr rhs cur_block ctx in
      let t, ctx = Context.gen_tmp ctx in
      let tmp_val = Temp t in
      match op with
      | Typed_ast.Plus ->
          (tmp_val, cfg_add_insts [ Add (t, lval, rval) ] cur_block, ctx)
      | Typed_ast.Minus ->
          (tmp_val, cfg_add_insts [ Sub (t, lval, rval) ] cur_block, ctx)
      | Typed_ast.Mul ->
          (tmp_val, cfg_add_insts [ Mul (t, lval, rval) ] cur_block, ctx)
      | Typed_ast.Div ->
          (tmp_val, cfg_add_insts [ Div (t, lval, rval) ] cur_block, ctx)
      | Typed_ast.Mod ->
          (tmp_val, cfg_add_insts [ Mod (t, lval, rval) ] cur_block, ctx)
      | Typed_ast.Eq ->
          (tmp_val, cfg_add_insts [ Ceq (t, lval, rval) ] cur_block, ctx)
      | Typed_ast.Neq ->
          (tmp_val, cfg_add_insts [ Cne (t, lval, rval) ] cur_block, ctx)
      | Typed_ast.Gt ->
          (tmp_val, cfg_add_insts [ Cgt (t, lval, rval) ] cur_block, ctx)
      | Typed_ast.Lt ->
          (tmp_val, cfg_add_insts [ Clt (t, lval, rval) ] cur_block, ctx)
      | Typed_ast.Ge ->
          (tmp_val, cfg_add_insts [ Cge (t, lval, rval) ] cur_block, ctx)
      | Typed_ast.Le ->
          (tmp_val, cfg_add_insts [ Cle (t, lval, rval) ] cur_block, ctx))
  | Typed_ast.Call (fn, params) -> (
      let t, ctx = Context.gen_tmp ctx in
      let t_val = Temp t in
      let rec aux params cur_block acc ctx =
        match params with
        | [] -> (List.rev acc, cur_block, ctx)
        | p :: tl ->
            let p, cur_block, ctx = flatten_expr p cur_block ctx in
            aux tl cur_block (p :: acc) ctx
      in
      let params, cur_block, ctx = aux params cur_block [] ctx in
      match exp.typ with
      | Typed_ast.U64 ->
          (t_val, cfg_add_insts [ Call (Some t, fn, params) ] cur_block, ctx)
      | Typed_ast.Void ->
          (t_val, cfg_add_insts [ Call (None, fn, params) ] cur_block, ctx))

and flatten_stmt_list (stmts : Typed_ast.stmt list) (cur_block : block)
    (cfg : block list) (ctx : Context.t) :
    (block * block list * Context.t, Report.t) result =
  let rec aux stmts cur_block cfg ctx =
    match stmts with
    | [] -> Ok (cur_block, cfg, ctx)
    | s :: tl ->
        let* cur_block, cfg, ctx = flatten_stmt s cur_block cfg ctx in
        aux tl cur_block cfg ctx
  in
  aux stmts cur_block cfg ctx

and flatten_if (cond : Typed_ast.expr) (then_stmts : Typed_ast.stmt list)
    (cur_block : block) (cfg : block list) (ctx : Context.t) :
    (block * block list * Context.t, Report.t) result =
  let cond_value, cur_block, ctx = flatten_expr cond cur_block ctx in
  let then_label, ctx = Context.gen_label ctx in
  let end_label, ctx = Context.gen_label ctx in
  let cfg =
    cfg_add_block cur_block (Jnz (cond_value, then_label, end_label)) cfg
  in
  let then_block = cfg_make_block then_label in
  let ctx = Context.push_scope ctx in
  let* then_block, cfg, ctx = flatten_stmt_list then_stmts then_block cfg ctx in
  let ctx = Context.pop_scope ctx in
  let cfg = cfg_add_block then_block (Jmp end_label) cfg in
  let end_block = cfg_make_block end_label in
  Ok (end_block, cfg, ctx)

and flatten_if_else (cond : Typed_ast.expr) (then_stmts : Typed_ast.stmt list)
    (else_stmts : Typed_ast.stmt list) (cur_block : block) (cfg : block list)
    (ctx : Context.t) : (block * block list * Context.t, Report.t) result =
  let cond_value, cur_block, ctx = flatten_expr cond cur_block ctx in
  let then_label, ctx = Context.gen_label ctx in
  let else_label, ctx = Context.gen_label ctx in
  let end_label, ctx = Context.gen_label ctx in
  let cfg =
    cfg_add_block cur_block (Jnz (cond_value, then_label, else_label)) cfg
  in
  let then_block = cfg_make_block then_label
  and else_block = cfg_make_block else_label in
  let ctx = Context.push_scope ctx in
  let* then_block, cfg, ctx = flatten_stmt_list then_stmts then_block cfg ctx in
  let ctx = Context.pop_scope ctx in
  let cfg = cfg_add_block then_block (Jmp end_label) cfg in
  let ctx = Context.push_scope ctx in
  let* else_block, cfg, ctx = flatten_stmt_list else_stmts else_block cfg ctx in
  let ctx = Context.pop_scope ctx in
  let cfg = cfg_add_block else_block (Jmp end_label) cfg in
  let end_block = cfg_make_block end_label in
  Ok (end_block, cfg, ctx)

and flatten_for (cond : Typed_ast.expr) (body_stmts : Typed_ast.stmt list)
    (cur_block : block) (cfg : block list) (ctx : Context.t) :
    (block * block list * Context.t, Report.t) result =
  let cond_label, ctx = Context.gen_label ctx in
  let body_label, ctx = Context.gen_label ctx in
  let end_label, ctx = Context.gen_label ctx in
  let cfg = cfg_add_block cur_block (Jmp cond_label) cfg in
  let cur_block = cfg_make_block cond_label in
  let cond_value, cur_block, ctx = flatten_expr cond cur_block ctx in
  let cfg =
    cfg_add_block cur_block (Jnz (cond_value, body_label, end_label)) cfg
  in
  let cur_block = cfg_make_block body_label in
  let ctx = Context.push_labels body_label end_label ctx in
  let* cur_block, cfg, ctx = flatten_stmt_list body_stmts cur_block cfg ctx in
  let ctx = Context.pop_labels ctx in
  let cfg = cfg_add_block cur_block (Jmp cond_label) cfg in
  let cur_block = cfg_make_block end_label in
  Ok (cur_block, cfg, ctx)

and flatten_stmt (stmt : Typed_ast.stmt) (cur_block : block) (cfg : block list)
    (ctx : Context.t) : (block * block list * Context.t, Report.t) result =
  match stmt.kind with
  | Typed_ast.Print exp ->
      let value, cur_block, ctx = flatten_expr exp cur_block ctx in
      Ok (cfg_add_insts [ Print value ] cur_block, cfg, ctx)
  | Typed_ast.VarDecl (v, exp) ->
      let value, cur_block, ctx = flatten_expr exp cur_block ctx in
      let t, ctx = Context.gen_tmp ctx in
      let ctx = Context.add_var_tmp v t ctx in
      Ok (cfg_add_insts [ Copy (t, value) ] cur_block, cfg, ctx)
  | Typed_ast.If (cond, then_stmts, else_stmts_opt) -> (
      match else_stmts_opt with
      | Some else_stmts ->
          flatten_if_else cond then_stmts else_stmts cur_block cfg ctx
      | None -> flatten_if cond then_stmts cur_block cfg ctx)
  | Typed_ast.For (cond, body) -> flatten_for cond body cur_block cfg ctx
  | Typed_ast.Assign (i, e) ->
      let t = Context.get_var_tmp i ctx in
      let e_value, cur_block, ctx = flatten_expr e cur_block ctx in
      let cur_block = cfg_add_insts [ Copy (t, e_value) ] cur_block in
      Ok (cur_block, cfg, ctx)
  | Typed_ast.Break -> (
      match Context.get_labels ctx with
      | None ->
          let msg = "break outside of loop" in
          let report = Report.make stmt.loc msg in
          Error report
      | Some (_, label) ->
          let cfg = cfg_add_block cur_block (Jmp label) cfg in
          let next_label, ctx = Context.gen_label ctx in
          let cur_block = cfg_make_block next_label in
          Ok (cur_block, cfg, ctx))
  | Typed_ast.Continue -> (
      match Context.get_labels ctx with
      | None ->
          let msg = "continue outside of loop" in
          let report = Report.make stmt.loc msg in
          Error report
      | Some (label, _) ->
          let cfg = cfg_add_block cur_block (Jmp label) cfg in
          let next_label, ctx = Context.gen_label ctx in
          let cur_block = cfg_make_block next_label in
          Ok (cur_block, cfg, ctx))
  | Typed_ast.Return e_opt -> (
      match e_opt with
      | None ->
          let cfg = cfg_add_block cur_block (Ret None) cfg in
          let label, ctx = Context.gen_label ctx in
          let cur_block = cfg_make_block label in
          Ok (cur_block, cfg, ctx)
      | Some e ->
          let t_val, cur_block, ctx = flatten_expr e cur_block ctx in
          let cfg = cfg_add_block cur_block (Ret (Some t_val)) cfg in
          let label, ctx = Context.gen_label ctx in
          let cur_block = cfg_make_block label in
          Ok (cur_block, cfg, ctx))
  | Typed_ast.Expr e ->
      let _, cur_block, ctx = flatten_expr e cur_block ctx in
      Ok (cur_block, cfg, ctx)

and flatten_fn_def (fn : Typed_ast.fn_def) (ctx : Context.t) :
    (fn * Context.t, Report.t) result =
  let ctx = Context.push_scope ctx in
  let rec aux params acc ctx =
    match params with
    | [] -> (List.rev acc, ctx)
    | (p, _) :: tl ->
        let t, ctx = Context.gen_tmp ctx in
        let ctx = Context.add_var_tmp p t ctx in
        aux tl (t :: acc) ctx
  in
  let params, ctx = aux fn.typ.params [] ctx in
  let start_label, ctx = Context.gen_label ctx in
  let start_block = cfg_make_block start_label in
  let* last_block, body, ctx = flatten_stmt_list fn.body start_block [] ctx in
  let body = cfg_add_block last_block (Ret None) body in
  let ctx = Context.pop_scope ctx in
  let f : fn = { name = fn.name; params; body } in
  Ok (f, ctx)

and flatten_fn_def_list (fl : Typed_ast.fn_def list) (ctx : Context.t) :
    (fn list * Context.t, Report.t) result =
  let rec aux fl acc ctx =
    match fl with
    | [] -> Ok (List.rev acc, ctx)
    | fn :: tl ->
        let* f, ctx = flatten_fn_def fn ctx in
        aux tl (f :: acc) ctx
  in
  aux fl [] ctx

and flatten (tree : Typed_ast.t) : (t, Report.t) result =
  let ctx = Context.empty in
  let* ir, _ = flatten_fn_def_list tree ctx in
  Ok ir
