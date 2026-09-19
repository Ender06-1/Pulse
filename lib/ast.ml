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

and stmt = {
  kind : stmt_kind;
  loc : Location.t;
}

and t = stmt list

let rec string_of_binop (b : bin_op) : string =
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

and string_of_expr_kind (e : expr_kind) : string =
  match e with
  | Integer i -> Int64.to_string i
  | Var v -> v
  | BinExpr (op, l, r) ->
      let ls = string_of_expr_kind l.kind
      and rs = string_of_expr_kind r.kind
      and ops = string_of_binop op in
      Printf.sprintf "(BinExpr (%s %s %s))" ops ls rs

and string_of_stmt_kind (s : stmt_kind) : string =
  match s with
  | Print e ->
      let exps = string_of_expr_kind e.kind in
      Printf.sprintf "(Print %s)" exps
  | VarDecl (v, e) ->
      let exps = string_of_expr_kind e.kind in
      Printf.sprintf "(VarDecl (%s, %s))" v exps
  | If (cond, then_block, else_block_opt) ->
      let conds = string_of_expr_kind cond.kind
      and thes =
        List.map (fun (s : stmt) -> string_of_stmt_kind s.kind) then_block
        |> String.concat " "
      and elss =
        Option.map
          (List.map (fun (s : stmt) -> string_of_stmt_kind s.kind))
          else_block_opt
        |> Option.fold ~none:"" ~some:(String.concat " ")
      in
      Printf.sprintf "(If (%s (%s) (%s)))" conds thes elss
  | For (cond, body) ->
      let cond_str = string_of_expr_kind cond.kind
      and body_str =
        List.map (fun (s : stmt) -> string_of_stmt_kind s.kind) body
        |> String.concat " "
      in
      Printf.sprintf "(For (%s %s))" cond_str body_str
  | Assign (i, e) ->
      let e_str = string_of_expr_kind e.kind in
      Printf.sprintf "(Assign (%s %s))" i e_str
  | Break -> "(Break)"
  | Continue -> "(Continue)"

and to_string (tree : t) : string =
  List.map (fun (s : stmt) -> string_of_stmt_kind s.kind) tree
  |> String.concat "\n"

module CheckVar = struct
  module Context = struct
    module StringSet = Set.Make (String)

    type t = { var_scopes : StringSet.t list }

    let empty : t = { var_scopes = [ StringSet.empty ] }

    let is_var_defined (name : string) (ctx : t) : bool =
      let rec aux scopes =
        match scopes with
        | [] -> false
        | s :: tl -> if StringSet.mem name s then true else aux tl
      in
      aux ctx.var_scopes

    and push_scope (ctx : t) : t =
      { var_scopes = StringSet.empty :: ctx.var_scopes }

    and pop_scope (ctx : t) : t =
      assert (List.length ctx.var_scopes > 0);
      { var_scopes = List.tl ctx.var_scopes }

    and add_var (name : string) (ctx : t) : t =
      assert (List.length ctx.var_scopes > 0);
      let s = List.hd ctx.var_scopes and tl = List.tl ctx.var_scopes in
      { var_scopes = StringSet.add name s :: tl }
  end

  let ( let* ) = Result.bind

  let check (tree : t) : (unit, Report.t) result =
    let rec check_var_expr (exp : expr) (ctx : Context.t) :
        (unit, Report.t) result =
      match exp.kind with
      | Integer _ -> Ok ()
      | Var v ->
          if Context.is_var_defined v ctx then Ok ()
          else
            let msg = Printf.sprintf "unbound variable '%s'" v in
            let report = Report.make exp.loc msg in
            Error report
      | BinExpr (_, l, r) ->
          let* _ = check_var_expr l ctx in
          check_var_expr r ctx
    and check_var_stmt_list (stmts : stmt list) (ctx : Context.t) :
        (unit * Context.t, Report.t) result =
      let rec aux stmts ctx =
        match stmts with
        | [] -> Ok ((), ctx)
        | s :: tl ->
            let* _, ctx = check_var_stmt s ctx in
            aux tl ctx
      in
      aux stmts ctx
    and check_var_stmt (stmt : stmt) (ctx : Context.t) :
        (unit * Context.t, Report.t) result =
      match stmt.kind with
      | Print e ->
          let* _ = check_var_expr e ctx in
          Ok ((), ctx)
      | VarDecl (v, e) ->
          let ctx = Context.add_var v ctx in
          let* _ = check_var_expr e ctx in
          Ok ((), ctx)
      | If (cond, then_block, else_block_opt) -> (
          let* _ = check_var_expr cond ctx in
          let ctx = Context.push_scope ctx in
          let* _, ctx = check_var_stmt_list then_block ctx in
          let ctx = Context.pop_scope ctx in
          match else_block_opt with
          | Some else_block ->
              let ctx = Context.push_scope ctx in
              let* _, ctx = check_var_stmt_list else_block ctx in
              let ctx = Context.pop_scope ctx in
              Ok ((), ctx)
          | None -> Ok ((), ctx))
      | For (cond, body) ->
          let* _ = check_var_expr cond ctx in
          let ctx = Context.push_scope ctx in
          let* _, ctx = check_var_stmt_list body ctx in
          let ctx = Context.pop_scope ctx in
          Ok ((), ctx)
      | Assign (i, e) ->
          let* _ = check_var_expr e ctx in
          if Context.is_var_defined i ctx then Ok ((), ctx)
          else
            let msg = Printf.sprintf "unbound variable '%s'" i in
            let report = Report.make stmt.loc msg in
            Error report
      | Break | Continue -> Ok ((), ctx)
    in
    let* _ = check_var_stmt_list tree Context.empty in
    Ok ()
end
