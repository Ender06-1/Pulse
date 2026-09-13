let ( let* ) = Result.bind

module StringMap = Map.Make (String)

type var_map = string StringMap.t
type value = Var of string | Integer of int64

type instruction =
  | Print of value
  | Copy of string * value
  | Add of string * value * value
  | Sub of string * value * value
  | Mul of string * value * value
  | Div of string * value * value
  | Mod of string * value * value

type var_generator = { acc : int }

let gen_var (generator : var_generator) : string * var_generator =
  (Printf.sprintf "v%d" generator.acc, { acc = generator.acc + 1 })

let rec flatten_expr (expr : Ast.expr) (g : var_generator) (tmp_map : var_map) :
    (value * instruction list * var_generator * var_map, string) result =
  match expr with
  | Integer i ->
      let var, g = gen_var g in
      Ok (Integer i, [], g, tmp_map)
  | BinExpr (op, l, r) -> (
      let* vlhs, lhs, g, v_map = flatten_expr l g tmp_map in
      let* vrhs, rhs, g, v_map = flatten_expr r g v_map in
      let v, g = gen_var g in
      match op with
      | Plus ->
          Ok
            ( Var v,
              List.append (List.append lhs rhs) [ Add (v, vlhs, vrhs) ],
              g,
              v_map )
      | Minus ->
          Ok
            ( Var v,
              List.append (List.append lhs rhs) [ Sub (v, vlhs, vrhs) ],
              g,
              v_map )
      | Mul ->
          Ok
            ( Var v,
              List.append (List.append lhs rhs) [ Mul (v, vlhs, vrhs) ],
              g,
              v_map )
      | Div ->
          Ok
            ( Var v,
              List.append (List.append lhs rhs) [ Div (v, vlhs, vrhs) ],
              g,
              v_map )
      | Mod ->
          Ok
            ( Var v,
              List.append (List.append lhs rhs) [ Mod (v, vlhs, vrhs) ],
              g,
              v_map ))
  | Var v -> (
      match StringMap.find_opt v tmp_map with
      | Some tmp -> Ok (Var tmp, [], g, tmp_map)
      | _ -> Error (Printf.sprintf "unbound variable %s" v))

and flatten_stmt (stmt : Ast.stmt) (g : var_generator) (v_map : var_map) :
    (instruction list * var_generator * var_map, string) result =
  match stmt with
  | Print expr ->
      let* v, insts, g, v_map = flatten_expr expr g v_map in
      Ok (List.append insts [ Print v ], g, v_map)
  | VarDecl (v, expr) ->
      let tmp, g = gen_var g in
      let v_map = StringMap.add v tmp v_map in
      let* vl, insts, g, v_map = flatten_expr expr g v_map in
      Ok (List.append insts [ Copy (tmp, vl) ], g, v_map)

and flatten (tree : Ast.t) : (instruction list, string) result =
  let rec aux tree g v_map acc =
    match tree with
    | [] -> Ok (acc, g)
    | stmt :: tl ->
        let* insts, g, v_map = flatten_stmt stmt g v_map in
        aux tl g v_map (List.append acc insts)
  in
  let* insts, _ = aux tree { acc = 0 } StringMap.empty [] in
  Ok insts
