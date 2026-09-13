let ( let* ) = Result.bind

module StringMap = Map.Make (String)

type tmp_map = string StringMap.t

type value =
  | Tmp of string
  | Integer of int64

type instruction =
  | Print of value
  | Copy of string * value
  | Add of string * value * value
  | Sub of string * value * value
  | Mul of string * value * value
  | Div of string * value * value
  | Mod of string * value * value

let rec string_of_tmp (t : string) : string = Printf.sprintf "%%%s" t

and string_of_value (v : value) : string =
  match v with Tmp v -> string_of_tmp v | Integer i -> Int64.to_string i

and to_string (i : instruction) : string =
  match i with
  | Print v ->
      let vals = string_of_value v in
      Printf.sprintf "print %s" vals
  | Copy (t, v) ->
      let ts = string_of_tmp t and vs = string_of_value v in
      Printf.sprintf "%s = copy %s" ts vs
  | Add (t, l, r) ->
      let ts = string_of_tmp t
      and ls = string_of_value l
      and rs = string_of_value r in
      Printf.sprintf "%s = add %s, %s" ts ls rs
  | Sub (t, l, r) ->
      let ts = string_of_tmp t
      and ls = string_of_value l
      and rs = string_of_value r in
      Printf.sprintf "%s = sub %s, %s" ts ls rs
  | Mul (t, l, r) ->
      let ts = string_of_tmp t
      and ls = string_of_value l
      and rs = string_of_value r in
      Printf.sprintf "%s = mul %s, %s" ts ls rs
  | Div (t, l, r) ->
      let ts = string_of_tmp t
      and ls = string_of_value l
      and rs = string_of_value r in
      Printf.sprintf "%s = div %s, %s" ts ls rs
  | Mod (t, l, r) ->
      let ts = string_of_tmp t
      and ls = string_of_value l
      and rs = string_of_value r in
      Printf.sprintf "%s = mod %s, %s" ts ls rs

type tmp_generator = { acc : int }

let gen_tmp (generator : tmp_generator) : string * tmp_generator =
  (Printf.sprintf "v%d" generator.acc, { acc = generator.acc + 1 })

let rec flatten_expr (expr : Ast.expr) (g : tmp_generator) (tmp_map : tmp_map) :
    (value * instruction list * tmp_generator * tmp_map, string) result =
  match expr with
  | Integer i -> Ok (Integer i, [], g, tmp_map)
  | BinExpr (op, l, r) -> (
      let* vlhs, lhs, g, tmp_map = flatten_expr l g tmp_map in
      let* vrhs, rhs, g, tmp_map = flatten_expr r g tmp_map in
      let v, g = gen_tmp g in
      match op with
      | Plus ->
          Ok
            ( Tmp v,
              List.append (List.append lhs rhs) [ Add (v, vlhs, vrhs) ],
              g,
              tmp_map )
      | Minus ->
          Ok
            ( Tmp v,
              List.append (List.append lhs rhs) [ Sub (v, vlhs, vrhs) ],
              g,
              tmp_map )
      | Mul ->
          Ok
            ( Tmp v,
              List.append (List.append lhs rhs) [ Mul (v, vlhs, vrhs) ],
              g,
              tmp_map )
      | Div ->
          Ok
            ( Tmp v,
              List.append (List.append lhs rhs) [ Div (v, vlhs, vrhs) ],
              g,
              tmp_map )
      | Mod ->
          Ok
            ( Tmp v,
              List.append (List.append lhs rhs) [ Mod (v, vlhs, vrhs) ],
              g,
              tmp_map ))
  | Var v -> (
      match StringMap.find_opt v tmp_map with
      | Some tmp -> Ok (Tmp tmp, [], g, tmp_map)
      | _ -> Error (Printf.sprintf "unbound variable %s" v))

and flatten_stmt (stmt : Ast.stmt) (g : tmp_generator) (tmp_map : tmp_map) :
    (instruction list * tmp_generator * tmp_map, string) result =
  match stmt with
  | Print expr ->
      let* v, insts, g, tmp_map = flatten_expr expr g tmp_map in
      Ok (List.append insts [ Print v ], g, tmp_map)
  | VarDecl (v, expr) ->
      let tmp, g = gen_tmp g in
      let tmp_map = StringMap.add v tmp tmp_map in
      let* vl, insts, g, tmp_map = flatten_expr expr g tmp_map in
      Ok (List.append insts [ Copy (tmp, vl) ], g, tmp_map)

and flatten (tree : Ast.t) : (instruction list, string) result =
  let rec aux tree g tmp_map acc =
    match tree with
    | [] -> Ok (acc, g)
    | stmt :: tl ->
        let* insts, g, tmp_map = flatten_stmt stmt g tmp_map in
        aux tl g tmp_map (List.append acc insts)
  in
  let* insts, _ = aux tree { acc = 0 } StringMap.empty [] in
  Ok insts
