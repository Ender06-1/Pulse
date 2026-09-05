type variable = string
type value = Var of variable | Integer of int64

type instruction =
  | Copy of variable * value
  | Add of variable * value * value
  | Sub of variable * value * value

type var_generator = { acc : int }

let gen_var (generator : var_generator) : string * var_generator =
  (Printf.sprintf "v%d" generator.acc, { acc = generator.acc + 1 })

let rec flatten (tree : Ast.t) : instruction list =
  let rec aux (tree : Ast.t) g =
    match tree with
    | Integer i ->
        let var, g = gen_var g in
        ([ Copy (var, Integer i) ], Var var, g)
    | BinExpr (op, l, r) -> (
        let lhs, vlhs, g = aux l g in
        let rhs, vrhs, g = aux r g in
        let var, g = gen_var g in
        match op with
        | Plus ->
            ( List.append (List.append lhs rhs) [ Add (var, vlhs, vrhs) ],
              Var var,
              g )
        | Minus ->
            ( List.append (List.append lhs rhs) [ Sub (var, vlhs, vrhs) ],
              Var var,
              g ))
  in
  let insts, _, _ = aux tree { acc = 0 } in
  insts
