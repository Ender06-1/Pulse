let prologue =
  {|
global _start

section .text

exit:
  mov   rax, 60
  mov   rdi, 0
  syscall

dump:
  push  rbp
  mov   rbp, rsp
  sub   rsp, 64
  
  ; n:    rbp-10
  ; size: rbp-18
  ; buf:  rbp-50
  
  mov   [rbp-10], rdi
  mov   qword [rbp-18], 0
  mov   byte  [rbp-19], 10

.div:
  ; rax, rdx = q, r => n / 10
  mov   rax, [rbp-10]
  mov   rdx, 0
  mov   rcx, 10
  div   rcx
  mov   [rbp-10], rax
  
  add   rdx, 48
  
  ; buf[rbp-18] = rdx
  mov   rcx, [rbp-18]
  add   rcx, 20
  neg   rcx
  lea   r8, [rbp+rcx]
  mov   [r8], dl
  
  ; size += 1
  inc   qword [rbp-18]
  
  ; n == 0
  cmp   [rbp-10], 0
  jne   dump.div
  
  ; write(fd, *buf, size)
  mov   rdi, 1 ; fd
  mov   rdx, [rbp-18]
  mov   rcx, rdx
  add   rcx, 19
  neg   rcx
  lea   rsi, [rbp+rcx] ; *buf
  inc   rdx ; size
  mov   rax, 1
  syscall
  
  leave
  ret

_start:
  xor   ebp, ebp
  and   rsp, -16
  push  rbp
  mov   rbp, rsp
|}

module StringMap = Map.Make (String)
module IntMap = Map.Make (Int)

module Context = struct
  type t = {
    vreg_acc : int;
    var_map : int StringMap.t;
    vreg_map : int IntMap.t;
  }

  let empty : t =
    { vreg_acc = 1; var_map = StringMap.empty; vreg_map = IntMap.empty }

  let gen_vreg (ctx : t) : int * t =
    (ctx.vreg_acc, { ctx with vreg_acc = ctx.vreg_acc + 1 })

  let add_var (name : string) (vreg : int) (ctx : t) : t =
    let var_map = StringMap.add name vreg ctx.var_map in
    { ctx with var_map }

  let get_var_vreg_opt (name : string) (ctx : t) : int option =
    StringMap.find_opt name ctx.var_map

  let add_vreg (vreg : int) (mem : int) (ctx : t) : t =
    let vreg_map = IntMap.add vreg mem ctx.vreg_map in
    { ctx with vreg_map }

  let get_vreg_mem_opt (vreg : int) (ctx : t) : int option =
    IntMap.find_opt vreg ctx.vreg_map
end

type register =
  | Rax
  | Rcx
  | Rdx
  | Rdi

type value =
  | Reg of register
  | Mem of int
  | Imm of int64
  | VReg of int

type instruction =
  | Call of string
  | Mov of value * value
  | Add of value * value
  | Sub of value * value
  | Mul of value
  | Div of value
  | Cmp of value * value
  | CMovE of value * value
  | Jmp of string
  | Jnz of string

type block = {
  label : int;
  instructions : instruction list;
}

let rec string_of_label (l : int) : string = Printf.sprintf ".l%d" l

and string_of_register (r : register) : string =
  match r with Rax -> "rax" | Rdi -> "rdi" | Rdx -> "rdx" | Rcx -> "rcx"

and string_of_value (v : value) : string =
  match v with
  | Reg r -> string_of_register r
  | Mem i -> Printf.sprintf "QWORD [rbp-%d]" i
  | Imm i -> Int64.to_string i
  | VReg i -> Printf.sprintf "vreg%d" i

and string_of_instruction (i : instruction) : string =
  match i with
  | Call f -> Printf.sprintf "  call  %s\n" f
  | Mov (dst, src) ->
      let dst_str = string_of_value dst and src_str = string_of_value src in
      Printf.sprintf "  mov   %s, %s\n" dst_str src_str
  | Add (dst, src) ->
      let dst_str = string_of_value dst and src_str = string_of_value src in
      Printf.sprintf "  add   %s, %s\n" dst_str src_str
  | Sub (dst, src) ->
      let dst_str = string_of_value dst and src_str = string_of_value src in
      Printf.sprintf "  sub   %s, %s\n" dst_str src_str
  | Mul src ->
      let src_str = string_of_value src in
      Printf.sprintf "  mul   %s\n" src_str
  | Div src ->
      let src_str = string_of_value src in
      Printf.sprintf "  div   %s\n" src_str
  | Cmp (src1, src2) ->
      let src1_str = string_of_value src1 and src2_str = string_of_value src2 in
      Printf.sprintf "  cmp   %s, %s\n" src1_str src2_str
  | CMovE (dst, src) ->
      let dst_str = string_of_value dst and src_str = string_of_value src in
      Printf.sprintf "  cmove %s, %s\n" dst_str src_str
  | Jmp l -> Printf.sprintf "  jmp  %s\n" l
  | Jnz l -> Printf.sprintf "  jnz  %s\n" l

and string_of_block (b : block) : string =
  let inst_str =
    List.map string_of_instruction b.instructions |> String.concat ""
  and l_str = string_of_label b.label in
  Printf.sprintf "%s:\n%s" l_str inst_str

let rec value_of_variable (v : Ir.variable) (ctx : Context.t) :
    value * Context.t =
  let name = match v with Var v -> v | Tmp t -> t in
  match Context.get_var_vreg_opt name ctx with
  | Some vreg -> (VReg vreg, ctx)
  | None ->
      let vreg, ctx = Context.gen_vreg ctx in
      let ctx = Context.add_var name vreg ctx in
      (VReg vreg, ctx)

and value_of_ir (v : Ir.value) (ctx : Context.t) : value * Context.t =
  match v with
  | Integer i -> (Imm i, ctx)
  | Variable v -> value_of_variable v ctx

let rec instruction_selection (cfg : Ir.cfg) (ctx : Context.t) :
    block list * Context.t =
  let instruction_of_ir (inst : Ir.instruction) (ctx : Context.t) :
      instruction list * Context.t =
    match inst with
    | Print v ->
        let v, ctx = value_of_ir v ctx in
        ([ Mov (Reg Rdi, v); Call "dump" ], ctx)
    | Copy (dst, src) ->
        let dst_v, ctx = value_of_variable dst ctx in
        let src_v, ctx = value_of_ir src ctx in
        ([ Mov (dst_v, src_v) ], ctx)
    | Add (dst, l, r) ->
        let dst_v, ctx = value_of_variable dst ctx in
        let l_v, ctx = value_of_ir l ctx in
        let r_v, ctx = value_of_ir r ctx in
        ([ Mov (dst_v, l_v); Add (dst_v, r_v) ], ctx)
    | Sub (dst, l, r) ->
        let dst_v, ctx = value_of_variable dst ctx in
        let l_v, ctx = value_of_ir l ctx in
        let r_v, ctx = value_of_ir r ctx in
        ([ Mov (dst_v, l_v); Sub (dst_v, r_v) ], ctx)
    | Mul (dst, l, r) ->
        let dst_v, ctx = value_of_variable dst ctx in
        let l_v, ctx = value_of_ir l ctx in
        let r_v, ctx = value_of_ir r ctx in
        ( [
            Mov (Reg Rax, l_v);
            Mov (Reg Rcx, r_v);
            Mul (Reg Rcx);
            Mov (dst_v, Reg Rax);
          ],
          ctx )
    | Div (dst, l, r) ->
        let dst_v, ctx = value_of_variable dst ctx in
        let l_v, ctx = value_of_ir l ctx in
        let r_v, ctx = value_of_ir r ctx in
        ( [
            Mov (Reg Rdx, Imm 0L);
            Mov (Reg Rax, l_v);
            Mov (Reg Rcx, r_v);
            Div (Reg Rcx);
            Mov (dst_v, Reg Rax);
          ],
          ctx )
    | Mod (dst, l, r) ->
        let dst_v, ctx = value_of_variable dst ctx in
        let l_v, ctx = value_of_ir l ctx in
        let r_v, ctx = value_of_ir r ctx in
        ( [
            Mov (Reg Rdx, Imm 0L);
            Mov (Reg Rax, l_v);
            Mov (Reg Rcx, r_v);
            Div (Reg Rcx);
            Mov (dst_v, Reg Rdx);
          ],
          ctx )
    | Cmp (dst, l, r) ->
        let dst_v, ctx = value_of_variable dst ctx in
        let l_v, ctx = value_of_ir l ctx in
        let r_v, ctx = value_of_ir r ctx in
        ( [
            Cmp (l_v, r_v);
            Mov (Reg Rax, Imm 0L);
            Mov (Reg Rdx, Imm 1L);
            CMovE (Reg Rax, Reg Rdx);
            Mov (dst_v, Reg Rax);
          ],
          ctx )
  in
  let instruction_of_terminator (t : Ir.terminator) (ctx : Context.t) :
      instruction list * Context.t =
    match t with
    | Jmp l -> ([ Jmp (string_of_label l) ], ctx)
    | Jnz (v, nzero, zero) ->
        let v, ctx = value_of_ir v ctx in
        ( [
            Cmp (v, Imm 0L);
            Jnz (string_of_label nzero);
            Jmp (string_of_label zero);
          ],
          ctx )
    | Halt -> ([ Call "exit" ], ctx)
  in
  let rec map_ir_instructions (insts : Ir.instruction list) (ctx : Context.t) :
      instruction list * Context.t =
    let rec aux insts ctx acc =
      match insts with
      | [] -> (acc, ctx)
      | i :: tl ->
          let insts, ctx = instruction_of_ir i ctx in
          aux tl ctx (acc @ insts)
    in
    aux insts ctx []
  in
  let rec aux (cfg : Ir.cfg) (ctx : Context.t) (acc : block list) :
      block list * Context.t =
    match cfg with
    | [] -> (acc, ctx)
    | b :: tl ->
        let body, ctx = map_ir_instructions b.instrs ctx in
        let term, ctx = instruction_of_terminator b.term ctx in
        let b = { label = b.label; instructions = body @ term } in
        aux tl ctx (acc @ [ b ])
  in
  aux cfg ctx []

and register_allocation (program : block list) (ctx : Context.t) :
    block list * Context.t =
  let is_vreg (v : value) : bool = match v with VReg _ -> true | _ -> false in
  let regalloc_value (v : value) (ctx : Context.t) : value * Context.t =
    match v with
    | VReg v -> (
        match Context.get_vreg_mem_opt v ctx with
        | Some m -> (Mem m, ctx)
        | None ->
            let ctx = Context.add_vreg v (v * 8) ctx in
            (Mem (v * 8), ctx))
    | _ -> (v, ctx)
  in
  let regalloc_inst (i : instruction) (ctx : Context.t) :
      instruction list * Context.t =
    match i with
    | Call _ -> ([ i ], ctx)
    | Mov (dst, src) ->
        let src_value, ctx = regalloc_value src ctx in
        let dst_value, ctx = regalloc_value dst ctx in
        if is_vreg dst && is_vreg src then
          ([ Mov (Reg Rax, src_value); Mov (dst_value, Reg Rax) ], ctx)
        else ([ Mov (dst_value, src_value) ], ctx)
    | Add (dst, src) ->
        let src_value, ctx = regalloc_value src ctx in
        let dst_value, ctx = regalloc_value dst ctx in
        if is_vreg dst && is_vreg src then
          ([ Mov (Reg Rax, src_value); Add (dst_value, Reg Rax) ], ctx)
        else ([ Add (dst_value, src_value) ], ctx)
    | Sub (dst, src) ->
        let src_value, ctx = regalloc_value src ctx in
        let dst_value, ctx = regalloc_value dst ctx in
        if is_vreg dst && is_vreg src then
          ([ Mov (Reg Rax, src_value); Sub (dst_value, Reg Rax) ], ctx)
        else ([ Sub (dst_value, src_value) ], ctx)
    | Mul src ->
        let src_value, ctx = regalloc_value src ctx in
        ([ Mul src_value ], ctx)
    | Div src ->
        let src_value, ctx = regalloc_value src ctx in
        ([ Div src_value ], ctx)
    | Cmp (src1, src2) ->
        let src1_value, ctx = regalloc_value src1 ctx in
        let src2_value, ctx = regalloc_value src2 ctx in
        if is_vreg src1 && is_vreg src2 then
          ([ Mov (Reg Rax, src2_value); Cmp (src1_value, Reg Rax) ], ctx)
        else ([ Cmp (src1_value, src2_value) ], ctx)
    | CMovE (dst, src) ->
        let src_value, ctx = regalloc_value src ctx in
        let dst_value, ctx = regalloc_value dst ctx in
        if is_vreg dst && is_vreg src then
          ([ Mov (Reg Rax, src_value); CMovE (dst_value, Reg Rax) ], ctx)
        else ([ CMovE (dst_value, src_value) ], ctx)
    | Jmp _ | Jnz _ -> ([ i ], ctx)
  in
  let regalloc_block b ctx =
    let rec aux insts ctx acc =
      match insts with
      | [] -> (acc, ctx)
      | i :: tl ->
          let is, ctx = regalloc_inst i ctx in
          aux tl ctx (acc @ is)
    in
    let insts, ctx = aux b.instructions ctx [] in
    ({ b with instructions = insts }, ctx)
  in
  let rec aux program ctx acc =
    match program with
    | [] -> (acc, ctx)
    | b :: tl ->
        let b, ctx = regalloc_block b ctx in
        aux tl ctx (acc @ [ b ])
  in
  aux program ctx []

and codegen (cfg : Ir.cfg) : string =
  let align16 (n : int) : int =
    int_of_float (ceil (float_of_int n /. 16.)) * 16
  in
  let string_of_program (program : block list) (ctx : Context.t) : string =
    let program_str = List.map string_of_block program |> String.concat "\n" in
    let stack_alloc_str =
      Printf.sprintf "  sub   rsp, %d\n" (align16 ((ctx.vreg_acc - 1) * 8))
    in
    Printf.sprintf "%s%s%s" prologue stack_alloc_str program_str
  in
  let ctx = Context.empty in
  let program, ctx = instruction_selection cfg ctx in
  let program, _ = register_allocation program ctx in
  string_of_program program ctx
