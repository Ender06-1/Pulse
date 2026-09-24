type register =
  (* Caller-saved *)
  | Rax
  | Rcx
  | Rdx
  | Rsi
  | Rdi
  | R8
  | R9
  (* Callee-saved *)
  | Rsp
  | Rbp

and value =
  | Reg of register
  | Mem of int
  | Imm of int64
  | VReg of int

and instruction =
  | Call of string
  | Mov of value * value
  | Add of value * value
  | Sub of value * value
  | Mul of value
  | Div of value
  | Cmp of value * value
  | CMovE of value * value
  | CMovNe of value * value
  | CMovG of value * value
  | CMovGe of value * value
  | CMovL of value * value
  | CMovLe of value * value
  | Jmp of string
  | Jne of string
  | Ret
  | Push of value
  | Leave

and block = {
  label : int;
  instructions : instruction list;
}

and fn = {
  name : string;
  init : instruction list;
  body : block list;
}

and t = fn list

module Context = struct
  module IntMap = Map.Make (Int)

  type t = {
    vreg_acc : int;
    tmp_vreg_scopes : int IntMap.t list;
    stack_acc : int;
    vreg_mem_map : int IntMap.t;
  }

  let empty : t =
    {
      vreg_acc = 1;
      tmp_vreg_scopes = [ IntMap.empty ];
      vreg_mem_map = IntMap.empty;
      stack_acc = 0;
    }

  let gen_vreg (ctx : t) : int * t =
    (ctx.vreg_acc, { ctx with vreg_acc = ctx.vreg_acc + 1 })

  and add_vreg (t : int) (vreg : int) (ctx : t) : t =
    assert (List.length ctx.tmp_vreg_scopes > 0);
    let s = List.hd ctx.tmp_vreg_scopes in
    let s = IntMap.add t vreg s in
    let scopes = List.tl ctx.tmp_vreg_scopes in
    { ctx with tmp_vreg_scopes = s :: scopes }

  and find_vreg_opt (t : int) (ctx : t) : int option =
    let rec aux scopes =
      match scopes with
      | [] -> None
      | s :: tl -> (
          match IntMap.find_opt t s with
          | None -> aux tl
          | Some vreg -> Some vreg)
    in
    aux ctx.tmp_vreg_scopes

  and push_scope (ctx : t) : t =
    { ctx with tmp_vreg_scopes = IntMap.empty :: ctx.tmp_vreg_scopes }

  and pop_scope (ctx : t) : t =
    assert (List.length ctx.tmp_vreg_scopes > 0);
    { ctx with tmp_vreg_scopes = List.tl ctx.tmp_vreg_scopes }

  and alloc (ctx : t) : int * t =
    (ctx.stack_acc + 8, { ctx with stack_acc = ctx.stack_acc + 8 })

  and add_alloc (vreg : int) (mem : int) (ctx : t) : t =
    let vreg_mem_map = IntMap.add vreg mem ctx.vreg_mem_map in
    { ctx with vreg_mem_map }

  and find_alloc_opt (vreg : int) (ctx : t) : int option =
    IntMap.find_opt vreg ctx.vreg_mem_map
end

let rec string_of_label (l : int) : string = Printf.sprintf ".l%d" l

and string_of_register (r : register) : string =
  match r with
  (* Caller-saved *)
  | Rax -> "rax"
  | Rcx -> "rcx"
  | Rdx -> "rdx"
  | Rsi -> "rsi"
  | Rdi -> "rdi"
  | R8 -> "r8"
  | R9 -> "r9"
  (* Callee-saved *)
  | Rsp -> "rsp"
  | Rbp -> "rbp"

and string_of_value (v : value) : string =
  match v with
  | Reg r -> string_of_register r
  | Mem i -> Printf.sprintf "QWORD [rbp-%d]" i
  | Imm i -> Int64.to_string i
  | VReg i -> Printf.sprintf "vreg%d" i

and string_of_instruction (i : instruction) : string =
  match i with
  | Call f -> Printf.sprintf "call  %s" f
  | Mov (dst, src) ->
      let dst_str = string_of_value dst and src_str = string_of_value src in
      Printf.sprintf "mov   %s, %s" dst_str src_str
  | Add (dst, src) ->
      let dst_str = string_of_value dst and src_str = string_of_value src in
      Printf.sprintf "add   %s, %s" dst_str src_str
  | Sub (dst, src) ->
      let dst_str = string_of_value dst and src_str = string_of_value src in
      Printf.sprintf "sub   %s, %s" dst_str src_str
  | Mul src ->
      let src_str = string_of_value src in
      Printf.sprintf "mul   %s" src_str
  | Div src ->
      let src_str = string_of_value src in
      Printf.sprintf "div   %s" src_str
  | Cmp (src1, src2) ->
      let src1_str = string_of_value src1 and src2_str = string_of_value src2 in
      Printf.sprintf "cmp   %s, %s" src1_str src2_str
  | CMovE (dst, src) ->
      let dst_str = string_of_value dst and src_str = string_of_value src in
      Printf.sprintf "cmove %s, %s" dst_str src_str
  | CMovNe (dst, src) ->
      let dst_str = string_of_value dst and src_str = string_of_value src in
      Printf.sprintf "cmovne %s, %s" dst_str src_str
  | CMovG (dst, src) ->
      let dst_str = string_of_value dst and src_str = string_of_value src in
      Printf.sprintf "cmovg %s, %s" dst_str src_str
  | CMovGe (dst, src) ->
      let dst_str = string_of_value dst and src_str = string_of_value src in
      Printf.sprintf "cmovge %s, %s" dst_str src_str
  | CMovL (dst, src) ->
      let dst_str = string_of_value dst and src_str = string_of_value src in
      Printf.sprintf "cmovl %s, %s" dst_str src_str
  | CMovLe (dst, src) ->
      let dst_str = string_of_value dst and src_str = string_of_value src in
      Printf.sprintf "cmovle %s, %s" dst_str src_str
  | Jmp l -> Printf.sprintf "jmp  %s" l
  | Jne l -> Printf.sprintf "jne  %s" l
  | Ret -> "ret"
  | Push v ->
      let v_str = string_of_value v in
      Printf.sprintf "push  %s" v_str
  | Leave -> "leave"

and string_of_instruction_list (il : instruction list) : string =
  let rec aux il acc =
    match il with
    | [] -> List.rev acc |> List.map (String.cat "  ") |> String.concat "\n"
    | i :: tl ->
        let i_str = string_of_instruction i in
        aux tl (i_str :: acc)
  in
  aux il []

and string_of_block (b : block) : string =
  let inst_str = string_of_instruction_list b.instructions
  and l_str = string_of_label b.label in
  Printf.sprintf "%s:\n%s" l_str inst_str

and string_of_block_list (bl : block list) : string =
  let rec aux bl acc =
    match bl with
    | [] -> List.rev acc |> String.concat "\n"
    | b :: tl ->
        let b_str = string_of_block b in
        aux tl (b_str :: acc)
  in
  aux bl []

and string_of_fn (f : fn) : string =
  let init_str = string_of_instruction_list f.init
  and body_str = string_of_block_list f.body in
  Printf.sprintf "%s:\n%s\n%s\n" f.name init_str body_str

and string_of_fn_list (fl : fn list) : string =
  let rec aux fl acc =
    match fl with
    | [] -> List.rev acc |> String.concat "\n"
    | f :: tl ->
        let fn_str = string_of_fn f in
        aux tl (fn_str :: acc)
  in
  aux fl []

let rec instruction_selection (program : Ir.fn list) : fn list =
  let rec select_value (v : Ir.value) (ctx : Context.t) : value * Context.t =
    match v with
    | Integer i -> (Imm i, ctx)
    | Temp t -> (
        match Context.find_vreg_opt t ctx with
        | Some vreg -> (VReg vreg, ctx)
        | None ->
            let vreg, ctx = Context.gen_vreg ctx in
            let ctx = Context.add_vreg t vreg ctx in
            (VReg vreg, ctx))
  and select_tmp (t : int) (ctx : Context.t) : value * Context.t =
    select_value (Temp t) ctx
  and select_reg_i (i : int) =
    assert (i <= 6);
    match i with
    | 0 -> Rdi
    | 1 -> Rsi
    | 2 -> Rdx
    | 3 -> Rcx
    | 4 -> R8
    | 5 -> R9
    | _ -> failwith "Codegen_x86_64_linux: select_reg_i: unreachable"
  and select_reg_call (params : value list) : instruction list =
    assert (List.length params <= 6);
    List.mapi (fun i value -> Mov (Reg (select_reg_i i), value)) params
  and select_instruction (inst : Ir.instruction) (ctx : Context.t) :
      instruction list * Context.t =
    match inst with
    | Print v ->
        let v, ctx = select_value v ctx in
        ([ Mov (Reg Rdi, v); Call "dump" ], ctx)
    | Copy (dst, src) ->
        let dst_v, ctx = select_tmp dst ctx in
        let src_v, ctx = select_value src ctx in
        ([ Mov (dst_v, src_v) ], ctx)
    | Add (dst, l, r) ->
        let dst_v, ctx = select_tmp dst ctx in
        let l_v, ctx = select_value l ctx in
        let r_v, ctx = select_value r ctx in
        ([ Mov (dst_v, l_v); Add (dst_v, r_v) ], ctx)
    | Sub (dst, l, r) ->
        let dst_v, ctx = select_tmp dst ctx in
        let l_v, ctx = select_value l ctx in
        let r_v, ctx = select_value r ctx in
        ([ Mov (dst_v, l_v); Sub (dst_v, r_v) ], ctx)
    | Mul (dst, l, r) ->
        let dst_v, ctx = select_tmp dst ctx in
        let l_v, ctx = select_value l ctx in
        let r_v, ctx = select_value r ctx in
        ( [
            Mov (Reg Rax, l_v);
            Mov (Reg Rcx, r_v);
            Mul (Reg Rcx);
            Mov (dst_v, Reg Rax);
          ],
          ctx )
    | Div (dst, l, r) ->
        let dst_v, ctx = select_tmp dst ctx in
        let l_v, ctx = select_value l ctx in
        let r_v, ctx = select_value r ctx in
        ( [
            Mov (Reg Rdx, Imm 0L);
            Mov (Reg Rax, l_v);
            Mov (Reg Rcx, r_v);
            Div (Reg Rcx);
            Mov (dst_v, Reg Rax);
          ],
          ctx )
    | Mod (dst, l, r) ->
        let dst_v, ctx = select_tmp dst ctx in
        let l_v, ctx = select_value l ctx in
        let r_v, ctx = select_value r ctx in
        ( [
            Mov (Reg Rdx, Imm 0L);
            Mov (Reg Rax, l_v);
            Mov (Reg Rcx, r_v);
            Div (Reg Rcx);
            Mov (dst_v, Reg Rdx);
          ],
          ctx )
    | Ceq (dst, l, r) ->
        let dst_v, ctx = select_tmp dst ctx in
        let l_v, ctx = select_value l ctx in
        let r_v, ctx = select_value r ctx in
        ( [
            Cmp (l_v, r_v);
            Mov (Reg Rax, Imm 0L);
            Mov (Reg Rdx, Imm 1L);
            CMovE (Reg Rax, Reg Rdx);
            Mov (dst_v, Reg Rax);
          ],
          ctx )
    | Cne (dst, l, r) ->
        let dst_v, ctx = select_tmp dst ctx in
        let l_v, ctx = select_value l ctx in
        let r_v, ctx = select_value r ctx in
        ( [
            Cmp (l_v, r_v);
            Mov (Reg Rax, Imm 0L);
            Mov (Reg Rdx, Imm 1L);
            CMovNe (Reg Rax, Reg Rdx);
            Mov (dst_v, Reg Rax);
          ],
          ctx )
    | Cgt (dst, l, r) ->
        let dst_v, ctx = select_tmp dst ctx in
        let l_v, ctx = select_value l ctx in
        let r_v, ctx = select_value r ctx in
        ( [
            Cmp (l_v, r_v);
            Mov (Reg Rax, Imm 0L);
            Mov (Reg Rdx, Imm 1L);
            CMovG (Reg Rax, Reg Rdx);
            Mov (dst_v, Reg Rax);
          ],
          ctx )
    | Cge (dst, l, r) ->
        let dst_v, ctx = select_tmp dst ctx in
        let l_v, ctx = select_value l ctx in
        let r_v, ctx = select_value r ctx in
        ( [
            Cmp (l_v, r_v);
            Mov (Reg Rax, Imm 0L);
            Mov (Reg Rdx, Imm 1L);
            CMovGe (Reg Rax, Reg Rdx);
            Mov (dst_v, Reg Rax);
          ],
          ctx )
    | Clt (dst, l, r) ->
        let dst_v, ctx = select_tmp dst ctx in
        let l_v, ctx = select_value l ctx in
        let r_v, ctx = select_value r ctx in
        ( [
            Cmp (l_v, r_v);
            Mov (Reg Rax, Imm 0L);
            Mov (Reg Rdx, Imm 1L);
            CMovL (Reg Rax, Reg Rdx);
            Mov (dst_v, Reg Rax);
          ],
          ctx )
    | Cle (dst, l, r) ->
        let dst_v, ctx = select_tmp dst ctx in
        let l_v, ctx = select_value l ctx in
        let r_v, ctx = select_value r ctx in
        ( [
            Cmp (l_v, r_v);
            Mov (Reg Rax, Imm 0L);
            Mov (Reg Rdx, Imm 1L);
            CMovLe (Reg Rax, Reg Rdx);
            Mov (dst_v, Reg Rax);
          ],
          ctx )
    | Call (dst_opt, fn, params) -> (
        let rec aux params acc ctx =
          match params with
          | [] -> (List.rev acc, ctx)
          | p :: tl ->
              let p_v, ctx = select_value p ctx in
              aux tl (p_v :: acc) ctx
        in
        let param_values, ctx = aux params [] ctx in
        let param_insts = select_reg_call param_values in
        match dst_opt with
        | None -> (param_insts @ [ Call fn ], ctx)
        | Some dst ->
            let vreg, ctx = select_tmp dst ctx in
            (param_insts @ [ Call fn; Mov (vreg, Reg Rax) ], ctx))
  and select_terminator (t : Ir.terminator) (ctx : Context.t) :
      instruction list * Context.t =
    match t with
    | Ir.Jmp l -> ([ Jmp (string_of_label l) ], ctx)
    | Ir.Jnz (v, nzero, zero) ->
        let v, ctx = select_value v ctx in
        ( [
            Cmp (v, Imm 0L);
            Jne (string_of_label nzero);
            Jmp (string_of_label zero);
          ],
          ctx )
    | Ir.Ret v_opt -> (
        match v_opt with
        | None -> ([ Leave; Ret ], ctx)
        | Some v ->
            let v, ctx = select_value v ctx in
            ([ Mov (Reg Rax, v); Leave; Ret ], ctx))
    | Ir.Halt -> ([ Call "exit" ], ctx)
  and select_instruction_list (il : Ir.instruction list) (ctx : Context.t) :
      instruction list * Context.t =
    let rec aux il acc ctx =
      match il with
      | [] -> (acc, ctx)
      | i :: tl ->
          let insts, ctx = select_instruction i ctx in
          aux tl (acc @ insts) ctx
    in
    aux il [] ctx
  and select_block (b : Ir.block) (ctx : Context.t) : block * Context.t =
    let body_insts, ctx = select_instruction_list b.instrs ctx in
    let term_insts, ctx = select_terminator b.term ctx in
    let block : block =
      { label = b.label; instructions = body_insts @ term_insts }
    in
    (block, ctx)
  and select_block_list (bl : Ir.block list) (ctx : Context.t) :
      block list * Context.t =
    let rec aux bl acc ctx =
      match bl with
      | [] -> (List.rev acc, ctx)
      | b :: tl ->
          let b, ctx = select_block b ctx in
          aux tl (b :: acc) ctx
    in
    aux bl [] ctx
  and select_fn (fn : Ir.fn) : fn =
    let gen_vreg_params params ctx =
      let rec aux params i acc ctx =
        match params with
        | [] -> (List.rev acc, ctx)
        | p :: tl ->
            let vreg, ctx = Context.gen_vreg ctx in
            let ctx = Context.add_vreg p vreg ctx in
            let reg = select_reg_i i in
            let inst = Mov (VReg vreg, Reg reg) in
            aux tl (i + 1) (inst :: acc) ctx
      in
      aux params 0 [] ctx
    in
    let ctx = Context.empty in
    let param_insts, ctx = gen_vreg_params fn.params ctx in
    let body, ctx = select_block_list fn.body ctx in
    let f : fn = { name = fn.name; init = param_insts; body } in
    f
  and select_fn_list (fl : Ir.fn list) : fn list =
    let rec aux fl acc =
      match fl with
      | [] -> List.rev acc
      | f :: tl ->
          let f = select_fn f in
          aux tl (f :: acc)
    in
    aux fl []
  in
  select_fn_list program

and register_allocation (program : fn list) : fn list =
  let rec is_vreg (v : value) : bool =
    match v with VReg _ -> true | _ -> false
  and is_imm (v : value) : bool = match v with Imm _ -> true | _ -> false
  and align16 (n : int) : int = int_of_float (ceil (float_of_int n /. 16.)) * 16
  and regalloc_vreg (vreg : int) (ctx : Context.t) : value * Context.t =
    match Context.find_alloc_opt vreg ctx with
    | Some i -> (Mem i, ctx)
    | None ->
        let i, ctx = Context.alloc ctx in
        let ctx = Context.add_alloc vreg i ctx in
        (Mem i, ctx)
  and regalloc_value (v : value) (ctx : Context.t) : value * Context.t =
    match v with VReg v -> regalloc_vreg v ctx | _ -> (v, ctx)
  and regalloc_inst (i : instruction) (ctx : Context.t) :
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
        if is_imm src1 then
          ([ Mov (Reg Rax, src1_value); Cmp (Reg Rax, src2_value) ], ctx)
        else if is_vreg src1 && is_vreg src2 then
          ([ Mov (Reg Rax, src2_value); Cmp (src1_value, Reg Rax) ], ctx)
        else ([ Cmp (src1_value, src2_value) ], ctx)
    | CMovE (dst, src) ->
        let src_value, ctx = regalloc_value src ctx in
        let dst_value, ctx = regalloc_value dst ctx in
        if is_vreg dst && is_vreg src then
          ([ Mov (Reg Rax, src_value); CMovE (dst_value, Reg Rax) ], ctx)
        else ([ CMovE (dst_value, src_value) ], ctx)
    | CMovNe (dst, src) ->
        let src_value, ctx = regalloc_value src ctx in
        let dst_value, ctx = regalloc_value dst ctx in
        if is_vreg dst && is_vreg src then
          ([ Mov (Reg Rax, src_value); CMovNe (dst_value, Reg Rax) ], ctx)
        else ([ CMovNe (dst_value, src_value) ], ctx)
    | CMovG (dst, src) ->
        let src_value, ctx = regalloc_value src ctx in
        let dst_value, ctx = regalloc_value dst ctx in
        if is_vreg dst && is_vreg src then
          ([ Mov (Reg Rax, src_value); CMovG (dst_value, Reg Rax) ], ctx)
        else ([ CMovG (dst_value, src_value) ], ctx)
    | CMovGe (dst, src) ->
        let src_value, ctx = regalloc_value src ctx in
        let dst_value, ctx = regalloc_value dst ctx in
        if is_vreg dst && is_vreg src then
          ([ Mov (Reg Rax, src_value); CMovGe (dst_value, Reg Rax) ], ctx)
        else ([ CMovGe (dst_value, src_value) ], ctx)
    | CMovL (dst, src) ->
        let src_value, ctx = regalloc_value src ctx in
        let dst_value, ctx = regalloc_value dst ctx in
        if is_vreg dst && is_vreg src then
          ([ Mov (Reg Rax, src_value); CMovL (dst_value, Reg Rax) ], ctx)
        else ([ CMovL (dst_value, src_value) ], ctx)
    | CMovLe (dst, src) ->
        let src_value, ctx = regalloc_value src ctx in
        let dst_value, ctx = regalloc_value dst ctx in
        if is_vreg dst && is_vreg src then
          ([ Mov (Reg Rax, src_value); CMovLe (dst_value, Reg Rax) ], ctx)
        else ([ CMovLe (dst_value, src_value) ], ctx)
    | Jmp _ -> ([ i ], ctx)
    | Jne _ -> ([ i ], ctx)
    | Ret -> ([ i ], ctx)
    | Push v ->
        let v, ctx = regalloc_value v ctx in
        ([ Push v ], ctx)
    | Leave -> ([ i ], ctx)
  and regalloc_instruction_list (il : instruction list) (ctx : Context.t) :
      instruction list * Context.t =
    let rec aux insts acc ctx =
      match insts with
      | [] -> (List.rev acc |> List.concat, ctx)
      | i :: tl ->
          let i, ctx = regalloc_inst i ctx in
          aux tl (i :: acc) ctx
    in
    aux il [] ctx
  and regalloc_block (b : block) (ctx : Context.t) : block * Context.t =
    let instructions, ctx = regalloc_instruction_list b.instructions ctx in
    let b : block = { label = b.label; instructions } in
    (b, ctx)
  and regalloc_block_list (bl : block list) (ctx : Context.t) :
      block list * Context.t =
    let rec aux bl acc ctx =
      match bl with
      | [] -> (List.rev acc, ctx)
      | b :: tl ->
          let b, ctx = regalloc_block b ctx in
          aux tl (b :: acc) ctx
    in
    aux bl [] ctx
  and regalloc_fn (f : fn) : fn =
    let ctx = Context.empty in
    let init, ctx = regalloc_instruction_list f.init ctx in
    let body, ctx = regalloc_block_list f.body ctx in
    let stack_size = align16 ctx.stack_acc |> Int64.of_int in
    let stack_init =
      [ Push (Reg Rbp); Mov (Reg Rbp, Reg Rsp); Sub (Reg Rsp, Imm stack_size) ]
    in
    let f : fn = { name = f.name; init = stack_init @ init; body } in
    f
  and regalloc_fn_list (fl : fn list) : fn list =
    let rec aux fl acc =
      match fl with
      | [] -> List.rev acc
      | f :: tl ->
          let f = regalloc_fn f in
          aux tl (f :: acc)
    in
    aux fl []
  in
  regalloc_fn_list program

let dump =
  {|
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
|}

and exit = {|
exit:
  mov   rax, 60
  syscall
|}

and prologue = {|
section .text
|}

and start =
  {|
global _start
_start:
  xor   ebp, ebp
  and   rsp, -16
  call  main
  
  mov   rdi, 0
  call  exit
|}

let codegen (program : Ir.t) : string =
  let program = instruction_selection program in
  let program = register_allocation program in
  let program_str = string_of_fn_list program in
  Printf.sprintf "%s\n%s\n%s\n%s\n%s\n" prologue exit dump program_str start
