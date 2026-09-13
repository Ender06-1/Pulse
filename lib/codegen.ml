let prologue =
  {|
global _start

section .text

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

dump.div:
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
  push  rbp
  mov   rbp, rsp
|}

and epilogue = {| 
  mov rax, 60
  mov rdi, 0
  syscall
|}

module StringMap = Map.Make (String)

type register =
  | Rax
  | Rcx
  | Rdx
  | Rsi
  | Rdi

type value =
  | Reg of register
  | VReg of string
  | Memory of string
  | Imm of int64

type instruction =
  | Mov of value * value
  | Add of value * value
  | Sub of value * value
  | Mul of value
  | Div of value
  | Call of string

type var_generator = { acc : int }

let gen_var (g : var_generator) : string * var_generator =
  let v = Printf.sprintf "r_v%d" g.acc in
  (v, { acc = g.acc + 1 })

let rec is_vreg (v : value) : bool = match v with VReg _ -> true | _ -> false
and is_imm (v : value) : bool = match v with Imm _ -> true | _ -> false

and string_of_instruction (i : instruction) : string =
  let string_of_register (r : register) : string =
    match r with
    | Rax -> "rax"
    | Rcx -> "rcx"
    | Rdx -> "rdx"
    | Rsi -> "rsi"
    | Rdi -> "rdi"
  in
  let string_of_value (v : value) : string =
    match v with
    | Reg r -> string_of_register r
    | VReg v -> v
    | Memory m -> Printf.sprintf "QWORD %s" m
    | Imm i -> Int64.to_string i
  in
  match i with
  | Mov (v1, v2) ->
      Printf.sprintf "  mov %s, %s" (string_of_value v1) (string_of_value v2)
  | Add (v1, v2) ->
      Printf.sprintf "  add %s, %s" (string_of_value v1) (string_of_value v2)
  | Sub (v1, v2) ->
      Printf.sprintf "  sub %s, %s" (string_of_value v1) (string_of_value v2)
  | Mul v -> Printf.sprintf "  mul %s" (string_of_value v)
  | Div v -> Printf.sprintf "  div %s" (string_of_value v)
  | Call l -> Printf.sprintf "  call %s" l

and value_of_ir (v : Ir.value) : value =
  match v with Tmp v -> VReg v | Integer i -> Imm i

and inst_selection (ir : Ir.instruction list) (g : var_generator) :
    instruction list * var_generator =
  let inst_of_ir (i : Ir.instruction) (g : var_generator) :
      instruction list * var_generator =
    match i with
    | Print v -> ([ Mov (Reg Rdi, value_of_ir v); Call "dump" ], g)
    | Add (tmp, l, r) ->
        let var = VReg tmp in
        ([ Mov (var, value_of_ir l); Add (var, value_of_ir r) ], g)
    | Sub (tmp, l, r) ->
        let var = VReg tmp in
        ([ Mov (var, value_of_ir l); Sub (var, value_of_ir r) ], g)
    | Mul (tmp, l, r) ->
        let var = VReg tmp in
        let rvalue = value_of_ir r in
        let mul_insts =
          if is_imm rvalue then
            let rvar, g = gen_var g in
            let rreg = VReg rvar in
            [ Mov (rreg, rvalue); Mul rreg ]
          else [ Mul rvalue ]
        in
        ( [ Mov (Reg Rax, value_of_ir l) ] @ mul_insts @ [ Mov (var, Reg Rax) ],
          g )
    | Div (tmp, l, r) ->
        let var = VReg tmp in
        let rvalue = value_of_ir r in
        let div_insts =
          if is_imm rvalue then
            let rvar, g = gen_var g in
            let rreg = VReg rvar in
            [ Mov (rreg, rvalue); Div rreg ]
          else [ Div rvalue ]
        in
        ( [ Mov (Reg Rax, value_of_ir l); Mov (Reg Rdx, Imm 0L) ]
          @ div_insts
          @ [ Mov (var, Reg Rax) ],
          g )
    | Mod (tmp, l, r) ->
        let var = VReg tmp in
        let rvalue = value_of_ir r in
        let mod_insts =
          if is_imm rvalue then
            let rvar, g = gen_var g in
            let rreg = VReg rvar in
            [ Mov (rreg, rvalue); Div rreg ]
          else [ Div rvalue ]
        in
        ( [ Mov (Reg Rax, value_of_ir l); Mov (Reg Rdx, Imm 0L) ]
          @ mod_insts
          @ [ Mov (var, Reg Rdx) ],
          g )
    | Copy (tmp, exp) -> ([ Mov (VReg tmp, value_of_ir exp) ], g)
  in
  let rec aux ir g acc =
    match ir with
    | [] -> (acc, g)
    | i :: tl ->
        let insts, g = inst_of_ir i g in
        aux tl g (List.append acc insts)
  in
  aux ir g []

and regalloc (ir : instruction list) : instruction list * int =
  let vreg_alloc (i : value) (v_map : string StringMap.t) (acc : int) :
      value * string StringMap.t * int =
    let alloc_stack_slot (v : string) (v_map : string StringMap.t) (acc : int) =
      let acc = acc + 8 in
      let mem_ref = string_of_int acc |> Printf.sprintf "[rbp-%s]" in
      (Memory mem_ref, StringMap.add v mem_ref v_map, acc)
    in
    match i with
    | VReg v -> (
        match StringMap.find_opt v v_map with
        | None -> alloc_stack_slot v v_map acc
        | Some m -> (Memory m, v_map, acc))
    | _ -> (i, v_map, acc)
  in
  let rec aux (ir : instruction list) (acc : instruction list) (stack_acc : int)
      (v_map : string StringMap.t) =
    match ir with
    | [] -> (List.rev acc, stack_acc)
    | i :: tl -> (
        match i with
        | Mov (v1, v2) ->
            let v1_alloc, v_map, stack_acc = vreg_alloc v1 v_map stack_acc in
            let v2_alloc, v_map, stack_acc = vreg_alloc v2 v_map stack_acc in
            let insts =
              if is_vreg v1 && is_vreg v2 then
                [ Mov (v1_alloc, Reg Rax); Mov (Reg Rax, v2_alloc) ]
              else [ Mov (v1_alloc, v2_alloc) ]
            in
            aux tl (List.append insts acc) stack_acc v_map
        | Add (v1, v2) ->
            let v1_alloc, v_map, stack_acc = vreg_alloc v1 v_map stack_acc in
            let v2_alloc, v_map, stack_acc = vreg_alloc v2 v_map stack_acc in
            let insts =
              if is_vreg v1 && is_vreg v2 then
                [ Add (v1_alloc, Reg Rax); Mov (Reg Rax, v2_alloc) ]
              else [ Add (v1_alloc, v2_alloc) ]
            in
            aux tl (List.append insts acc) stack_acc v_map
        | Sub (v1, v2) ->
            let v1_alloc, v_map, stack_acc = vreg_alloc v1 v_map stack_acc in
            let v2_alloc, v_map, stack_acc = vreg_alloc v2 v_map stack_acc in
            let insts =
              if is_vreg v1 && is_vreg v2 then
                [ Sub (v1_alloc, Reg Rax); Mov (Reg Rax, v2_alloc) ]
              else [ Sub (v1_alloc, v2_alloc) ]
            in
            aux tl (List.append insts acc) stack_acc v_map
        | Mul v ->
            let v_alloc, v_map, stack_acc = vreg_alloc v v_map stack_acc in
            let insts = [ Mul v_alloc ] in
            aux tl (List.append insts acc) stack_acc v_map
        | Div v ->
            let v_alloc, v_map, stack_acc = vreg_alloc v v_map stack_acc in
            let insts = [ Div v_alloc ] in
            aux tl (List.append insts acc) stack_acc v_map
        | Call l -> aux tl (i :: acc) stack_acc v_map)
  in
  aux ir [] 0 StringMap.empty

and codegen (ir : Ir.instruction list) : string =
  let insts, g = inst_selection ir { acc = 0 } in
  let insts, raw_stack_size = regalloc insts in
  let stack_size =
    Float.div (Float.of_int raw_stack_size) 16.
    |> Float.round |> Int.of_float |> Int.mul 16
  in
  let prologue =
    Printf.sprintf "  sub rsp, %d\n" stack_size |> String.cat prologue
  in
  let body = List.map string_of_instruction insts |> String.concat "\n" in
  Printf.sprintf "%s\n%s\n%s\n" prologue body epilogue
