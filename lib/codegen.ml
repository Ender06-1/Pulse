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
|}

and epilogue = {| 
  call dump
  
  mov rax, 60
  mov rdi, 0
  syscall
|}

type value = Reg of string | Val of string

type instruction =
  | Mov of value * value
  (* Operations *)
  | Add of value * value
  | Sub of value * value

let rec codegen (ir : Ir.instruction list) : instruction list =
  let value_of_ir (v : Ir.value) : value =
    match v with Var v -> Reg v | Integer i -> Val (Int64.to_string i)
  in
  let aux (i : Ir.instruction) : instruction list =
    match i with
    | Copy (var, v) -> [ Mov (Reg var, value_of_ir v) ]
    | Add (var, l, r) ->
        [ Mov (Reg var, value_of_ir r); Add (Reg var, value_of_ir l) ]
    | Sub (var, l, r) ->
        [ Mov (Reg var, value_of_ir r); Sub (Reg var, value_of_ir l) ]
  in
  List.map aux ir |> List.concat

and string_of_value (v : value) : string =
  match v with Reg r -> r | Val v -> v

and string_of_instruction (i : instruction) : string =
  match i with
  | Mov (vl, vr) ->
      Printf.sprintf "mov %s, %s" (string_of_value vl) (string_of_value vr)
  | Add (vl, vr) ->
      Printf.sprintf "add %s, %s" (string_of_value vl) (string_of_value vr)
  | Sub (vl, vr) ->
      Printf.sprintf "sub %s, %s" (string_of_value vl) (string_of_value vr)
