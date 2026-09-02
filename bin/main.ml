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

type instruction = Mov of int64 | Add of int64

let codegen (insts : instruction list) : string =
  let string_of_inst inst =
    match inst with
    | Mov v -> Printf.sprintf "mov rdi, %Lu" v
    | Add v -> Printf.sprintf "add rdi, %Lu" v
  in
  let first =
    List.map string_of_inst insts
    |> List.map (String.cat "  ")
    |> String.concat "\n" |> String.cat prologue
  in
  String.cat first epilogue

type ast = Num of int64 | Plus of ast * ast

let insts_of_ast (tree : ast) : instruction list =
  let rec aux tree =
    match tree with
    | Num v -> [ Add v ]
    | Plus (l, r) -> List.append (aux l) (aux r)
  in
  match aux tree with
  | [] -> failwith "empty ast"
  | Add v :: tl -> Mov v :: tl
  | _ -> failwith "unreachable"

let () =
  let tree = Plus (Plus (Num 34L, Num 35L), Num 36L) in
  Out_channel.with_open_text "main.pulse.asm" (fun c ->
      Out_channel.output_string c (insts_of_ast tree |> codegen))
