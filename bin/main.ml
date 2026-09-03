let ( let* ) = Result.bind

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

type token_type = Int of string | PlusSign | EOF
type lexer = { input : string; pos : int }

let rec next (lexer : lexer) : (token_type * lexer, string) result =
  let rec lex_int lexer acc =
    match String.get lexer.input lexer.pos with
    | '0' .. '9' as c ->
        lex_int
          { lexer with pos = lexer.pos + 1 }
          (String.make 1 c |> String.cat acc)
    | _ -> (Int acc, lexer)
  in
  if lexer.pos >= String.length lexer.input then Ok (EOF, lexer)
  else
    match String.get lexer.input lexer.pos with
    | '0' .. '9' -> Ok (lex_int lexer "")
    | '+' -> Ok (PlusSign, { lexer with pos = lexer.pos + 1 })
    | ' ' | '\n' -> next { lexer with pos = lexer.pos + 1 }
    | _ -> Error "invalid character"

(* Grammar *)
(* program = add_expr EOF ;*)
(* add_expr = prim_expr ( "+" add_expr )* ; *)
(* prim_expr = num *)

let rec parse_program (lexer : lexer) : (ast * lexer, string) result =
  let* expr, lexer = parse_add_expr lexer in
  let* tt, lexer = next lexer in
  match tt with EOF -> Ok (expr, lexer) | _ -> Error "expected EOF"

and parse_add_expr (lexer : lexer) : (ast * lexer, string) result =
  let* prim, lexer = parse_prim_expr lexer in
  let* tt, lexer = next lexer in
  match tt with
  | PlusSign ->
      let* exp, lexer = parse_add_expr lexer in
      Ok (Plus (prim, exp), lexer)
  | _ -> Ok (prim, lexer)

and parse_prim_expr (lexer : lexer) : (ast * lexer, string) result =
  let* tt, lexer = next lexer in
  match tt with
  | Int n -> Ok (Num (Int64.of_string n), lexer)
  | _ -> Error "expected num"

let () =
  let input =
    In_channel.with_open_text (Array.get Sys.argv 1) In_channel.input_all
  in
  let lexer = { input; pos = 0 } in
  let ret_code =
    Result.fold
      ~ok:(fun (tree, _) ->
        Out_channel.with_open_text "main.pulse.asm" (fun c ->
            Out_channel.output_string c (insts_of_ast tree |> codegen));
        let ret_code = Sys.command "nasm -felf64 main.pulse.asm" in
        if ret_code <> 0 then ret_code
        else Sys.command "ld main.pulse.o -o main.pulse.exe")
      ~error:(fun e ->
        print_endline e;
        1)
      (parse_program lexer)
  in
  exit ret_code
