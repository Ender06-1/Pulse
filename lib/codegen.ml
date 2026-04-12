let gen (tree : Ast.t) : string =
  match tree with
  | Return i ->
      {|format ELF64 executable 3

segment readable executable

entry _start
_start:
    mov rdi, |}
      ^ Int64.to_string i ^ {|
    mov rax, 60
    syscall
|}
