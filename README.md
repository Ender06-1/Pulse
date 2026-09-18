# Pulse

Simple programming language.

# Usage

`pulse [OPTIONS] <file>.pulse` then exec `./<file>`

OPTIONS:
- `--dump-tokens`: print the tokens from the lexer
- `--dump-ast`: print the ast in s-expr form
- `--dump-ir`: print the pulse ir

## Features

- u64 numbers
- unsigned 64 bit addition, substraction, multiplication, division and modulo
- variables
- if/else conditions (then branch on cond != 0)
- unconditional loops (for {...})

## TODO

- Conditional loops
- Break/Continue
- Functions
- Stdlib/main and multi file compilation
- polymorphic print (u64, char)
- Turing completeness

## Sys V ABI conventions

```asm
; Args order: rdi, rsi, rdx, rcx, r8, r9, stack...
; Caller-saved (can use freely in function):
;   rax, rcx, rdx, rsi, rdi, r8-11, xmm0-15
; Callee-saved (must save before use in function):
;   rbx, rsp, rbp, r12-15
; rsp must be aligned to 16 bytes before any fn/syscall calls
```
