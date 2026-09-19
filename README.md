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
- u64 addition, substraction, multiplication, division and modulo
- u64 gt, lt, ge, le, equality and inequality comparisons
- variables
- if/else conditions (then branch on cond != 0)
- conditional loops (for ... {...}), with break and continue

## TODO

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
