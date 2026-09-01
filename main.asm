global _start

section .text

; Args order: rdi, rsi, rdx, rcx, r8, r9, stack...
; Caller-saved (can use freely in function):
;   rax, rcx, rdx, rsi, rdi, r8-11, xmm0-15
; Callee-saved (must save before use in function):
;   rbx, rsp, rbp, r12-15
; rsp must be aligned to 16 bytes before any fn/syscall calls

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
  mov rdi, 100
  call dump

  mov rax, 60
  mov rdi, 0
  syscall

