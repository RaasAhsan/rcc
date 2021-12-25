.intel_syntax noprefix
.global _start

.extern main

.text

_start:
    call main

    mov rdi, rax
    mov rax, 60
    syscall
