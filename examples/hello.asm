.intel_syntax noprefix
.global _start

.text

_start:
    push rbp
    pop rbp
    mov rbp, rsp

    sub rsp, 20

    sub DWORD PTR [rbp - 4], 0x20
    mov DWORD PTR [rbp - 4], eax

    mov rax, 60
    mov rdi, 3
    syscall

.data

message:
    .string "Hello world!\n"
