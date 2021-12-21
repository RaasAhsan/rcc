.intel_syntax noprefix
.global _start

.text

_start:
    push rbp
    mov rbp, rsp

    mov rsp, 20

    mov DWORD PTR [rbp - 4], 0x20

    mov rax, 60
    mov rdi, 3
    syscall

.data

message:
    .string "Hello world!\n"
