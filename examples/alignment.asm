.intel_syntax noprefix
.global _start

.text

_start:
    sub rsp, 120
    mov rax, 60
    mov rdi, rsp
    syscall

.data

message:
    .string "Hello world!\n"
