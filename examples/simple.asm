.intel_syntax noprefix
.global _start

.text

_start:
    call main

    mov rdi, rax
    mov rax, 60
    syscall

main:
    push rbp
    mov rbp, rsp
    sub rsp, 40
    mov eax, 5
    mov DWORD PTR [rbp - 4], eax
    mov eax, 6
    mov DWORD PTR [rbp - 8], eax
    mov eax, DWORD PTR [rbp - 4]
    mov edx, DWORD PTR [rbp - 8]
    add eax, edx
    mov DWORD PTR [rbp - 24], eax
    mov eax, DWORD PTR [rbp - 24]
    mov DWORD PTR [rbp - 12], eax
    mov eax, DWORD PTR [rbp - 12]
    mov edx, DWORD PTR [rbp - 8]
    add eax, edx
    mov DWORD PTR [rbp - 28], eax
    mov eax, DWORD PTR [rbp - 28]
    mov DWORD PTR [rbp - 16], eax
    mov eax, DWORD PTR [rbp - 16]
    mov edx, DWORD PTR [rbp - 12]
    add eax, edx
    mov DWORD PTR [rbp - 32], eax
    mov eax, DWORD PTR [rbp - 32]
    mov DWORD PTR [rbp - 20], eax
    mov eax, DWORD PTR [rbp - 16]
    mov rsp, rbp
    pop rbp
    ret
    mov rsp, rbp
    pop rbp
    ret

.data

message:
    .string "Hello world!\n"
