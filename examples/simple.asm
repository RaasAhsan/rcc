.intel_syntax noprefix
.global _start

.text

_start:
    call main

    mov rdi, rax
    mov rax, 60
    syscall

add:
    push rbp
    mov rbp, rsp
    sub rsp, 8
    mov eax, edi
    mov edx, esi
    add eax, edx
    mov DWORD PTR [rbp - 4], eax
    mov eax, DWORD PTR [rbp - 4]
    mov rsp, rbp
    pop rbp
    ret
    mov rsp, rbp
    pop rbp
    ret

main:
    push rbp
    mov rbp, rsp
    sub rsp, 24
    mov eax, 5
    mov DWORD PTR [rbp - 4], eax
    mov eax, 6
    mov DWORD PTR [rbp - 8], eax
    mov edi, DWORD PTR [rbp - 4]
    mov esi, DWORD PTR [rbp - 8]
    call add
    mov DWORD PTR [rbp - 16], eax
    mov eax, DWORD PTR [rbp - 16]
    mov DWORD PTR [rbp - 12], eax
    mov eax, DWORD PTR [rbp - 12]
    mov rsp, rbp
    pop rbp
    ret
    mov rsp, rbp
    pop rbp
    ret

.data

message:
    .string "Hello world!\n"