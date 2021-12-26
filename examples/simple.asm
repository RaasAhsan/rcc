.intel_syntax noprefix
.text
.global add
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

.global main
main:
    push rbp
    mov rbp, rsp
    sub rsp, 24
    mov DWORD PTR [rbp - 4], 5
    mov DWORD PTR [rbp - 8], 6
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

