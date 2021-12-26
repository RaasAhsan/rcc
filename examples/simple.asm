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
    sub rsp, 40
    mov DWORD PTR [rbp - 8], 5
    mov DWORD PTR [rbp - 16], 6
    mov edi, DWORD PTR [rbp - 8]
    mov esi, DWORD PTR [rbp - 16]
    call add
    mov DWORD PTR [rbp - 28], eax
    mov eax, DWORD PTR [rbp - 28]
    mov DWORD PTR [rbp - 24], eax
    mov eax, DWORD PTR [rbp - 24]
    mov rsp, rbp
    pop rbp
    ret
    mov rsp, rbp
    pop rbp
    ret
