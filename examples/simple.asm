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

.global foo
foo:
    push rbp
    mov rbp, rsp
    sub rsp, 8
    mov eax, edi
    cmp eax, 0
    je L1
    mov eax, 3
    mov rsp, rbp
    pop rbp
    ret
    jmp L2
L1:
    mov eax, 4
    mov rsp, rbp
    pop rbp
    ret
L2:
    mov rsp, rbp
    pop rbp
    ret

.global main
main:
    push rbp
    mov rbp, rsp
    sub rsp, 24
    mov edi, 1
    call foo
    mov DWORD PTR [rbp - 8], eax
    mov eax, DWORD PTR [rbp - 8]
    mov DWORD PTR [rbp - 4], eax
    mov eax, DWORD PTR [rbp - 4]
    mov rsp, rbp
    pop rbp
    ret
    mov rsp, rbp
    pop rbp
    ret

