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
    mov eax, edi
    mov edx, 1
    add eax, edx
    mov DWORD PTR [rbp - 4], eax
    mov eax, DWORD PTR [rbp - 4]
    cmp eax, 0
    je L3
    mov eax, 4
    mov rsp, rbp
    pop rbp
    ret
    jmp L4
L3:
    mov eax, 5
    mov rsp, rbp
    pop rbp
    ret
L4:
L2:
    mov rsp, rbp
    pop rbp
    ret

.global main
main:
    push rbp
    mov rbp, rsp
    sub rsp, 24
    mov DWORD PTR [rbp - 12], LC1
    mov eax, DWORD PTR [rbp - 12]
    mov DWORD PTR [rbp - 4], eax
    mov eax, 3
    mov rsp, rbp
    pop rbp
    ret
    mov rsp, rbp
    pop rbp
    ret

.data
LC1:
.string "hello world"
