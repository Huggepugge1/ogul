section .text
global _start

allocate_str:
    pop rax
    pop r8
    pop r9
    pop r10
    push r10
    push r9
    push r8
    push rax 
    mov rax, [counter]
    mov rbx, r9[rax]
    mov byte [r10 + rax], bl
    add qword [counter], 1
    mov rax, [counter]
    mov rbx, r8
    cmp rbx, rax
    jne allocate_str
    mov rax, [counter]
    mov word [r10 + rax], 0
    ret

write:
    pop rax
    pop rdx
    pop rsi
    push rax
    mov rax, 1
    mov rbx, 1
    syscall
    ret

exit:
    pop rax
    mov rax, 60
    pop rdi
    syscall

_start:
    jmp ins0

ins0:
    mov qword [counter], 0
    push hello
    push str0
    push len0
    call allocate_str

ins1:
    push hello
    push len0
    call write

ins2:
    mov qword [exit_code], 0

ins3:
    push qword [exit_code]
    call exit

stop:
    push 0
    call exit

section .data
    str0: db "Hello, World!", 10""
    len0: EQU $- str0

section .bss
    counter: resq 1
    exit_code: resq 1
    hello: resb 10000
