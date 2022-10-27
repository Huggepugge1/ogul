section .data
msg: db "Hello, World!", 10
msglen: EQU $- msg

section .text
global _start
write:
    pop rax
    pop rsi
    pop rdx
    push rax
    mov rax, 1
    mov rbx, 1
    syscall
    ret

_start:
    push msglen
    push msg
    call write
    mov rax, 60
    mov rbx, 0
    syscall
