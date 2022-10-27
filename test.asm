section .data
    msg: db "Hello, World!", 10
    msglen: EQU $- msg

section .text
global _start
_start:
    mov rax, 1
    mov rbx, 1
    mov rsi, msg
    mov rdx, msglen
    syscall    

    mov rax, 60
    mov rbx, 0
    syscall
