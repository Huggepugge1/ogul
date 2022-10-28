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

exit:
    pop rax
    pop rbx
    push rax
    mov rax, 60
    syscall

_start:
    jmp ins0
ins0:
    push strlen0
    push str0
    call write
ins1:
    push strlen1
    push str1
    call write
stop:
    push 0
    call exit
section .data
    str0: db "Hello World!", 10
    strlen0: EQU $- str0
    str1: db "Hello bitch!!", 10
    strlen1: EQU $- str1
