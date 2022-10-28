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
    mov rax, 60
    pop rdi
    syscall

_start:
    jmp ins0
ins0:
    push exit_code
    call exit
ins1:
    push lenhello
    push hello
    call write
ins2:
    push lenhello
    push hello
    call write
stop:
    push 0
    call exit
section .data
    hello: db "Hello World!", 10
    lenhello: EQU $- hello
