segment .text
dump:
        mov r9, -3689348814741910323
        sub rsp, 40
        mov BYTE [rsp+31], 10
        lea rcx, [rsp+30]
.L2:
        mov rax, rdi
        lea r8, [rsp+32]
        mul r9
        mov rax, rdi
        sub r8, rcx
        shr rdx, 3
        lea rsi, [rdx+rdx*4]
        add rsi, rsi
        sub rax, rsi
        add eax, 48
        mov BYTE [rcx], al
        mov rax, rdi
        mov rdi, rdx
        mov rdx, rcx
        sub rcx, 1
        cmp rax, 9
        ja  .L2
        lea rax, [rsp+32]
        mov edi, 1
        sub rdx, rax
        xor eax, eax
        lea rsi, [rsp+32+rdx]
        mov rdx, r8
        mov rax, 1
        syscall
        add rsp, 40
        ret
global _start
_start:
op0:
	push 34
op1:
	push 35
op2:
	pop rbx
	pop rax
	add rax, rbx
	push rax
op3:
	pop rdi
	call dump
op4:
	push 500
op5:
	push 80
op6:
	pop rbx
	pop rax
	sub rax, rbx
	push rax
op7:
	pop rdi
	call dump

        mov rax, 60
        mov rbx, 0
        syscall