section .data
section .text
global _start
_start:
   jmp ip1
ip1:
   mov rax, 1
   mov rbx, 1
   mov rsi, string0
   mov rdx, string0len
   syscall
ip2:
   mov rax, 60
   mov rbx, 0
   syscall
