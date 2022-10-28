nasm -g -f elf64 -o ./compiled/$1.o ./compiled/$1.asm
ld -o $1 ./compiled/$1.o

gdb $1
