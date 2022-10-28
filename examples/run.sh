#!/bin/bash

cargo run -- $1.ogul ./compiled/$1
nasm -f elf64 -o ./compiled/$1.o ./compiled/$1.asm
ld -o ./$1 ./compiled/$1.o

echo "


"

./$1
