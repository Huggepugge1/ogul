#!/bin/bash

cargo run
nasm -f elf64 -o ../compiled/$1.o ../compiled/$1.asm
ld -o ../compiled/$1 ../compiled/$1.o

echo "



"

../compiled/$1
