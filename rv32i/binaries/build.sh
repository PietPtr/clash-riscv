#!/bin/bash

riscv32-unknown-elf-gcc -o bin.o $1
riscv32-unknown-elf-objdump -d bin.o | grep "\s[a-f0-9]\{8\}.*" -o | awk '{$1=$1;print}' > $1.bin
