#!/usr/bin/python3

# WARNING: this thing only actually works if the functions are right next to
# eachother, because otherwise the relative jumps don't work

import sys, os, math

def extractfunction(function, text):
    instructions = []
    found = False
    for line in text:
        if line == "":
            found = False

        if found:
            instructions.append("0x" + line.split("\t")[1].strip())

        if "<" + function + ">:" in line:
            found = True

    return instructions



functions = ["main"]

if len(sys.argv) < 2:
    print("No input file supplied")
    exit()
elif len(sys.argv) == 2:
    print("Building file " + sys.argv[1] + "...")
else:
    functions += sys.argv[2:]

print("  with functions: ")
for f in functions:
    print("  - " + f)

os.popen("riscv32-unknown-elf-gcc -o bin.o " + sys.argv[1])

prgmtext = os.popen("riscv32-unknown-elf-objdump -d bin.o").read().split('\n')

binary = []

for f in functions:
    binary += (extractfunction(f, prgmtext))


binary += ["0"] * (2 ** math.ceil(math.log2(len(binary))) - len(binary))

print(":>".join(binary) + ":>Nil")
