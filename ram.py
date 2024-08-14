import sys
import os

program_filepath = sys.argv[1]

print("[CMD] Parsing")

# Tokenize the program
program_lines = []
with open(program_filepath, "r") as program_file:
    program_lines = [line.strip() for line in program_file.readlines()]

program = []
labels = {}

for line in program_lines:
    parts = line.split()
    
    # Skip empty lines
    if not parts:
        continue
    
    opcode = parts[0]
    
    # Check for label definitions (e.g., L1:)
    if opcode.endswith(':'):
        label_name = opcode[:-1]
        labels[label_name] = len(program)  # Store the index of the label
        program.append(label_name + ":")
        continue
    
    # Store opcode token
    program.append(opcode)
    
    # Handle each opcode
    if opcode == "PUSH":
        number = int(parts[1])
        program.append(number)
    elif opcode == "PRINT":
        # Parse string literal
        string_literal = ' '.join(parts[1:])[1:-1]
        program.append(string_literal)
    elif opcode in ["JUMP.EQ.0", "JUMP.GT.0"]:
        # Store the label reference
        label = parts[1]
        program.append(label)

# Bookkeep string literals
string_literals = []
for ip in range(len(program)):
    if program[ip] == "PRINT":
        string_literal = program[ip + 1]
        program[ip + 1] = len(string_literals)
        string_literals.append(string_literal)

# Compile to assembly
print("[CMD] Compiling")

asm_filename = program_filepath[:-4] + ".asm"
with open(asm_filename, "w") as out:
    out.write("""; -- Header --\nsection .bss\n""")
    out.write("read_number resq 1\n")  # 64-bit resq instead of 32-bit resd
    out.write("section .data\n")
    out.write("read_format db \"%d\", 0\n")
    
    for i, string_literal in enumerate(string_literals):
        out.write(f"string_{i} db '{string_literal}', 0\n")
    
    out.write("section .text\n")
    out.write("global main\n")
    out.write("extern ExitProcess\n")
    out.write("extern printf\n")
    out.write("extern scanf\n")
    out.write("main:\n")
    out.write("\tPUSH rbp\n")
    out.write("\tMOV rbp, rsp\n")
    out.write("\tSUB rsp, 32\n")
    
    ip = 0
    while ip < len(program):
        opcode = program[ip]
        ip += 1
        
        # Handle labels
        if opcode.endswith(":"):
            out.write(f"{opcode}\n")
            continue
        
        elif opcode == "PUSH":
            number = program[ip]
            ip += 1
            out.write(f"\tMOV rax, {number}\n")
            out.write(f"\tPUSH rax\n")
        
        elif opcode == "POP":
            out.write("\tPOP rax\n")
        
        elif opcode == "ADD":
            out.write("\tPOP rax\n")
            out.write("\tPOP rbx\n")
            out.write("\tADD rax, rbx\n")
            out.write("\tPUSH rax\n")
        
        elif opcode == "SUB":
            out.write("\tPOP rax\n")
            out.write("\tPOP rbx\n")
            out.write("\tSUB rax, rbx\n")
            out.write("\tPUSH rax\n")
        
        elif opcode == "PRINT":
            string_literal_index = program[ip]
            ip += 1
            out.write(f"\tLEA rcx, [rel string_{string_literal_index}]\n")  # Use rel
            out.write("\tXOR eax, eax\n")
            out.write("\tCALL printf\n")
        
        elif opcode == "READ":
            out.write("\tLEA rdx, [rel read_number]\n")  # Use rel
            out.write("\tLEA rcx, [rel read_format]\n")  # Use rel
            out.write("\tXOR rax, rax\n")
            out.write("\tCALL scanf\n")
            out.write("\tMOV rax, [rel read_number]\n")  # Use rel
            out.write("\tPUSH rax\n")
        
        elif opcode == "JUMP.EQ.0":
            label = program[ip]
            ip += 1
            out.write("\tPOP rax\n")
            out.write("\tCMP rax, 0\n")
            out.write(f"\tJE {label}\n")
        
        elif opcode == "JUMP.GT.0":
            label = program[ip]
            ip += 1
            out.write("\tPOP rax\n")
            out.write("\tCMP rax, 0\n")
            out.write(f"\tJG {label}\n")
        
        elif opcode == "HALT":
            out.write("\tCALL ExitProcess\n")
    
    out.write("\tXOR rax, rax\n")
    out.write("\tCALL ExitProcess\n")

print("[CMD] Assembling")
assemble_command = f"nasm -f elf64 {asm_filename} -o {asm_filename[:-4] + '.o'}"
os.system(assemble_command)

print("[CMD] Linking")
link_command = f"gcc -o {asm_filename[:-4] + '.exe'} {asm_filename[:-4] + '.o'} -m64 -nostartfiles -fPIC"
os.system(link_command)

print("[CMD] Running")
run_command = f"{asm_filename[:-4] + '.exe'}"
os.system(run_command)
