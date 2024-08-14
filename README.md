### Overview

The script provided is a basic compiler for a simple stack-based programming language. The language supports basic operations like pushing numbers onto a stack, arithmetic operations (addition and subtraction), reading input from the user, printing strings, conditional jumps based on stack values, and halting execution. The compiler takes a source code file as input, tokenizes it, processes labels, compiles it to assembly, and then assembles and links the assembly code into an executable binary.

### Workflow

1. **Parsing the Input File**
   - The script begins by reading the program's source code from a file specified as a command-line argument (`program_filepath`).
   - The source code is split into individual lines, and each line is further tokenized into its components (e.g., opcode, operands).
   - Labels (e.g., `L1:`) are identified and stored in a dictionary, mapping each label to its corresponding position in the program.

2. **Tokenization**
   - The tokenization process involves splitting each line of the program into its constituent parts.
   - Empty lines are skipped, and the opcode (e.g., `PUSH`, `PRINT`) is identified.
   - If a line contains a label definition (e.g., `L1:`), the label is stored in the `labels` dictionary with its associated position in the program. This allows for resolving jumps to these labels later.

3. **Handling String Literals**
   - String literals used in the `PRINT` command are stored in a list called `string_literals`.
   - Each occurrence of a `PRINT` command in the program is associated with an index pointing to the corresponding string in `string_literals`.

4. **Compiling to Assembly**
   - The program is compiled to x86-64 assembly language, targeting the NASM assembler.
   - The assembly code is divided into three sections: `.bss` (uninitialized data), `.data` (initialized data), and `.text` (code).
   - In the `.bss` section, a variable (`read_number`) is reserved for storing integers read from the user.
   - The `.data` section contains the format string for reading integers and the string literals used in the `PRINT` command.
   - The `.text` section contains the main logic of the program, starting with a `main` function that sets up the stack frame.

5. **Handling OpCodes**
   - The core of the program is a loop that iterates through the tokenized program, generating corresponding assembly instructions for each opcode.
   - **PUSH**: Pushes a number onto the stack by moving it into the `rax` register and then pushing it onto the stack.
   - **POP**: Pops the top value off the stack into the `rax` register.
   - **ADD** and **SUB**: Perform arithmetic operations by popping two values off the stack, performing the operation, and then pushing the result back onto the stack.
   - **PRINT**: Prints a string by loading the address of the string literal into `rcx` and calling the `printf` function.
   - **READ**: Reads an integer from the user, stores it in `read_number`, and pushes it onto the stack.
   - **JUMP.EQ.0** and **JUMP.GT.0**: Conditional jumps based on the top value of the stack. If the condition is met, the program jumps to the corresponding label.
   - **HALT**: Terminates the program by calling the `ExitProcess` function.

6. **Assembly and Linking**
   - The generated assembly file is assembled into an object file using NASM.
   - The object file is then linked using GCC, creating an executable binary.

7. **Execution**
   - Finally, the script attempts to run the generated executable.

### Data and Memory Flow

- **Stack-Based Operations**: The language uses a stack-based approach, where all operations (e.g., arithmetic, comparisons) are performed using values pushed onto and popped from a stack.
  - The stack is a region of memory where values (like integers) are temporarily stored. The stack grows and shrinks as values are pushed onto or popped off it.
  - Each arithmetic operation (e.g., `ADD`, `SUB`) involves popping two values off the stack, performing the operation, and then pushing the result back onto the stack.

- **Memory Sections**:
  - **.bss**: Used to reserve space for variables that are uninitialized, like `read_number`, which stores the value read from the user.
  - **.data**: Stores initialized data, such as string literals and the format string used by `scanf`.
  - **.text**: Contains the program's code, including the logic for each opcode and system calls to external functions (e.g., `printf`, `scanf`, `ExitProcess`).

- **Instruction Pointer**:
  - The program processes each instruction sequentially, using an instruction pointer (`ip`) to keep track of its current position.
  - When a jump instruction (`JUMP.EQ.0`, `JUMP.GT.0`) is encountered, the instruction pointer may be modified to jump to a different position in the program.

### Overview of the Language

This custom language is a stack-based language that operates on a series of simple instructions to perform basic operations like arithmetic, conditional jumps, input/output, and program control. The language is designed to be compiled into assembly code, which is then assembled and linked into an executable file that can be run on a system.

### Type of Language

The language is a low-level, imperative, and procedural programming language. It's stack-based, meaning that most operations are performed on a stack where data is pushed, popped, and manipulated. The language supports basic operations like `PUSH`, `POP`, `ADD`, `SUB`, `PRINT`, `READ`, and conditional jumps like `JUMP.EQ.0` and `JUMP.GT.0`.

### Example Code with Dry Run

#### Example 1: Simple Arithmetic and Printing

```plaintext
PUSH 5
PUSH 3
ADD
PRINT "Result: "
PRINT
HALT
```

**Dry Run:**
1. `PUSH 5`: Pushes the integer 5 onto the stack.
2. `PUSH 3`: Pushes the integer 3 onto the stack.
3. `ADD`: Pops the top two values (5 and 3), adds them, and pushes the result (8) back onto the stack.
4. `PRINT "Result: "`: Prints the string "Result: ".
5. `PRINT`: Pops the top value (8) from the stack and prints it.
6. `HALT`: Stops the program.

**Output:**
```
Result: 8
```

#### Example 2: Conditional Jump

```plaintext
READ
READ
SUB
JUMP.EQ.0 L1
PRINT "not equal"
HALT

L1:
PRINT "equal"
HALT
```

**Dry Run:**
1. `READ`: Reads a number from the user and pushes it onto the stack.
2. `READ`: Reads another number from the user and pushes it onto the stack.
3. `SUB`: Pops the top two values, subtracts the second one from the first, and pushes the result onto the stack.
4. `JUMP.EQ.0 L1`: If the result of the subtraction is zero (meaning the two numbers were equal), jumps to the label `L1`.
5. `PRINT "not equal"`: If the jump didn’t happen, prints "not equal".
6. `HALT`: Stops the program.
7. `L1:`: Label that acts as a jump target.
8. `PRINT "equal"`: If the jump to `L1` happened, prints "equal".
9. `HALT`: Stops the program.

**Possible Outputs:**
- If the numbers are equal: `equal`
- If the numbers are not equal: `not equal`

### Data Flow and Memory Management

- **Stack Operations**: The language primarily operates on a stack where data is pushed (added to the stack) and popped (removed from the stack). The stack is used for temporary storage of numbers and intermediate results.
  
- **Memory Layout**: The language uses memory sections typical for assembly programming:
  - **.bss**: For uninitialized data (e.g., a variable to store a number read from the user).
  - **.data**: For initialized data (e.g., string literals).
  - **.text**: For the executable instructions of the program.

- **Labels and Jumps**: Labels in the code define points to which the program can jump conditionally. These are used to implement control flow like loops and conditionals.

This language is ideal for understanding basic computing principles, low-level programming, and how higher-level constructs like loops and conditionals can be implemented using jumps and labels.

### Summary

The script works as a simple compiler for a stack-based language, translating high-level instructions into low-level assembly code, which is then assembled, linked, and executed. The language's simplicity allows it to demonstrate fundamental concepts of compilers, such as tokenization, label handling, stack-based operations, and code generation. The flow of data and memory through the stack and memory sections reflects how higher-level logic is implemented at the machine level.
