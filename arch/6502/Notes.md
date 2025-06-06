# 6502.MINT

_this still a stub_

A still raw [Mint](https://github.com/monsonite/MINT) for 6502.

Original MINT is for Z80, a 16bit register CPU, and 6502 is a 8bit register CPU, there is a lot of diferences. 

The code and jump tables are bigger and need be fine touched.

Use to clean: make clean

Use to compile: make

Use to run: make run 

Still not working

## Notes

_25/02/2025

    Verifiy each funtion and interpret loop.
    not working, just pulling in/out stack

_12/02/2025

    Reviews and custom local changes

_05/02/2025

    Changes in configuration file 6502-MINT.cfg
    running tests with run6502

_04/02/2025

    Compiled Ok, 2128 code, 960 tables, now verify each function.

_29/01/2025

Rewrite Makefile and compiling complete, less than 4k, need to review of functions and macros 

_28/01/2025

Review of MINT-6502, using ideas from milliforth-6502, 6502-toy forth, simplify io using emulator run6502
all system I/O dependent functions in one file

_02/11/2023

No progress with lib6502 emulator run6502, the getchar and putchar are buffered and give strange results for the parser and gets. Trying put some break/print statments as usual debug suffers same problem.

_01/11/2023

Review of stacks load/save indexes overload. With both stacks at page zero offset by X, the options: Any operation with any stack must load/save index; Only return stack operations must load/save both indexes; 

_31/10/2023_

All code done, trying use emulators to test. 
    With lib6502, the getch and putch are bufferd then only works with crlf
    With asm80, wont compile giving a undefinde error

_24/10/2023_
      
Most of code done, looking for a emulator to tests
      
_20/10/2023_

It is a 6502, a 8-bit CPU doing 16-bit things, then better safe than sorry, tweak it if need more speed.

      a Z80, does 16-bit add (ADD HL, DE) in 12 cycles and use 1 byte, at 10 MHz  
      a 6502, does a 16-bit add it in 13 cycles and uses 9 bytes, 1 MHz 

_10/10/2023_

Start of coding, using Z80-MINT as reference and some code of [Forth immu](https://github.com/agsb/immu) and [6502toy](https://github.com/agsb/6502toy)
      
## Code Design

_No plans for multi-task or multi-user or garbage collector or flush flash eeproms._ 

   - the code is for ROM and uses less than 4096 bytes.
   - if change for RAM, caller must save A, X, Y and leave at least 16 words at hardware stack 
   - a cell is 16-bit.
   - terminal input buffer is limited to 255 bytes, also any macro, function, input or output.  
   - extense use of Y indexed indirect addressing and absolute offsets by X
   - uses 16 bytes at page zero $F0 to $FF.
   - all input, output and macros are stored on the heap in chunks of up to 255 bytes.
   - all MINT called functions ended with \_
   - all routines must end with: _jmp next_ or _jmp (vNext)_ or _jmp drop__ 
   - the code references was (still) not ordered into pages of "half address and mapped pages".
   - the addresses of the routines were organized into lists of "low bytes and high bytes" for parallel query.
   - It uses more 256 bytes than original Z80 layout, but allow almost 256 references per list.

## Stacks

_Charles Moore says 22 levels is enough for Forth._

Two options for stacks. A "slim", with stacks of 24 words deep and allocated at zero page direct indexed by X, faster and with less bytes; Other "wide" with stacks of 128 cells deep, using absolute address direct indexed by X and round-robin. Both grows backwards. 

Both data stack and return stack are indexed by X, and load the index at start and save the index at end, if changed. This leaves SP, X and Y, free for general use. 

This mode of stack operation, direct indexed by X, allow direct memory access for operations, without need of pull and push. 

More in [Stacks](https://github.com/agsb/6502.MINT/blob/main/arch/6502/stks/Stacks.md)
     
## MINT Changes

1. Depends on external BIOS for:
   
      - _hitc_, check if a key pressed,
      - _getc_, wait and receive a byte,
      - _putc_, wait and transmit a byte
      - _tick_, counts clock ticks, *not yet*
        
      A basic 6551 code for hitchar, getchar, putchar and setup included, but must adjust the ACIA address.
   
3. Expanded mint variables
   
      - _alt-a_, used for vS0, start of data stack, *hardcoded*
      - _alt-f_, used for vR0, start of return stack  *hardcoded*
      - _alt-g_, used for vNext, indirect dispatcher  
      - _alt-r_, return the return stack pointer
      - _alt-s_, return the data stack pointer

5. Expanded mint functions:

      - _alt-U_, classic Forth R> 
      - _alt-V_, classic Forth >R 
      - _alt-H_, verify if a key was hit 

      still not assigned
            add_store ( a b c -- a ) [c] += b 
            sub_store ( a b c -- a ) [c] -= b
    
      As is in code:
   
      - _*_, multiplication 16x16 leaves a reminder of product by 65536 and a quotient of product by 65536
      - _/_, division 16x16 leaves a reminder by divisor and a quotient by divisor   

7. A 6502 is a memory mapped I/O CPU, then InPort is mapped to cFetch and OutPort is mapped to cStore.
  
8. All variables and user macros are composed in groups,

      - Each group have 32 cells, 26 are accessed from 'a' to 'z', and last 6 cells below 'z'.
      - User functions extended for 5 groups.  

10. Extra string functions, both ends at crlf or asciiz:

      - _gets\__, gets a line into a buffer, 
      - _puts\__, puts a line from a buffer,  

11. The parser is pos-incremented. No need decrement instruction pointer before goto parser. Easy check for end of buffer.

12. FALSE is _0x00_ and TRUE is non FALSE, maybe 0x01 to 0xFF.
      - Why? Easy Zero flag test, beq is FALSE, bne is TRUE.

13. Loops counts from 0 to 1 till 65535.
      - and begin tha flame wars...

