# The Stacks

_this is still a stub_

The 6502 have two peculiar pages, the zero page and stack page, both unique and with 256 bytes. 

All sub-routines calls (JSR) and returns (RTS) uses the stack page for 16-bit pointers, also do the indirect indexed and indexed indirect modes. Those are valuable resources.

Almost 6502 indexed stacks implementations are 128 words, Charles Moore says 22 is enough fo Forth. These are most commom: 

## at hardware stack SP

      .macro push lsb, msb 
            LDA \lsb; PHA; LDA \msb; PHA; 
      .endmacro ; 12 cycles
      
      .macro pull lsb, msb 
            PLA; STA \msb; PLA; STA \lsb; 
      .endmacro ; 12 cycles
      
allow 128 words deep at hardware stack.
_any operations with values at stack must do pushs and pulls_.
_multitask or multiuser could split or copy the stack_

## at page zero indexed by X
      
      .macro push idz, ptrz, lsb, msb 
            LDX \idz; DEX; LDA \msb; STA \ptrz, X; DEX; LDA \lsb; STA \ptrz, X; STX \idz
      .endmacro      ;  cycles
      
      .macro pull idz, ptrz, lsb, msb 
            LDX \idz; LDA \ptrz, X; STA \msb; INX; LDA \ptrz, X; STA \lsb; INX; STX \idz 
      .endmacro

allow 124 words deep at page zero, uses 4 bytes at page zero, stack size at idz+1.
_any operations with values at stack must do pushs and pulls_.
_multitask or multiuser could split or copy the stack_

## indirect by page zero indexed by Y

      .macro push idz, ptrz, lsb, msb 
            LDY \idz; DEY; LDA \msb; STA (\ptrz), Y; DEY; LDA \lsb; STA (\ptrz), Y; STY \idz} 
      .endmacro      ;  cycles
      
      .macro pull idz, ptrz, lsb, msb 
            LDY \idz; LDA (\ptrz), Y; STA \msb; INY; LDA (\ptrz), Y; STA \lsb; INY; STY \idz} 
      .endmacro

allow 128 words deep at any address of memory, uses 4 bytes at page zero, stack size at idz+1.
_any operations with values at stack must do pushs and pulls_.
_multitask or multiuser could change the reference at ptrz and could split or copy the stack_

## absolute address indexed by Y
      
      .macro push idz, ptr, lsb, msb 
            LDY \idz; LDA \msb; STA \ptr - 1, Y; LDA \lsb; STA \ptr - 2, Y; DEY; DEY; STY \idz} 
      .endmacro      ;  cycles
      
      .macro pull idz, ptrz, lsb, msb 
            LDY \idz; LDA \ptr + 0, Y; STA \lsb; LDA \ptr + 1, Y; STA \msb; INY; INY; STY \idx} 
      .endmacro

allow 128 words deep at any address of memory, uy2ses 4 bytes at page zero, stack size at idz+1.
_any operations with values at stack could be at direct offset_
_multitask or multiuser could split or copy the stack_

## split absolute addres indexed by Y
      
      .macro push idz, ptr_lo, ptr_hi, lsb, msb 
            LDY \idz; LDA \msb; STA \ptr_lo - 1, Y; LDA \lsb; STA \ptr_hi - 1, Y; DEY; STY \idz} 
      .endmacro      ;  cycles
      
      .macro pull idz, ptrz, lsb, msb 
            LDY \idz; LDA \ptr_lo + 0, Y; STA \lsb; LDA \ptr_hi + 0, Y; STA \msb; INY; STY \idx} 
      .endmacro

allow 256 words deep at any address of memory, uses 6 bytes at page zero, stack size at idz+1.
_any operations with values at stack could be at direct offset_
_multitask or multiuser could split or copy the stack_
_multitask or multiuser need split the stack_

#### what do 

In 6502 code, to pass a byte between memory, need use LDA and STA (there are exotic alternatives, but all uses Accumulator)

Using absolute address indirect access for stacks. It provides the most fast overall implementation.

pros:
   offsets inline from a fixed reference
   direct memory access and exchange
   128 deep stack in round-robin

cons:
   can not change fixed reference

multitask and multiuser :
   Then could split 5 stacks for users or tasks, more than must exchange stacks values and include checks for stack limits.

  low memory
   -4  LSB
   -3  MSB
   -2  LSB
   -1  MSB
    0  LSB TOS
   +1  MSB
   +2  LSB NOS
   +3  MSB
   +4  LSB
   +5  MSB
 high memory

; to keep code safe do not using "fall throught".
; uses A, Y, X caller must saves.
; needs 2 levels of hardware stack
; uses 4 bytes in page zero as temporary
; uses 6 bytes in memory for internal use
;


    
