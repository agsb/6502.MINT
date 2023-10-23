# The Stacks

_this is still a stub_

_Charles Moore says 22 levels is enough for Forth._

## Back stack

The 6502 have two peculiar pages, the zero page and stack page, both unique and with 256 bytes. All sub-routines calls (JSR) and returns (RTS) uses the stack page for 16-bit pointers, also the indirect indexed and indexed indirect modes uses page zero. Those are valuable resources.

Almost 6502 typical stack implementations does: Allow till 128 words deep stack; Any operation with values at stack must do pushs and pulls. A multitask or multiuser system must split or copy the stack.

These are most commom: 

### at hardware stack SP

      .macro push stk, lsb, msb 
            LDA \stk; TSX; STX \stk; TAX; TXS;
            LDA \lsb; PHA; LDA \msb; PHA; 
            LDA \stk; TSX; STX \stk; TAX; TXS;
      .endmacro ; 
      
      .macro pull stk, lsb, msb
            LDA \stk; TSX; STX \stk; TAX; TXS;
            PLA; STA \msb; PLA; STA \lsb; 
            LDA \stk; TSX; STX \stk; TAX; TXS;
      .endmacro ;  

Uses the hardware stack, and must be split in 3 parts, one for inline code, one for data stack, one for return stack;
Minimal cycles when stk, lsb, msb are in zero page.

## at page zero indexed by X
      
      .macro push idz, ptrz, lsb, msb 
            LDX \idz; DEX; LDA \msb; STA \ptrz, X; DEX; LDA \lsb; STA \ptrz, X; STX \idz
      .endmacro      ;  cycles
      
      .macro pull idz, ptrz, lsb, msb 
            LDX \idz; LDA \ptrz, X; STA \msb; INX; LDA \ptrz, X; STA \lsb; INX; STX \idz 
      .endmacro

## indirect by page zero indexed by Y

      .macro push idz, ptrz, lsb, msb 
            LDY \idz; DEY; LDA \msb; STA (\ptrz), Y; DEY; LDA \lsb; STA (\ptrz), Y; STY \idz} 
      .endmacro      ;  cycles
      
      .macro pull idz, ptrz, lsb, msb 
            LDY \idz; LDA (\ptrz), Y; STA \msb; INY; LDA (\ptrz), Y; STA \lsb; INY; STY \idz} 
      .endmacro

 _Could change the reference at ptrz to any address in memory_ 

## absolute address indexed by Y
      
      .macro push idz, ptr, lsb, msb 
            LDY \idz; LDA \msb; STA \ptr - 1, Y; LDA \lsb; STA \ptr - 2, Y; DEY; DEY; STY \idz} 
      .endmacro      ;  cycles
      
      .macro pull idz, ptrz, lsb, msb 
            LDY \idz; LDA \ptr + 0, Y; STA \lsb; LDA \ptr + 1, Y; STA \msb; INY; INY; STY \idx} 
      .endmacro

_Any operations with values at stack could be at direct offset_

## split absolute addres indexed by Y
      
      .macro push idz, ptr_lo, ptr_hi, lsb, msb 
            LDY \idz; LDA \msb; STA \ptr_lo - 1, Y; LDA \lsb; STA \ptr_hi - 1, Y; DEY; STY \idz} 
      .endmacro      ;  cycles
      
      .macro pull idz, ptrz, lsb, msb 
            LDY \idz; LDA \ptr_lo + 0, Y; STA \lsb; LDA \ptr_hi + 0, Y; STA \msb; INY; STY \idx} 
      .endmacro

Uses 6 bytes at page zero, stack size at idz+1.
_Any operations with values at stack could be at direct offset_

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


    
