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

Uses the hardware stack, and it must be split in 3 parts, one for inline code ( < 84 words ), one for data stack ( > 22 words ) , one for return stack ( > 22 words ). 
When stk, lsb, msb are in zero page, each stack uses cycles ~66 cc, 40 bytes and could not use JSR/RTS inside;

### at page zero indexed by X
      
      .macro push idz, ptrz, lsb, msb 
            LDX \idz; DEX; LDA \msb; STA \ptrz, X; DEX; LDA \lsb; STA \ptrz, X; STX \idz;
      .endmacro     
      
      .macro pull idz, ptrz, lsb, msb 
            LDX \idz; LDA \ptrz, X; STA \msb; INX; LDA \ptrz, X; STA \lsb; INX; STX \idz;
      .endmacro

Uses the page zero as stack, and it must be split in 3 parts, one for inline code ( < 81 words ), one for data stack ( > 22 words ) , one for return stack ( > 22 words ).
When idz, ptrz, lsb, msb are in zero page, each stack uses cycles ~48 cc, uses 28 bytes of code and 4 bytes at zero page;

### indirect by page zero indexed by Y

      .macro push idz, ptrz, lsb, msb 
            LDY \idz; DEY; LDA \msb; STA (\ptrz), Y; DEY; LDA \lsb; STA (\ptrz), Y; STY \idz; 
      .endmacro      
      
      .macro pull idz, ptrz, lsb, msb 
            LDY \idz; LDA (\ptrz), Y; STA \msb; INY; LDA (\ptrz), Y; STA \lsb; INY; STY \idz; 
      .endmacro

Uses the a pointer in page zero to anywhere in memory. Stacks with up to 128 cells. When idz, ptrz, lsb, msb are in zero page, each stack uses ~50 cc, 28 bytes of code and 4 bytes at zero page. _Multiuser and Multitask systems can change the pointers anytime._ 

### absolute address indexed by Y
      
      .macro push idz, lsb, msb 
            LDY \idz; LDA \msb; STA ptr - 1, Y; LDA \lsb; STA ptr - 2, Y; DEY; DEY; STY \idz; 
      .endmacro    
      
      .macro pull idz, lsb, msb 
            LDY \idz; LDA ptr + 0, Y; STA \lsb; LDA ptr + 1, Y; STA \msb; INY; INY; STY \idz; 
      .endmacro

Uses one absolute pointer (ptr) to memory. Stacks with up to 128 cells. when idz, lsb, msb are in zero page, each stack uses ~52 cc, 32 bytes of code and 2 bytes at zero page.  _Any operations with values at stack could be at direct offset, no need use pulls and pushs_

### split absolute addres indexed by Y
      
      .macro push idz, lsb, msb 
            LDY \idz; LDA \msb; STA ptr_lo - 1, Y; LDA \lsb; STA ptr_hi - 1, Y; DEY; STY \idz;
      .endmacro    
      
      .macro pull idz, lsb, msb 
            LDY \idz; LDA ptr_lo + 0, Y; STA \lsb; LDA ptr_hi + 0, Y; STA \msb; INY; STY \idz;
      .endmacro

Uses two absolute pointers (ptr_lo and ptr_hi) to memory. Stacks with up to 256 cells, splited in two parts. When idz, lsb, msb are in zero page, each stack uses ~48 cc, 30 bytes of code and 2 bytes at zero page.  _Any operations with values at stack could be at direct offset, no need pulls and pushs_

### direct address with indirect access

      .macro push ptr, lsb, msb 
            LDY #0; 
            LDA \msb; STA (ptr), Y; 
            INC ptr + 0; BNE :+ ; INC ptr + 1; : ;
            LDA \lsb; STA (ptr), Y; 
            INC ptr + 0; BNE :+ ; INC ptr + 1; : ;
       .endmacro    
      
      .macro pull ptr, lsb, msb 
            LDY #0; 
            LDA ptr + 0; BNE :+ ; DEC ptr + 1; : DEC ptr + 0; 
            LDA (ptr), Y; STA \msb; 
            LDA ptr + 0; BNE :+ ; DEC ptr + 1; : DEC ptr + 0; 
            LDA (ptr), Y; STA \lsb;
      .endmacro

Uses an absolute pointer (ptr) to memory. _Stacks with up to any size_. When ptr, lsb, msb are in zero page, each stack uses ~48 cc, 58 bytes of code and 2 bytes at page zero. 

| type | code size | cycles | notes |
| -- | -- | -- | -- |
| at hardware stack SP | 40 | | 
| at page zero indexed by X | 28 |  |
| indirect by page zero indexed by Y | 28  | |
| absolute address indexed by Y | 32 | 32 | |
| split absolute addres indexed by Y | 30 | | 
| direct address with indirect access | 58 | |

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


    
