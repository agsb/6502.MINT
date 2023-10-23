# The Stacks

_this is still a stub_

_Charles Moore says 22 levels is enough for Forth._

## Back stack

The 6502 have two peculiar pages, the zero page and stack page, both unique and with 256 bytes. All sub-routines calls (JSR) and returns (RTS) uses the stack page for 16-bit pointers, also the indirect indexed and indexed indirect modes uses page zero. Those are valuable resources.

Almost 6502 typical stack implementations does: Allow till 128 words deep stack; Any operation with values at stack must do pushs and pulls. A multitask or multiuser system must split or copy the stack.

These are most commom: 

### at hardware stack SP

      .macro push stk, lsb, msb 
            LDA \stk; TSX; STX \stk; TAX; TXS;      ; 12 cc, 7 bytes
            LDA \lsb; PHA; LDA \msb; PHA;           ; 14 cc, 6 bytes
            LDA \stk; TSX; STX \stk; TAX; TXS;      ; 12 cc, 7 bytes
      .endmacro ; 
      
      .macro pull stk, lsb, msb
            LDA \stk; TSX; STX \stk; TAX; TXS;      ; 12 cc, 7 bytes
            PLA; STA \msb; PLA; STA \lsb;           ; 12 cc, 6 bytes
            LDA \stk; TSX; STX \stk; TAX; TXS;      ; 12 cc, 7 byytes
      .endmacro ;  

Uses the hardware stack, and it must be split in 3 parts, one for inline code ( < 84 words ), one for data stack ( > 22 words ) , one for return stack ( > 22 words );
Minimal cycles ~36 cc when stk, lsb, msb are in zero page. Each uses 40 bytes and could not use JSR/RTS inside;

## at page zero indexed by X
      
      .macro push idz, ptrz, lsb, msb 
            LDX \idz; DEX; LDA \msb; STA \ptrz, X; DEX; LDA \lsb; STA \ptrz, X; STX \idz;   ; 22 cc
      .endmacro     
      
      .macro pull idz, ptrz, lsb, msb 
            LDX \idz; LDA \ptrz, X; STA \msb; INX; LDA \ptrz, X; STA \lsb; INX; STX \idz;   ; 22 cc
      .endmacro

Uses the page zero as stack, and it must be split in 3 parts, one for inline code ( < 81 words ), one for data stack ( > 22 words ) , one for return stack ( > 22 words );
Minimal cycles ~22 cc when idz, ptrz, lsb, msb are in zero page. Each stack uses 28 bytes of code and 4 bytes at zero page;

## indirect by page zero indexed by Y

      .macro push idz, ptrz, lsb, msb 
            LDY \idz; DEY; LDA \msb; STA (\ptrz), Y; DEY; LDA \lsb; STA (\ptrz), Y; STY \idz;  ; 22 cc
      .endmacro      
      
      .macro pull idz, ptrz, lsb, msb 
            LDY \idz; LDA (\ptrz), Y; STA \msb; INY; LDA (\ptrz), Y; STA \lsb; INY; STY \idz;  ; 22 cc
      .endmacro

Uses the a pointer in page zero to anywhere in memory. Stacks with up to 128 cells. Minimal cycles ~22 cc when idz, ptrz, lsb, msb are in zero page. Each stack uses 28 bytes of code and 4 bytes at zero page. _Multiuser and Multitask systems can change the pointers anytime._ 

## absolute address indexed by Y
      
      .macro push idz, lsb, msb 
            LDY \idz; LDA \msb; STA ptr - 1, Y; LDA \lsb; STA ptr - 2, Y; DEY; DEY; STY \idz;  ; 24 cc
      .endmacro    
      
      .macro pull idz, lsb, msb 
            LDY \idz; LDA ptr + 0, Y; STA \lsb; LDA ptr + 1, Y; STA \msb; INY; INY; STY \idz;  ; 24 cc
      .endmacro

Uses one absolute pointer (ptr) to memory. Stacks with up to 128 cells. Minimal cycles ~24 cc when idz, lsb, msb are in zero page. Each stack uses 32 bytes of code and 2 bytes at zero page.  _Any operations with values at stack could be at direct offset, no need use pulls and pushs_

## split absolute addres indexed by Y
      
      .macro push idz, lsb, msb 
            LDY \idz; LDA \msb; STA ptr_lo - 1, Y; LDA \lsb; STA ptr_hi - 1, Y; DEY; STY \idz;  ; 19 cc
      .endmacro    
      
      .macro pull idz, lsb, msb 
            LDY \idz; LDA ptr_lo + 0, Y; STA \lsb; LDA ptr_hi + 0, Y; STA \msb; INY; STY \idz;  ; 19 cc
      .endmacro

Uses two absolute pointers (ptr_lo and ptr_hi) to memory. Stacks with up to 256 cells, splited in two parts. Minimal cycles ~21 cc when idz, lsb, msb are in zero page. Each stack uses 24 bytes of code and 2 bytes at zero page.  _Any operations with values at stack could be at direct offset, no need pulls and pushs_

## direct address with indirect access

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

Uses an absolute pointer (ptr) to memory. _Stacks with up to any size_. Minimal cycles ~48 cc when ptr, lsb, msb are in zero page. 
Each stack uses 24 bytes of code and 2 bytes at zero page.  

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


    
