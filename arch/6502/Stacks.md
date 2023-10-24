# The Stacks

_this is still a stub_

_Charles Moore says 22 levels is enough for Forth._

## Back Stacks

The 6502 have two peculiar pages, the zero page and stack page, both unique and with 256 bytes. All sub-routines calls (JSR) and returns (RTS) uses the stack page for 16-bit pointers, also the indirect indexed and indexed indirect modes uses page zero. Those are valuable resources.

In 6502 code, to pass a byte between memory, always need use LDA and STA, there are exotic alternatives, but all uses the accumulator.

Almost 6502 typical stack implementations does as standart: 
      
      1. Allow till 128 words deep stack; 
      2. Any operation with values at stack must do pushs and pulls. 
      3. Any multitask or multiuser system must split or copy the stack.

These are most commom: 

### hardware stack SP

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

Uses the hardware stack, and it must be split in 3 parts, one for inline code ( < 84 words ), one for data stack ( > 22 words ), one for return stack ( > 22 words ). When stk, lsb, msb are in zero page, each stack uses cycles ~66 cc, 40 bytes and could not use JSR/RTS inside;

### page zero indexed by X
      
      .macro push idz, ptrz, lsb, msb 
            LDX \idz; DEX; LDA \msb; STA \ptrz, X; DEX; LDA \lsb; STA \ptrz, X; STX \idz;
      .endmacro     
      
      .macro pull idz, ptrz, lsb, msb 
            LDX \idz; LDA \ptrz, X; STA \msb; INX; LDA \ptrz, X; STA \lsb; INX; STX \idz;
      .endmacro

Uses the page zero as stack, and it must be split in 3 parts, one for inline code ( < 81 words ), one for data stack ( > 22 words ), one for return stack ( > 22 words ). When idz, ptrz, lsb, msb are in zero page, each stack uses cycles ~48 cc, uses 28 bytes of code and 4 bytes at zero page;

### page zero indirect indexed by Y

      .macro push idz, ptrz, lsb, msb 
            LDY \idz; DEY; LDA \msb; STA (\ptrz), Y; DEY; LDA \lsb; STA (\ptrz), Y; STY \idz; 
      .endmacro      
      
      .macro pull idz, ptrz, lsb, msb 
            LDY \idz; LDA (\ptrz), Y; STA \msb; INY; LDA (\ptrz), Y; STA \lsb; INY; STY \idz; 
      .endmacro

Uses the a pointer in page zero to anywhere in memory. Stacks with up to 128 cells. When idz, ptrz, lsb, msb are in zero page, each stack uses ~50 cc, 28 bytes of code and 4 bytes at zero page. _Multiuser and Multitask systems can change the pointers anytime._ 

### absolute address indexed by Y or X
      
      .macro push idz, lsb, msb 
            LDY \idz; LDA \msb; STA ptr - 1, Y; LDA \lsb; STA ptr - 2, Y; DEY; DEY; STY \idz; 
      .endmacro    
      
      .macro pull idz, lsb, msb 
            LDY \idz; LDA ptr + 0, Y; STA \lsb; LDA ptr + 1, Y; STA \msb; INY; INY; STY \idz; 
      .endmacro

Uses one absolute pointer (ptr) to memory. Stacks with up to 128 cells. when idz, lsb, msb are in zero page, each stack uses ~52 cc, 32 bytes of code and 2 bytes at zero page. _Any operation with values at stack could be at direct offset, no need use pulls and pushs_

### split absolute address indexed by Y or X
      
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

### Comparasion

| type | code size | cycles | cells  | notes |
| -- | -- | -- | -- | -- | 
| hardware stack SP | 40 | 66 | 128 | must split in 3 parts*, must use push and pull | 
| page zero indexed by X | 28 | 48 | 128 | must split in 3 parts*, must use push and pull |
| indirect page zero indexed by Y | 28 | 50 | 128 | must split in 3 parts*, must use push and pull |
| absolute address indexed by Y | 32 | 52 | 128 | any operation at direct offset, no need pull and push |
| split absolute addres indexed by Y | 30 | 48 | 256 | any operation at direct offset, no need pull and push |
| direct address with indirect access | 58 | 96 | any size | must use push and pull | 

\* a least 22 cells of each stack and rest for inline code
  
### What Do 

Using absolute address indexed access for stacks. 

It provides the most fast overall implementation because does not need use push and pull for operations as DROP, DUP, OVER, SWAP, ROT, AND, OR, XOR, NEG, INV, 
ADD, SUB, INC, DEC, EQ, LT, GT, SHL, SHR, Fetch, Store. 

  | memory | |
  | -- | -- |
  | low | |
  | -4  | LSB |
  | -3  | MSB |
  | -2  | LSB |
  | -1  | MSB |
  |  0  | LSB TOS |
  | +1  | MSB |
  | +2  | LSB NOS |
  | +3  | MSB |
  | +4  | LSB |
  | +5  | MSB |
  | +6  | LSB |
  | +7  | MSB |
  | high | |

#### pros:
   offsets inline from a fixed reference
   direct memory access and exchange
   128 deep stack in round-robin

#### cons:
   can not change fixed reference

#### multitask and multiuser

   Could split 5 stacks of 22 cells for users or tasks, more than must exchange stacks values and include checks for stack limits.




    
