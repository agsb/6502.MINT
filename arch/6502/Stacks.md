### The Stacks

_this is still a stub_

There are four commom stacks implementations: 

1. Use of A,Y as top of data stack, hardware stack SP as data stack, and a indirect indexed access with Y for return stack.
2. Use SP for data stack and indirect indexed access with Y for return stack.
3. Use indirect indexed access with Y for data and return stack.
4. Use absolute indexed access with X for data and return stack.

The 6502 have two peculiar pages, the zero page and stack page, both unique and with 256 bytes. All sub-routines calls (JSR) and returns (RTS) uses the stack page for 16-bit pointers, also do the indirect indexed and indexed indirect modes. Those are valuable resources.

#### use hardware stack
      - push {LDA?; PHA; LDA?; PHA;} ; 12 cycles
      - pull {PLA; STA?; PLA; STA?;} ; 12 cycles
      
Uses none of page zero and two bytes at hardware stack.

#### use indirect access
      prepare {LDY PZBYTE} ; 3 cycles
      push {LDA?; STA(PZADDR),Y; INCY; LDA?; STA(PZADDR),Y; INCY;} ; 3 cycles
      pull {DECY; LDA(PZADDR),Y; STA?; DECY; LDA(PZADDR),Y; STA?}
      resume {STY PZBYTE}
      
Uses three bytes of page zero and none bytes at hardware stack.
      
#### use absolute indexed
     prepare: {LDY PZBYTE}
     push: {LDA?; STA [ADDR-2],Y; LDA?; STA[ADDR-1],Y; DECY; DECY;}
     pull: {LDA [ADDR+0],Y; STA?; LDA [ADDR+1],Y; STA?; INCY; INCY;}
     resume {STY PZBYTE}

Uses one byte of page zero, one address hardcoded inline and none bytes at hardware stack.

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
   indexed stacks are 128 words, Charles Moore says 22 is enough.
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


    