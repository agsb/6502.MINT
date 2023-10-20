;vim: filetype=asm sw=4 ts=4 autoindent expandtab shiftwidth=4 et

; ********************************************************************* 
; 
;  MINT Minimal Interpreter 
; 
;  GNU GENERAL PUBLIC LICENSE              Version 3, 29 June 2007 
; 
;  see the LICENSE file in this repo for more information 
; 
;  original for the Z80, by Ken Boak, John Hardy and Craig Jones. 
; 
;  adapted for the 6502, by Alvaro G. S. Barcellos, 10/2023 
;  (some code from 6502.org forum and FIG_Forth) 
;  (some code from https://www.nesdev.org/wiki/Programming_guide) 
 
;  star(tm) date 10/10/2023 
; ********************************************************************* 
 
;    DSIZE       = $80 
;    RSIZE       = $80 
;    TIBSIZE     = $100 

    TRUE        = 1 
    FALSE       = 0 

    CR  = 13
    LF  = 10
    ETX = 3
    NUL = 0
 
    ; more groups for defs
    NUMGRPS = 5 

    ; all groups size
    GRPSIZE = $40
 
    ; size page
    PAGE = $100

;---------------------------------------------------------------------- 
; notes 6502 version V0.4 : 
; 
;   code is for ROM use. 
; 
;   if made for RAM:
;       caller must save a, x, y and 
;       reserve at least 32 words at hardware stack 
;
;      a Z80, does ADD HL, DE in 12 t and 1 byte  
;      a 6502, does it in 13 t and 9 bytes 
;      
;   0. it is a 8-bit doing 16-bit things
;   1. a cell is 16-bit 
;   2. no multiuser, no multitask 
;   3. no garbage collector
;   4. stacks are offsets from absolute address. 
;   5. data stack indexed by X and return stack indexed by Y 
;   6. terminal input buffer and stacks are  
;      all just 128 cells deep and byte size round-robin 
;   7. jump table is 16-bits
;   8. extense use of Y indexed indirect addressing 
;   9. uses 16 bytes at page zero $F0 to $FF, 
;  10. user functions are mapped in five groups, or more
;  11. user functions are stored in heap
;  12. all rotines must end with: 
;   	jmp next_ or jmp (vNext) or jmp drop_ 
;
;  Changes in mint:

;   1. expanded mint variables
;        alt-a, used for vS0, start of data stack 
;        alt-f, used for vR0, start of return stack **** 
;        alt-g, used for vNext, indirect dispatcher **** 
;        alt-r, return the return stack pointer
;        alt-s, return the data stack pointer
;
;   2. expanded mint functions
;        alt-U, classic Forth R> 
;        alt-V, classic Forth >R 
;        alt-H, verify if a key was hit 
;
;   3. all variables and functions are composed in groups,
;        each group have 32 cells, accessed from 'a' to 'z',
;        with 6 more cells below 'z'.
;
;   4. extra functions 
;       gets_, gets a line into a buffer, asciiz
;       puts_, puts a line from a buffer, asciiz 
;
;---------------------------------------------------------------------- 
 
;---------------------------------------------------------------------- 
; page 0, reserved cells 
    zpage = $f0 

; reserved 
    yp = zpage + $0  ; y index, return stack pointer, 
    xp = zpage + $1  ; x index, parameter stack pointer, 

; copycat 
    ap = zpage + $2  ; accumulator 
    ns = zpage + $3  ; nests 
 
; pseudos 
    tos = zpage + $4  ; tos  register 
    nos = zpage + $6  ; nos  register 
    wrk = zpage + $8  ; work register 
    tmp = zpage + $a  ; work register 

; holds
    lnk = zpage + $c  ; next pointer 
    ips = zpage + $e  ; instruction pointer 
 
; all in RAM, better put tables at end of code ? 
 

;---------------------------------------------------------------------- 
.segment "VECTORS"

.word init
.word init
.word init

;---------------------------------------------------------------------- 
.segment "CODE"

; start of RAM

    ; .align $100

VOID:

; data stack
    .res PAGE, $00
spz:

; return stack
    .res PAGE, $00
rpz: 

; terminal input buffer
tib:    
    .res PAGE, $00

; mint variables, 26 plus 6 from z
vsys:
    .res GRPSIZE, $00

; user variable, 26 plus 6 from z
vars:
    .res GRPSIZE, $00

; user function groups, each with 26 plus 6 from Z
defs:
    .res GRPSIZE * NUMGRPS, $00

; internals

vNextJmp:
    .word $0

vEdited:
    .word $0

vByteMode:
    .word $0

; free = GRPSIZE0  ; free ram start 
; heap must be here ! 
    .word $DEAD, $C0DE

heap:
    .word $0

; aliases

vS0      =  vsys + $00     ;    a  ; start of data stack variable
vBase16  =  vsys + $02     ;    b  ; base16 flag
vTIBPtr  =  vsys + $04     ;    c  ; TIBPtr variable
vDefs    =  vsys + $08     ;    d  ; functions defines
;        =  vsys + $0a     ;    e  ; 
vR0      =  vsys + $0c     ;    f  ; start of return stack variable
vNext    =  vsys + $0e     ;    g  ; next routine dispatcher
vHeapPtr =  vsys + $10     ;    h  ; heap ptr variable

dStack = vS0
rStack = vR0
HEAP = heap
DEFS = defs

;---------------------------------------------------------------------- 
.segment "ONCE"
 
; ********************************************************************** 
; 
; (not yet) routines are ordered to occupy pages of 256 bytes 
; 
; ********************************************************************** 
 
init:
    jmp mint_
    .asciiz "6502 MINT"

;---------------------------------------------------------------------- 
;    depends on hardware
;---------------------------------------------------------------------- 
putchar:
    clc
    rts

getchar:
    clc
    rts

hitchar:
    clc
    rts

;---------------------------------------------------------------------- 
key_:
    jsr getchar
keyk:
    sta tos + 0
    jsr spush
    jmp (vNext)
    
;---------------------------------------------------------------------- 
emit_:
    jsr spull
    lda tos + 0
    jsr putchar
    jmp (vNext)

;---------------------------------------------------------------------- 
keyq_:
    jsr hitchar
    clv
    bvc keyk

;---------------------------------------------------------------------- 
aNop_:
nop_:
    clc
    jmp next

;---------------------------------------------------------------------- 
; increase instruction pointer 
incps: 
    inc ips + 0 
    bne @noeq 
    inc ips + 1 
@noeq: 
    rts 
 
;---------------------------------------------------------------------- 
; decrease instruction pointer 
decps: 
    lda ips + 0
    bne @noeq 
    dec ips + 1 
@noeq: 
    dec ips + 0 
    rts 
 
;---------------------------------------------------------------------- 
; load char at instruction pointer 
ldaps: 
    ldy NUL 
    lda (ips), y 
    rts 
 
;---------------------------------------------------------------------- 
pushps:
; push ps into RS 
    ldy yp 
    dey 
    dey 
    lda ips + 0 
    sta rpz + 0, y 
    lda ips + 1 
    sta rpz + 1, y 
    sty yp 
    rts

;---------------------------------------------------------------------- 
pullps:
; pull ps from RS 
    ldy yp 
    lda rpz + 0, y 
    sta ips + 0 
    lda rpz + 1, y 
    sta ips + 1 
    iny 
    iny 
    sty yp 
    rts

;---------------------------------------------------------------------- 
; push tos into return stack 
rpush: 
    ldy yp 
    dey 
    dey 
    lda tos + 0 
    sta rpz + 0, y 
    lda tos + 1 
    lda rpz + 1, y 
    sty yp 
    rts 
 
;---------------------------------------------------------------------- 
; push tos from return stack 
rpull: 
    ldy yp 
    lda rpz + 0, y 
    sta tos + 0 
    lda rpz + 1, y 
    sta tos + 1 
    iny 
    iny 
    sty yp 
    rts 
 
;---------------------------------------------------------------------- 
; push tos into stack 
spush: 
    ; ldx xp
    dex 
    dex 
    lda tos + 0 
    sta spz + 0, x 
    lda tos + 1 
    sta spz + 1, x 
    ; stx xp
    rts 
 
;---------------------------------------------------------------------- 
; pull tos from stack 
spull: 
    ; ldx xp
    lda spz + 0, x 
    sta tos + 0 
    lda spz + 1, x 
    sta tos + 1 
    inx 
    inx 
    ; stx xp
    rts 
 
;---------------------------------------------------------------------- 
; take two from stack 
take2: 
    ; ldx xp
    lda spz + 0, x 
    sta tos + 0 
    lda spz + 1, x 
    sta tos + 1 
    lda spz + 2, x 
    sta nos + 2 
    lda spz + 3, x 
    sta nos + 3 
    inx 
    inx 
    inx 
    inx 
    ; stx xp
    rts 

;---------------------------------------------------------------------- 
; classic R>
R2S_:
    jsr rpull
    jsr spush
    rts

;---------------------------------------------------------------------- 
; classic >R
S2R_:
    jsr spull
    jsr rpush
    rts

;---------------------------------------------------------------------- 
; NEGate the value on top of stack (2's complement) 
neg_: 
    ; ldx xp
    lda NUL 
    sec 
    sbc spz + 0, x 
    sta spz + 0, x 
    lda NUL
    sec 
    sbc spz + 1, x 
    sta spz + 1, x 
    ; stx xp
    jmp (vNext) 
 
;---------------------------------------------------------------------- 
; Bitwise INVert the top member of the stack (1's complement) 
inv_: 
    ; ldx xp
    lda #$FF 
    eor spz + 0, x 
    sta spz + 0, x 
    lda #$FF 
    eor spz + 1, x 
    sta spz + 1, x 
    ; stx xp
    jmp (vNext) 
 
;---------------------------------------------------------------------- 
; Duplicate the top member of the stack 
; a b c -- a b c c 
dup_: 
    ; ldx xp
    dex 
    dex 
    lda spz + 2, x 
    sta spz + 0, x 
    lda spz + 3, x 
    sta spz + 1, x 
    ; stx xp
    jmp (vNext) 
 
;---------------------------------------------------------------------- 
; Duplicate 2nd element of the stack 
; a b c -- a b c b 
over_: 
    ; ldx xp
    dex 
    dex 
    lda spz + 4, x 
    sta spz + 0, x 
    lda spz + 5, x 
    sta spz + 1, x 
    ; stx xp
    jmp (vNext) 
 
;---------------------------------------------------------------------- 
; Rotate 3 elements at stack 
; a b c -- b c a 
rot_: 
    ; ldx xp
    ; c -> t 
    lda spz + 0, x 
    sta tos + 0 
    lda spz + 0, x 
    sta tos + 1 
    ; b -> n 
    lda spz + 2, x 
    sta nos + 0 
    lda spz + 3, x 
    sta nos + 1 
    ; a -> c 
    lda spz + 4, x 
    sta spz + 0, x 
    lda spz + 5, x 
    sta spz + 1, x 
    ; t -> b 
    lda tos + 0 
    sta spz + 2, x 
    lda tos + 1 
    sta spz + 3, x 
    ; n -> a 
    lda nos + 0 
    sta spz + 4, x 
    lda nos + 1 
    sta spz + 5, x 
    ; stx xp
    jmp (vNext) 
 
;---------------------------------------------------------------------- 
; Swap 2nd and 1st elements of the stack 
; a b c -- a c b 
swap_: 
    ; ldx xp
    ; b -> w 
    lda spz + 0, x 
    sta tos + 0 
    lda spz + 1, x 
    sta tos + 1 
    ; a -> b 
    lda spz + 2, x 
    sta spz + 0, x 
    lda spz + 3, x 
    sta spz + 1, x 
    ; w -> a 
    lda tos + 0 
    sta spz + 2, x 
    lda tos + 1 
    sta spz + 3, x 
    ; stx xp
    jmp (vNext) 
 
;---------------------------------------------------------------------- 
;  Left shift { is multply by 2 
shl_: 
    ; ldx xp
    asl spz + 0, x 
    rol spz + 1, x 
    ; stx xp
    jmp (vNext) 
 
;---------------------------------------------------------------------- 
;  Right shift } is a divide by 2 
shr_: 
    ; ldx xp
    lsr spz + 0, x 
    ror spz + 1, x 
    ; stx xp
    jmp (vNext) 
 
;---------------------------------------------------------------------- 
; Drop the top member of the stack 
; a b c -- a b 
drop_: 
    ; ldx xp
    inx 
    inx 
    ; stx xp
    jmp (vNext) 
 
;---------------------------------------------------------------------- 
;  Bitwise AND the top 2 elements of the stack 
and_: 
    ; ldx xp
    lda spz + 2, x 
    and spz + 0, x 
    sta spz + 2, x 
    lda spz + 3, x 
    and spz + 1, x 
    sta spz + 3, x 
    ; stx xp
    jmp drop_ 
 
;---------------------------------------------------------------------- 
;  Bitwise OR the top 2 elements of the stack 
or_: 
    ; ldx xp
    lda spz + 2, x 
    ora spz + 0, x 
    sta spz + 2, x 
    lda spz + 3, x 
    ora spz + 1, x 
    sta spz + 3, x 
    ; stx xp
    jmp drop_ 
 
;---------------------------------------------------------------------- 
;  Bitwise XOR the top 2 elements of the stack 
xor_: 
    ; ldx xp
    lda spz + 2, x 
    eor spz + 0, x 
    sta spz + 2, x 
    lda spz + 3, x 
    eor spz + 1, x 
    sta spz + 3, x 
    ; stx xp
    jmp drop_ 
 
;---------------------------------------------------------------------- 
; Add the top 2 members of the stack 
; a b c -- a (b+c) 
add_:   
    ; ldx xp
    clc 
    lda spz + 2, x 
    adc spz + 0, x 
    sta spz + 2, x 
    lda spz + 3, x 
    adc spz + 1, x 
    sta spz + 3, x 
    ; stx xp
    jmp drop_ 
 
;---------------------------------------------------------------------- 
; Subtract the top 2 members of the stack 
; a b c -- a (b-c) 
sub_: 
    ; ldx xp
    sec 
    lda spz + 2, x 
    sbc spz + 0, x 
    sta spz + 2, x 
    lda spz + 3, x 
    sbc spz + 1, x 
    sta spz + 3, x 
    ; stx xp
    jmp drop_ 
 
;---------------------------------------------------------------------- 
; prepare for mult or divd
opin:
    ldy yp
    ; pseudo tos
    lda spz + 0, y
    sta wrk + 0
    lda spz + 1, y
    sta wrk + 1
    ; pseudo nos 
    lda spz + 2, y
    sta tmp + 0
    lda spz + 3, y
    sta tmp + 1
    ; clear results
    lda NUL
    sta tos + 0
    sta tos + 1
    sta nos + 0 
    sta nos + 1
    ; countdown
    ldy #16
    rts

;---------------------------------------------------------------------- 
; resume from mult or divd
opout:
    ; copy results
    ldy yp
    lda nos + 0
    sta spz + 0, y
    lda nos + 1
    sta spz + 1, y
    lda tos + 0
    sta spz + 2, y
    lda tos + 1
    sta spz + 3, y
    rts

;---------------------------------------------------------------------- 
; Divide the top 2 members of the stack 
; http://codebase64.org/doku.php?id=base:16bit_division_16-bit_result
; dividend divisor -- result remainder
; ( tmp wrk -- nos tos )
div_: 
    jsr opin
@loop:
    asl tmp + 0
    rol tmp + 1
    rol tos + 0
    rol tos + 1
    
    lda tos + 0
    sec
    sbc wrk + 0
    sta nos + 0
    lda tos + 1
    sbc wrk + 1
    bcc @skip

    sta tos + 1
    lda nos + 0
    sta tos + 0
    inc tmp + 0

@skip:
    ; countdown
    dey
    bne @loop
    lda tmp + 0
    sta nos + 0
    lda tmp + 1
    sta nos + 1
    jsr opout
    jmp (vNext)

;---------------------------------------------------------------------- 
; 16-bit multiply 16x16, 32 result
; http://codebase64.org/doku.php?id=base:16bit_multiplication_32-bit_product
; ( multiplicand multiplier -- resultLSW resultMSW )
; ( tmp wrk -- nos tos )
mul_:
    jsr opin
@shift_r:
    ; divide by 2
    lsr wrk + 1
    ror wrk + 0
    bcc @rotate_r
    ; add multiplicand to upper half product
    lda tos + 0
    clc
    adc tmp + 0
    sta tos + 0
    lda tmp + 1
    adc tos + 1
@rotate_r:    
    ; rotate partial product upper to low
    ror
    sta tos + 1
    ror tos + 0
    ror nos + 1
    ror nos + 0
    ; countdown
    dey
    bne @shift_r
    jsr opout
    jmp (vNext)
 
;---------------------------------------------------------------------- 
; \+    a b c -- a ; [c]+b  ; increment variable at c by b 
incr_: 
    jsr take2 
    ldy NUL 
    lda (tos), y 
    clc 
    adc nos + 0 
    sta (tos), y 
    iny 
    lda (tos), y 
    clc
    adc nos + 1 
    sta (tos), y 
    jmp (vNext) 
 
;---------------------------------------------------------------------- 
; \-    a b c -- a ; [c]-b  ; decrement variable at c by b 
decr_: 
    jsr take2 
    ldy NUL 
    lda (tos), y 
    sec 
    sbc nos + 0 
    sta (tos), y 
    iny 
    lda (tos), y 
    sec 
    sbc nos + 1 
    sta (tos), y 
    jmp (vNext) 
 
;---------------------------------------------------------------------- 
; false 
false2: 
    ; ldx xp
    lda NUL 
    sta spz + 2, x 
    sta spz + 3, x 
    ; stx xp
    jmp drop_ 
 
;---------------------------------------------------------------------- 
; true 
true2: 
    ; ldx xp
    lda #$01 
    sta spz + 2, x 
    lda NUL 
    sta spz + 3, x 
    ; stx xp
    jmp drop_ 
 
;---------------------------------------------------------------------- 
; subtract for compare 
cmps: 
    ; ldx xp
    lda spz + 2, x 
    sec 
    sbc spz + 0, x 
    lda spz + 3, x 
    sbc spz + 1, x 
    ; stx xp
    rts 
 
;---------------------------------------------------------------------- 
; signed equal than 
eq_: 
    jsr cmps 
    bne false2 
    beq true2 
 
;---------------------------------------------------------------------- 
; signed less than 
lt_: 
    jsr cmps 
    bmi true2 
    bpl false2 
 
;---------------------------------------------------------------------- 
; signed greather than 
; must be in that order, bpl is non negative flag 
gt_: 
    jsr cmps 
    bmi false2 
    beq false2 
    bpl true2 
 
;---------------------------------------------------------------------- 
; fetch the value from the address placed on the top of the stack 
; a b c - a b (c) 
; fetch a byte 
cFetch_: 
    lda NUL 
    sta tos + 1 
    sec
    jmp isfetch 

;---------------------------------------------------------------------- 
; fetch a word 
fetch_: 
    clc 
    jmp isfetch 
 
;---------------------------------------------------------------------- 
isfetch: 
    ; ldx xp
    ; load the reference 
    lda spz + 0, x 
    sta nos + 0 
    lda spz + 1, x 
    sta nos + 1 
    ; then the value 
    ldy NUL
    lda (nos), y 
    sta tos + 0
    bcs @cset
    iny
    lda (nos), y 
    sta tos + 1
@cset:
    ; save the value 
    lda tos + 0 
    sta spz + 0, x 
    lda tos + 1 
    sta spz + 1, x 
    ; next 
    ; stx xp
    jmp (vNext) 
 
;---------------------------------------------------------------------- 
; store the value into the address placed on the top of the stack 
; a b c -- a 
; store a byte 
cStore_: 
    sec 
    jmp isstore 
 
;---------------------------------------------------------------------- 
; store a word 
store_: 
    clc 
    jmp isstore 
 
;---------------------------------------------------------------------- 
isstore: 
    jsr take2 
    ; copy the value 
    ldy NUL
    lda nos + 0
    sta (tos), y
    bcs @cset
    iny
    lda nos + 1
    sta (tos), y
    ; next 
@cset:    
    ; stx xp
    jmp (vNext) 
 
;---------------------------------------------------------------------- 
; puts a string 
str_: 
    ldy NUL
@loop:
    lda (ips), y
    beq @ends
    cmp #'`'              ; ` is the string terminator 
    beq @ends 
    jsr putchar 
    iny
    bne @loop
@ends: 
    jmp (vNext) 
 
;---------------------------------------------------------------------- 
macro:
    tay
    lda ctlcodeslo, y
    sta tos + 0
    lda ctlcodeshi, y
    sta tos + 1
    jsr spush
    jsr enter
    .asciiz "\\G"
    jmp interpret2

;---------------------------------------------------------------------- 
interpret: 
    jsr enter 
    .asciiz "\\N`> `" 
    ; fall throught

; used by tests 
interpret1:
    lda NUL 
    sta vTIBPtr + 0 
    sta vTIBPtr + 1 

interpret2:                     
    lda NUL
    sta ns
    tay
    beq @cast

; calc nesting (a macro might have changed it) 
@loop: 
    lda tib, y 
    iny 
    jsr nesting            ; update nesting value 

@cast: 
    cpy NUL 
    bne @loop 
    ; fall throught

;---------------------------------------------------------------------- 
; loop around waiting for character 
; get a line into tib
waitchar: 
    lda #<tib
    sta tos + 0
    lda #>tib
    sta tos + 1
    jsr spush
    ; fall throught

;---------------------------------------------------------------------- 
; get a line into buffer pointer by TOS
gets_:
    ; already 
    ldy NUL
    jsr spull
@loop:
    ; limit 254
    cpy #$FE
    beq @endstr
    jsr getchar 
    cmp #32                 ; ge space ? 
    bcs @ischar 
    cmp #$0                 ; is it end of string ? 
    beq @endstr 
    cmp CR                 ; carriage return ? 
    beq @iscrlf 
    cmp LF                 ; line feed ? 
    beq @iscrlf 
    ; ????
@ismacro: 
    jmp macro 
@ischar: 
    jsr @echo 
    ; nest ? 
    jsr nesting 
    clv
    bvc @loop            ; wait for next character 
@iscrlf: 
    lda CR 
    jsr @echo 
    lda LF 
    jsr @echo 
    ; pending nest ? 
    lda ns 
    cmp NUL 
    beq @loop 
; mark etx, used later to check Z80 stack deep, 
; not need in 6502 round-robin stack,
; preserved for compability
@endstr: 
    ; mark ETX 
    lda ETX 
    sta (tos), y
    iny
    ; mark NUL
    lda NUL 
    sta (tos), y 

    lda tos + 0
    sta ips + 0
    lda tos + 1
    sta ips + 1
    jsr decps
    jmp next

; maximum 254 chars 
@echo:
    ; echo 
    jsr putchar 
    ; store
    sta (tos), y 
    iny 
    rts 
 
;---------------------------------------------------------------------- 
; calculate nesting value 
nesting: 
    cmp #'`' 
    bne @nests 
    ; clear bit 7
    lda #$80 
    eor ns 
    sta ns 
    rts 
@nests: 
    bit ns 
    bmi @nonest 
    cmp #':' 
    beq @nestinc 
    cmp #'[' 
    beq @nestinc 
    cmp #'(' 
    beq @nestinc 
    cmp #';' 
    beq @nestdec 
    cmp #']' 
    beq @nestdec 
    cmp #')' 
    beq @nestdec 
@nonest: 
    rts 
@nestinc: 
    inc ns 
    rts 
@nestdec: 
    dec ns 
    rts 
 
;---------------------------------------------------------------------- 
; prints a asciiz, refered by hardware stack 
printStr: 
    pla 
    sta tos + 1 
    pla 
    sta tos + 0 
    jsr putstr 
    clc
    adc tos + 0 
    pha 
    adc tos + 1 
    pha 
    rts 
 
;---------------------------------------------------------------------- 
; prints a asciiz line from a buffer pointer by tos 
puts_: 
    jsr spull
    ; fall throught

;---------------------------------------------------------------------- 
; prints a asciiz 
putstr: 
    ldy NUL 
@loop: 
    lda (tos), y 
    beq @ends 
    jsr putchar
    iny
    bne @loop 
@ends: 
    tya
    rts 
 
;---------------------------------------------------------------------- 
; prints number in tos to decimal ASCII 
; ps. putchar ends with rts
printdec: 
    lda #<10000 
    sta nos + 0 
    lda #>10000 
    sta nos + 1 
    jsr @nums 
    lda #<1000 
    sta nos + 0 
    lda #>1000 
    sta nos + 1 
    jsr @nums 
    lda #<100 
    sta nos + 0 
    lda #>100 
    sta nos + 1 
    jsr @nums 
    lda #<10 
    sta nos + 0 
    lda #>10 
    sta nos + 1 
@nums: 
    ldy #'0'-1 
@loop: 
    iny 
    lda tos + 0 
    sec 
    sbc nos + 0 
    sta tos + 0 
    lda tos + 1 
    sbc nos + 1 
    sta tos + 1 
    bcc @loop 
    lda tos + 0 
    clc 
    adc nos + 0 
    sta tos + 0 
    lda tos + 1 
    adc nos + 1 
    sta tos + 1 
    tya 
    jmp putchar 
 
;---------------------------------------------------------------------- 
; prints number in tos to hexadecimal ASCII 
printhex: 
    lda tos + 1 
    jsr printhex8 
    lda tos + 0 
    jsr printhex8 
    rts 
 
;---------------------------------------------------------------------- 
; print a 8-bit HEX 
printhex8: 
    sta ap 
    lsr 
    ror 
    ror 
    ror 
    jsr @conv 
    lda ap 
@conv: 
    and #$0F 
    clc 
    adc #$30 
    cmp #$3A 
    bcc @ends 
    adc #$06 
@ends: 
    jmp putchar 
 
;---------------------------------------------------------------------- 
; convert a decimal value to binary 
num_: 
    jsr decps
    lda NUL 
    sta tos + 0 
    sta tos + 1 
@loop: 
    jsr incps
    jsr ldaps
    cmp #'0' + 0 
    bcc @ends 
    cmp #'9' + 1 
    bcs @ends 
@cv10: 
    sec 
    sbc #'0' 
@uval: 
    clc
    adc tos + 0 
    sta tos + 0 
    lda NUL
    adc tos + 1
    sta tos + 1
    jsr mul10 
    clv
    bvc @loop 
@ends: 
    jsr spush 
    jmp (vNext)
 
;---------------------------------------------------------------------- 
; multiply by ten 
; 2x + 8x 
mul10: 
    ; 2x
    asl tos + 0 
    sta tos + 0 
    sta nos + 0 
    rol tos + 1 
    sta tos + 1 
    sta nos + 1 
    ; 2x
    asl tos + 0 
    sta tos + 0 
    rol tos + 1 
    sta tos + 1 
    ; 2x
    asl tos + 0 
    sta tos + 0 
    rol tos + 1 
    sta tos + 1 
    ; 2x + 8x
    clc
    lda tos + 0 
    adc nos + 0 
    sta tos + 0 
    lda tos + 1 
    adc nos + 1 
    sta tos + 1 
    rts 
 
;---------------------------------------------------------------------- 
; convert a hexadecimal value to binary 
hex_: 
    jsr decps
    lda NUL 
    sta tos + 0 
    sta tos + 1 
@loop: 
    jsr incps
    jsr ldaps
@isd: 
    cmp #'0' 
    bcc @ends 
    cmp #'9' + 1 
    bcs @ish 
@cv10: 
    sec 
    sbc #'0' 
    clv
    bvc @uval 
@ish: 
    ; to upper
    and #%11011111 
    cmp 'A' 
    bcc @ends 
    cmp 'F' + 1 
    bcs @ends 
@cv16: 
    sec 
    sbc #'A' - 10 
@uval: 
    clc
    adc tos + 0 
    sta tos + 0 
    lda NUL
    adc tos + 1
    sta tos + 1
    jsr mul16 
    clv
    bvc @loop 
@ends: 
    jsr spush 
    jmp (vNext)
 
;---------------------------------------------------------------------- 
; multiply by sixteen 
mul16: 
    ldy #4 
@loop: 
    asl tos + 0 
    sta tos + 0 
    rol tos + 1 
    sta tos + 1 
    dey 
    bne @loop 
    rts 
 
;---------------------------------------------------------------------- 
; skip to eol
comment_: 
    ldy NUL
@loop:    
    iny
    lda (ips), y
    cmp CR 
    bne @loop 
    ; skip \r
    iny
    ; skip \n
    iny
    ; offset
    tya
    clc
    adc ips + 0
    bcc @iscc
    inc ips + 1
@iscc:
    jmp (vNext) 
 
;---------------------------------------------------------------------- 
depth_: 
    ; limit to 255 bytes
    lda #$FF 
    sec 
    sbc xp 
    ; words 
    lsr
    sta tos + 0 
    lda NUL 
    sta tos + 1 
    jsr spush 
    jmp (vNext) 
 
;---------------------------------------------------------------------- 
; print hexadecimal 
hdot_: 
    jsr spull 
    jsr printhex 
    jmp dotsp 
 
;---------------------------------------------------------------------- 
; print decimal 
dot_: 
    jsr spull 
    jsr printdec 
    jmp dotsp 
 
;---------------------------------------------------------------------- 
; print space 
dotsp: 
    lda #' ' 
    jsr writeChar1 
    jmp (vNext) 
 
;---------------------------------------------------------------------- 
writeChar1: 
    jmp putchar 
 
;---------------------------------------------------------------------- 
newln_: 
    jsr crlf 
    jmp (vNext) 
 
;---------------------------------------------------------------------- 
crlf: 
    jsr printStr 
    .asciiz "\r\n" 
    rts 
 
;---------------------------------------------------------------------- 
prompt: 
    jsr printStr 
    .asciiz "\r\n> " 
    rts 
 
;---------------------------------------------------------------------- 
printStk_:  
    jsr enter
    ;.asciiz  "\\a@2-\\D1-(",$22,"@\\b@\\(,)(.)2-)'" 
    .asciiz  "\\a@2-\\D1-(34@\\b@\\(,)(.)2-)'" 
    jmp (vNext) 
 
;---------------------------------------------------------------------- 
; 6502 is memory mapped IO, just read 
inPort_: 
    jmp cFetch_ 
 
;---------------------------------------------------------------------- 
; 6502 is memory mapped IO, just write 
outPort_: 
    jmp cStore_ 
 
;---------------------------------------------------------------------- 
charCode_:
    jsr incps
    jsr ldaps
    sta tos + 0
    lda NUL
    sta tos + 1
    jsr spush
    jmp (vNext)

;---------------------------------------------------------------------- 
; ??? maybe just count and at end copy and update ?
;
compNext:
    ; array start
    lda vHeapPtr + 0
    sta nos + 0
    lda vHeapPtr + 1
    sta nos + 1

    ldy NUL
    jsr spull

    lda tos + 0
    sta (nos), y
    iny
    
    lda vByteMode + 0
    bne @isbm  

    lda tos + 1
    sta (nos), y
    iny

@isbm:
    
    tya
    clc
    adc nos + 0
    sta nos + 0
    bcc @isnc
    inc nos + 1
@isnc:
    lda nos + 0
    sta vHeapPtr + 0
    lda nos + 1
    sta vHeapPtr + 1
    ; fall throught 

;---------------------------------------------------------------------- 
; Execute next opcode
next: 
opt_:
    ; using full jump table 
    jsr incps
    jsr ldaps
    tay 
    lda optcodeslo, y 
    sta wrk + 0 
    lda optcodeshi, y 
    sta wrk + 1 
    jmp (wrk) 
 
;---------------------------------------------------------------------- 
; Execute next alt opcode
alt_: 
    ; using full jump table 
    jsr incps
    jsr ldaps
    tay 
    lda altcodeslo, y 
    sta wrk + 0 
    lda altcodeshi, y 
    sta wrk + 1 
    jmp (wrk) 
 
;---------------------------------------------------------------------- 
; Execute code inline 
; where it returns ????
enter:                           
    jsr pushps
; pull from system stack
    pla 
    sta ips + 0 
    pla
    sta ips + 1 
    jsr decps
    jmp (vNext) 

;---------------------------------------------------------------------- 
; Execute code from data stack and return
exec_:
    jsr spull
    sta #<exec1
    pha
    sta #>exec1
    pha
    jmp (tos)
exec1:
    jmp (vNext)

;---------------------------------------------------------------------- 
; Interpret code from data stack
go_: 
    jsr pushps
; pull ps from data stack 
    ; ldx xp
    lda spz + 0, x 
    sta ips + 0 
    lda spz + 1, x 
    sta ips + 1 
    inx 
    inx 
    ; stx xp
    jsr decps 
    jmp (vNext) 
 
;---------------------------------------------------------------------- 
; Execute code from a user function
call_:
    sta ap
    jsr pushps
    jsr lookupDefs
    jsr decps
    jmp (vNext)

lookupDeft:
    lda ap
    sta vEdited
    ; fall throught

lookupDefs:
    lda ap
    sbc 'A'
    asl
    tay
    lda vDefs + 0
    sta tos + 0
    lda vDefs + 1
    sta tos + 1
    rts 

;---------------------------------------------------------------------- 
; zzzz
editDef_:
    ; which one 
    jsr spull
    ; toChar
    lda #'A'
    clc
    adc tos + 0
    sta ap
    ; in words
    asl tos + 0
    rol tos + 1
    ; reference
    lda vDefs + 0
    sta nos + 0
    lda vDefs + 1
    sta nos + 1
    ; offset
    clc
    lda nos + 0
    adc tos + 0
    sta nos + 0
    lda nos + 1
    adc tos + 1
    sta nos + 1
    ; reference
    ldy NUL

    lda (nos), y
    sta tos + 0
    iny
    lda (nos), y
    sta tos + 1

    ; ldy NUL
    ; empty ?
    lda (tos), y
    cmp #';'
    beq editDef3

    ; destin
    lda #<tib
    sta wrk + 0
    lda #>tib
    sta wrk + 1
    
    ; else
    ; ldy NUL
    
    lda #':'
    jsr writeChar
    jsr incwrk
    
    lda ap
    jsr writeChar
    jsr incwrk
    
    clv
    bvc editDef2

editDef1:
    iny
    beq editDef3

editDef2:
    lda (tos), y
    jsr writeChar
    cmp #';'
    bne editDef1

editDef3:
    lda #<tib
    sta tos + 0
    lda #>tib
    sta tos + 1

    lda tos + 0
    sta vTIBPtr + 0
    lda tos + 1
    sta vTIBPtr + 1
    jmp (vNext)

;---------------------------------------------------------------------- 
writeChar: 
    sta (wrk), y
    jmp putchar

;---------------------------------------------------------------------- 
; push an user variable 
var_: 
    sta ap 
    lda #<vars 
    sta tos + 0 
    lda #>vars 
    sta tos + 1 
    jmp a2z 
 
;---------------------------------------------------------------------- 
; push a mint variable
sysVar_: 
    sta ap 
    lda #<vsys 
    sta tos + 0 
    lda #>vsys 
    sta tos + 1 
    jmp a2z 
 
;---------------------------------------------------------------------- 
; push a reference into stack
a2z: 
    lda ap 
    sec 
    sbc #'a' 
    asl 
    clc 
    adc tos + 0 
    bcc @iscc 
    inc tos + 1 
@iscc:
    jsr spush 
    jmp (vNext) 
 
;---------------------------------------------------------------------- 
; skip spaces
nosp:
    jsr incps
    jsr ldaps
    cmp #' '
    beq nosp
    rts

;---------------------------------------------------------------------- 
group_:
    jsr spull
    lda tos + 0
    sta nos + 1
    lda NUL
    sta nos + 0

    ; group is 64 bytes
    lsr nos + 1
    ror nos + 0
    lsr nos + 1
    ror nos + 0
    ; save last group
    lda vDefs + 0
    sta tos + 0
    lda vDefs + 1
    sta tos + 1
    jsr rpush
    ; set origin
    lda defs + 0
    sta tos + 0
    lda defs + 1
    sta tos + 1
    ; update group
    clc
    lda tos + 0
    adc nos + 0
    sta vDefs + 0
    lda tos + 1
    adc nos + 1
    sta vDefs + 1

    jmp (vNext)

;---------------------------------------------------------------------- 
endGroup_:
    ; load last group
    jsr rpull
    lda tos + 0
    sta vDefs + 0
    lda tos + 1
    sta vDefs + 1
    jmp (vNext)

;---------------------------------------------------------------------- 
getRef_:
    jsr nosp
    sta ap
    jsr lookupDefs
    jmp fetch_

;---------------------------------------------------------------------- 
arrDef_:
    lda FALSE
    beq arrDef1

;---------------------------------------------------------------------- 
cArrDef_:
    lda TRUE
    ; fall throught

;---------------------------------------------------------------------- 
arrDef1:
    sta vByteMode
    
    ; array next
    lda #<compNext
    sta vNext + 0
    lda #>compNext
    sta vNext + 1

    ; array start
    lda vHeapPtr + 0
    sta tos + 0
    lda vHeapPtr + 1
    sta tos + 1

    jsr rpush
    jmp next
    
;---------------------------------------------------------------------- 
arrEnd_:

    jsr rpull
    jsr spush ; ????

    lda vHeapPtr + 0
    sta nos + 0
    lda vHeapPtr + 1
    sta nos + 1

    sec
    lda nos + 0
    sbc tos + 0
    sta tos + 0
    lda nos + 1
    sbc tos + 1
    sta tos + 1

    lda vByteMode
    cmp FALSE
    bne @isne
    ; words
    lsr tos + 0
    ror tos + 1
@isne:    
    jsr spush

    lda #<next
    sta vNext + 0
    lda #>next
    sta vNext + 1

    jmp (vNext)

;---------------------------------------------------------------------- 
def_:
    ; skip spaces
    jsr nosp
    sta ap
    jsr lookupDefs
    lda vHeapPtr + 0
    sta (tos), y
    sta nos + 0
    iny
    lda vHeapPtr + 1
    sta (tos), y
    sta nos + 1
    ldy NUL
    ; copy until 255 or ; 
@loop:
    lda (ips), y
    sta (nos), y
    iny
    beq @ends
    cmp #';'
    bne @loop
@ends:
    tya
    sta ap
    ; update Heap
    clc
    adc vHeapPtr + 0
    sta vHeapPtr + 0
    bcc @iscc
    inc vHeapPtr + 1
@iscc:
    lda ap
    ; update ip
    clc
    adc ips + 0
    sta ips + 0
    bcc @iscci
    inc ips + 1
@iscci:
    jmp (vNext)

;---------------------------------------------------------------------- 
break_:
    jsr spull
    lda tos + 0
    ora tos + 1
    bne @isne
    jmp (vNext)
@isne:
    lda yp
    clc
    adc #$06
    jmp begin1

;---------------------------------------------------------------------- 
; Left parentesis ( begins a loop
begin_:

    ; tos is zero ?
    jsr spull
    lda tos + 0
    ora tos + 1
    beq begin1

    ; alloc frame
    lda yp
    sec
    sbc #6
    sta yp

    ldy yp
    ; counter
    lda NUL
    sta rpz + 0, y
    sta rpz + 1, y
    ; limit
    lda tos + 0
    sta rpz + 2, y
    lda tos + 1
    sta rpz + 3, y
    ; pointer
    lda ips + 0
    sta rpz + 4, y
    lda ips + 1
    sta rpz + 5, y
    jmp (vNext) 

begin1: 
    lda #$01
    sta ns

; escape
@loop: 
    jsr incps
    jsr ldaps
    jsr nesting 
    lda ns
    eor ns
    bne @loop
    jmp (vNext) 

;----------------------------------------------------------------------
; Right parentesis ) again a loop 
again_: 
    ldy yp
    ; counter
    lda rpz + 0, y
    sta wrk + 0
    lda rpz + 1, y
    sta wrk + 1

    ; check if IFTEMode $FFFF

    lda wrk + 0
    and wrk + 1
    sta ap
    inc ap
    bne again1
    
    ; push FALSE
    lda FALSE
    sta tos + 0
    sta tos + 1
    jsr spush

    ; drop IFTEMmode
    lda yp
    clc
    adc #2
    jmp (vNext)
 
again1: 
    ; peek loop limit 
    lda rpz + 2
    sta nos + 0                 
    lda rpz + 3
    sta nos + 3

    ; test end
    sec
    lda nos + 0
    sbc wrk + 0
    bne @noeq
    lda nos + 1
    sbc wrk + 1
    bne @noeq

    ; drop loop vars
    lda yp
    clc
    adc #6
    sta yp
    jmp (vNext)

@noeq:
    ; increase counter
    inc wrk + 0
    bne @novr
    inc wrk + 1
@novr:    
    ; poke loop var 
    lda wrk + 0
    sta rpz + 0
    lda wrk + 1
    sta rpz + 1

    ; return at begin    
    lda rpz + 4, y
    sta ips + 0
    lda rpz + 5, y
    sta ips + 1

    jmp (vNext) 
 
;----------------------------------------------------------------------
j_:
    lda yp
    sec
    sbc #6
    tay
    ; fall through
;----------------------------------------------------------------------
i_:
    ldy yp
    lda spz + 0, y
    sta tos + 0
    iny
    lda spz + 0, y
    sta tos + 1
    jsr spush
    jmp (vNext)

;----------------------------------------------------------------------
ifte_:
    jsr spull
    lda tos + 0
    ora tos + 1
    bne @isne
    inc tos + 0
    jsr spush
    jmp begin1
@isne:
    lda #$FF
    sta tos + 0
    sta tos + 1
    jsr spush
    jmp (vNext)

;---------------------------------------------------------------------- 
ret_:
    jsr pullps
    jmp (vNext)

;---------------------------------------------------------------------- 
; 
exit_:
    jsr incps
    lda ips + 0
    sta tos + 0
    lda ips + 1
    sta tos + 1
    jsr pullps
    jmp (tos)

;---------------------------------------------------------------------- 
; 6502 stack is fixed and round robin
; no need control deep
etx_:
    jmp interpret
    
;---------------------------------------------------------------------- 
iSysVars:
    .word  dStack               ; a vS0
    .word  FALSE                ; b vBase16
    .word  tib                  ; c vTIBPtr
    .word  DEFS                 ; d vDEFS
    .word  FALSE                ; e vEdited
    .word  rStack               ; f vR0
    .word  next                 ; g dispatcher
    .word  HEAP                 ; h vHeapPtr
fSysVars:

dysys = fSysVars - iSysVars

;---------------------------------------------------------------------- 
mint_:

; wise

    sei
    lda #$FF
    tay
    tax
    txs
    lda NUL
    sta ns
    cld
    cli

    jsr initialize

    jsr printStr
    .asciiz "MINT 6502 V1.0\r\n"

    jmp interpret

;---------------------------------------------------------------------- 
initialize:

; defaults values
    lda #<vars
    sta tos + 0
    lda #>vars
    sta tos + 1
    lda #<vsys
    sta nos + 0
    lda #>vsys
    sta nos + 1
    lda GRPSIZE
    tay
    lda NUL
@loop1:
    sta (tos), y
    sta (nos), y
    dey
    sta (tos), y
    sta (nos), y
    dey
    bne @loop1

; default function
    lda #<defs
    sta wrk + 0
    lda #>defs
    sta wrk + 1
    ldy #(GRPSIZE/2 * NUMGRPS)
@loop2:
    dey
    lda #<empty_
    sta (wrk), y
    dey
    lda #>empty_
    sta (wrk), y
    cpy NUL
    bne @loop2

; default system values 
    ldy dysys
@loop3:
    lda iSysVars, y    
    sta vsys, y
    dey
    bne @loop3

; safe
    jmp ldnext

;---------------------------------------------------------------------- 
ldnext:
    lda #<next
    sta vNext + 0
    lda #>next
    sta vNext + 1
    rts

;---------------------------------------------------------------------- 
ldcomp:
    lda #<compNext
    sta vNext + 0
    lda #>compNext
    sta vNext + 1
    rts

;---------------------------------------------------------------------- 
;optcodes:
;altcodes:
;ctlcodes:

.include "jumptablesdual.s"

