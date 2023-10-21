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

;--------------------------------------------------------
;
;	specifc for ca65 assembler 
;
;--------------------------------------------------------
; enable listing

.list on

; identifiers

.case +

; debug

.debuginfo +

; enable features

.feature c_comments

; .feature string_escapes

.feature org_per_seg

.feature dollar_is_pc

.feature pc_assignment

; enable 6502 mode

.p02

 
;--------------------------------------------------------
; constants, must be.
;

    TRUE        = 1 
    FALSE       = 0 

    CR  = 13
    LF  = 10
    BS  = 9
    ETX = 3
    NUL = 0
 
    ; size page
    PAGE = $100

    ; group size
    GRPSIZE = $40

    ; groups for defs, could be more
    NUMGRPS = 6 

;---------------------------------------------------------------------- 
; page 0, reserved cells 
.segment "ZERO"
; offset
* = $00F0

; index for data stack
sps:    .byte $0
; index for return stack
rps:    .byte $0
; instruction pointer
ips:    .word $0
; copycat for registers
yp: 	.byte $0
xp: 	.byte $0
ap: 	.byte $0
ns: 	.byte $0
; pseudo registers
tos:    .word $0
nos:    .word $0
wrk:    .word $0
tmp:    .word $0

;---------------------------------------------------------------------- 
.segment "VECTORS"

.word init
.word init
.word init

;---------------------------------------------------------------------- 
.segment "CODE"

; start of RAM

;.align $100

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

vEdited:
    .byte $0

vByteMode:
    .byte $0

; heap must be here ! 
heap:
    .word $0

;---------------------------------------------------------------------- 
; aliases

vS0      =  vsys + $00     ;    a  ; start of data stack 
vBase16  =  vsys + $02     ;    b  ; base16 flag
vTIBPtr  =  vsys + $04     ;    c  ; TIBPtr variable
vDefs    =  vsys + $08     ;    d  ; reference for group user functions
;        =  vsys + $0a     ;    e  ; 
vR0      =  vsys + $0c     ;    f  ; start of return stack 
vNext    =  vsys + $0e     ;    g  ; next routine dispatcher
vHeapPtr =  vsys + $10     ;    h  ; heap ptr variable

; the address of stacks are hardcoded, any change do no apply
dStack = vS0
rStack = vR0
; any change will cause unexpected behavior
HEAP = heap
DEFS = defs

;---------------------------------------------------------------------- 
.segment "ONCE"
 
; start of ROM

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
; get a char
key_:
    jsr getchar
keyk:
    sta tos + 0
    jsr spush
    ; next 
    jmp (vNext)
    
;---------------------------------------------------------------------- 
; put a char
emit_:
    jsr spull
    lda tos + 0
    jsr putchar
    ; next 
    jmp (vNext)

;---------------------------------------------------------------------- 
; hit a char ?
keyq_:
    jsr hitchar
    clv
    bvc keyk

;---------------------------------------------------------------------- 
; NOOP
aNop_:
nop_:
    ; next 
    jmp next

;---------------------------------------------------------------------- 
; add a byte offset to instruction pointer
addps:    
; update ip
    clc
    adc ips + 0
    sta ips + 0
    bcc @iscc
    inc ips + 1
@iscc:
    ; next 
    jmp (vNext)

;---------------------------------------------------------------------- 
; load a byte and increase instruction pointer 
ldaps: 
    ldy NUL 
    lda (ips), y 
    inc ips + 0 
    bne @noeq 
    inc ips + 1 
@noeq: 
    rts 
 
;---------------------------------------------------------------------- 
; copy vHeap to nos
heap2nos:
    ; array start
    lda vHeapPtr + 0
    sta nos + 0
    lda vHeapPtr + 1
    sta nos + 1
    rts

add2heap:
    clc
    adc vHeapPtr + 0
    sta vHeapPtr + 0
    bcc @ends
    inc vHeapPtr + 1
@iends:
    rts 

;---------------------------------------------------------------------- 
pushps:
; push ps into RS 
    ldy rps 
    dey 
    dey 
    lda ips + 0 
    sta rpz + 0, y 
    lda ips + 1 
    sta rpz + 1, y 
    sty rps 
    rts

;---------------------------------------------------------------------- 
pullps:
; pull ps from RS 
    ldy rps 
    lda rpz + 0, y 
    sta ips + 0 
    lda rpz + 1, y 
    sta ips + 1 
    iny 
    iny 
    sty rps 
    rts

;---------------------------------------------------------------------- 
; push tos into return stack 
rpush: 
    ldy rps 
    dey 
    dey 
    lda tos + 0 
    sta rpz + 0, y 
    lda tos + 1 
    lda rpz + 1, y 
    sty rps 
    rts 
 
;---------------------------------------------------------------------- 
; push tos from return stack 
rpull: 
    ldy rps 
    lda rpz + 0, y 
    sta tos + 0 
    lda rpz + 1, y 
    sta tos + 1 
    iny 
    iny 
    sty rps 
    rts 
 
;---------------------------------------------------------------------- 
; push tos into stack 
spush: 
    ldx sps
    dex 
    dex 
    lda tos + 0 
    sta spz + 0, x 
    lda tos + 1 
    sta spz + 1, x 
    stx sps
    rts 
 
;---------------------------------------------------------------------- 
; pull tos from stack 
spull: 
    ldx sps
    lda spz + 0, x 
    sta tos + 0 
    lda spz + 1, x 
    sta tos + 1 
    inx 
    inx 
    stx sps
    rts 
 
;---------------------------------------------------------------------- 
; take two from stack 
take2: 
    ldx sps
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
    stx sps
    rts 

;---------------------------------------------------------------------- 
; classic R>
R2S_:
    jsr rpull
    jsr spush
    ; next 
    jmp (vNext) 

;---------------------------------------------------------------------- 
; classic >R
S2R_:
    jsr spull
    jsr rpush
    ; next 
    jmp (vNext) 

;---------------------------------------------------------------------- 
; NEGate the value on top of stack (2's complement) 
neg_: 
    ldx sps
    sec 
    lda NUL 
    sbc spz + 0, x 
    sta spz + 0, x 
    sec 
    lda NUL
    sbc spz + 1, x 
    sta spz + 1, x 
    jmp (vNext) 
 
;---------------------------------------------------------------------- 
; Bitwise INVert the top member of the stack (1's complement) 
inv_: 
    ldx sps
    lda #$FF 
    eor spz + 0, x 
    sta spz + 0, x 
    lda #$FF 
    eor spz + 1, x 
    sta spz + 1, x 
    ; next 
    jmp (vNext) 
 
;---------------------------------------------------------------------- 
; copy 1st element of the stack 
; a b c -- a b c c 
dup_: 
    ldx sps
    dex 
    dex 
    lda spz + 2, x 
    sta spz + 0, x 
    lda spz + 3, x 
    sta spz + 1, x 
    stx sps
    ; next 
    jmp (vNext) 
 
;---------------------------------------------------------------------- 
; copy 2nd element of the stack 
; a b c -- a b c b 
over_: 
    ldx sps
    dex 
    dex 
    lda spz + 4, x 
    sta spz + 0, x 
    lda spz + 5, x 
    sta spz + 1, x 
    stx sps
    ; next 
    jmp (vNext) 
 
;---------------------------------------------------------------------- 
; Rotate down 3 elements at stack 
; a b c -- b c a 
rot_: 
    ldx sps
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
    ; next 
    jmp (vNext) 
 
;---------------------------------------------------------------------- 
; Swap 2nd and 1st elements of the stack 
; a b c -- a c b 
swap_: 
    ldx sps
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
    ; next 
    jmp (vNext) 
 
;---------------------------------------------------------------------- 
;  Left shift { is multply by 2 
shl_: 
    ldx sps
    asl spz + 0, x 
    rol spz + 1, x 
    ; next 
    jmp (vNext) 
 
;---------------------------------------------------------------------- 
;  Right shift } is a divide by 2 
shr_: 
    ldx sps
    lsr spz + 0, x 
    ror spz + 1, x 
    ; next 
    jmp (vNext) 
 
;---------------------------------------------------------------------- 
; Drop the 1st element of the stack 
; a b c -- a b 
drop_: 
    ldx sps
    inx 
    inx 
    stx sps
    ; next 
    jmp (vNext) 
 
;---------------------------------------------------------------------- 
;  Bitwise AND the top 2 elements of the stack 
and_: 
    ldx sps
    lda spz + 2, x 
    and spz + 0, x 
    sta spz + 2, x 
    lda spz + 3, x 
    and spz + 1, x 
    sta spz + 3, x 
    jmp drop_ 
 
;---------------------------------------------------------------------- 
;  Bitwise OR the top 2 elements of the stack 
or_: 
    ldx sps
    lda spz + 2, x 
    ora spz + 0, x 
    sta spz + 2, x 
    lda spz + 3, x 
    ora spz + 1, x 
    sta spz + 3, x 
    jmp drop_ 
 
;---------------------------------------------------------------------- 
;  Bitwise XOR the top 2 elements of the stack 
xor_: 
    ldx sps
    lda spz + 2, x 
    eor spz + 0, x 
    sta spz + 2, x 
    lda spz + 3, x 
    eor spz + 1, x 
    sta spz + 3, x 
    jmp drop_ 
 
;---------------------------------------------------------------------- 
; Add the top 2 members of the stack 
; a b c -- a (b+c) 
add_:   
    ldx sps
    clc 
    lda spz + 2, x 
    adc spz + 0, x 
    sta spz + 2, x 
    lda spz + 3, x 
    adc spz + 1, x 
    sta spz + 3, x 
    jmp drop_ 
 
;---------------------------------------------------------------------- 
; Subtract the top 2 members of the stack 
; a b c -- a (b-c) 
sub_: 
    ldx sps
    sec 
    lda spz + 2, x 
    sbc spz + 0, x 
    sta spz + 2, x 
    lda spz + 3, x 
    sbc spz + 1, x 
    sta spz + 3, x 
    jmp drop_ 
 
;---------------------------------------------------------------------- 
; prepare for mult or divd
opin:
    ldx sps 
    ; pseudo tos
    lda spz + 0, x
    sta wrk + 0
    lda spz + 1, x
    sta wrk + 1
    ; pseudo nos 
    lda spz + 2, x
    sta tmp + 0
    lda spz + 3, x
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
    ldx sps
    lda nos + 0
    sta spz + 0, x
    lda nos + 1
    sta spz + 1, x
    lda tos + 0
    sta spz + 2, x
    lda tos + 1
    sta spz + 3, x
    rts

;---------------------------------------------------------------------- 
; Divide the top 2 cell of the stack 
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
    
    sec
    lda tos + 0
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
    ; next 
    jmp (vNext)

;---------------------------------------------------------------------- 
; 16-bit multiply 16x16, 32 result
; http://codebase64.org/doku.php?id=base:16bit_multiplication_32-bit_product
; ( multiplicand multiplier -- resultMSW resultLSW )
; ( tmp wrk -- nos tos )
mul_:
    jsr opin
@shift_r:
    ; divide by 2
    lsr wrk + 1
    ror wrk + 0
    bcc @rotate_r
    ; add multiplicand to upper half product
    clc
    lda tos + 0
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
    ; next 
    jmp (vNext)
 
;---------------------------------------------------------------------- 
; \+    a b c -- a ; [c]+b  ; increment variable at c by b 
incr_: 
    jsr take2 
    ldy NUL 
    clc 
    lda (tos), y 
    adc nos + 0 
    sta (tos), y 
    iny 
    lda (tos), y 
    adc nos + 1 
    sta (tos), y 
    ; next 
    jmp (vNext) 
 
;---------------------------------------------------------------------- 
; \-    a b c -- a ; [c]-b  ; decrement variable at c by b 
decr_: 
    jsr take2 
    ldy NUL 
    sec 
    lda (tos), y 
    sbc nos + 0 
    sta (tos), y 
    iny 
    lda (tos), y 
    sbc nos + 1 
    sta (tos), y 
    ; next 
    jmp (vNext) 
 
;---------------------------------------------------------------------- 
; false 
false2: 
    ldx sps
    lda NUL 
    sta spz + 2, x 
    sta spz + 3, x 
    jmp drop_ 
 
;---------------------------------------------------------------------- 
; true 
true2: 
    ldx sps
    lda #$01 
    sta spz + 2, x 
    lda NUL 
    sta spz + 3, x 
    jmp drop_ 
 
;---------------------------------------------------------------------- 
; subtract for compare 
cmps: 
    ldx sps
    sec 
    lda spz + 2, x 
    sbc spz + 0, x 
    lda spz + 3, x 
    sbc spz + 1, x 
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
    sec
    sta tos + 1 
    jmp isfetch 

;---------------------------------------------------------------------- 
; fetch a word 
fetch_: 
    clc 
    jmp isfetch 
 
;---------------------------------------------------------------------- 
isfetch: 
    ; load the reference 
    ldx sps
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
@cset:    
    ; next 
    jmp (vNext) 
 
;---------------------------------------------------------------------- 
; puts a string 
str_: 
    ldy NUL
@loop:
    lda (ips), y
    beq @ends       ; NUL
    cmp #'`'        ; ` is the string terminator 
    beq @ends 
    jsr putchar 
    iny
    bne @loop       ; limit 255
@ends: 
    ; next 
    jmp (vNext) 
 
;---------------------------------------------------------------------- 
; $00 to $1F, reserved for macros
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
    jsr tib2tos
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
    ; ge space ? 
    cmp #32                 
    bcs @ischar 
    ; is it end of string ? 
    cmp #$0                
    beq @endstr 
    ; windows CRLF, linux CR, Mac LF
    cmp CR                 ; carriage return ? 
    beq @iscrlf 
    cmp LF                 ; line feed ? 
    beq @iscrlf 

@ismacro: 
    ; $00 to $1F
    jmp macro 

@ischar: 
    jsr @echo 
    ; nest ? 
    jsr nesting 
    ; wait for next character 
    clv
    bvc @loop            

@iscrlf: 
    lda CR 
    jsr @echo 
    lda LF 
    jsr @echo 
    ; pending nest ? 
    lda ns 
    cmp NUL 
    beq @loop 

; mark with etx, 
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
    ; next 
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
    ; reference
    pla 
    sta tos + 1 
    pla 
    sta tos + 0 
    jsr putstr 
    ; offset
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
    beq @ends   ; limit NUL 
    jsr putchar
    iny
    bne @loop   ; limit 255
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
    sec 
    lda tos + 0 
    sbc nos + 0 
    sta tos + 0 
    lda tos + 1 
    sbc nos + 1 
    sta tos + 1 
    bcc @loop 
    clc 
    lda tos + 0 
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
prenum:
    lda NUL 
    sta tos + 0 
    sta tos + 1 
    rts

;---------------------------------------------------------------------- 
; convert a decimal value to binary 
num_: 
    jsr prenum
@loop: 
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
    ; next 
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
    jsr prenum
@loop: 
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
    ; next 
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
    beq @ends   ; limit 255 
    lda (ips), y
    cmp CR 
    bne @loop 
    ; skip \r ?
    ; iny
    ; skip \n ?
    ; iny
    ; offset
@ends:
    tya
    jmp addps
 
;---------------------------------------------------------------------- 
depth_: 
    ; limit to 255 bytes
    sec 
    lda #$FF 
    sbc sps 
    ; words 
    lsr
    sta tos + 0 
    lda NUL 
    sta tos + 1 
    jsr spush 
    ; next 
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
    jsr putchar 
    ; next 
    jmp (vNext) 
 
;---------------------------------------------------------------------- 
newln_: 
    jsr crlf 
    ; next 
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
    ; next 
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
; ascii code
charCode_:
    jsr ldaps
    sta tos + 0
    lda NUL
    sta tos + 1
    jsr spush
    ; next 
    jmp (vNext)

;---------------------------------------------------------------------- 
; copy and update 
compNext:
    ; array start
    jsr heap2nos

    jsr spull

    ; byte
    ldy NUL
    lda tos + 0
    sta (nos), y
    iny
    
    lda vByteMode + 0
    bne @isbm  
    
    ; word
    lda tos + 1
    sta (nos), y
    iny
@isbm:
    
    tya
    jsr add2heap
    ; fall throught 

;---------------------------------------------------------------------- 
; Execute next opcode
next: 
opt_:
    ; using full jump table 
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
    jsr ldaps
    tay 
    lda altcodeslo, y 
    sta wrk + 0 
    lda altcodeshi, y 
    sta wrk + 1 
    jmp (wrk) 
 
;---------------------------------------------------------------------- 
; Execute code inline 
enter:                           
; pull from system stack
    pla 
    sta ips + 0 
    pla
    sta ips + 1 
    ; next 
    jmp (vNext) 

;---------------------------------------------------------------------- 
; char 0, Continue from enter 
exit_:
    jmp (ips)

;---------------------------------------------------------------------- 
; Execute code from data stack 
; 
exec_:
    jsr spull
    jmp (tos)

;---------------------------------------------------------------------- 
ret_:
    jsr pullps
    ; next 
    jmp (vNext)

;---------------------------------------------------------------------- 
; Interpret code from data stack
go_: 
    jsr pushps
; pull ps from data stack 
    ldx sps
    lda spz + 0, x 
    sta ips + 0 
    lda spz + 1, x 
    sta ips + 1 
    inx 
    inx 
    stx sps
    ; next 
    jmp (vNext) 
 
;---------------------------------------------------------------------- 
; Execute code from a user function
call_:
    sta ap
    jsr pushps

    jsr lookupDefs
    tya
    clc
    adc tos + 0
    sta ips + 0
    adc tos + 1
    sta ips + 1

    ; next 
    jmp (vNext)

lookupDeft:
    lda ap
    sta vEdited
    ; fall throught

lookupDefs:
    sec
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
; Copy a user macro to tib 
; lookup up def based on a number at data stack
;
editDef_:
    ; which one 
    jsr spull
    
    ; toChar
    clc
    lda #'A'
    adc tos + 0
    sta ap
 
    jsr lookupDeft

    tya
    clc
    adc tos + 0
    sta ips + 0
    adc tos + 1
    sta ips + 1
    
    ; origin
    lda (tos), y
    sta nos + 0
    iny
    lda (tos), y
    sta nos + 1

    ldy NUL
    ; empty ?
    lda (nos), y
    beq editDef3    ; is NUL ?
    cmp #';'        ; is end ?
    beq editDef3

    ; else
    ; ldy NUL

    ; destiny
    jsr tib2tos

    lda #':'
    jsr writeChar
    jsr inctos

    lda ap
    jsr writeChar
    jsr inctos
    
    clv
    bvc editDef2

editDef1:
    iny
    beq editDef3

editDef2:
    lda (nos), y
    jsr writeChar
    cmp #';'
    bne editDef1

editDef3:
    lda #<tib
    sta vTIBPtr + 0
    lda #>tib
    sta vTIBPtr + 1
    ; next 
    jmp (vNext)

;---------------------------------------------------------------------- 
writeChar: 
    sta (tos), y
    jmp putchar

;---------------------------------------------------------------------- 
inctos:
    inc tos + 0
    bcc @iscc1
    inc tos + 1
@iscc1:
    rts
    
;---------------------------------------------------------------------- 
tib2tos:
    lda #<tib
    sta tos + 0
    lda #>tib
    sta tos + 1
    rts

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
    sec 
    lda ap 
    sbc #'a' 
    asl 
    clc 
    adc tos + 0 
    bcc @iscc 
    inc tos + 1 
@iscc:
    jsr spush 
    ; next 
    jmp (vNext) 
 
;---------------------------------------------------------------------- 
; skip spaces
nosp:
    jsr ldaps
    cmp #' '
    beq nosp
    rts

;---------------------------------------------------------------------- 
group_:
    jsr spull
    ; multiply by GROUP (64)
    ; swap byte
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
    ; update group
    lda defs + 0
    clc
    adc nos + 0
    sta vDefs + 0
    lda defs + 1
    adc nos + 1
    sta vDefs + 1
    ; next 
    jmp (vNext)

;---------------------------------------------------------------------- 
endGroup_:
    ; load last group
    jsr rpull
    lda tos + 0
    sta vDefs + 0
    lda tos + 1
    sta vDefs + 1
    ; next 
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

    ; save array start
    ldx rps
    dex
    dex
    lda vHeapPtr + 0
    sta rpz + 0, x
    lda vHeapPtr + 1
    sta rpz + 1, x
    sty rps

    ; next 
    jmp next
    
;---------------------------------------------------------------------- 
arrEnd_:

    ; start of array
    jsr rpull
    jsr spush 

    ; bytes
    sec
    lda vHeapPtr + 0
    sbc tos + 0
    sta tos + 0
    lda vHeapPtr + 1
    sbc tos + 1
    sta tos + 1

    lda vByteMode
    bne @isne	
    ; words
    lsr tos + 0
    ror tos + 1
@isne:    
    ; size of array
    jsr spush

    lda #<next
    sta vNext + 0
    lda #>next
    sta vNext + 1

    ; next 
    jmp (vNext)

;---------------------------------------------------------------------- 
def_:
    ; skip spaces
    jsr nosp

    ; get slot at list
    sta ap
    jsr lookupDefs

    ; offset
    tya
    clc
    adc tos + 0
    sta tos + 0
    lda NUL
    adc tos + 1
    sta tos + 1

    ; get heap
    jsr heap2nos
    
    ; put heap at list
    lda nos + 0
    sta (tos), y
    iny
    lda nos + 1
    sta (tos), y

    ; copy  
    ldy NUL
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
    jsr add2heap

    lda ap
    jmp addps

;---------------------------------------------------------------------- 
break_:
    jsr spull
    lda tos + 0
    bne @isne
    jmp (vNext)
@isne:
    ; drop frame
    clc
    lda rps
    adc #$06
    sta rps
    jmp skipnest

;---------------------------------------------------------------------- 
; Left parentesis ( begins a loop
begin_:

    ; tos is zero ?
    jsr spull
    lda tos + 0
    beq skipnest

    ; alloc frame 
    sec
    lda rps
    sbc #6
    sta rps

    ; make a frame
    ldy rps
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
    ; next 
    jmp (vNext) 

;----------------------------------------------------------------------
; skip while nest
skipnest: 
    lda #$01
    sta ns
@loop: 
    jsr ldaps
    jsr nesting 
    lda ns
    bne @loop
    ; next 
    jmp (vNext) 

;----------------------------------------------------------------------
; Right parentesis ) again a loop 
again_: 
    ldy rps
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
    ; tax
    ; inx
    bne again1
    
    ; push FALSE
    lda FALSE
    sta tos + 0
    sta tos + 1
    jsr spush

    ; drop IFTEMmode
    clc
    lda rps
    adc #2
    sta rps
    ; next 
    jmp (vNext)
 
again1: 
    ldy rps
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

    ; end of loop
    ; drop frame
    clc
    lda yp
    adc #6
    sta yp
    ; next 
    jmp (vNext)

@noeq:
    ; increase counter
    inc wrk + 0
    bne @novr
    inc wrk + 1
@novr:    
    ldy rpz
    ; poke loop var 
    lda wrk + 0
    sta rpz + 0, y
    lda wrk + 1
    sta rpz + 1, y

    ; return at begin    
    lda rpz + 4, y
    sta ips + 0
    lda rpz + 5, y
    sta ips + 1

    ; next 
    jmp (vNext) 
 
;----------------------------------------------------------------------
j_:
    sec
    lda rps
    sbc #6
    tay
    jmp indx

;----------------------------------------------------------------------
i_:
    ldy rps
    ; fall through

;----------------------------------------------------------------------
indx:
    lda spz + 0, y
    sta tos + 0
    iny
    lda spz + 0, y
    sta tos + 1
    jsr spush
    ; next 
    jmp (vNext)

;----------------------------------------------------------------------
ifte_:
    jsr spull
    lda tos + 0
    ora tos + 1
    bne @isne
    inc tos + 0
    jsr spush
    jmp skipnest
@isne:
    lda #$FF
    sta tos + 0
    sta tos + 1
    jsr rpush
    ; next 
    jmp (vNext)

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
    .word  defs                 ; d vDEFS
    .word  FALSE                ; e vEdited
    .word  rStack               ; f vR0
    .word  next                 ; g dispatcher
    .word  heap                 ; h vHeapPtr
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

; default system values 
    ldy dysys
@loop:
    lda iSysVars, y    
    sta vsys, y
    dey
    bne @loop

; safe
    lda #<next
    sta vNext + 0
    lda #>next
    sta vNext + 1

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
    ldy GRPSIZE
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
    lda #<(GRPSIZE * NUMGRPS)
    sta nos + 0
    lda #>(GRPSIZE * NUMGRPS)
    sta nos + 1
    lda #<defs
    sta tos + 0
    lda #>defs
    sta tos + 1

@loop2:
    ; default
    ldy NUL
    lda #<empty_
    sta (tos), y
    iny
    lda #>empty_
    sta (tos), y

    ; increment 
    clc
    lda tos + 0
    adc #2
    sta tos + 0
    lda tos + 1
    adc #0
    sta tos + 1

    ; decrement
    sec
    lda nos + 0
    sbc #2
    sta nos + 0
    lda nos + 1
    sbc #0
    sta nos + 1

    ; ends ?
    ora nos + 0
    bne @loop2

    rts

.word $DEAD

;---------------------------------------------------------------------- 
;optcodes: parsed by opt_ (next)
;altcodes: parsed by alt_
;ctlcodes: maybe in a future...

; ********************************************************************* 
; Jump Tables, optmized for single index
; ********************************************************************* 

; .align $100 
 
;---------------------------------------------------------------------- 
optcodeslo: 
   .byte  <exit_    ;   NUL 
   .byte  <nop_     ;   SOH 
   .byte  <nop_     ;   STX 
   .byte  <etx_     ;   ETX 
   .byte  <nop_     ;   EOT 
   .byte  <nop_     ;   ENQ 
   .byte  <nop_     ;   apK 
   .byte  <nop_     ;   BEL 
   .byte  <nop_     ;   BS 
   .byte  <nop_     ;   TAB 
   .byte  <nop_     ;   LF 
   .byte  <nop_     ;   VT 
   .byte  <nop_     ;   FF 
   .byte  <nop_     ;   CR 
   .byte  <nop_     ;   SO 
   .byte  <nop_     ;   SI 
   .byte  <nop_     ;   DLE 
   .byte  <nop_     ;   DC1 
   .byte  <nop_     ;   DC2 
   .byte  <nop_     ;   DC3 
   .byte  <nop_     ;   DC4 
   .byte  <nop_     ;   NAK 
   .byte  <nop_     ;   SYN 
   .byte  <nop_     ;   ETB 
   .byte  <nop_     ;   CAN 
   .byte  <nop_     ;   EM 
   .byte  <nop_     ;   SUB 
   .byte  <nop_     ;   ESC 
   .byte  <nop_     ;   FS 
   .byte  <nop_     ;   GS 
   .byte  <nop_     ;   RS 
   .byte  <nop_     ;   US 
   .byte  <nop_     ;   SP 
   .byte  <store_   ;   ! 
   .byte  <dup_     ;   " 
   .byte  <hex_     ;    # 
   .byte  <swap_    ;    $ 
   .byte  <over_    ;    % 
   .byte  <and_     ;    & 
   .byte  <drop_    ;    ' 
   .byte  <begin_   ;    ( 
   .byte  <again_   ;    ) 
   .byte  <mul_     ;    * multiply 16x16
   .byte  <add_     ;    + 
   .byte  <hdot_    ;    , 
   .byte  <sub_     ;    - 
   .byte  <dot_     ;    . 
   .byte  <div_     ;    / divide 16x16
   .byte  <num_     ;    0 
   .byte  <num_     ;    1 
   .byte  <num_     ;    2 
   .byte  <num_     ;    3 
   .byte  <num_     ;    4 
   .byte  <num_     ;    5 
   .byte  <num_     ;    6 
   .byte  <num_     ;    7 
   .byte  <num_     ;    8 
   .byte  <num_     ;    9 
   .byte  <def_     ;    : 
   .byte  <ret_     ;    ; 
   .byte  <lt_      ;    < 
   .byte  <eq_      ;    = 
   .byte  <gt_      ;    > 
   .byte  <getRef_  ;    ? 
   .byte  <fetch_   ;    @ 
   .byte  <call_    ;    A 
   .byte  <call_    ;    B 
   .byte  <call_    ;    C 
   .byte  <call_    ;    D 
   .byte  <call_    ;    E 
   .byte  <call_    ;    F 
   .byte  <call_    ;    G 
   .byte  <call_    ;    H 
   .byte  <call_    ;    I 
   .byte  <call_    ;    J 
   .byte  <call_    ;    K 
   .byte  <call_    ;    L 
   .byte  <call_    ;    M 
   .byte  <call_    ;    N 
   .byte  <call_    ;    O 
   .byte  <call_    ;    P 
   .byte  <call_    ;    Q 
   .byte  <call_    ;    R 
   .byte  <call_    ;    S 
   .byte  <call_    ;    T 
   .byte  <call_    ;    U 
   .byte  <call_    ;    V 
   .byte  <call_    ;    W 
   .byte  <call_    ;    X 
   .byte  <call_    ;    Y 
   .byte  <call_    ;    Z 
   .byte  <arrDef_  ;    [ 
   .byte  <alt_     ;    \ 
   .byte  <arrEnd_  ;    ] 
   .byte  <xor_     ;    ^ 
   .byte  <neg_     ;    _ 
   .byte  <str_     ;    ` 
   .byte  <var_     ;    a 
   .byte  <var_     ;    b 
   .byte  <var_     ;    c 
   .byte  <var_     ;    d 
   .byte  <var_     ;    e 
   .byte  <var_     ;    f 
   .byte  <var_     ;    g 
   .byte  <var_     ;    h 
   .byte  <var_     ;    i 
   .byte  <var_     ;    j 
   .byte  <var_     ;    k 
   .byte  <var_     ;    l 
   .byte  <var_     ;    m 
   .byte  <var_     ;    n 
   .byte  <var_     ;    o 
   .byte  <var_     ;    p 
   .byte  <var_     ;    q 
   .byte  <var_     ;    r 
   .byte  <var_     ;    s 
   .byte  <var_     ;    t 
   .byte  <var_     ;    u 
   .byte  <var_     ;    v 
   .byte  <var_     ;    w 
   .byte  <var_     ;    x 
   .byte  <var_     ;    y 
   .byte  <var_     ;    z 
   .byte  <shl_     ;    { 
   .byte  <or_      ;    | 
   .byte  <shr_     ;    } 
   .byte  <inv_     ;    ~ 
   .byte  <nop_     ;    backspace 

optcodeshi: 
   .byte  >exit_    ;   NUL 
   .byte  >nop_     ;   SOH 
   .byte  >nop_     ;   STX 
   .byte  >etx_     ;   ETX 
   .byte  >nop_     ;   EOT 
   .byte  >nop_     ;   ENQ 
   .byte  >nop_     ;   apK 
   .byte  >nop_     ;   BEL 
   .byte  >nop_     ;   BS 
   .byte  >nop_     ;   TAB 
   .byte  >nop_     ;   LF 
   .byte  >nop_     ;   VT 
   .byte  >nop_     ;   FF 
   .byte  >nop_     ;   CR 
   .byte  >nop_     ;   SO 
   .byte  >nop_     ;   SI 
   .byte  >nop_     ;   DLE 
   .byte  >nop_     ;   DC1 
   .byte  >nop_     ;   DC2 
   .byte  >nop_     ;   DC3 
   .byte  >nop_     ;   DC4 
   .byte  >nop_     ;   NAK 
   .byte  >nop_     ;   SYN 
   .byte  >nop_     ;   ETB 
   .byte  >nop_     ;   CAN 
   .byte  >nop_     ;   EM 
   .byte  >nop_     ;   SUB 
   .byte  >nop_     ;   ESC 
   .byte  >nop_     ;   FS 
   .byte  >nop_     ;   GS 
   .byte  >nop_     ;   RS 
   .byte  >nop_     ;   US 
   .byte  >nop_     ;   SP 
   .byte  >store_   ;   ! 
   .byte  >dup_     ;   " 
   .byte  >hex_     ;    # 
   .byte  >swap_    ;    $ 
   .byte  >over_    ;    % 
   .byte  >and_     ;    & 
   .byte  >drop_    ;    ' 
   .byte  >begin_   ;    ( 
   .byte  >again_   ;    ) 
   .byte  >mul_     ;    *  multiply 16x16
   .byte  >add_     ;    +
   .byte  >hdot_    ;    , 
   .byte  >sub_     ;    - 
   .byte  >dot_     ;    . 
   .byte  >div_     ;    /  divide 16x16
   .byte  >num_     ;    0 
   .byte  >num_     ;    1 
   .byte  >num_     ;    2 
   .byte  >num_     ;    3 
   .byte  >num_     ;    4 
   .byte  >num_     ;    5 
   .byte  >num_     ;    6 
   .byte  >num_     ;    7 
   .byte  >num_     ;    8 
   .byte  >num_     ;    9 
   .byte  >def_     ;    : 
   .byte  >ret_     ;    ; 
   .byte  >lt_      ;    < 
   .byte  >eq_      ;    = 
   .byte  >gt_      ;    > 
   .byte  >getRef_  ;    ? 
   .byte  >fetch_   ;    @ 
   .byte  >call_    ;    A 
   .byte  >call_    ;    B 
   .byte  >call_    ;    C 
   .byte  >call_    ;    D 
   .byte  >call_    ;    E 
   .byte  >call_    ;    F 
   .byte  >call_    ;    G 
   .byte  >call_    ;    H 
   .byte  >call_    ;    I 
   .byte  >call_    ;    J 
   .byte  >call_    ;    K 
   .byte  >call_    ;    L 
   .byte  >call_    ;    M 
   .byte  >call_    ;    N 
   .byte  >call_    ;    O 
   .byte  >call_    ;    P 
   .byte  >call_    ;    Q 
   .byte  >call_    ;    R 
   .byte  >call_    ;    S 
   .byte  >call_    ;    T 
   .byte  >call_    ;    U 
   .byte  >call_    ;    V 
   .byte  >call_    ;    W 
   .byte  >call_    ;    X 
   .byte  >call_    ;    Y 
   .byte  >call_    ;    Z 
   .byte  >arrDef_  ;    [ 
   .byte  >alt_     ;    \ 
   .byte  >arrEnd_  ;    ] 
   .byte  >xor_     ;    ^ 
   .byte  >neg_     ;    _ 
   .byte  >str_     ;    ` 
   .byte  >var_     ;    a 
   .byte  >var_     ;    b 
   .byte  >var_     ;    c 
   .byte  >var_     ;    d 
   .byte  >var_     ;    e 
   .byte  >var_     ;    f 
   .byte  >var_     ;    g 
   .byte  >var_     ;    h 
   .byte  >var_     ;    i 
   .byte  >var_     ;    j 
   .byte  >var_     ;    k 
   .byte  >var_     ;    l 
   .byte  >var_     ;    m 
   .byte  >var_     ;    n 
   .byte  >var_     ;    o 
   .byte  >var_     ;    p 
   .byte  >var_     ;    q 
   .byte  >var_     ;    r 
   .byte  >var_     ;    s 
   .byte  >var_     ;    t 
   .byte  >var_     ;    u 
   .byte  >var_     ;    v 
   .byte  >var_     ;    w 
   .byte  >var_     ;    x 
   .byte  >var_     ;    y 
   .byte  >var_     ;    z 
   .byte  >shl_     ;    { 
   .byte  >or_      ;    | 
   .byte  >shr_     ;    } 
   .byte  >inv_     ;    ~ 
   .byte  >nop_     ;    backspace 

;---------------------------------------------------------------------- 
; alternate function codes 
ctlcodeslo: 
altcodeslo: 
   .byte  <empty_      ; NUL ^@ 
   .byte  <empty_      ; SOH ^A 
   .byte  <toggleBase_ ; STX ^B 
   .byte  <empty_      ; ETX ^C 
   .byte  <empty_      ; EOT ^D 
   .byte  <edit_       ; ENQ ^E 
   .byte  <empty_      ; ACK ^F 
   .byte  <empty_      ; BEL ^G 
   .byte  <backsp_     ; BS  ^H 
   .byte  <empty_      ; TAB ^I 
   .byte  <reedit_     ; LF  ^J 
   .byte  <empty_      ; VT  ^K 
   .byte  <list_       ; FF  ^L 
   .byte  <empty_      ; CR  ^M 
   .byte  <empty_      ; SO  ^N 
   .byte  <empty_      ; SI  ^O 
   .byte  <printStack_ ; DLE ^P 
   .byte  <empty_      ; DC1 ^Q 
   .byte  <empty_      ; DC2 ^R 
   .byte  <empty_      ; DC3 ^S 
   .byte  <empty_      ; DC4 ^T 
   .byte  <empty_      ; NAK ^U 
   .byte  <empty_      ; SYN ^V 
   .byte  <empty_      ; ETB ^W 
   .byte  <empty_      ; CAN ^X 
   .byte  <empty_      ; EM  ^Y 
   .byte  <empty_      ; SUB ^Z 
   .byte  <empty_      ; ESC ^[ 
   .byte  <empty_      ; FS  ^\ 
   .byte  <empty_      ; GS  ^] 
   .byte  <empty_      ; RS  ^^ 
   .byte  <empty_      ; US  ^_) 
   .byte  <aNop_       ; SP  ^` 
   .byte  <cStore_     ;    ! 
   .byte  <aNop_       ;    " 
   .byte  <aNop_       ;    # 
   .byte  <aNop_       ;    $  ( -- adr ) text input ptr 
   .byte  <aNop_       ;    % 
   .byte  <aNop_       ;    & 
   .byte  <aNop_       ;    ' 
   .byte  <ifte_       ;    (  ( b -- ) 
   .byte  <aNop_       ;    ) 
   .byte  <aNop_       ;    *  
   .byte  <incr_       ;    +  ( adr -- ) decrements variable at address 
   .byte  <aNop_       ;    , 
   .byte  <aNop_       ;    - 
   .byte  <aNop_       ;    . 
   .byte  <aNop_       ;    /  
   .byte  <aNop_       ;    0 
   .byte  <aNop_       ;    1 
   .byte  <aNop_       ;    2 
   .byte  <aNop_       ;    3 
   .byte  <aNop_       ;    4 
   .byte  <aNop_       ;    5 
   .byte  <aNop_       ;    6 
   .byte  <aNop_       ;    7 
   .byte  <aNop_       ;    8 
   .byte  <aNop_       ;    9 
   .byte  <aNop_       ;    :  start defining a macro 
   .byte  <aNop_       ;    ; 
   .byte  <aNop_       ;    < 
   .byte  <aNop_       ;    = 
   .byte  <aNop_       ;    > 
   .byte  <aNop_       ;    ? 
   .byte  <cFetch_     ;    @ 
   .byte  <aNop_       ;    A 
   .byte  <break_      ;    B 
   .byte  <nop_        ;    C 
   .byte  <depth_      ;    D  ( -- val ) depth of data stack 
   .byte  <emit_       ;    E  ( val -- ) emits a char to output 
   .byte  <aNop_       ;    F 
   .byte  <go_         ;    G  ( -- ? ) execute mint definition 
   .byte  <keyq_       ;    H  ( verify if key hit )
   .byte  <inPort_     ;    I  ( port -- val ) 
   .byte  <aNop_       ;    J 
   .byte  <key_        ;    K  ( -- val )  read a char from input 
   .byte  <aNop_       ;    L 
   .byte  <aNop_       ;    M 
   .byte  <newln_      ;    N  ; prints a newline to output 
   .byte  <outPort_    ;    O  ( val port -- ) 
   .byte  <printStk_   ;    P  ( -- ) non-destructively prints stack 
   .byte  <aNop_       ;    Q  quits from Mint REPL 
   .byte  <rot_        ;    R  ( a b c -- b c a ) 
   .byte  <aNop_       ;    S 
   .byte  <aNop_       ;    T 
   .byte  <R2S_        ;    U  S( -- w ) R( w -- ) 
   .byte  <S2R_        ;    V  S( w -- ) R( -- w )
   .byte  <aNop_       ;    W   ; ( b -- ) if false, skip to end of loop 
   .byte  <exec_       ;    X 
   .byte  <aNop_       ;    Y 
   .byte  <editDef_    ;    Z 
   .byte  <cArrDef_    ;    [ 
   .byte  <comment_    ;    \  comment text, skips reading until end of line 
   .byte  <aNop_       ;    ] 
   .byte  <charCode_   ;    ^ 
   .byte  <aNop_       ;    _ 
   .byte  <aNop_       ;    ` 
   .byte  <sysVar_     ;    a  ; start of data stack *fixed 
   .byte  <sysVar_     ;    b  ; base16 flag 
   .byte  <sysVar_     ;    c  ; TIBPtr variable 
   .byte  <sysVar_     ;    d  ; vDefs variable
   .byte  <sysVar_     ;    e  ; 
   .byte  <sysVar_     ;    f  ; start of return stack *fixed
   .byte  <sysVar_     ;    g  ; next dispatcher 
   .byte  <sysVar_     ;    h  ; heap ptr variable 
   .byte  <i_          ;    i  ; returns index of current loop 
   .byte  <j_          ;    j  ; returns index of outer loop 
   .byte  <sysVar_     ;    k 
   .byte  <sysVar_     ;    l 
   .byte  <sysVar_     ;    m  ( a b -- c ) return the minimum value 
   .byte  <sysVar_     ;    n 
   .byte  <sysVar_     ;    o 
   .byte  <sysVar_     ;    p 
   .byte  <sysVar_     ;    q 
   .byte  <sysVar_     ;    r  ; return stack pointer
   .byte  <sysVar_     ;    s  ; data stack pointer
   .byte  <sysVar_     ;    t 
   .byte  <sysVar_     ;    u 
   .byte  <sysVar_     ;    v 
   .byte  <sysVar_     ;    w 
   .byte  <sysVar_     ;    x 
   .byte  <sysVar_     ;    y 
   .byte  <sysVar_     ;    z 
   .byte  <group_      ;    { 
   .byte  <aNop_       ;    | 
   .byte  <endGroup_   ;    } 
   .byte  <aNop_       ;    ~ 
   .byte  <aNop_       ;    BS 

ctlcodeshi: 
altcodeshi: 
   .byte  >empty_      ; NUL ^@ 
   .byte  >empty_      ; SOH ^A 
   .byte  >toggleBase_ ; STX ^B 
   .byte  >empty_      ; ETX ^C 
   .byte  >empty_      ; EOT ^D 
   .byte  >edit_       ; ENQ ^E 
   .byte  >empty_      ; ACK ^F 
   .byte  >empty_      ; BEL ^G 
   .byte  >backsp_     ; BS  ^H 
   .byte  >empty_      ; TAB ^I 
   .byte  >reedit_     ; LF  ^J 
   .byte  >empty_      ; VT  ^K 
   .byte  >list_       ; FF  ^L 
   .byte  >empty_      ; CR  ^M 
   .byte  >empty_      ; SO  ^N 
   .byte  >empty_      ; SI  ^O 
   .byte  >printStack_ ; DLE ^P 
   .byte  >empty_      ; DC1 ^Q 
   .byte  >empty_      ; DC2 ^R 
   .byte  >empty_      ; DC3 ^S 
   .byte  >empty_      ; DC4 ^T 
   .byte  >empty_      ; NAK ^U 
   .byte  >empty_      ; SYN ^V 
   .byte  >empty_      ; ETB ^W 
   .byte  >empty_      ; CAN ^X 
   .byte  >empty_      ; EM  ^Y 
   .byte  >empty_      ; SUB ^Z 
   .byte  >empty_      ; ESC ^[ 
   .byte  >empty_      ; FS  ^\ 
   .byte  >empty_      ; GS  ^] 
   .byte  >empty_      ; RS  ^^ 
   .byte  >empty_      ; US  ^_) 
   .byte  >aNop_       ; SP  ^` 
   .byte  >cStore_     ;    ! 
   .byte  >aNop_       ;    " 
   .byte  >aNop_       ;    # 
   .byte  >aNop_       ;    $  ( -- adr ) text input ptr 
   .byte  >aNop_       ;    % 
   .byte  >aNop_       ;    & 
   .byte  >aNop_       ;    ' 
   .byte  >ifte_       ;    (  ( b -- ) 
   .byte  >aNop_       ;    ) 
   .byte  >aNop_       ;    * 
   .byte  >incr_       ;    +  ( adr -- ) decrements variable at address 
   .byte  >aNop_       ;    , 
   .byte  >aNop_       ;    - 
   .byte  >aNop_       ;    . 
   .byte  >aNop_       ;    / 
   .byte  >aNop_       ;    0 
   .byte  >aNop_       ;    1 
   .byte  >aNop_       ;    2 
   .byte  >aNop_       ;    3 
   .byte  >aNop_       ;    4 
   .byte  >aNop_       ;    5 
   .byte  >aNop_       ;    6 
   .byte  >aNop_       ;    7 
   .byte  >aNop_       ;    8 
   .byte  >aNop_       ;    9 
   .byte  >aNop_       ;    :  start defining a macro 
   .byte  >aNop_       ;    ; 
   .byte  >aNop_       ;    < 
   .byte  >aNop_       ;    = 
   .byte  >aNop_       ;    > 
   .byte  >aNop_       ;    ? 
   .byte  >cFetch_     ;    @ 
   .byte  >aNop_       ;    A 
   .byte  >break_      ;    B 
   .byte  >nop_        ;    C 
   .byte  >depth_      ;    D  ( -- val ) depth of data stack 
   .byte  >emit_       ;    E  ( val -- ) emits a char to output 
   .byte  >aNop_       ;    F 
   .byte  >go_         ;    G  ( -- ? ) execute mint definition 
   .byte  >keyq_       ;    H  ( verify if key hit )
   .byte  >inPort_     ;    I  ( port -- val ) 
   .byte  >aNop_       ;    J 
   .byte  >key_        ;    K  ( -- val )  read a char from input 
   .byte  >aNop_       ;    L 
   .byte  >aNop_       ;    M 
   .byte  >newln_      ;    N  ; prints a newline to output 
   .byte  >outPort_    ;    O  ( val port -- ) 
   .byte  >printStk_   ;    P  ( -- ) non-destructively prints stack 
   .byte  >aNop_       ;    Q  quits from Mint REPL 
   .byte  >rot_        ;    R  ( a b c -- b c a ) 
   .byte  >aNop_       ;    S 
   .byte  >aNop_       ;    T 
   .byte  >R2S_        ;    U  S( -- w ) R( w -- ) 
   .byte  >S2R_        ;    V  S( w -- ) R( -- w )
   .byte  >aNop_       ;    W   ; ( b -- ) if false, skip to end of loop 
   .byte  >exec_       ;    X 
   .byte  >aNop_       ;    Y 
   .byte  >editDef_    ;    Z 
   .byte  >cArrDef_    ;    [ 
   .byte  >comment_    ;    \  comment text, skips reading until end of line 
   .byte  >aNop_       ;    ] 
   .byte  >charCode_   ;    ^ 
   .byte  >aNop_       ;    _ 
   .byte  >aNop_       ;    ` 
   .byte  >sysVar_     ;    a  ; start of data stack *fixed 
   .byte  >sysVar_     ;    b  ; base16 flag 
   .byte  >sysVar_     ;    c  ; TIBPtr variable 
   .byte  >sysVar_     ;    d  ; vDefs variable
   .byte  >sysVar_     ;    e  ; 
   .byte  >sysVar_     ;    f  ; start of return stack *fixed
   .byte  >sysVar_     ;    g  ; next dispatcher 
   .byte  >sysVar_     ;    h  ; heap ptr variable 
   .byte  >i_          ;    i  ; returns index of current loop 
   .byte  >j_          ;    j  ; returns index of outer loop 
   .byte  >sysVar_     ;    k 
   .byte  >sysVar_     ;    l 
   .byte  >sysVar_     ;    m  ( a b -- c ) return the minimum value 
   .byte  >sysVar_     ;    n 
   .byte  >sysVar_     ;    o 
   .byte  >sysVar_     ;    p 
   .byte  >sysVar_     ;    q 
   .byte  >sysVar_     ;    r  ; return stack pointer
   .byte  >sysVar_     ;    s  ; data stack pointer
   .byte  >sysVar_     ;    t 
   .byte  >sysVar_     ;    u 
   .byte  >sysVar_     ;    v 
   .byte  >sysVar_     ;    w 
   .byte  >sysVar_     ;    x 
   .byte  >sysVar_     ;    y 
   .byte  >sysVar_     ;    z 
   .byte  >group_      ;    { 
   .byte  >aNop_       ;    | 
   .byte  >endGroup_   ;    } 
   .byte  >aNop_       ;    ~ 
   .byte  >aNop_       ;    BS 

; ********************************************************************* 
; Macros must be written in Mint and end with ; 
; this code must not span pages 
; ********************************************************************* 
macros: 
 
.include "MINT.macros.asm" 
 

