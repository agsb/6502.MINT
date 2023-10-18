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
 
    DSIZE       = $80 
    RSIZE       = $80 
 
    TIBSIZE     = $100 
    TRUE        = 1 
    FALSE       = 0 
 
    NUMGRPS     = 5 
    GRPSIZE     = $40 
 
;---------------------------------------------------------------------- 
; notes 6502 version: 
; 
;   code is for RAM use. 
; 
;   caller must save a, x, y and 
;   reserve 32 (?) words at hardware stack 
; 
;   stacks are from absolute address. 
;   data stack indexed by x 
;   return stack indexed by y 
;   terminal input buffer 
;   all just 128 cells deep and round-robin 
;   a cell is 16-bit 
;   16-bit jump table 
;   extense use of post-indexed indirect addressing 
; 
;   alt-a used for vS0, start of data stack 
;   alt-f used for vR0, start of return stack **** 
;   alt-g reserved, copycat of references **** 
; 
;   all rotines must end with: 
;   jmp next_ or jmp drop_ or a jmp / branch 
;---------------------------------------------------------------------- 
 
;---------------------------------------------------------------------- 
; page 0, reserved cells 
    zpage = $f0 
 
; copycat 
    yp = zpage + $0  ; y index, return stack pointer, 
    xp = zpage + $1  ; x index, parameter stack pointer, 
    ap = zpage + $2  ; accumulator 
    ns = zpage + $3  ; nests 
 
; pseudos 
    tos = zpage + $4  ; tos  register 
    nos = zpage + $6  ; nos  register 
    wrk = zpage + $8  ; work register 
    tmp = zpage + $a  ; work register 

; holds
    nxt = zpage + $c  ; next pointer 
    ips = zpage + $e  ; instruction pointer 
 
; all in RAM, better put tables at end of code ? 
 
;    start = $200 
;    tib = start  ; terminal input buffer, upwards 
;    spz = start + $1FF  ; absolute data stack, backwards 
;    rpz = start + $2FF  ; absolute parameter stack, backwards 
; 
;    vars = start + $300  ;   26 words 
;    vsys = start + $336  ;   26 words 
;    defs = start + $36C  ;   26 words 
;    tmps = start + $3D8  ;   14 words 
; 
;    free = start + $400  ; free ram start 

;---------------------------------------------------------------------- 
.segment "VECTORS"

.word init
.word init
.word init

;---------------------------------------------------------------------- 
.segment "CODE"

VOID:

    .res $100, $00
spz:

    .res $100, $00
rpz: 

tib:    
    .res $100, $00

vsys:
    .res $36, $00

vars:
    .res $36, $00

defs:
    .res $36, $00

vtmp:
    .res $5E, $00

;---------------------------------------------------------------------- 
.segment "ONCE"
 
; ********************************************************************** 
; 
; (not yet) routines are ordered to occupy pages of 256 bytes 
; 
; ********************************************************************** 
 
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
    sta tos + 0
    lda #0
    sta tos + 1
    jsr spush_
    jmp next_
    
;---------------------------------------------------------------------- 
emit_:
    jsr spull_
    lda tos + 0
    jsr putchar
    jmp next_

;---------------------------------------------------------------------- 
aNop_:
nop_:
    clc
    jmp next_

;---------------------------------------------------------------------- 
; increase instruction pointer 
incps_: 
    inc ips + 0 
    bne @noeq 
    inc ips + 1 
@noeq: 
    rts 
 
;---------------------------------------------------------------------- 
; decrease instruction pointer 
decps_: 
    lda ips + 0
    bne @noeq 
    dec ips + 1 
@noeq: 
    dec ips + 0 
    rts 
 
;---------------------------------------------------------------------- 
; load char at instruction pointer 
ldaps_: 
    ldy #$00 
    lda (ips), y 
    rts 
 
;---------------------------------------------------------------------- 
pushps_:
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
pullps_:
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
rpush_: 
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
rpull_: 
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
spush_: 
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
spull_: 
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
take2_: 
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
; NEGate the value on top of stack (2's complement) 
neg_: 
    ; ldx xp
    sec 
    lda #0 
    sbc spz + 0, x 
    sta spz + 0, x 
    sec 
    lda #0 
    sbc spz + 1, x 
    sta spz + 1, x 
    ; stx xp
    jmp next_ 
 
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
    jmp next_ 
 
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
    jmp next_ 
 
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
    jmp next_ 
 
;---------------------------------------------------------------------- 
; Rotate 3 elements at stack 
; a b c -- b c a 
rot_: 
    ; ldx xp
    ; c -> w 
    lda spz + 0, x 
    sta tos + 0 
    lda spz + 0, x 
    sta tos + 1 
    ; b -> u 
    lda spz + 2, x 
    sta nos + 0 
    lda spz + 3, x 
    sta nos + 1 
    ; a -> c 
    lda spz + 4, x 
    sta spz + 0, x 
    lda spz + 5, x 
    sta spz + 1, x 
    ; u -> a 
    lda nos + 0 
    sta spz + 4, x 
    lda nos + 1 
    sta spz + 5, x 
    ; w -> b 
    lda tos + 0 
    sta spz + 2, x 
    lda tos + 1 
    sta spz + 3, x 
    ; stx xp
    jmp next_ 
 
;---------------------------------------------------------------------- 
; Swap 2nd and 1st elements of the stack 
; a b c -- a c b 
swap_: 
    ; ldx xp
    ; b -> w 
    lda spz + 2, x 
    sta wrk + 0 
    lda spz + 3, x 
    sta wrk + 1 
    ; a -> b 
    lda spz + 0, x 
    sta spz + 2, x 
    lda spz + 1, x 
    sta spz + 3, x 
    ; w -> a 
    lda wrk + 0 
    sta spz + 0, x 
    lda wrk + 1 
    sta spz + 1, x 
    ; stx xp
    jmp next_ 
 
;---------------------------------------------------------------------- 
;  Left shift { is multply by 2 
shl_: 
    ; ldx xp
    asl spz + 0, x 
    rol spz + 1, x 
    ; stx xp
    jmp next_ 
 
;---------------------------------------------------------------------- 
;  Right shift } is a divide by 2 
shr_: 
    ; ldx xp
    lsr spz + 0, x 
    ror spz + 1, x 
    ; stx xp
    jmp next_ 
 
;---------------------------------------------------------------------- 
; Drop the top member of the stack 
; a b c -- a b 
drop_: 
    ; ldx xp
    inx 
    inx 
    ; stx xp
    jmp next_ 
 
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
    lda #0
    sta tos + 0
    sta tos + 1
    sta nos + 0 
    sta nos + 0
    rts

;---------------------------------------------------------------------- 
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
; divisor dividend -- quontient remainder
div_: 
    jsr divd 
    jmp next_
 
divd:
    jsr opin
    ; countdown
    ldy #16
@loop:
    asl tmp + 0
    rol tmp + 1
    rol tos + 0
    rol tos + 1
    sec
    lda nos + 0
    sbc tos + 0
    sta tos + 0
    lda nos + 1
    sbc tos + 1
    sta tos + 1
    bcc @iscc
    clc 
    lda nos + 0
    adc tos + 0
    sta tos + 0
    lda nos + 1
    adc tos + 1
    sta tos + 1
@iscc:
    ; countdown
    dey
    bne @loop
    jsr opout
    jmp next_

;---------------------------------------------------------------------- 
; 16-bit multiply 16x16, 32 result
; multiplier multiplicand -- resultLSW resultMSW
; 
mul_:
    jsr mult
    jmp next_

mult:                         
    jsr opin
    ; countdown
    ldy #16
@loop:
    asl tos + 0
    rol tos + 1
    rol nos + 0
    rol nos + 0
    bcc @iscc
    clc
    lda tmp + 0
    adc tos + 0
    sta tos + 0
    lda tmp + 1
    adc tos + 1
    sta tos + 1
    lda #0
    adc nos + 0
    sta nos + 0
@iscc:
    ; countdown
    dey
    bne @loop
    jsr opout
    jmp next_
 
;---------------------------------------------------------------------- 
; \+    a b c -- a ; [c]+b  ; increment variable at c by b 
incr_: 
    jsr take2_ 
    clc 
    ldy #$00 
    lda (tos), y 
    adc nos + 0 
    sta (tos), y 
    iny 
    lda (tos), y 
    adc nos + 1 
    sta (tos), y 
    jmp next_ 
 
;---------------------------------------------------------------------- 
; \-    a b c -- a ; [c]-b  ; decrement variable at c by b 
decr_: 
    jsr take2_ 
    sec 
    ldy #$00 
    lda (tos), y 
    sbc nos + 0 
    sta (tos), y 
    iny 
    lda (tos), y 
    sbc nos + 1 
    sta (tos), y 
    jmp next_ 
 
;---------------------------------------------------------------------- 
; false 
false2: 
    ; ldx xp
    lda #$00 
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
    lda #$00 
    sta spz + 3, x 
    ; stx xp
    jmp drop_ 
 
;---------------------------------------------------------------------- 
; subtract for compare 
cmp_: 
    ; ldx xp
    sec 
    lda spz + 2, x 
    sbc spz + 0, x 
    lda spz + 3, x 
    sbc spz + 1, x 
    ; stx xp
    rts 
 
;---------------------------------------------------------------------- 
; signed equal than 
eq_: 
    jsr cmp_ 
    bne false2 
    beq true2 
 
;---------------------------------------------------------------------- 
; signed less than 
lt_: 
    jsr cmp_ 
    bmi true2 
    bpl false2 
 
;---------------------------------------------------------------------- 
; signed greather than 
; must be in that order, bpl is non negative flag 
gt_: 
    jsr cmp_ 
    bmi false2 
    beq false2 
    bpl true2 
 
;---------------------------------------------------------------------- 
; fetch the value from the address placed on the top of the stack 
; a b c - a b (c) 
; fetch a byte 
cFetch_: 
    lda #$00 
    sta tos + 1 
    sec
    jmp isfetch_ 
 
;---------------------------------------------------------------------- 
; fetch a word 
fetch_: 
    clc 
    jmp isfetch_ 
 
;---------------------------------------------------------------------- 
isfetch_: 
    ; ldx xp
    ; load the reference 
    lda spz + 0, x 
    sta nos + 0 
    lda spz + 1, x 
    sta nos + 1 
    ; then the value 
    ldy #$00
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
    jmp next_ 
 
;---------------------------------------------------------------------- 
; store the value into the address placed on the top of the stack 
; a b c -- a 
; store a byte 
cStore_: 
    sec 
    jmp isstore_ 
 
;---------------------------------------------------------------------- 
; store a word 
store_: 
    clc 
    jmp isstore_ 
 
;---------------------------------------------------------------------- 
isstore_: 
    jsr take2_ 
    ; copy the value 
    ldy #$00
    lda nos + 0
    sta (tos), y
    bcs @cset
    iny
    lda nos + 1
    sta (tos), y
    ; next 
@cset:    
    ; stx xp
    jmp next_ 
 
;---------------------------------------------------------------------- 
; hook for debug 
exec_: 
    jsr spull_ 
    jmp (tos) 
 
;---------------------------------------------------------------------- 
_empty_:
    jsr printStr 
    .asciiz  "void define\r\n" 
    jmp next_
 
;---------------------------------------------------------------------- 
; puts a string, limit 255 chars 
str_: 
    ldy #$00
@loop:
    iny
    lda (ips), y
    cmp #'`'              ; ` is the string terminator 
    beq @ends 
    jsr putchar 
    clc
    bcc @loop 
    ; error in putchar
@ends: 
    jmp next_ 
 
;---------------------------------------------------------------------- 
macro:
    asl
    sta ap
    lda #<ctlcodes
    sta tos + 0
    lda #>ctlcodes
    sta tos + 1
    lda ap
    clc
    adc tos + 0
    sta tos + 0
    adc tos + 1
    sta tos + 1
    jsr spush_
    jsr enter_
    .asciiz "\\G"
    jmp interpret2

;---------------------------------------------------------------------- 
interpret: 
    jsr enter_ 
    .asciiz "\\N`> `" 
    ; fall throught

; used by tests 
interpret1:
    lda #$00 
    sta vTIBPtr + 0 
    sta vTIBPtr + 1 

interpret2:                     
    lda #$00
    sta ns
    tay
    beq @cast

; calc nesting (a macro might have changed it) 
@loop: 
    lda tib, y 
    iny 
    jsr  nesting            ; update nesting value 

@cast: 
    cpy #0 
    bne @loop 
    ; fall throught

;---------------------------------------------------------------------- 
; loop around waiting for character 
; get a line into tib
waitchar: 
getstr:
    ; already ldy #$00 
@loop:
    jsr getchar 
    cmp #32                 ; ge space ? 
    bcs @ischar 
    cmp #$0                 ; is it end of string ? 
    beq @endstr 
    cmp #13                 ; carriage return ? 
    beq @iscrlf 
    cmp #10                 ; line feed ? 
    beq @iscrlf 
@ismacro: 
    jmp macro 
@ischar: 
    jsr @echo 
    ; nest ? 
    jsr nesting 
    jmp @loop            ; wait for next character 
@iscrlf: 
    ; CR 
    lda #13 
    jsr @echo 
    ; LF 
    lda #10 
    jsr @echo 
    ; pending nest ? 
    lda ns 
    cmp #$00 
    beq @loop 
; mark etx, used later to check Z80 stack deep, 
; not need in 6502 round-robin stack,
; preserved for compability
@endstr: 
    ; mark ETX 
    lda #$03 
    sta tib, y
    iny
    ; mark NUL
    lda #$00 
    sta tib, y 

    lda #<tib
    sta ips + 0
    lda #>tib
    sta ips + 1
    jsr decps_
    jmp next_ 

; maximum 255 chars 
@echo:
    ; echo 
    jsr putchar 
    ; store
    sta tib, y 
    iny 
    ; limit 253
    cpy #$FD
    beq @endstr
    rts 
 
;---------------------------------------------------------------------- 
; calculate nesting value 
nesting: 
    cmp #'`' 
    bne @nests 
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
    sta wrk + 1 
    pla 
    sta wrk + 0 
    jsr puts_ 
    lda wrk + 0 
    pha 
    lda wrk + 1 
    pha 
    rts 
 
;---------------------------------------------------------------------- 
; prints a asciiz, refered by wrk 
puts_: 
    ldy #$00 
    jsr @noeq 
@loop: 
    jsr putchar 
    inc wrk + 0 
    bne @noeq 
    inc wrk + 1 
@noeq: 
    lda (wrk), y 
    bne @loop 
@ends: 
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
    clc 
    ror 
    ror 
    ror 
    ror 
    jsr @conv 
    lda ap 
@conv: 
    clc 
    and #$0F 
    adc #$30 
    cmp #$3A 
    bcc @ends 
    adc #$06 
@ends: 
    jmp putchar 
 
;---------------------------------------------------------------------- 
; convert a decimal value to binary 
num_: 
    jsr decps_
    lda #$00 
    sta tos + 0 
    sta tos + 1 
@loop: 
    jsr incps_
    jsr ldaps_
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
    lda #$00
    adc tos + 1
    sta tos + 1
    jsr mul10_ 
    clc
    bcc @loop 
@ends: 
    jsr spush_ 
    jmp next_
 
;---------------------------------------------------------------------- 
; multiply by ten 
; 2x + 8x 
mul10_: 
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
    jsr decps_
    lda #$00 
    sta tos + 0 
    sta tos + 1 
@loop: 
    jsr incps_
    jsr ldaps_
@isd: 
    cmp #'0' 
    bcc @ends 
    cmp #'9' + 1 
    bcs @ish 
@cv10: 
    sec 
    sbc #'0' 
    clc
    bcc @uval 
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
    lda #$00
    adc tos + 1
    sta tos + 1
    jsr mul16_ 
    clc
    bcc @loop 
@ends: 
    jsr spush_ 
    jmp next_
 
;---------------------------------------------------------------------- 
; multiply by sixteen 
mul16_: 
    ldy #04 
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
    ldy #$00
@loop:    
    iny
    lda (ips), y
    cmp #13 
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
    jmp next_ 
 
;---------------------------------------------------------------------- 
depth_: 
    ; limit to 255 bytes
    lda #$FF 
    sec 
    sbc xp 
    ; words 
    lsr
    sta tos + 0 
    lda #00 
    sta tos + 1 
    jsr spush_ 
    jmp next_ 
 
;---------------------------------------------------------------------- 
; print hexadecimal 
hdot_: 
    jsr spull_ 
    jsr printhex 
    jmp dotsp 
 
;---------------------------------------------------------------------- 
; print decimal 
dot_: 
    jsr spull_ 
    jsr printdec 
    jmp dotsp 
 
;---------------------------------------------------------------------- 
; print space 
dotsp: 
    lda #' ' 
    jsr writeChar1 
    jmp next_ 
 
;---------------------------------------------------------------------- 
writeChar: 
    jsr ldaps_
    jsr writeChar1 
    jmp next_ 

;---------------------------------------------------------------------- 
writeChar1: 
    jmp putchar 
 
;---------------------------------------------------------------------- 
newln_: 
    jsr crlf 
    jmp next_ 
 
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
    jsr enter_
    ;.asciiz  "\\a@2-\\D1-(",$22,"@\\b@\\(,)(.)2-)'" 
    .asciiz  "\\a@2-\\D1-(34@\\b@\\(,)(.)2-)'" 
    jmp next_ 
 
;---------------------------------------------------------------------- 
; 6502 is memory mapped IO 
inPort_: 
    jmp cFetch_ 
 
;---------------------------------------------------------------------- 
; 6502 is memory mapped IO 
outPort_: 
    jmp cStore_ 
 
;---------------------------------------------------------------------- 
charCode_:
    jsr incps_
    jsr ldaps_
    sta tos + 0
    lda #$00
    sta tos + 1
    jsr spush_
    jmp next_

;---------------------------------------------------------------------- 
; Execute next opcode
next_: 
    ; using full jump table 
    jsr incps_
    jsr ldaps_
    asl 
    tay 
    lda optcodes, y 
    sta wrk + 0 
    iny 
    lda optcodes, y 
    sta wrk + 1 
    jmp (wrk) 
 
;---------------------------------------------------------------------- 
; Execute next alt opcode
alt_: 
    ; using full jump table 
    jsr incps_
    jsr ldaps_
    asl 
    tay 
    lda altcodes, y 
    sta wrk + 0 
    iny 
    lda altcodes, y 
    sta wrk + 1 
    jmp (wrk) 
 
;---------------------------------------------------------------------- 
; Execute code inline 
enter_:                           
    jsr pushps_
; pull from system stack
    pla 
    sta ips + 0 
    pla
    sta ips + 1 
    jsr decps_
    jmp next_ 

;---------------------------------------------------------------------- 
; Execute code from data stack
go_: 
    jsr pushps_
; pull ps from data stack 
    ; ldx xp
    lda spz + 0, x 
    sta ips + 0 
    lda spz + 1, x 
    sta ips + 1 
    inx 
    inx 
    ; stx xp
    jsr decps_ 
    jmp next_ 
 
;---------------------------------------------------------------------- 
; Execute code from a user function
call_:
    sta ap
    jsr pushps_
    jsr lookupDefs
    jsr decps_
    jmp next_

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
; push an user variable 
var_: 
    sta ap 
    lda #<vars 
    sta tos + 0 
    lda #>vars 
    sta tos + 1 
    jmp a2z_ 
 
;---------------------------------------------------------------------- 
; push a mint variable
sysVar_: 
    sta ap 
    lda #<vsys 
    sta tos + 0 
    lda #>vsys 
    sta tos + 1 
    jmp a2z_ 
 
;---------------------------------------------------------------------- 
; push a reference into stack
a2z_: 
    lda ap 
    sec 
    sbc #'a' 
    asl 
    clc 
    adc tos + 0 
    bcc @iscc 
    inc tos + 1 
@iscc:
    jsr spush_ 
    jmp next_ 
 
;---------------------------------------------------------------------- 
; skip spaces
nosp_:
    jsr incps_
    jsr ldaps_
    cmp #' '
    beq nosp_
    rts

;---------------------------------------------------------------------- 
getRef_:
    jsr nosp_
    sta ap
    jsr lookupDefs
    jmp fetch_

;---------------------------------------------------------------------- 
def_:
    ; skip spaces
    jsr nosp_
    sta ap
    jsr lookupDefs
    lda vHeapPtr + 0
    sta (tos), y
    sta nos + 0
    iny
    lda vHeapPtr + 1
    sta (tos), y
    sta nos + 1
    ldy #0
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
    jmp next_
    


;---------------------------------------------------------------------- 
break_:
    jsr spull_
    lda tos + 0
    ora tos + 1
    bne @isne
    jmp next_
@isne:
    lda yp
    clc
    adc #$06
    jmp begin1

;---------------------------------------------------------------------- 
; Left parentesis ( begins a loop
begin_:

	; tos is zero ?
	jsr spull_
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
    lda #$00
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
    jmp next_ 

begin1: 
    lda #$01
    sta ns 

@loop: 
    jsr incps_
    jsr ldaps_
    jsr nesting 
    lda ns
    eor ns
    bne @loop
    jmp next_ 

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
    jsr spush_

    ; drop IFTEMmode
    lda yp
    clc
    adc #2
    jmp next_
 
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
	jmp next_

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

    jmp next_ 
 
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
    jsr spush_
    jmp next_

;----------------------------------------------------------------------
ifte_:
    jsr spull_
    lda tos + 0
    ora tos + 1
    bne @isne
    inc tos + 0
    jsr spush_
    jmp begin1
@isne:
    lda #$FF
    sta tos + 0
    sta tos + 1
    jsr spush_
    jmp next_

;---------------------------------------------------------------------- 
ret_:
    jsr pullps_
    jmp next_

;---------------------------------------------------------------------- 
; 
exit_:
    jsr incps_
    lda ips + 0
    sta tos + 0
    lda ips + 1
    sta tos + 1
    jsr pullps_
    jmp (tos)

;---------------------------------------------------------------------- 
; 6502 stack is fixed and round robin
; no need control deep
etx_:
    jmp interpret
    
;---------------------------------------------------------------------- 
init:

endGroup_:
group_:

cArrDef_:
editDef_:

arrEnd_:
arrDef_:


;---------------------------------------------------------------------- 
;optcodes:
;altcodes:
;ctlcodes:

.include "jumptables.s"

