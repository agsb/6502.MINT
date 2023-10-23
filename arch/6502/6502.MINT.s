; vim: filetype=asm sw=4 ts=4 autoindent expandtab shiftwidth=4 et:

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
;   specifc for ca65 assembler 
;
;--------------------------------------------------------
; enable listing

.list on

; identifiers

.case +

; debug

.debuginfo -

; enable features

.feature c_comments

.feature string_escapes

.feature org_per_seg

.feature dollar_is_pc

.feature pc_assignment

; enable 6502 mode

.p02

 
;--------------------------------------------------------
;
;   constants, must be.
;
;--------------------------------------------------------

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
    NUMGRPS = 5 

;---------------------------------------------------------------------- 
.segment "ZERO"
; offset
* = $00F0

; instruction pointer
ipt:    .addr $0
; index for data stack
isp:    .byte $0
; index for return stack
irp:    .byte $0
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
aps:

; return stack
    .res PAGE, $00
apr:

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
    .addr $0

;---------------------------------------------------------------------- 
; aliases

vS0      =  vsys + $00     ;    a  ; start of data stack 
vBase16  =  vsys + $02     ;    b  ; base16 flag
vTIBPtr  =  vsys + $04     ;    c  ; TIBPtr variable
vDefs    =  vsys + $08     ;    d  ; reference for group user functions
;        =  vsys + $0a     ;    e  ; 
vR0      =  vsys + $0c     ;    f  ; start of return stack 
vNext    =  vsys + $0e     ;    g  ; next routine dispatcher
vHeap =  vsys + $10     ;    h  ; heap ptr variable

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
    .asciiz "MINT@6502"

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

;------------------------------------------------------------------------------
;   data stack stuff

keep_: ; to push 
    ; ldx isp
    dex
    dex
    stx isp
    rts

lose_: ; to pull 
    ; ldx isp
    inx
    inx
    stx isp
    rts

spush:
push_:
    ldx isp
    lda tos + 0
    sta aps - 2, x
    lda tos + 1
    sta aps - 1, x
    jmp keep_
 
spull:
pull_:
    ldx isp
    lda aps + 0, x
    sta tos + 0
    lda aps + 1, x
    sta tos + 1
    jmp lose_

push2_:
    ldx isp
    lda nos + 0
    sta aps - 4, x
    lda nos + 1
    sta aps - 3, x
    lda tos + 0
    sta aps - 2, x
    lda tos + 1
    sta aps - 1, x
    jsr keep_
    jmp keep_
 
take2:
pull2_:
    ldx isp
    lda aps + 0, x
    sta tos + 0
    lda aps + 1, x
    sta tos + 1
    lda aps + 2, x
    sta nos + 0
    lda aps + 3, x
    sta nos + 1
    jsr lose_
    jmp lose_
   
drop_:  
    ldx isp 
    jsr lose_
    ; rts
    jmp (vNext)

dup_:
    ldx isp
    lda aps + 0, x
    sta aps - 2 
    lda aps + 1, x
    sta aps - 1
    jsr keep_
    ; rts
    jmp (vNext)

over_:
    ldx isp
    lda aps + 2, x
    sta aps - 2 
    lda aps + 3, x
    sta aps - 1
    jsr keep_
    ; rts
    jmp (vNext)

swap_:
    ldx isp
    lda aps + 0, x
    sta aps - 2 
    lda aps + 1, x
    sta aps - 1
    lda aps + 2, x
    sta aps + 0 
    lda aps + 3, x
    sta aps + 1
    lda aps - 2, x
    sta aps + 2 
    lda aps - 1, x
    sta aps + 3
    ; rts
    jmp (vNext)

rot_:
    ldx isp
    lda aps + 4, x
    sta aps - 2 
    lda aps + 5, x
    sta aps - 1
    lda aps + 2, x
    sta aps + 4 
    lda aps + 3, x
    sta aps + 5
    lda aps + 0, x
    sta aps + 2 
    lda aps + 1, x
    sta aps + 3
    lda aps - 2, x
    sta aps + 0 
    lda aps - 1, x
    sta aps + 1
    ; rts
    jmp (vNext)

and_:
    ldx isp
    lda aps + 0, x
    and aps + 2, x
    sta aps + 2, x 
    lda aps + 1, x 
    and aps + 3, x
    sta aps + 3, x
    jmp drop_

or_:
    ldx isp
    lda aps + 0, x
    ora aps + 2, x
    sta aps + 2, x
    lda aps + 1, x
    ora aps + 3, x
    sta aps + 3, x
    jmp drop_

xor_:
    ldx isp
    lda aps + 0, x
    eor aps + 2, x
    sta aps + 2, x
    lda aps + 1, x
    eor aps + 3, x
    sta aps + 3, x
    jmp drop_

cpt_:
    ldx isp
    sec
    tya
    sbc aps + 0, x
    sta aps + 0, x
    sec
    tya
    sbc aps + 1, x
    sta aps + 1, x
    ; rts
    jmp (vNext)

neg_:
    lda #$00
    tay
    jmp cpt_

inv_: 
    lda #$FF
    tay
    jmp cpt_

sub_:
    ldx isp
    sec
    lda aps + 2, x
    sbc aps + 0, x
    sta aps + 2, x
    lda aps + 3, x
    sbc aps + 1, x
    sta aps + 3, x
    jmp drop_

add_:
    ldx isp
    clc
    lda aps + 2, x
    adc aps + 0, x
    sta aps + 2, x
    lda aps + 3, x
    adc aps + 1, x
    sta aps + 3, x
    jmp drop_

cmp_:
    ldx isp
    sec
    lda aps + 2, x
    sbc aps + 0, x
    lda aps + 3, x
    sbc aps + 1, x
    rts

eq_:
    jsr cmp_
    beq true2_
    bne false2_

lt_:
    jsr cmp_
    bmi true2_
    bpl false2_

gt_:
    jsr cmp_
    bmi false2_
    beq false2_
    bpl true2_

same2_:
    ldx isp
    sta aps + 2, x
    sta aps + 3, x
    jmp drop_

false2_:
    lda #(FALSE)
    beq same2_

true2_:
    lda #(!FALSE)
    bne same2_

shl_:
    ldx isp
    asl aps + 0, x
    rol aps + 1, x
    ; rts
    jmp (vNext)

shr_:
    ldx isp
    lsr aps + 0, x
    ror aps + 1, x
    ; rts
    jmp (vNext)

cto_:
    jsr pull2_
    ldy #0
    lda nos + 0
    sta (tos), y
    rts

to_:
    jsr cto_
    iny
    lda nos + 1
    sta (tos), y
    rts

cStore_:
    jsr cto_
    ; rts
    jmp (vNext)

store_:
    jsr to_
    ; rts
    jmp (vNext)

cat_:
    ldx isp
    lda aps + 0, x
    sta tos + 0
    lda aps + 1, x
    sta tos + 1
    ldy #0
    lda (tos), y
    sta aps + 0, x
    rts

at_:
    jsr cat_
    iny 
    lda (tos), y
    sta aps + 1, x
    rts

cFetch_:
    jsr cat_
    ; rts
    jmp (vNext)

fetch_:
    jsr at_
    ; rts
    jmp (vNext)

incr_:
    ldx isp
    inc aps + 0, x
    bne @ends
    inc aps + 1, x 
@ends:
    ; rts
    jmp (vNext)

decr_:
    ldx isp
    lda aps + 0, x
    bne @ends
    dec aps + 1, x 
@ends:
    dec aps + 0, x 
    ; rts
    jmp (vNext)

.if 0
addto_:
    jsr pull2_
    ldy NUL
    clc
    lda (tos), y
    adc nos + 0
    sta (tos), y
    iny
    lda (tos), y
    adc nos + 1
    sta (tos), y
	jmp (vNext)

subto_:
    jsr pull2_
    ldy NUL
    sec
    lda (tos), y
    sbc nos + 0
    sta (tos), y
    iny
    lda (tos), y
    sbc nos + 1
    sta (tos), y
	jmp (vNext)
.endif

;------------------------------------------------------------------------------
;   return stack stuff

rpush:
rpush_:
    ldx isp
    lda tos + 0
    sta apr - 2, x
    lda tos + 1
    sta apr - 1, x
    dex
    dex
    stx isp
    rts

rpull:
rpull_:
    ldx isp
    lda apr + 0, x
    sta tos + 0
    lda apr + 1, x
    sta tos + 1
    inx
    inx
    stx isp
    rts

.if 0
rshow_:
    ldx isp
    lda apr + 0, x
    sta tos + 0
    lda apr + 1, x
    sta tos + 1
    jsr push_
    ; rts
    jmp (vNext)
.endif

r2s_:
    jsr rpull_
    jsr push_
    ; rts
    jmp (vNext)

s2r_:
    jsr pull_
    jsr rpush_
    ; rts
    jmp (vNext)

;---------------------------------------------------------------------- 
; prepare for mult or divd
opin:
    ldx isp 
    ; pseudo tos
    lda aps + 0, x
    sta wrk + 0
    lda aps + 1, x
    sta wrk + 1
    ; pseudo nos 
    lda aps + 2, x
    sta tmp + 0
    lda aps + 3, x
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
    ldx isp
    lda nos + 0
    sta aps + 0, x
    lda nos + 1
    sta aps + 1, x
    lda tos + 0
    sta aps + 2, x
    lda tos + 1
    sta aps + 3, x
    jmp (vNext)

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
    ; results
    lda tmp + 0
    sta nos + 0
    lda tmp + 1
    sta nos + 1
    ; ends
    jmp opout

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
    ; ends
    jmp opout
 
;---------------------------------------------------------------------- 
;   MINT
;---------------------------------------------------------------------- 
; NOOP
aNop_:
nop_:
    ; next 
    jmp next

;---------------------------------------------------------------------- 
; add a byte offset to instruction pointer
add2ps:    
; update ip
    clc
    adc ipt + 0
    sta ipt + 0
    bcc @ends
    inc ipt + 1
@ends:
    ; next 
    jmp (vNext)

;---------------------------------------------------------------------- 
pushps:
    ldx irp
    lda ipt + 0
    sta apr - 2, x
    lda ipt + 1
    sta apr - 1, x
    dex
    dex
    stx irp
    rts

;---------------------------------------------------------------------- 
pullps:
    ldx irp
    lda apr + 0, x
    sta ipt + 0
    lda apr + 1, x
    sta ipt + 1
    inx
    inx
    stx irp
    rts

;---------------------------------------------------------------------- 
seekps: 
    ldy NUL 
    lda (ipt), y 
    inc ipt + 0 
    bne @ends 
    inc ipt + 1 
@ends: 
    rts 
 
;---------------------------------------------------------------------- 
heap2nos:
    ; array start
    lda vHeap + 0
    sta nos + 0
    lda vHeap + 1
    sta nos + 1
    rts

;---------------------------------------------------------------------- 
add2heap:
    clc
    adc vHeap + 0
    sta vHeap + 0
    bcc @ends
    inc vHeap + 1
@ends:
    rts 

;---------------------------------------------------------------------- 
inctos:
    inc tos + 0
    bcc @ends
    inc tos + 1
@ends:
    rts
    
;---------------------------------------------------------------------- 
tib2tos:
    lda #<tib
    sta tos + 0
    lda #>tib
    sta tos + 1
    rts

;---------------------------------------------------------------------- 
; puts a string 
str_: 
    ldy NUL
@loop:
    lda (ipt), y
    beq @ends       ; NUL
    cmp #'`'        ; ` is the string terminator 
    beq @ends 
    jsr putchar 
    iny
    bne @loop       ; limit 255
@ends: 
    ; next 
    jmp  (vNext) 
 
;---------------------------------------------------------------------- 
; $00 to $1F, reserved for macros
; macros could no call macros.
macro:
    sty vTIBPtr + 0
    tay
    lda ctlcodeslo, y
    sta tos + 0
    lda ctlcodeshi, y
    sta tos + 1
    jsr spush
    jsr enter
    .asciiz "\\G"
    ldy vTIBPtr + 0
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

interpret2:                     
    lda NUL
    sta ns
    tay
    beq @isnest

; calc nesting (a macro might have changed it) 
@loop: 
    lda tib, y 
    iny 
    jsr nesting            ; update nesting value 

@isnest: 
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
    clc
    bcc @loop            

@iscrlf: 
    lda CR 
    jsr @echo 
    lda LF 
    jsr @echo 
    ; pending nest ? 
    lda ns 
    cmp NUL 
    beq @loop 

; mark end with etx, 
@endstr: 
    ; mark ETX 
    lda ETX 
    sta (tos), y
    iny

    ; mark NUL
    ;lda NUL 
    ;sta (tos), y 

    lda tos + 0
    sta ipt + 0
    lda tos + 1
    sta ipt + 1
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
    bne @loop   ; limit 256
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
    ora #$30 
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
    jsr seekps
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
    clc
    bcc @loop 
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
    jsr seekps
@isd: 
    cmp #'0' 
    bcc @ends 
    cmp #'9' + 1 
    bcs @ish 
@cv10: 
    sec 
    sbc #'0' 
    bcc @uval 
@ish: 
    ; to upper, clear bit-6
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
    beq @ends   ; limit 256 
    lda (ipt), y
    cmp CR 
    bne @loop 
    ; skip \r ?
    ; iny
    ; skip \n ?
    ; iny
    ; offset
@ends:
    tya
    jmp add2ps
 
;---------------------------------------------------------------------- 
depth_: 
    ; limit to 255 bytes
    sec
    lda #$FF
    sbc isp 
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
; how many ? 14
printStk_:  
    jsr enter
    ;.asciiz  "\\a@2-\\D1-(",$22,"@\\b@\\(,)(.)2-)'" 
    .asciiz  "\\a@2-\\D1-(14@\\b@\\(,)(.)2-)'" 
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
    jsr seekps
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
    jsr seekps
    tay 
    lda optcodeslo, y 
    sta wrk + 0 
    lda optcodeshi, y 
    sta wrk + 1 
    jmp (wrk) 
 
;---------------------------------------------------------------------- 
; Execute next alt opcode
alt_: 
    jsr seekps
    tay 
    lda altcodeslo, y 
    sta wrk + 0 
    lda altcodeshi, y 
    sta wrk + 1 
    jmp (wrk) 
 
;---------------------------------------------------------------------- 
; Parse inline code, must be asciiz 
enter:                           
; pull from system stack
    pla 
    sta ipt + 0 
    pla
    sta ipt + 1 
    ; next 
    jmp (vNext) 

;---------------------------------------------------------------------- 
; char 0, Continue from enter 
exit_:
    jmp (ipt)

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
    ldx isp
    lda aps + 0, x 
    sta ipt + 0 
    lda aps + 1, x 
    sta ipt + 1 
    inx 
    inx 
    stx isp
    ; next 
    jmp (vNext) 
 
;---------------------------------------------------------------------- 
; Execute code from a user function
call_:
    sta ap
    
    jsr pushps
    
    jsr lookupDefs

    ; update instruction pointer
    ldy NUL
    lda (tos), y
    sta ipt + 0
    iny
    lda (tos), y
    sta ipt + 1

    ; next 
    jmp (vNext)

;---------------------------------------------------------------------- 
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
    ; offset
    clc
    adc vDefs + 0
    sta tos + 0
    lda NUL
    adc vDefs + 1
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
    jsr seekps
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
    beq arrDefs

;---------------------------------------------------------------------- 
cArrDef_:
    lda TRUE
    ; fall throught

;---------------------------------------------------------------------- 
arrDefs:
    ; save array mode
    sta vByteMode

    ; save array start
    ldx isp
    dex
    dex
    lda vHeap + 0
    sta apr + 0, x
    lda vHeap + 1
    sta apr + 1, x
    stx isp

    ; array next
    lda #<compNext
    sta vNext + 0
    lda #>compNext
    sta vNext + 1

    ; next 
    jmp next
    
;---------------------------------------------------------------------- 
arrEnd_:

    ; start of array
    jsr rpull
    jsr spush 

    ; bytes
    sec
    lda vHeap + 0
    sbc tos + 0
    sta tos + 0
    lda vHeap + 1
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

    ; common next
    lda #<next
    sta vNext + 0
    lda #>next
    sta vNext + 1

    ; next 
    jmp next

;---------------------------------------------------------------------- 
def_:
    ; must be a A-Z 
    jsr nosp

    ; get slot at list
    sta ap
    jsr lookupDefs

    ; get heap
    jsr heap2nos
    
    ; put heap at list
    lda nos + 0
    sta (tos), y
    iny
    lda nos + 1
    sta (tos), y

    ; copy to heap
    ldy NUL
@loop:
    lda (ipt), y
    sta (nos), y
    beq @ends
    iny
    beq @ends
    cmp #';'
    bne @loop
@ends:
    ; update heap
    tya
    sta ap
    jsr add2heap
    ; update instruction pointer
    lda ap
    jmp add2ps

;----------------------------------------------------------------------
; skip while nest
skipnest: 
    lda #$01
    sta ns
@loop: 
    jsr seekps
    jsr nesting 
    lda ns
    bne @loop
    ; next 
    jmp (vNext) 

;---------------------------------------------------------------------- 
break_:
    jsr spull
    lda tos + 0
    bne @isne
    ; parse frame
    jmp (vNext)
@isne:
    ; skip frame
    clc
    lda irp
    adc #6
    sta irp
@iscc:
    jmp skipnest

;---------------------------------------------------------------------- 
; Left parentesis ( begins a loop
begin_:

    ; tos is zero ?
    jsr spull
    lda tos + 0
    beq skipnest

    ; alloc a frame 
    sec
    lda irp
    sbc #6
    sta irp

    ; make a frame
    ldx irp
    ; counter
    lda NUL
    sta apr + 0, x
    sta apr + 1, x 
    ; limit
    lda tos + 0
    sta apr + 2, x
    lda tos + 1
    sta apr + 3, x
    ; pointer
    lda ipt + 0
    sta apr + 4, x
    lda ipt + 1
    sta apr + 5, x
    ; next 
    jmp (vNext) 

;----------------------------------------------------------------------
; Right parentesis ) again a loop 
again_: 
    ; check if IFTEMode $FFFF
    lda apr + 0, x
    and apr + 1, x
    cmp #$FF
    bne again1
    
    ; push FALSE
    lda FALSE
    sta tos + 0
    sta tos + 1
    jsr spush

    ; drop IFTEMmode
    clc
    lda irp
    adc #2
    sta irp
    ; next 
    jmp (vNext)
 
again1: 
    ; test end
    ldx irp
    lda apr + 2, x
    cmp apr + 0, x
    bne @noend
    lda apr + 3, x
    cmp apr + 1, x
    bne @noend

    ; end of loop
    ; drop frame
    clc
    lda irp
    adc #6
    sta irp
    ; next 
    jmp (vNext)

@noend:
    ; increase counter
    inc apr + 0, x
    bne @novr
    inc apr + 1, x
@novr:    

    ; return at begin    
    lda apr + 4, x
    sta ipt + 0
    lda apr + 5, x
    sta ipt + 1

    ; next 
    jmp (vNext) 
 
;----------------------------------------------------------------------
j_:
    sec
    lda irp
    sbc #6
    tax
    jmp indx

;----------------------------------------------------------------------
i_:
    ldx irp
    ; fall through

;----------------------------------------------------------------------
indx:
    lda aps + 0, x
    sta tos + 0
    lda aps + 1, x
    sta tos + 1
    jsr spush
    ; next 
    jmp (vNext)

;----------------------------------------------------------------------
ifte_:
    jsr spull
    lda tos + 0
    ora tos + 1
    bne @istrue
    inc tos + 0
    jsr spush
    jmp skipnest
@istrue:
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
    .word  heap                 ; h vHeap
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

    ; auto reset if stack overflows
    jsr interpret
    jmp mint_

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
   .byte  <nop_     ;   ACK 
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
   .byte  >nop_     ;   ACK 
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
   .byte  <incr_       ;    +  ( adr -- ) increments variable at address 
   .byte  <aNop_       ;    , 
   .byte  <decr_       ;    -  ( adr -- ) decrements variable at address
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
   .byte  <r2s_        ;    U  S( -- w ) R( w -- ) 
   .byte  <s2r_        ;    V  S( w -- ) R( -- w )
   .byte  <aNop_       ;    W   ; ( b -- ) if false, skip to end of loop 
   .byte  <exec_       ;    X 
   .byte  <aNop_       ;    Y 
   .byte  <editDef_    ;    Z 
   .byte  <cArrDef_    ;    [ 
   .byte  <comment_    ;    \  comment text, skip reading until end of line 
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
   .byte  >incr_       ;    +  ( adr -- ) increments variable at address 
   .byte  >aNop_       ;    , 
   .byte  >decr_       ;    -  ( adr -- ) decrements veriable at address
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
   .byte  >r2s_        ;    U  S( -- w ) R( w -- ) 
   .byte  >s2r_        ;    V  S( w -- ) R( -- w )
   .byte  >aNop_       ;    W   ; ( b -- ) if false, skip to end of loop 
   .byte  >exec_       ;    X 
   .byte  >aNop_       ;    Y 
   .byte  >editDef_    ;    Z 
   .byte  >cArrDef_    ;    [ 
   .byte  >comment_    ;    \  comment text, skip reading until end of line 
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
 
.word $ADDE
