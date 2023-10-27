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

;---------------------------------------------------------------------
; /*
;  *  DISCLAIMER"
;  *
;  *  Copyright © 2023, Alvaro Gomes Sobral Barcellos,
;  *
;  *  Permission is hereby granted, free of charge, to any person obtaining
;  *  a copy of this software and associated documentation files (the
;  *  "Software"), to deal in the Software without restriction, including
;  *  without limitation the rights to use, copy, modify, merge, publish,
;  *  distribute, sublicense, and/or sell copies of the Software, and to
;  *  permit per0ons to whom the Software is furnished to do so, subject to
;  *  the following conditions"
;  *
;  *  The above copyright notice and this permission notice shall be
;  *  included in all copies or substantial portions of the Software.
;  *
;  *  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;  *  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;  *  MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE and
;  *  NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;  *  LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;  *  OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;  *
;  */
;---------------------------------------------------------------------

;--------------------------------------------------------
;
;  ca65 assembler specifics
;
;--------------------------------------------------------

; identifiers

.case +

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
;   constants, as must be.
;
;--------------------------------------------------------

    TRUE        = 1
    FALSE       = 0

    CR  = 13
    LF  = 10
    BS  = 9
    ETX = 3
    NUL = 0

    ; stack size in words
    STKSIZE = $18

    ; size page
    PAGE = $100

    ; group size
    GRPSIZE = $40

    ; groups for defs, could be more
    NUMGRPS = 5

;----------------------------------------------------------------------

    ; stacks at zero page, 24 word deep
    ZERO_PAGE_STACK = 1

    ; include extra functions
    FULL_STACK_CODES = 1

    ; define emulator mode
    EMULATOR = 1

    ; define test for emulator debug
    test = 1

;----------------------------------------------------------------------
.segment "ZERO"

; offset

.ifdef ZERO_PAGE_STACK

* = $00F0 - $60

; return stack with 24 words
.res  STKSIZE*2, $0
dat_zero:

; data stack with 24 words
.res  STKSIZE*2, $0
ret_zero:

.endif

* = $00F0
; instruction pointer
ins_ptr:    .addr $0
; index for data stack
dat_indx:    .byte $0
; index for return stack
ret_indx:    .byte $0
; copycat
nest: 	.byte $0
echo:   .byte $0
; pseudo registers
tos:    .word $0
nos:    .word $0
wrk:    .word $0
tmp:    .word $0

;
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

.ifndef ZERO_PAGE_STACK

 data stack
    .res PAGE, $00
 dat_zero:

 return stack
    .res PAGE, $00
 ret_zero:

.endif

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
; 25/10/2023, using lib6502, -M E000
.ifdef EMULATOR

hitchar:

getchar:
    ;jsr $E010
    lda $E000
    rts

putchar:
    ;jsr $E020
    sta $E000
    rts

.endif

.ifndef EMULATOR

;----------------------------------------------------------------------
;    depends on hardware, ACIA 6551 common
;----------------------------------------------------------------------
    CIA       =  $A000   ; The base address of the 6551 ACIA.
    CIA_DATA  =  CIA+0   ; Its data I/O register
    CIA_RX    =  CIA+0   ; Its data I/O register
    CIA_TX    =  CIA+0   ; Its data I/O register
    CIA_STAT  =  CIA+1   ; Its  status  register
    CIA_COMM  =  CIA+2   ; Its command  register
    CIA_CTRL  =  CIA+3   ; Its control  register

;----------------------------------------------------------------
; setup thru 6551
setchar:
pcia_init:
    ; reset CIA
    lda #0
    sta #CIA_STAT
    ; %0001 1110 =  9600 baud, external receiver, 8 bit , 1 stop bit
    ; %0001 1111 = 19200 baud, external receiver, 8 bit , 1 stop bit
    lda #$1F
    sta #CIA_CTRL
    ; %0000 1011 = no parity, normal mode, RTS low, INT disable, DTR low
    lda #$0B
    sta #CIA_COMM
    rts

;----------------------------------------------------------------
;   verify thru 6551, no waits
hitchar:
@acia_ht:
; verify
    lda #CIA_STAT
    and #8
    beq _nak
_ack:
    lda #$01
    rts
_nak:
    lda #$00
    rts

;----------------------------------------------------------------
;   receive a byte thru 6551, waits
getchar:
@acia_rx:
; verify
    lda #CIA_STAT
    and #8
    ; beq @ends
    beq @acia_rx
; receive
    lda #CIA_RX
    rts

;----------------------------------------------------------------
;   transmit a byte thru 6551, waits
putchar:
@acia_tx:
; verify
    pha
    lda #CIA_STAT
    and #16
    ; beq @ends
    beq @acia_tx
; transmit
    pla
    sta #CIA_TX
    rts

.endif

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
    clc
    bcc keyk

; ---------------------------------------------------------------------
; Forth like functions
; to keep code safe by not using "fall throught".
; uses A, Y, X caller must saves.
; needs 2 levels of hardware stack
; uses 4 bytes in page zero as temporary
; uses 6 bytes in memory for internal use
; ---------------------------------------------------------------------

; ---------------------------------------------------------------------
;   data stack stuff

keep_: ; to push
    ; ldx dat_indx
    dex
    dex
    stx dat_indx
    rts

lose_: ; to pull
    ; ldx dat_indx
    inx
    inx
    stx dat_indx
    rts

spush:
push_:
    ldx dat_indx
    lda tos + 0
    sta dat_zero - 2, x
    lda tos + 1
    sta dat_zero - 1, x
    jmp keep_

spull:
pull_:
    ldx dat_indx
    lda dat_zero + 0, x
    sta tos + 0
    lda dat_zero + 1, x
    sta tos + 1
    jmp lose_

.ifdef FULL_STACK_CODES
push2_:
    ldx dat_indx
    lda nos + 0
    sta dat_zero - 4, x
    lda nos + 1
    sta dat_zero - 3, x
    lda tos + 0
    sta dat_zero - 2, x
    lda tos + 1
    sta dat_zero - 1, x
    jsr keep_
    jmp keep_
.endif

take2:
pull2_:
    ldx dat_indx
    lda dat_zero + 0, x
    sta tos + 0
    lda dat_zero + 1, x
    sta tos + 1
    lda dat_zero + 2, x
    sta nos + 0
    lda dat_zero + 3, x
    sta nos + 1
    jsr lose_
    jmp lose_

drop_:
    ldx dat_indx
    jsr lose_
    ; rts
    jmp (vNext)

dup_:
    ldx dat_indx
    lda dat_zero + 0, x
    sta dat_zero - 2
    lda dat_zero + 1, x
    sta dat_zero - 1
    jsr keep_
    ; rts
    jmp (vNext)

over_:
    ldx dat_indx
    lda dat_zero + 2, x
    sta dat_zero - 2
    lda dat_zero + 3, x
    sta dat_zero - 1
    jsr keep_
    ; rts
    jmp (vNext)

swap_:
    ldx dat_indx
    lda dat_zero + 0, x
    sta dat_zero - 2
    lda dat_zero + 1, x
    sta dat_zero - 1
    lda dat_zero + 2, x
    sta dat_zero + 0
    lda dat_zero + 3, x
    sta dat_zero + 1
    lda dat_zero - 2, x
    sta dat_zero + 2
    lda dat_zero - 1, x
    sta dat_zero + 3
    ; rts
    jmp (vNext)

rot_:
    ldx dat_indx
    lda dat_zero + 4, x
    sta dat_zero - 2
    lda dat_zero + 5, x
    sta dat_zero - 1
    lda dat_zero + 2, x
    sta dat_zero + 4
    lda dat_zero + 3, x
    sta dat_zero + 5
    lda dat_zero + 0, x
    sta dat_zero + 2
    lda dat_zero + 1, x
    sta dat_zero + 3
    lda dat_zero - 2, x
    sta dat_zero + 0
    lda dat_zero - 1, x
    sta dat_zero + 1
    ; rts
    jmp (vNext)

and_:
    ldx dat_indx
    lda dat_zero + 0, x
    and dat_zero + 2, x
    sta dat_zero + 2, x
    lda dat_zero + 1, x
    and dat_zero + 3, x
    sta dat_zero + 3, x
    jmp drop_

or_:
    ldx dat_indx
    lda dat_zero + 0, x
    ora dat_zero + 2, x
    sta dat_zero + 2, x
    lda dat_zero + 1, x
    ora dat_zero + 3, x
    sta dat_zero + 3, x
    jmp drop_

xor_:
    ldx dat_indx
    lda dat_zero + 0, x
    eor dat_zero + 2, x
    sta dat_zero + 2, x
    lda dat_zero + 1, x
    eor dat_zero + 3, x
    sta dat_zero + 3, x
    jmp drop_

cpt_:
    ldx dat_indx
    sec
    tya
    sbc dat_zero + 0, x
    sta dat_zero + 0, x
    sec
    tya
    sbc dat_zero + 1, x
    sta dat_zero + 1, x
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
    ldx dat_indx
    sec
    lda dat_zero + 2, x
    sbc dat_zero + 0, x
    sta dat_zero + 2, x
    lda dat_zero + 3, x
    sbc dat_zero + 1, x
    sta dat_zero + 3, x
    jmp drop_

add_:
    ldx dat_indx
    clc
    lda dat_zero + 2, x
    adc dat_zero + 0, x
    sta dat_zero + 2, x
    lda dat_zero + 3, x
    adc dat_zero + 1, x
    sta dat_zero + 3, x
    jmp drop_

cmp_:
    ldx dat_indx
    sec
    lda dat_zero + 2, x
    sbc dat_zero + 0, x
    lda dat_zero + 3, x
    sbc dat_zero + 1, x
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
    ldx dat_indx
    sta dat_zero + 2, x
    sta dat_zero + 3, x
    jmp drop_

false2_:
    lda #(FALSE)
    beq same2_

true2_:
    lda #(!FALSE)
    bne same2_

shl_:
    ldx dat_indx
    asl dat_zero + 0, x
    rol dat_zero + 1, x
    ; rts
    jmp (vNext)

shr_:
    ldx dat_indx
    lsr dat_zero + 0, x
    ror dat_zero + 1, x
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
    ldx dat_indx
    lda dat_zero + 0, x
    sta tos + 0
    lda dat_zero + 1, x
    sta tos + 1
    ldy #0
    lda (tos), y
    sta dat_zero + 0, x
    rts

at_:
    jsr cat_
    iny
    lda (tos), y
    sta dat_zero + 1, x
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
    ldx dat_indx
    inc dat_zero + 0, x
    bne @ends
    inc dat_zero + 1, x
@ends:
    ; rts
    jmp (vNext)

decr_:
    ldx dat_indx
    lda dat_zero + 0, x
    bne @ends
    dec dat_zero + 1, x
@ends:
    dec dat_zero + 0, x
    ; rts
    jmp (vNext)

.ifdef FULL_STACK_CODES
jump_:
    ldx dat_indx
    lda dat_zero + 1,x
    pha
    lda dat_zero + 0,x
    pha
    php
    rti

addto_:
    jsr pull2_
    ldy #NUL
    clc
    lda (tos), y
    adc nos + 0
    sta (tos), y
    iny
    lda (tos), y
    adc nos + 1
    sta (tos), y
    ; rts
	jmp (vNext)

subto_:
    jsr pull2_
    ldy #NUL
    sec
    lda (tos), y
    sbc nos + 0
    sta (tos), y
    iny
    lda (tos), y
    sbc nos + 1
    sta (tos), y
    ; rts
	jmp (vNext)
.endif

;----------------------------------------------------------------------
;   return stack stuff

rpush:
rpush_:
    ldx dat_indx
    lda tos + 0
    sta ret_zero - 2, x
    lda tos + 1
    sta ret_zero - 1, x
    dex
    dex
    stx dat_indx
    rts

rpull:
rpull_:
    ldx dat_indx
    lda ret_zero + 0, x
    sta tos + 0
    lda ret_zero + 1, x
    sta tos + 1
    inx
    inx
    stx dat_indx
    rts

.ifdef FULL_STACK_CODES
rshow_:
    ldx dat_indx
    lda ret_zero + 0, x
    sta tos + 0
    lda ret_zero + 1, x
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
    ldx dat_indx
    ; pseudo tos
    lda dat_zero + 0, x
    sta wrk + 0
    lda dat_zero + 1, x
    sta wrk + 1
    ; pseudo nos
    lda dat_zero + 2, x
    sta tmp + 0
    lda dat_zero + 3, x
    sta tmp + 1
    ; clear results
    lda #NUL
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
    ldx dat_indx
    lda nos + 0
    sta dat_zero + 0, x
    lda nos + 1
    sta dat_zero + 1, x
    lda tos + 0
    sta dat_zero + 2, x
    lda tos + 1
    sta dat_zero + 3, x
    ; rts
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
    tax
    lda tos + 1
    sbc wrk + 1
    bcc @skip
    sta tos + 1
    stx tos + 0
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
    tax
    clc
    lda tmp + 0
    adc tos + 0
    sta tos + 0
    txa
    adc tmp + 1
@rotate_r:
    ; rotate partial product upper to low
    ror
    ror tos + 1
    ror nos + 1
    ror nos + 0
    ; countdown
    dey
    bne @shift_r
    sta tos + 0
    ; ends
    jmp opout

; set overflow bit
slv:
    bit @ends
@ends:
    rts

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
    adc ins_ptr + 0
    sta ins_ptr + 0
    bcc @ends
    inc ins_ptr + 1
@ends:
    ; next
    jmp (vNext)

;----------------------------------------------------------------------
pushps:
    ldx ret_indx
    lda ins_ptr + 0
    sta ret_zero - 2, x
    lda ins_ptr + 1
    sta ret_zero - 1, x
    dex
    dex
    stx ret_indx
    rts

;----------------------------------------------------------------------
pullps:
    ldx ret_indx
    lda ret_zero + 0, x
    sta ins_ptr + 0
    lda ret_zero + 1, x
    sta ins_ptr + 1
    inx
    inx
    stx ret_indx
    rts

;----------------------------------------------------------------------
seekps:
    ldy #NUL
    lda (ins_ptr), y
    inc ins_ptr + 0
    bne @ends
    inc ins_ptr + 1
@ends:
    rts

;----------------------------------------------------------------------
heap2nos:
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
tib2tos:
    lda #<tib
    sta tos + 0
    lda #>tib
    sta tos + 1
    rts

;----------------------------------------------------------------------
add2tos:
    clc
    adc tos + 0
    sta tos + 0
    bcc @ends
    inc tos + 1
@ends:
    rts

;----------------------------------------------------------------------
; sub
subn2t:
    sec
    lda tos + 0
    sbc nos + 0
    sta tos + 0
    lda tos + 1
    sbc nos + 1
    sta tos + 1
    rts

;----------------------------------------------------------------------
; add
addn2t:
    clc
    lda tos + 0
    adc nos + 0
    sta tos + 0
    lda tos + 1
    adc nos + 1
    sta tos + 1
    rts

;----------------------------------------------------------------------
; add 2x
addt2t:
    asl tos + 0
    sta tos + 0
    rol tos + 1
    sta tos + 1
    rts

;----------------------------------------------------------------------
; $00 to $1F, reserved for macros
; macros could not call macros.
macro:
    sty vTIBPtr + 0
    tay
    lda ctlcodeslo, y
    sta tos + 0
    lda ctlcodeshi, y
    sta tos + 1
    ; bypass offset for rts
    lda #1
    jsr add2tos
    ;
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
    lda #NUL
    sta vTIBPtr + 0

interpret2:
    lda #NUL
    sta nest
    tay
    beq @isnest

; calc nesting (a macro might have changed it)
@loop:
    lda tib, y
    iny
    jsr nesting            ; update nesting value

@isnest:
    cpy #NUL
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
    ldy #NUL
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
    ; y is the position in tib
    ; a is the code
    jmp macro

@ischar:
    jsr @toTib
    ; nest ?
    jsr nesting
    ; wait for next character
    clc
    bcc @loop

@iscrlf:
    ; just for easy
    lda #CR
    jsr @toTib
    lda #LF
    jsr @toTib
    ; pending nest ?
    lda nest
    bne @loop

; mark end with etx,
@endstr:
    ; mark ETX
    lda #ETX
    sta (tos), y
    iny

    ; update instruction pointer
    lda tos + 0
    sta ins_ptr + 0
    lda tos + 1
    sta ins_ptr + 1

    ; next
    jmp next

; maximum 254 chars
@toTib:
    ; echo
    jsr putchar
    ; store
    sta (tos), y
    iny
    rts

;----------------------------------------------------------------------
; nesting deep
nesting:
    cmp #'`'
    bne @nests
    ; clear bit 7
    lda #$80
    eor nest
    sta nest
    rts
@nests:
    bit nest
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
    inc nest
    rts
@nestdec:
    dec nest
    rts

;----------------------------------------------------------------------
; prints a asciiz, refered by hardware stack
printStr:
    ; reference
    pla
    sta tos + 0
    pla
    sta tos + 1
    ; asciiz
    ldx #NUL
    jsr putstr
    ; offset
    jsr add2tos
    lda tos + 1
    pha
    lda tos + 0
    pha
    rts

;----------------------------------------------------------------------
; puts a string, ends on `
str_:
    lda ins_ptr + 0
    sta tos + 0
    lda ins_ptr + 1
    sta tos + 1
    ldx #TRUE
    jsr putstr
    ; next
    jmp  (vNext)

;----------------------------------------------------------------------
; puts a string, asciiz
puts_:
    ldx #NUL
    jsr spull
    ; fall throught

;----------------------------------------------------------------------
; prints a asciiz
putstr:
    ldy #NUL
@loop:
    lda (tos), y
    beq @ends   ; limit NUL
    cpx #NUL
    beq @cont
    cmp #'`'        ; ` is the string terminator
    beq @ends
@cont:
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
    ; subtract
    iny
    jsr subn2t
    bcc @loop
    ; restore
    jsr addn2t
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
    tax
    lsr
    ror
    ror
    ror
    jsr @conv
    txa
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
nul2tos:
    lda #NUL
    sta tos + 0
    sta tos + 1
    rts

;---------------------------------------------------------------------
isdec:
    cmp #'0' + 0
    bcc nak
    cmp #'9' + 1
    bcs nak
    sec
    sbc #'0'
ack:
    clc
    rts
nak:
    sec
    rts

;---------------------------------------------------------------------
ishex:
    ; to upper, clear bit-6
    and #%11011111
    cmp 'A'
    bcc nak
    cmp 'F' + 1
    bcs nak
    sec
    sbc #'A' - 10
    bcc ack

;----------------------------------------------------------------------
; convert a decimal value to binary
dec_:
    jsr nul2tos
@loop:
    jsr seekps
    jsr isdec
    bcs @ends
@uval:
    jsr add2tos
    sta tos + 1
    jsr mul10
    clc
    bcc @loop
@ends:
    jsr spush
    ; next
    jmp (vNext)

;----------------------------------------------------------------------
; convert a hexadecimal value to binary
hex_:
    jsr nul2tos
@loop:
    jsr seekps
    jsr isdec
    bcc @uval
    jsr ishex
    bcc @uval
    bcs @ends
@uval:
    jsr add2tos
    jsr mul16
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
    jsr addt2t
    lda tos + 0
    sta nos + 0
    lda tos + 1
    sta nos + 1
    ; 2x
    jsr addt2t
    ; 2x
    jsr addt2t
    ; 2x + 8x
    jsr addn2t
    rts

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
    ldy #NUL
@loop:
    iny
    beq @ends   ; limit 256
    lda (ins_ptr), y
    beq @ends
    cmp #CR
    bne @loop
@ends:
    tya
    jmp add2ps

;----------------------------------------------------------------------
depth_:
    ; stacks are 128 words
    sec
    lda #$FF
    sbc dat_indx
    ; words
    lsr
    sta tos + 0
    lda #NUL
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
    lda #NUL
    sta tos + 1
    jsr spush
    ; next
    jmp (vNext)

;----------------------------------------------------------------------
; copy and update
compNext:

    ; pull heap
    jsr heap2nos

    ; pull value
    jsr spull

    ; byte
    ldy #NUL
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
    pha
    lda optcodeshi, y
    pha
    rts

;----------------------------------------------------------------------
; Execute next alt opcode
alt_:
    jsr seekps
    tay
    lda altcodeslo, y
    pha
    lda altcodeshi, y
    pha
    rts

;----------------------------------------------------------------------
; Parse inline code, must be asciiz
enter:
; pull from system stack
    pla
    sta ins_ptr + 0
    pla
    sta ins_ptr + 1
    inc ins_ptr + 0
    bcc @nock
    inc ins_ptr + 1
@nock:
    ; next
    jmp (vNext)

;----------------------------------------------------------------------
; char 0, Continue from enter
exit_:
    jmp (ins_ptr)

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
    ldx dat_indx
    lda dat_zero + 0, x
    sta ins_ptr + 0
    lda dat_zero + 1, x
    sta ins_ptr + 1
    inx
    inx
    stx dat_indx
    ; next
    jmp (vNext)

;----------------------------------------------------------------------
; Execute code from a user function
call_:

    tax
    jsr pushps
    tax

    jsr lookupDefs

    ; update instruction pointer
    ldy #NUL
    lda (tos), y
    sta ins_ptr + 0
    iny
    lda (tos), y
    sta ins_ptr + 1

    ; next
    jmp (vNext)

;----------------------------------------------------------------------
lookupDeft:
    sta vEdited
    ; fall throught

;----------------------------------------------------------------------
lookupDefs:
    sec
    sbc #'A'
    asl
    tay
    ; offset
    clc
    adc vDefs + 0
    sta tos + 0
    lda #NUL
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
    tax
    jsr lookupDeft

    ; origin
    lda (tos), y
    sta nos + 0
    iny
    lda (tos), y
    sta nos + 1

    ldy #NUL
    ; empty ?
    lda (nos), y
    beq @editDef3    ; is NUL ?
    cmp #';'        ; is end ?
    beq @editDef3

    ; copy

    jsr tib2tos

    lda #':'
    jsr writeChar
    lda #1
    jsr add2tos

    txa
    jsr writeChar
    lda #1
    jsr add2tos

    clc
    bcc @editDef2

@editDef1:
    iny
    beq @editDef3

@editDef2:
    lda (nos), y
    jsr writeChar
    cmp #';'
    bne @editDef1

@editDef3:
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
    tax
    lda #<vars
    sta tos + 0
    lda #>vars
    sta tos + 1
    jmp a2z

;----------------------------------------------------------------------
; push a mint variable
sysVar_:
    tax
    lda #<vsys
    sta tos + 0
    lda #>vsys
    sta tos + 1
    jmp a2z

;----------------------------------------------------------------------
; push a reference into stack
a2z:
    sec
    txa
    sbc #'a'
    asl
    jsr add2tos
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
    ;-----------------------
    ; multiply by GROUP of 64
    ; swap byte
    lda tos + 0
    sta nos + 1
    lda #NUL
    sta nos + 0
    ; group is 64 bytes
    lsr nos + 1
    ror nos + 0
    lsr nos + 1
    ror nos + 0
    ;-----------------------
    ; save last group
    ldx ret_indx
    lda vDefs + 0
    sta ret_zero - 2, x
    lda vDefs + 1
    sta ret_zero - 1, x
    dex
    dex
    stx ret_indx
    ; update group
    clc
    lda defs + 0
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
    ldx ret_indx
    lda ret_zero + 0, x
    sta vDefs + 0
    lda ret_zero + 1, x
    sta vDefs + 1
    inx
    inx
    stx ret_indx

    ; next
    jmp (vNext)

;----------------------------------------------------------------------
getRef_:
    jsr nosp
    jsr lookupDefs
    jmp fetch_

;----------------------------------------------------------------------
arrDef_:
    lda #FALSE
    beq arrDefs

;----------------------------------------------------------------------
cArrDef_:
    lda #TRUE
    ; fall throught

;----------------------------------------------------------------------
arrDefs:
    ; save array mode
    sta vByteMode

    ; save array start
    ldx dat_indx
    lda vHeap + 0
    sta ret_zero - 2, x
    lda vHeap + 1
    sta ret_zero - 1, x
    dex
    dex
    stx dat_indx

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

    ; save start
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
    ; save size
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
    ; must be a A-Z, can't be space
    jsr nosp
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
    ldy #NUL
@loop:
    lda (ins_ptr), y
    sta (nos), y
    beq @ends
    iny
    beq @ends
    cmp #';'
    bne @loop
@ends:
    ; update heap
    tya
    tax
    jsr add2heap
    ; update instruction pointer
    txa
    jmp add2ps

;----------------------------------------------------------------------
; skip while nest
skipnest:
    lda #$01
    sta nest
@loop:
    jsr seekps
    jsr nesting
    lda nest
    bne @loop
    ; next
    jmp (vNext)

;----------------------------------------------------------------------
; make a frame
mkframe:
    ; alloc a frame
    sec
    lda ret_indx
    sbc #6
    sta ret_indx
    rts

;----------------------------------------------------------------------
; skip a frame
skframe:
    ; alloc a frame
    clc
    lda ret_indx
    adc #6
    sta ret_indx
    rts

;----------------------------------------------------------------------
break_:
    jsr spull
    lda tos + 0
    bne @isne
    ; parse frame
    jmp (vNext)
@isne:
    jsr skframe
    jmp skipnest

;----------------------------------------------------------------------
; Left parentesis ( begins a loop
begin_:

    ; tos is zero ?
    jsr spull
    lda tos + 0
    beq skipnest

    ; alloc a frame
    jsr mkframe

    ; a frame
    ldx ret_indx
    ; counter
    lda #NUL
    sta ret_zero + 0, x
    sta ret_zero + 1, x
    ; limit
    lda tos + 0
    sta ret_zero + 2, x
    lda tos + 1
    sta ret_zero + 3, x
    ; pointer
    lda ins_ptr + 0
    sta ret_zero + 4, x
    lda ins_ptr + 1
    sta ret_zero + 5, x

    ; next
    jmp (vNext)

;----------------------------------------------------------------------
; Right parentesis ) again a loop
again_:
    ; check if IFTEMode $FFFF
    lda ret_zero + 0, x
    and ret_zero + 1, x
    cmp #$FF
    bne @again1

    ; push FALSE
    lda #FALSE
    sta tos + 0
    sta tos + 1
    jsr spush

    ; drop IFTEMmode
    inc ret_indx
    inc ret_indx

    ; next
    jmp (vNext)

@again1:
    ; test end
    ldx ret_indx
    lda ret_zero + 2, x
    cmp ret_zero + 0, x
    bne @noend
    lda ret_zero + 3, x
    cmp ret_zero + 1, x
    bne @noend

    ; end of loop
    jsr skframe

    ; next
    jmp (vNext)

@noend:
    ; increase counter
    inc ret_zero + 0, x
    bne @novr
    inc ret_zero + 1, x
@novr:

    ; return at begin
    lda ret_zero + 4, x
    sta ins_ptr + 0
    lda ret_zero + 5, x
    sta ins_ptr + 1

    ; next
    jmp (vNext)

;----------------------------------------------------------------------
j_:
    sec
    lda ret_indx
    sbc #6
    tax
    jmp index

;----------------------------------------------------------------------
i_:
    ldx ret_indx
    ; fall through

;----------------------------------------------------------------------
index:
    lda ret_zero + 0, x
    sta tos + 0
    lda ret_zero + 1, x
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
; verify stack
etx_:
    lda dat_indx
    cmp STKSIZE * 2 ; bytes
    bcc @ends
    lda #NUL
@ends:
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
    cld
    ldx #$FF
    txs
    inx
    txa
    tay
    cli

    jsr initialize

; safe
    lda #<next
    sta vNext + 0
    lda #>next
    sta vNext + 1

    jsr printStr
    .asciiz "MINT 6502 V1.0\r\n"

    ; auto reset
    jsr interpret

    jmp mint_

;----------------------------------------------------------------------
initialize:

.if 0
; defaults values
    lda #<vars
    sta tos + 0
    lda #>vars
    sta tos + 1
    lda #<vsys
    sta nos + 0
    lda #>vsys
    sta nos + 1
    ldy #GRPSIZE
    lda #NUL
@loop1:
    sta (tos), y
    sta (nos), y
    dey
    sta (tos), y
    sta (nos), y
    dey
    bne @loop1
.endif

; default system values
    lda #<iSysVars
    sta tos + 0
    lda #>iSysVars
    sta tos + 1
    lda #<vsys
    sta nos + 0
    lda #>vsys
    sta nos + 1
    ldy #dysys
@loop:
    lda (tos), y
    sta (nos), y
    dey
    bne @loop

; default function
    lda #<defs
    sta tos + 0
    lda #>defs
    sta tos + 1

    ldx #NUL
@loop2:
    ldy #NUL
@loop3:
    ; default
    lda #>empty_
    sta (tos), y
    iny
    lda #<empty_
    sta (tos), y
    iny
    cpy #GRPSIZE
    bne @loop3

    inx
    cpx #NUMGRPS
    beq @ends

    ; increment
    clc
    lda tos + 0
    adc #GRPSIZE
    sta tos + 0
    bcc @next
    inc tos + 1
@next:
    clc
    bcc @loop2
@ends:
    ; all done
    rts

;----------------------------------------------------------------------
;optcodes: parsed by opt_ (next)
;altcodes: parsed by alt_
;ctlcodes: maybe in a future...

; *********************************************************************
; Jump Tables, optmized for single index
; *********************************************************************

; .align $100

; using pla, pla, rts, references must be one less
;----------------------------------------------------------------------
optcodeslo:
   .byte  <(exit_ - 1)    ;   NUL
   .byte  <(nop_ - 1)     ;   SOH
   .byte  <(nop_ - 1)     ;   STX
   .byte  <(etx_ - 1)     ;   ETX
   .byte  <(nop_ - 1)     ;   EOT
   .byte  <(nop_ - 1)     ;   ENQ
   .byte  <(nop_ - 1)     ;   ACK
   .byte  <(nop_ - 1)     ;   BEL
   .byte  <(nop_ - 1)     ;   BS
   .byte  <(nop_ - 1)     ;   TAB
   .byte  <(nop_ - 1)     ;   LF
   .byte  <(nop_ - 1)     ;   VT
   .byte  <(nop_ - 1)     ;   FF
   .byte  <(nop_ - 1)     ;   CR
   .byte  <(nop_ - 1)     ;   SO
   .byte  <(nop_ - 1)     ;   SI
   .byte  <(nop_ - 1)     ;   DLE
   .byte  <(nop_ - 1)     ;   DC1
   .byte  <(nop_ - 1)     ;   DC2
   .byte  <(nop_ - 1)     ;   DC3
   .byte  <(nop_ - 1)     ;   DC4
   .byte  <(nop_ - 1)     ;   NAK
   .byte  <(nop_ - 1)     ;   SYN
   .byte  <(nop_ - 1)     ;   ETB
   .byte  <(nop_ - 1)     ;   CAN
   .byte  <(nop_ - 1)     ;   EM
   .byte  <(nop_ - 1)     ;   SUB
   .byte  <(nop_ - 1)     ;   ESC
   .byte  <(nop_ - 1)     ;   FS
   .byte  <(nop_ - 1)     ;   GS
   .byte  <(nop_ - 1)     ;   RS
   .byte  <(nop_ - 1)     ;   US
   .byte  <(nop_ - 1)     ;   SP
   .byte  <(store_ - 1)   ;   !
   .byte  <(dup_ - 1)     ;   "
   .byte  <(hex_ - 1)     ;    #
   .byte  <(swap_ - 1)    ;    $
   .byte  <(over_ - 1)    ;    %
   .byte  <(and_ - 1)     ;    &
   .byte  <(drop_ - 1)    ;    '
   .byte  <(begin_ - 1)   ;    (
   .byte  <(again_ - 1)   ;    )
   .byte  <(mul_ - 1)     ;    * multiply 16x16
   .byte  <(add_ - 1)     ;    +
   .byte  <(hdot_ - 1)    ;    ,
   .byte  <(sub_ - 1)     ;    -
   .byte  <(dot_ - 1)     ;    .
   .byte  <(div_ - 1)     ;    / divide 16x16
   .byte  <(dec_ - 1)     ;    0
   .byte  <(dec_ - 1)     ;    1
   .byte  <(dec_ - 1)     ;    2
   .byte  <(dec_ - 1)     ;    3
   .byte  <(dec_ - 1)     ;    4
   .byte  <(dec_ - 1)     ;    5
   .byte  <(dec_ - 1)     ;    6
   .byte  <(dec_ - 1)     ;    7
   .byte  <(dec_ - 1)     ;    8
   .byte  <(dec_ - 1)     ;    9
   .byte  <(def_ - 1)     ;    :
   .byte  <(ret_ - 1)     ;    ;
   .byte  <(lt_ - 1)      ;    <
   .byte  <(eq_ - 1)      ;    =
   .byte  <(gt_ - 1)      ;    >
   .byte  <(getRef_ - 1)  ;    ?
   .byte  <(fetch_ - 1)   ;    @
   .byte  <(call_ - 1)    ;    A
   .byte  <(call_ - 1)    ;    B
   .byte  <(call_ - 1)    ;    C
   .byte  <(call_ - 1)    ;    D
   .byte  <(call_ - 1)    ;    E
   .byte  <(call_ - 1)    ;    F
   .byte  <(call_ - 1)    ;    G
   .byte  <(call_ - 1)    ;    H
   .byte  <(call_ - 1)    ;    I
   .byte  <(call_ - 1)    ;    J
   .byte  <(call_ - 1)    ;    K
   .byte  <(call_ - 1)    ;    L
   .byte  <(call_ - 1)    ;    M
   .byte  <(call_ - 1)    ;    N
   .byte  <(call_ - 1)    ;    O
   .byte  <(call_ - 1)    ;    P
   .byte  <(call_ - 1)    ;    Q
   .byte  <(call_ - 1)    ;    R
   .byte  <(call_ - 1)    ;    S
   .byte  <(call_ - 1)    ;    T
   .byte  <(call_ - 1)    ;    U
   .byte  <(call_ - 1)    ;    V
   .byte  <(call_ - 1)    ;    W
   .byte  <(call_ - 1)    ;    X
   .byte  <(call_ - 1)    ;    Y
   .byte  <(call_ - 1)    ;    Z
   .byte  <(arrDef_ - 1)  ;    [
   .byte  <(alt_ - 1)     ;    \
   .byte  <(arrEnd_ - 1)  ;    ]
   .byte  <(xor_ - 1)     ;    ^
   .byte  <(neg_ - 1)     ;    _
   .byte  <(str_ - 1)     ;    `
   .byte  <(var_ - 1)     ;    a
   .byte  <(var_ - 1)     ;    b
   .byte  <(var_ - 1)     ;    c
   .byte  <(var_ - 1)     ;    d
   .byte  <(var_ - 1)     ;    e
   .byte  <(var_ - 1)     ;    f
   .byte  <(var_ - 1)     ;    g
   .byte  <(var_ - 1)     ;    h
   .byte  <(var_ - 1)     ;    i
   .byte  <(var_ - 1)     ;    j
   .byte  <(var_ - 1)     ;    k
   .byte  <(var_ - 1)     ;    l
   .byte  <(var_ - 1)     ;    m
   .byte  <(var_ - 1)     ;    n
   .byte  <(var_ - 1)     ;    o
   .byte  <(var_ - 1)     ;    p
   .byte  <(var_ - 1)     ;    q
   .byte  <(var_ - 1)     ;    r
   .byte  <(var_ - 1)     ;    s
   .byte  <(var_ - 1)     ;    t
   .byte  <(var_ - 1)     ;    u
   .byte  <(var_ - 1)     ;    v
   .byte  <(var_ - 1)     ;    w
   .byte  <(var_ - 1)     ;    x
   .byte  <(var_ - 1)     ;    y
   .byte  <(var_ - 1)     ;    z
   .byte  <(shl_ - 1)     ;    {
   .byte  <(or_ - 1)      ;    |
   .byte  <(shr_ - 1)     ;    }
   .byte  <(inv_ - 1)     ;    ~
   .byte  <(nop_ - 1)     ;    backspace

optcodeshi:
   .byte  >(exit_ - 1)    ;   NUL
   .byte  >(nop_ - 1)     ;   SOH
   .byte  >(nop_ - 1)     ;   STX
   .byte  >(etx_ - 1)     ;   ETX
   .byte  >(nop_ - 1)     ;   EOT
   .byte  >(nop_ - 1)     ;   ENQ
   .byte  >(nop_ - 1)     ;   ACK
   .byte  >(nop_ - 1)     ;   BEL
   .byte  >(nop_ - 1)     ;   BS
   .byte  >(nop_ - 1)     ;   TAB
   .byte  >(nop_ - 1)     ;   LF
   .byte  >(nop_ - 1)     ;   VT
   .byte  >(nop_ - 1)     ;   FF
   .byte  >(nop_ - 1)     ;   CR
   .byte  >(nop_ - 1)     ;   SO
   .byte  >(nop_ - 1)     ;   SI
   .byte  >(nop_ - 1)     ;   DLE
   .byte  >(nop_ - 1)     ;   DC1
   .byte  >(nop_ - 1)     ;   DC2
   .byte  >(nop_ - 1)     ;   DC3
   .byte  >(nop_ - 1)     ;   DC4
   .byte  >(nop_ - 1)     ;   NAK
   .byte  >(nop_ - 1)     ;   SYN
   .byte  >(nop_ - 1)     ;   ETB
   .byte  >(nop_ - 1)     ;   CAN
   .byte  >(nop_ - 1)     ;   EM
   .byte  >(nop_ - 1)     ;   SUB
   .byte  >(nop_ - 1)     ;   ESC
   .byte  >(nop_ - 1)     ;   FS
   .byte  >(nop_ - 1)     ;   GS
   .byte  >(nop_ - 1)     ;   RS
   .byte  >(nop_ - 1)     ;   US
   .byte  >(nop_ - 1)     ;   SP
   .byte  >(store_ - 1)   ;   !
   .byte  >(dup_ - 1)     ;   "
   .byte  >(hex_ - 1)     ;    #
   .byte  >(swap_ - 1)    ;    $
   .byte  >(over_ - 1)    ;    %
   .byte  >(and_ - 1)     ;    &
   .byte  >(drop_ - 1)    ;    '
   .byte  >(begin_ - 1)   ;    (
   .byte  >(again_ - 1)   ;    )
   .byte  >(mul_ - 1)     ;    *  multiply 16x16, (multpd multpr -- LSW MSW)
   .byte  >(add_ - 1)     ;    +
   .byte  >(hdot_ - 1)    ;    ,
   .byte  >(sub_ - 1)     ;    -
   .byte  >(dot_ - 1)     ;    .
   .byte  >(div_ - 1)     ;    /  divide 16x16, (divd divs -- quo rem)
   .byte  >(dec_ - 1)     ;    0
   .byte  >(dec_ - 1)     ;    1
   .byte  >(dec_ - 1)     ;    2
   .byte  >(dec_ - 1)     ;    3
   .byte  >(dec_ - 1)     ;    4
   .byte  >(dec_ - 1)     ;    5
   .byte  >(dec_ - 1)     ;    6
   .byte  >(dec_ - 1)     ;    7
   .byte  >(dec_ - 1)     ;    8
   .byte  >(dec_ - 1)     ;    9
   .byte  >(def_ - 1)     ;    :
   .byte  >(ret_ - 1)     ;    ;
   .byte  >(lt_ - 1)      ;    <( - 1)
   .byte  >(eq_ - 1)      ;    =
   .byte  >(gt_ - 1)      ;    >
   .byte  >(getRef_ - 1)  ;    ?
   .byte  >(fetch_ - 1)   ;    @
   .byte  >(call_ - 1)    ;    A
   .byte  >(call_ - 1)    ;    B
   .byte  >(call_ - 1)    ;    C
   .byte  >(call_ - 1)    ;    D
   .byte  >(call_ - 1)    ;    E
   .byte  >(call_ - 1)    ;    F
   .byte  >(call_ - 1)    ;    G
   .byte  >(call_ - 1)    ;    H
   .byte  >(call_ - 1)    ;    I
   .byte  >(call_ - 1)    ;    J
   .byte  >(call_ - 1)    ;    K
   .byte  >(call_ - 1)    ;    L
   .byte  >(call_ - 1)    ;    M
   .byte  >(call_ - 1)    ;    N
   .byte  >(call_ - 1)    ;    O
   .byte  >(call_ - 1)    ;    P
   .byte  >(call_ - 1)    ;    Q
   .byte  >(call_ - 1)    ;    R
   .byte  >(call_ - 1)    ;    S
   .byte  >(call_ - 1)    ;    T
   .byte  >(call_ - 1)    ;    U
   .byte  >(call_ - 1)    ;    V
   .byte  >(call_ - 1)    ;    W
   .byte  >(call_ - 1)    ;    X
   .byte  >(call_ - 1)    ;    Y
   .byte  >(call_ - 1)    ;    Z
   .byte  >(arrDef_ - 1)  ;    [
   .byte  >(alt_ - 1)     ;    \
   .byte  >(arrEnd_ - 1)  ;    ]
   .byte  >(xor_ - 1)     ;    ^
   .byte  >(neg_ - 1)     ;    _
   .byte  >(str_ - 1)     ;    `
   .byte  >(var_ - 1)     ;    a
   .byte  >(var_ - 1)     ;    b
   .byte  >(var_ - 1)     ;    c
   .byte  >(var_ - 1)     ;    d
   .byte  >(var_ - 1)     ;    e
   .byte  >(var_ - 1)     ;    f
   .byte  >(var_ - 1)     ;    g
   .byte  >(var_ - 1)     ;    h
   .byte  >(var_ - 1)     ;    i
   .byte  >(var_ - 1)     ;    j
   .byte  >(var_ - 1)     ;    k
   .byte  >(var_ - 1)     ;    l
   .byte  >(var_ - 1)     ;    m
   .byte  >(var_ - 1)     ;    n
   .byte  >(var_ - 1)     ;    o
   .byte  >(var_ - 1)     ;    p
   .byte  >(var_ - 1)     ;    q
   .byte  >(var_ - 1)     ;    r
   .byte  >(var_ - 1)     ;    s
   .byte  >(var_ - 1)     ;    t
   .byte  >(var_ - 1)     ;    u
   .byte  >(var_ - 1)     ;    v
   .byte  >(var_ - 1)     ;    w
   .byte  >(var_ - 1)     ;    x
   .byte  >(var_ - 1)     ;    y
   .byte  >(var_ - 1)     ;    z
   .byte  >(shl_ - 1)     ;    {
   .byte  >(or_ - 1)      ;    |
   .byte  >(shr_ - 1)     ;    }
   .byte  >(inv_ - 1)     ;    ~
   .byte  >(nop_ - 1)     ;    backspace

;----------------------------------------------------------------------
; alternate function codes
ctlcodeslo:
altcodeslo:
   .byte  <(empty_ - 1)      ; NUL ^@
   .byte  <(empty_ - 1)      ; SOH ^A
   .byte  <(toggleBase_ - 1) ; STX ^B
   .byte  <(empty_ - 1)      ; ETX ^C
   .byte  <(empty_ - 1)      ; EOT ^D
   .byte  <(edit_ - 1)       ; ENQ ^E
   .byte  <(empty_ - 1)      ; ACK ^F
   .byte  <(empty_ - 1)      ; BEL ^G
   .byte  <(backsp_ - 1)     ; BS  ^H
   .byte  <(empty_ - 1)      ; TAB ^I
   .byte  <(reedit_ - 1)     ; LF  ^J
   .byte  <(empty_ - 1)      ; VT  ^K
   .byte  <(list_ - 1)       ; FF  ^L
   .byte  <(empty_ - 1)      ; CR  ^M
   .byte  <(empty_ - 1)      ; SO  ^N
   .byte  <(empty_ - 1)      ; SI  ^O
   .byte  <(printStack_ - 1) ; DLE ^P
   .byte  <(empty_ - 1)      ; DC1 ^Q
   .byte  <(empty_ - 1)      ; DC2 ^R
   .byte  <(empty_ - 1)      ; DC3 ^S
   .byte  <(empty_ - 1)      ; DC4 ^T
   .byte  <(empty_ - 1)      ; NAK ^U
   .byte  <(empty_ - 1)      ; SYN ^V
   .byte  <(empty_ - 1)      ; ETB ^W
   .byte  <(empty_ - 1)      ; CAN ^X
   .byte  <(empty_ - 1)      ; EM  ^Y
   .byte  <(empty_ - 1)      ; SUB ^Z
   .byte  <(empty_ - 1)      ; ESC ^[
   .byte  <(empty_ - 1)      ; FS  ^\
   .byte  <(empty_ - 1)      ; GS  ^]
   .byte  <(empty_ - 1)      ; RS  ^^
   .byte  <(empty_ - 1)      ; US  ^_)
   .byte  <(aNop_ - 1)       ; SP  ^`
   .byte  <(cStore_ - 1)     ;    !
   .byte  <(aNop_ - 1)       ;    "
   .byte  <(aNop_ - 1)       ;    #
   .byte  <(aNop_ - 1)       ;    $  ( -- adr ) text input ptr
   .byte  <(aNop_ - 1)       ;    %
   .byte  <(aNop_ - 1)       ;    &
   .byte  <(aNop_ - 1)       ;    '
   .byte  <(ifte_ - 1)       ;    (  ( b -- )
   .byte  <(aNop_ - 1)       ;    )
   .byte  <(aNop_ - 1)       ;    *
   .byte  <(incr_ - 1)       ;    +  ( adr -- ) increments variable at address
   .byte  <(aNop_ - 1)       ;    ,
   .byte  <(decr_ - 1)       ;    -  ( adr -- ) decrements variable at address
   .byte  <(aNop_ - 1)       ;    .
   .byte  <(aNop_ - 1)       ;    /
   .byte  <(aNop_ - 1)       ;    0
   .byte  <(aNop_ - 1)       ;    1
   .byte  <(aNop_ - 1)       ;    2
   .byte  <(aNop_ - 1)       ;    3
   .byte  <(aNop_ - 1)       ;    4
   .byte  <(aNop_ - 1)       ;    5
   .byte  <(aNop_ - 1)       ;    6
   .byte  <(aNop_ - 1)       ;    7
   .byte  <(aNop_ - 1)       ;    8
   .byte  <(aNop_ - 1)       ;    9
   .byte  <(aNop_ - 1)       ;    :  start defining a macro
   .byte  <(aNop_ - 1)       ;    ;
   .byte  <(aNop_ - 1)       ;    <
   .byte  <(aNop_ - 1)       ;    =
   .byte  <(aNop_ - 1)       ;    >( - 1)
   .byte  <(aNop_ - 1)       ;    ?
   .byte  <(cFetch_ - 1)     ;    @
   .byte  <(aNop_ - 1)       ;    A
   .byte  <(break_ - 1)      ;    B
   .byte  <(nop_ - 1)        ;    C
   .byte  <(depth_ - 1)      ;    D  ( -- val ) depth of data stack
   .byte  <(emit_ - 1)       ;    E  ( val -- ) emits a char to output
   .byte  <(aNop_ - 1)       ;    F
   .byte  <(go_ - 1)         ;    G  ( -- ? ) execute mint definition
   .byte  <(keyq_ - 1)       ;    H  ( verify if key hit )
   .byte  <(inPort_ - 1)     ;    I  ( port -- val )
   .byte  <(aNop_ - 1)       ;    J
   .byte  <(key_ - 1)        ;    K  ( -- val )  read a char from input
   .byte  <(aNop_ - 1)       ;    L
   .byte  <(aNop_ - 1)       ;    M
   .byte  <(newln_ - 1)      ;    N  ; prints a newline to output
   .byte  <(outPort_ - 1)    ;    O  ( val port -- )
   .byte  <(printStk_ - 1)   ;    P  ( -- ) non-destructively prints stack
   .byte  <(aNop_ - 1)       ;    Q  quits from Mint REPL
   .byte  <(rot_ - 1)        ;    R  ( a b c -- b c a )
   .byte  <(aNop_ - 1)       ;    S
   .byte  <(aNop_ - 1)       ;    T
   .byte  <(r2s_ - 1)        ;    U  S( -- w ) R( w -- )
   .byte  <(s2r_ - 1)        ;    V  S( w -- ) R( -- w )
   .byte  <(aNop_ - 1)       ;    W   ; ( b -- ) if false, skip to end of loop
   .byte  <(exec_ - 1)       ;    X
   .byte  <(aNop_ - 1)       ;    Y
   .byte  <(editDef_ - 1)    ;    Z
   .byte  <(cArrDef_ - 1)    ;    [
   .byte  <(comment_ - 1)    ;    \  comment text, skip reading until end of line
   .byte  <(aNop_ - 1)       ;    ]
   .byte  <(charCode_ - 1)   ;    ^
   .byte  <(aNop_ - 1)       ;    _
   .byte  <(aNop_ - 1)       ;    `
   .byte  <(sysVar_ - 1)     ;    a  ; start of data stack *fixed
   .byte  <(sysVar_ - 1)     ;    b  ; base16 flag
   .byte  <(sysVar_ - 1)     ;    c  ; TIBPtr variable
   .byte  <(sysVar_ - 1)     ;    d  ; vDefs variable
   .byte  <(sysVar_ - 1)     ;    e  ;
   .byte  <(sysVar_ - 1)     ;    f  ; start of return stack *fixed
   .byte  <(sysVar_ - 1)     ;    g  ; next dispatcher
   .byte  <(sysVar_ - 1)     ;    h  ; heap ptr variable
   .byte  <(i_ - 1)          ;    i  ; returns index of current loop
   .byte  <(j_ - 1)          ;    j  ; returns index of outer loop
   .byte  <(sysVar_ - 1)     ;    k
   .byte  <(sysVar_ - 1)     ;    l
   .byte  <(sysVar_ - 1)     ;    m  ( a b -- c ) return the minimum value
   .byte  <(sysVar_ - 1)     ;    n
   .byte  <(sysVar_ - 1)     ;    o
   .byte  <(sysVar_ - 1)     ;    p
   .byte  <(sysVar_ - 1)     ;    q
   .byte  <(sysVar_ - 1)     ;    r  ; return stack pointer
   .byte  <(sysVar_ - 1)     ;    s  ; data stack pointer
   .byte  <(sysVar_ - 1)     ;    t
   .byte  <(sysVar_ - 1)     ;    u
   .byte  <(sysVar_ - 1)     ;    v
   .byte  <(sysVar_ - 1)     ;    w
   .byte  <(sysVar_ - 1)     ;    x
   .byte  <(sysVar_ - 1)     ;    y
   .byte  <(sysVar_ - 1)     ;    z
   .byte  <(group_ - 1)      ;    {
   .byte  <(aNop_ - 1)       ;    |
   .byte  <(endGroup_ - 1)   ;    }
   .byte  <(aNop_ - 1)       ;    ~
   .byte  <(aNop_ - 1)       ;    BS

ctlcodeshi:
altcodeshi:
   .byte  >(empty_ - 1)      ; NUL ^@
   .byte  >(empty_ - 1)      ; SOH ^A
   .byte  >(toggleBase_ - 1) ; STX ^B
   .byte  >(empty_ - 1)      ; ETX ^C
   .byte  >(empty_ - 1)      ; EOT ^D
   .byte  >(edit_ - 1)       ; ENQ ^E
   .byte  >(empty_ - 1)      ; ACK ^F
   .byte  >(empty_ - 1)      ; BEL ^G
   .byte  >(backsp_ - 1)     ; BS  ^H
   .byte  >(empty_ - 1)      ; TAB ^I
   .byte  >(reedit_ - 1)     ; LF  ^J
   .byte  >(empty_ - 1)      ; VT  ^K
   .byte  >(list_ - 1)       ; FF  ^L
   .byte  >(empty_ - 1)      ; CR  ^M
   .byte  >(empty_ - 1)      ; SO  ^N
   .byte  >(empty_ - 1)      ; SI  ^O
   .byte  >(printStack_ - 1) ; DLE ^P
   .byte  >(empty_ - 1)      ; DC1 ^Q
   .byte  >(empty_ - 1)      ; DC2 ^R
   .byte  >(empty_ - 1)      ; DC3 ^S
   .byte  >(empty_ - 1)      ; DC4 ^T
   .byte  >(empty_ - 1)      ; NAK ^U
   .byte  >(empty_ - 1)      ; SYN ^V
   .byte  >(empty_ - 1)      ; ETB ^W
   .byte  >(empty_ - 1)      ; CAN ^X
   .byte  >(empty_ - 1)      ; EM  ^Y
   .byte  >(empty_ - 1)      ; SUB ^Z
   .byte  >(empty_ - 1)      ; ESC ^[
   .byte  >(empty_ - 1)      ; FS  ^\
   .byte  >(empty_ - 1)      ; GS  ^]
   .byte  >(empty_ - 1)      ; RS  ^^
   .byte  >(empty_ - 1)      ; US  ^_)
   .byte  >(aNop_ - 1)       ; SP  ^`
   .byte  >(cStore_ - 1)     ;    !
   .byte  >(aNop_ - 1)       ;    "
   .byte  >(aNop_ - 1)       ;    #
   .byte  >(aNop_ - 1)       ;    $  ( -- adr ) text input ptr
   .byte  >(aNop_ - 1)       ;    %
   .byte  >(aNop_ - 1)       ;    &
   .byte  >(aNop_ - 1)       ;    '
   .byte  >(ifte_ - 1)       ;    (  ( b -- )
   .byte  >(aNop_ - 1)       ;    )
   .byte  >(aNop_ - 1)       ;    *
   .byte  >(incr_ - 1)       ;    +  ( adr -- ) increments variable at address
   .byte  >(aNop_ - 1)       ;    ,
   .byte  >(decr_ - 1)       ;    -  ( adr -- ) decrements veriable at address
   .byte  >(aNop_ - 1)       ;    .
   .byte  >(aNop_ - 1)       ;    /
   .byte  >(aNop_ - 1)       ;    0
   .byte  >(aNop_ - 1)       ;    1
   .byte  >(aNop_ - 1)       ;    2
   .byte  >(aNop_ - 1)       ;    3
   .byte  >(aNop_ - 1)       ;    4
   .byte  >(aNop_ - 1)       ;    5
   .byte  >(aNop_ - 1)       ;    6
   .byte  >(aNop_ - 1)       ;    7
   .byte  >(aNop_ - 1)       ;    8
   .byte  >(aNop_ - 1)       ;    9
   .byte  >(aNop_ - 1)       ;    :  start defining a macro
   .byte  >(aNop_ - 1)       ;    ;
   .byte  >(aNop_ - 1)       ;    <( - 1)
   .byte  >(aNop_ - 1)       ;    =
   .byte  >(aNop_ - 1)       ;    >
   .byte  >(aNop_ - 1)       ;    ?
   .byte  >(cFetch_ - 1)     ;    @
   .byte  >(aNop_ - 1)       ;    A
   .byte  >(break_ - 1)      ;    B
   .byte  >(nop_ - 1)        ;    C
   .byte  >(depth_ - 1)      ;    D  ( -- val ) depth of data stack
   .byte  >(emit_ - 1)       ;    E  ( val -- ) emits a char to output
   .byte  >(aNop_ - 1)       ;    F
   .byte  >(go_ - 1)         ;    G  ( -- ? ) execute mint definition
   .byte  >(keyq_ - 1)       ;    H  ( verify if key hit )
   .byte  >(inPort_ - 1)     ;    I  ( port -- val )
   .byte  >(aNop_ - 1)       ;    J
   .byte  >(key_ - 1)        ;    K  ( -- val )  read a char from input
   .byte  >(aNop_ - 1)       ;    L
   .byte  >(aNop_ - 1)       ;    M
   .byte  >(newln_ - 1)      ;    N  ; prints a newline to output
   .byte  >(outPort_ - 1)    ;    O  ( val port -- )
   .byte  >(printStk_ - 1)   ;    P  ( -- ) non-destructively prints stack
   .byte  >(aNop_ - 1)       ;    Q  quits from Mint REPL
   .byte  >(rot_ - 1)        ;    R  ( a b c -- b c a )
   .byte  >(aNop_ - 1)       ;    S
   .byte  >(aNop_ - 1)       ;    T
   .byte  >(r2s_ - 1)        ;    U  S( -- w ) R( w -- )
   .byte  >(s2r_ - 1)        ;    V  S( w -- ) R( -- w )
   .byte  >(aNop_ - 1)       ;    W   ; ( b -- ) if false, skip to end of loop
   .byte  >(exec_ - 1)       ;    X
   .byte  >(aNop_ - 1)       ;    Y
   .byte  >(editDef_ - 1)    ;    Z
   .byte  >(cArrDef_ - 1)    ;    [
   .byte  >(comment_ - 1)    ;    \  comment text, skip reading until end of line
   .byte  >(aNop_ - 1)       ;    ]
   .byte  >(charCode_ - 1)   ;    ^
   .byte  >(aNop_ - 1)       ;    _
   .byte  >(aNop_ - 1)       ;    `
   .byte  >(sysVar_ - 1)     ;    a  ; start of data stack *fixed
   .byte  >(sysVar_ - 1)     ;    b  ; base16 flag
   .byte  >(sysVar_ - 1)     ;    c  ; TIBPtr variable
   .byte  >(sysVar_ - 1)     ;    d  ; vDefs variable
   .byte  >(sysVar_ - 1)     ;    e  ;
   .byte  >(sysVar_ - 1)     ;    f  ; start of return stack *fixed
   .byte  >(sysVar_ - 1)     ;    g  ; next dispatcher
   .byte  >(sysVar_ - 1)     ;    h  ; heap ptr variable
   .byte  >(i_ - 1)          ;    i  ; returns index of current loop
   .byte  >(j_ - 1)          ;    j  ; returns index of outer loop
   .byte  >(sysVar_ - 1)     ;    k
   .byte  >(sysVar_ - 1)     ;    l
   .byte  >(sysVar_ - 1)     ;    m  ( a b -- c ) return the minimum value
   .byte  >(sysVar_ - 1)     ;    n
   .byte  >(sysVar_ - 1)     ;    o
   .byte  >(sysVar_ - 1)     ;    p
   .byte  >(sysVar_ - 1)     ;    q
   .byte  >(sysVar_ - 1)     ;    r  ; return stack pointer
   .byte  >(sysVar_ - 1)     ;    s  ; data stack pointer
   .byte  >(sysVar_ - 1)     ;    t
   .byte  >(sysVar_ - 1)     ;    u
   .byte  >(sysVar_ - 1)     ;    v
   .byte  >(sysVar_ - 1)     ;    w
   .byte  >(sysVar_ - 1)     ;    x
   .byte  >(sysVar_ - 1)     ;    y
   .byte  >(sysVar_ - 1)     ;    z
   .byte  >(group_ - 1)      ;    {
   .byte  >(aNop_ - 1)       ;    |
   .byte  >(endGroup_ - 1)   ;    }
   .byte  >(aNop_ - 1)       ;    ~
   .byte  >(aNop_ - 1)       ;    BS

; *********************************************************************
; Macros must be written in Mint and end with ;
; this code must not span pages
; *********************************************************************
macros:

.include "MINT.macros.asm"

.word $DEAD
