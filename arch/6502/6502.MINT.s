; vim: filetype=asm sw=8 ts=8 autoindent expandtab shiftwidth=8 et:

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
;  original for the 6502, by Alvaro G. S. Barcellos, 2023
;
;  agsb@ see the disclaimer file in this repo for more information.
;
;  agsb@ star(tm) date 10/10/2023
;
; *********************************************************************

; using data stack in page zero
; using return stack in page one
; rom usable code, no relocable
; code depends on host memory maps
; using SP for index on return stack
; usinf X for index on data stack
;
; stacks grows backwards, push decrements, pull increments
;
; this MINT is to be called after boot.
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

        TRUE  = 1
        FALSE = 0

; useful ascii

        CAN = 24        ; ascii cancel
        CR  = 13        ; ascii carriage return
        LF  = 10        ; ascii line feed
        BS  = 9         ; ascii backspace
        ETX = 3         ; ascii end of text
        NUL = 0         ; ascii null
        BKX = 92        ; ascii back slash


        ; stack LIMIT 26 words, backwards
        STKENDS = 256 - 52

        ; size page
        PAGE = 256

        ; group size, 32 x 16-bit words
        GRPSIZE = 64

        ; groups for defs, could be more
        NUMGRPS = 5

;----------------------------------------------------------------------

        ; define emulator mode for basic IO

        EMULATOR = 1

;----------------------------------------------------------------------
; for easy
;       1. page zero used for internal variable and data stack
;               (for lesser and faster code)
;       2. page one  used for system stack
;               (could be at page zero also)
;       3. page two  used as terminal input buffer
;               ( classic TIB )
;       4. page three mint data
;       5. page four  mint code
;       
;----------------------------------------------------------------------
.segment "ZERO"

; offsets

; mint internals, depends on host

; 52 bytes stack and 12 bytes variables
* = $00C0

; instruction pointer
ins_ptr:    .addr $0
; copycat nest counter
nest: 	.byte $0
; safe for x 
xpf:    .byte $0

; pseudo registers
tos:    .word $0
nos:    .word $0
wrk:    .word $0
tmp:    .word $0

; data stack
stk:    .res 52, $0

stk_end:
        .word $0
;----------------------------------------------------------------------

; for easy, Chuck does with 22 deep.

; bottom of data stack, reserve 26 words, depends on system host
S0      = $00FF

; bottom of return stack, reserves 26 words, depends on system host
R0      = $01FF

;----------------------------------------------------------------------
;
; these does not belongs to MINT
;
;.segment "VECTORS"
;
;.word init
;.word init
;.word init
;
;----------------------------------------------------------------------
.segment "CODE"

; this is page two $200
; terminal input buffer
tib:
        .res PAGE, $0

; this is page three $300
; mint variables, 26 plus 6 from z
vsys:
        .res GRPSIZE, $0

; user variable, 26 plus 6 from z
vars:
        .res GRPSIZE, $0

; internals

vEdited:
        .byte $0

vByteMode:
        .byte $0

;----------------------------------------------------------------------
; aliases

vS0      =  vsys + $00     ;    \a  ; start of data stack
vBase16  =  vsys + $02     ;    \b  ; base16 flag
vTIBPtr  =  vsys + $04     ;    \c  ; TIBPtr variable
vDefs    =  vsys + $08     ;    \d  ; reference for group user functions
;        =  vsys + $0a     ;    \e  ;
vR0      =  vsys + $0c     ;    \f  ; start of return stack
vNext    =  vsys + $0e     ;    \g  ; next routine dispatcher
vHeap    =  vsys + $10     ;    \h  ; heap ptr variable
vIx      =  vsys + $12     ;    \i  ; inner loop counter
vJx      =  vsys + $14     ;    \j  ; inner loop counter

endofit:
        .word $DE, $AD

;----------------------------------------------------------------------

* = $400

init:

;       normal boot
        sei
        cld
        ldx #$FF
        txs
        cli

;       normal init

        jmp main_
        .asciiz "MINT@6502"

;----------------------------------------------------------------------
.ifdef EMULATOR
;
; 25/10/2023, using lib6502, -M E000 -X 0000
;

hitch:

getch:
        lda $E000

        ; test EOF
        cmp #$FF
        beq byes

putch:
        sta $E000
        rts

byes:
        ; exit of emulator
        jmp $0000


; ---------------------------------------------------------------------
; keyq does not work with emulator, using default key_
keyq_:

key_:
        jsr getch
        dex
        dex
        sta 0, x
        jmp (vNext)

emit_:
        lda 0, x
        inx 
        inx
        jsr putch
        jmp (vNext)

.endif

; ---------------------------------------------------------------------
; Forth like functions

; ---------------------------------------------------------------------
;   data stack stuff

spush:
push_:
        dex
        dex
        lda tos + 1
        sta 1, x
        lda tos + 0
        sta 0, x
        rts

spull:
pull_:
        lda 0, x
        sta tos + 0
        lda 1, x
        sta tos + 1
        inx
        inx
        rts

pull2_:
        jsr pull_
        lda 0, x
        sta nos + 0
        lda 1, x
        sta nos + 1
        inx
        inx
        rts

; DROP
drop_:
        inx
        inx
        jmp (vNext)
; DUP
dup_:
        dex
        dex
        lda 2, x
        sta 0, x
        lda 3, x
        sta 1, x
        jmp (vNext)

; OVER
over_:
        dex 
        dex
        lda 4, x
        sta 0, x
        lda 5, x
        sta 1, x
        jmp (vNext)


; SWAP
swap_:
        lda 0, x
        pha
        lda 1, x
        pha
        lda 2, x
        sta 0, x
        lda 3, x
        sta 1, x
        pla
        sta 3, x
        pla
        sta 2, x
        jmp (vNext)

; ROT
rot_:
        lda 4, x
        pha
        lda 5, x
        pha
        lda 2, x
        sta 4, x
        lda 3, x
        sta 5, x
        lda 0, x
        sta 2, x
        lda 1, x
        sta 3, x
        pla
        sta 1, x
        pla
        sta 0, x
        jmp (vNext)

; AND
and_:
        lda  0, x
        and  2, x
        sta  2, x
        lda  1, x
        and  3, x
        jmp nsta3_

; OR
or_:
        lda  0, x
        ora  2, x
        sta  2, x
        lda  1, x
        ora  3, x
        jmp nsta3_

; XOR
xor_:
        lda  0, x
        eor  2, x
        sta  2, x
        lda  1, x
        eor  3, x
        jmp nsta3_

; ADD
add_:
        clc
        lda  2, x
        adc  0, x
        sta  2, x
        lda  3, x
        adc  1, x
        jmp nsta3_

; SUB
sub_:
        sec
        lda  2, x
        sbc  0, x
        sta  2, x
        lda  3, x
        sbc  1, x

nsta3_:
        sta  3, x
        jmp drop_

; NEGATE
neg_:
        lda #$00
        beq cpt_

; INVERT
inv_:
        lda #$FF
cpt_:
        sec
        pha
        sbc  0, x
        sta  0, x
        sec
        pla
        sbc  1, x
        sta  1, x
        jmp (vNext)

; COMPARES
cmp_:
        sec
        lda  2, x
        sbc  0, x
        lda  3, x
        sbc  1, x
        rts

; EQ
eq_:
        jsr cmp_
        beq true2_
        bne false2_

; LESS THAN
lt_:
        jsr cmp_
        bmi true2_
        bpl false2_

; GREATER THAN
gt_:
        jsr cmp_
        bpl true2_
        bmi false2_
        beq false2_

; SAMES
same2_:
        sta  2, x
        sta  3, x
        jmp drop_

; FALSE
false2_:
        lda #(FALSE)
        beq same2_

; TRUE
true2_:
        lda #(!FALSE)
        bne same2_

; SHIFT LEFT
shl_:
        asl  0, x
        rol  1, x
        jmp (vNext)

; SHIFT RIGHT
shr_:
        lsr  0, x
        ror  1, x
        jmp (vNext)

; store byte
cto_:
        jsr pull2_
        ldy #0
        lda nos + 0
        sta (tos), y
        rts

; store word
to_:
        jsr cto_
        iny
        lda nos + 1
        sta (tos), y
        rts

; byte store
cstore_:
        jsr cto_
        ; rts
        jmp (vNext)

; word store
store_:
        jsr to_
        ; rts
        jmp (vNext)

; fetch byte
cat_:
        lda  0, x
        sta tos + 0
        lda  1, x
        sta tos + 1
        ldy #0
        lda (tos), y
        sta  0, x
        rts

; fetch word
at_:
        jsr cat_
        iny
        lda (tos), y
        sta  1, x
        rts

; fetch byte
cfetch_:
        jsr cat_
        jmp (vNext)

; fetch word
fetch_:
        jsr at_
        jmp (vNext)

; increase a word at TOS
incr_:
        inc  0, x
        bne @ends
        inc  1, x
@ends:
        jmp (vNext)

; decrease a word at TOS
decr_:
        lda  0, x
        bne @ends
        dec  1, x
@ends:
        dec  0, x
        jmp (vNext)

; absolute jump to code
; on 6502 use rti, the rts increase the return address
goto_:
        lda  1, x
        pha
        lda  0, x
        pha
        php
        inx
        inx
        rti

; +! ADD STORE
addto_:
        jsr pull2_
        ldy #0
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

; -! SUB STORE
subto_:
        jsr pull2_
        ldy #0
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

;----------------------------------------------------------------------
;   return stack stuff

; usual >R
rpush_:
        lda 0, x
        pha
        lda 1, x
        pha
        inx 
        inx
        jmp (vNext)

; usual R>
rpull_:
        dex
        dex
putw_:
        pla
        sta 1, x
        pla
        sta 0, x
        jmp (vNext)

;----------------------------------------------------------------------

; usual R@
rshow_:
        dex
        dex
        stx xpf
        tsx
        lda 1, x
        tay
        lda 0, x
        ldx xpf
        sta 0, x
        tya 
        sta 1, x
        ; rts
        jmp (vNext)

; SP@
dat2t_:
        txa
        pha
        lda #0
        pha
        beq rpull_
; RP@
ret2t_:
        stx xpf
        tsx
        txa
        ldx xpf
        pha
        lda #1
        pha
        bne rpull_

; SP!
t2dat_:
        lda S0
        tax
        jmp (vNext)

; RP!
t2ret_:
        lda R0
        stx xpf
        tax
        tsx
        ldx xpf
        jmp (vNext)

;----------------------------------------------------------------------
; need review
;----------------------------------------------------------------------
; prepare for mult or divd
opin:
        ; pseudo tos
        lda  0, x
        sta wrk + 0
        lda  1, x
        sta wrk + 1
        ; pseudo nos
        lda  2, x
        sta tmp + 0
        lda  3, x
        sta tmp + 1
        ; clear results
        lda #0
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
        lda nos + 0
        sta  0, x
        lda nos + 1
        sta  1, x
        lda tos + 0
        sta  2, x
        lda tos + 1
        sta  3, x
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

;----------------------------------------------------------------------
;
; some extras
;
; vide eorBookV1.0.1

; set overflow bit
setov_:
        bit @ends
@ends:
        rts

; where I am
here_:
        jsr @pops
@pops:
        pla
        tay
        pla
        tax
        rts

; Z flag is zero in NMOS6502
nmos_:
        sed
        clc
        lda #$99
        adc #$01
        cld
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
inc2tos:
        inc tos + 0
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
        sty vTIBPtr ; maybe spush 
        tay
        lda ctlcodeslo, y
        sta tos + 0
        lda ctlcodeshi, y
        sta tos + 1
        jsr spush
        ;
        jsr enter
        .asciiz "\\G"
        ldy vTIBPtr ; maybe spull
        jmp interpret2

;----------------------------------------------------------------------
interpret:
        jsr enter
        .asciiz "\\N`> `"
        ; fall thru

; used by TESTs
interpret1:
        lda #0
        tay

interpret2:
        sty vTIBPtr 
        lda #NUL
        sta nest
        tay
        beq @isnest

; calc nesting (a macro might have changed it)
@loop:
        lda tib, y
        beq waitchar
        jsr nesting            ; update nesting value
        iny
@isnest:
        cpy vTIBPtr
        bne @loop
        ; fall thru

;----------------------------------------------------------------------
; loop around waiting for character
; get a line into tib
waitchar:
        jsr tib2tos
        jsr spush
        ; fall thru

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

        jsr getch

        ; ge space ?
        cmp #32
        bcs @ischar
        ; is it ia NUL ?
        cmp #$0
        beq @endstr
        ; windows CRLF, linux CR, Mac LF
        cmp #CR                 ; carriage return ?
        beq @iscrlf
        cmp #LF                 ; line feed ?
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
        jsr putch
        ; store
        sta (tos), y
        iny
        rts

;----------------------------------------------------------------------
; nesting deep
nesting:
        cmp #'`'
        bne @nests
        ; toggle bit 7
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

        jsr inc2tos

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
        ; fall thru

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
        jsr putch
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
        bcs @loop
        ; restore
        jsr addn2t
        tya
        jmp putch

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
        jmp putch

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
        ; fall thru
;----------------------------------------------------------------------
pends:
        jsr spush
        ; next
        jmp (vNext)

;----------------------------------------------------------------------
; ascii code
charCode_:
        jsr seekps
        ; fall thru

bytos:
        sta tos + 0
        lda #NUL
        sta tos + 1
        beq pends

;----------------------------------------------------------------------
; convert a decimal value to binary
dec_:
        jsr nul2tos
@loop:
        jsr seekps
        jsr isdec
        bcs pends
@uval:
        jsr add2tos
        jsr mul10
        clc
        bcc @loop

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
        bcs pends
@uval:
        jsr add2tos
        jsr mul16
        clc
        bcc @loop

;----------------------------------------------------------------------
depth_:
        ; stacks moves backwards
        sec
        txa
        lsr ; words
        jmp bytos

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
; skip to eol, crlf 
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
        jsr putch
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
        jmp cfetch_

;----------------------------------------------------------------------
; 6502 is memory mapped IO, just write
outPort_:
        jmp cstore_

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
        ; fall thru

;----------------------------------------------------------------------
; Execute next opcode
next:
opt_:
        jsr seekps
        tay
        lda optcodeslo, y
        sta tos + 0
        lda optcodeshi, y
        sta tos + 1
        jmp (tos)

;----------------------------------------------------------------------
; Execute next alt opcode
alt_:
        jsr seekps
        tay
        lda altcodeslo, y
        sta tos + 0
        lda altcodeshi, y
        sta tos + 1
        jmp (tos)

;----------------------------------------------------------------------
; Parse inline mint, must be asciiz
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
; char 0, Continue from enter, past inline mint
exit_:
        jmp (ins_ptr)

;----------------------------------------------------------------------
; Execute code from data stack
;
exec_:
        jsr spull
        jmp (tos)

;----------------------------------------------------------------------
; Interpret code from data stack
go_:
        lda ins_ptr + 0
        pha
        lda ins_ptr + 1
        pha
        ; pull ps from data stack
        lda  0, x
        sta ins_ptr + 0
        lda  1, x
        sta ins_ptr + 1
        inx
        inx
        ; next
        jmp (vNext)

;----------------------------------------------------------------------
ret_:
        pla
        sta ins_ptr + 1
        pla 
        sta ins_ptr + 0
        ; next
        jmp (vNext)

;----------------------------------------------------------------------
; Execute code from a user function
call_:

        tay
        lda ins_ptr + 0
        pha
        lda ins_ptr + 1
        pha
        tya

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
        ; fall thru

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
        jmp putch

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
        lda vDefs + 0
        pha
        lda vDefs + 1
        pha
        ;-----------------------
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
        pla
        sta vDefs + 0
        pla
        sta vDefs + 1
        ; next
        jmp (vNext)

;----------------------------------------------------------------------
getRef_:
        jsr seekps
        jsr lookupDefs
        jmp fetch_

;----------------------------------------------------------------------
arrDef_:
        lda #FALSE
        beq arrDefs

;----------------------------------------------------------------------
cArrDef_:
        lda #TRUE
        ; fall thru

;----------------------------------------------------------------------
arrDefs:
        ; save array mode
        sta vByteMode

        ; save array start
        lda vHeap + 0
        pha
        lda vHeap + 1
        pha

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
        dex
        dex
        pha
        sta 0, x
        pha
        sta 1, x

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
        jsr seekps
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

;----------------------------------------------------------------------
break_:
        jsr spull
        lda tos + 0
        beq iseq
skframe:
        ; skip a frame 3 words
        pla
        pla
        pla
        pla
        pla
        pla
        ; skip while nest
skipnest:
        lda #$01
        sta nest
@loop:
        jsr seekps
        jsr nesting
        lda nest
        bne @loop
iseq:
        ; parse frame
        jmp (vNext)

;----------------------------------------------------------------------
; Left parentesis ( begins a loop
begin_:

        ; tos is zero ?
        jsr spull
        lda tos + 0
        beq skipnest

        ; do a frame
        ; counter
        lda #NUL
        pha
        pha
        ; limit
        lda tos + 0
        pha
        lda tos + 1
        pha
        ; pointer
        lda ins_ptr + 0
        pha
        lda ins_ptr + 1
        pha

        ; next
        jmp (vNext)

;----------------------------------------------------------------------
; Right parentesis ) again a loop
again_:
        ; check if IFTEMode $FFFF
        stx xpf
        tsx
        lda 0, x
        and 1, x
        cmp #$FF
        bne @again1

        ; push FALSE
        ldx xpf
        lda #0
        dex
        dex
        sta 0, x
        sta 1, x

        ; drop IFTEMmode
        pla
        pla

        ; next
        jmp (vNext)

@again1:
        ; test end
        lda 2, x
        cmp 0, x
        bne @noend
        lda 3, x
        cmp 1, x
        bne @noend
        
        ; end of loop
        ldx xpf
        pla
        pla
        pla
        pla
        pla
        pla
        ; next
        jmp (vNext)

@noend:
        ; increase counter
        inc 0, x
        bne @novr
        inc 1, x
@novr:

        ; return at begin
        lda 4, x
        sta ins_ptr + 0
        lda 5, x
        sta ins_ptr + 1

        ; next
        ldx xpf
        jmp (vNext)

; ZZZZZ
;----------------------------------------------------------------------
; do not update indx
j_:
        stx xpf
        tsx
        dex
        dex
        dex
        dex
        dex
        dex
        txa
        ldx xpf
        jmp index

;----------------------------------------------------------------------
; do not update indx
i_:
        stx xpf
        tsx
        txa
        ldx xpf
        ; fall through

;----------------------------------------------------------------------
index:
        pla
        sta tos + 0
        pla
        sta tos + 1
        
        lda tos + 0
        pha
        lda tos + 1
        pha

        jmp pends

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
        pha
        pha
        jmp (vNext)

;----------------------------------------------------------------------
; verify stack
etx_:
        txa
        cmp #STKENDS    ; bytes
        bmi @ends
        lda #$FF          ; leave one word as offset
        tax
@ends:
        jmp interpret

;----------------------------------------------------------------------
iSysVars:
        .word  vS0              ; a vS0
        .word  FALSE            ; b vBase16
        .word  tib              ; c vTIBPtr
        .word  defs             ; d vDEFS
        .word  FALSE            ; e vEdited
        .word  vR0              ; f vR0
        .word  next             ; g dispatcher
        .word  heap             ; h vHeap
fSysVars:

dysys = fSysVars - iSysVars

;----------------------------------------------------------------------

main_:

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
        lda #<empty_
        sta (tos), y
        iny
        lda #>empty_
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

; safe
        lda #<next
        sta vNext + 0
        lda #>next
        sta vNext + 1

mint_:
; prompt
        jsr printStr
        .asciiz "MINT 6502 V1.0\r\n"

        ; auto reset
        jsr interpret

        jmp mint_

;----------------------------------------------------------------------
;optcodes: parsed by opt_ (next)
;altcodes: parsed by alt_
;ctlcodes: maybe in a future...

; *********************************************************************
; Jump Tables, optmized for single index
; *********************************************************************

.align $100

; using pla, pla, rts, references must be one less
;----------------------------------------------------------------------
optcodeslo:
   .byte  <(exit_)    ;   NUL
   .byte  <(nop_)     ;   SOH
   .byte  <(nop_)     ;   STX
   .byte  <(etx_)     ;   ETX
   .byte  <(nop_)     ;   EOT
   .byte  <(nop_)     ;   ENQ
   .byte  <(nop_)     ;   ACK
   .byte  <(nop_)     ;   BEL
   .byte  <(nop_)     ;   BS
   .byte  <(nop_)     ;   TAB
   .byte  <(nop_)     ;   LF
   .byte  <(nop_)     ;   VT
   .byte  <(nop_)     ;   FF
   .byte  <(nop_)     ;   CR
   .byte  <(nop_)     ;   SO
   .byte  <(nop_)     ;   SI
   .byte  <(nop_)     ;   DLE
   .byte  <(nop_)     ;   DC1
   .byte  <(nop_)     ;   DC2
   .byte  <(nop_)     ;   DC3
   .byte  <(nop_)     ;   DC4
   .byte  <(nop_)     ;   NAK
   .byte  <(nop_)     ;   SYN
   .byte  <(nop_)     ;   ETB
   .byte  <(nop_)     ;   CAN
   .byte  <(nop_)     ;   EM
   .byte  <(nop_)     ;   SUB
   .byte  <(nop_)     ;   ESC
   .byte  <(nop_)     ;   FS
   .byte  <(nop_)     ;   GS
   .byte  <(nop_)     ;   RS
   .byte  <(nop_)     ;   US
   .byte  <(nop_)     ;   SP
   .byte  <(store_)   ;   !
   .byte  <(dup_)     ;   "
   .byte  <(hex_)     ;    #
   .byte  <(swap_)    ;    $
   .byte  <(over_)    ;    %
   .byte  <(and_)     ;    &
   .byte  <(drop_)    ;    '
   .byte  <(begin_)   ;    (
   .byte  <(again_)   ;    )
   .byte  <(mul_)     ;    * multiply 16x16
   .byte  <(add_)     ;    +
   .byte  <(hdot_)    ;    ,
   .byte  <(sub_)     ;    -
   .byte  <(dot_)     ;    .
   .byte  <(div_)     ;    / divide 16x16
   .byte  <(dec_)     ;    0
   .byte  <(dec_)     ;    1
   .byte  <(dec_)     ;    2
   .byte  <(dec_)     ;    3
   .byte  <(dec_)     ;    4
   .byte  <(dec_)     ;    5
   .byte  <(dec_)     ;    6
   .byte  <(dec_)     ;    7
   .byte  <(dec_)     ;    8
   .byte  <(dec_)     ;    9
   .byte  <(def_)     ;    :
   .byte  <(ret_)     ;    ;
   .byte  <(lt_)      ;    <
   .byte  <(eq_)      ;    =
   .byte  <(gt_)      ;    >
   .byte  <(getRef_)  ;    ?
   .byte  <(fetch_)   ;    @
   .byte  <(call_)    ;    A
   .byte  <(call_)    ;    B
   .byte  <(call_)    ;    C
   .byte  <(call_)    ;    D
   .byte  <(call_)    ;    E
   .byte  <(call_)    ;    F
   .byte  <(call_)    ;    G
   .byte  <(call_)    ;    H
   .byte  <(call_)    ;    I
   .byte  <(call_)    ;    J
   .byte  <(call_)    ;    K
   .byte  <(call_)    ;    L
   .byte  <(call_)    ;    M
   .byte  <(call_)    ;    N
   .byte  <(call_)    ;    O
   .byte  <(call_)    ;    P
   .byte  <(call_)    ;    Q
   .byte  <(call_)    ;    R
   .byte  <(call_)    ;    S
   .byte  <(call_)    ;    T
   .byte  <(call_)    ;    U
   .byte  <(call_)    ;    V
   .byte  <(call_)    ;    W
   .byte  <(call_)    ;    X
   .byte  <(call_)    ;    Y
   .byte  <(call_)    ;    Z
   .byte  <(arrDef_)  ;    [
   .byte  <(alt_)     ;    \
   .byte  <(arrEnd_)  ;    ]
   .byte  <(xor_)     ;    ^
   .byte  <(neg_)     ;    _
   .byte  <(str_)     ;    `
   .byte  <(var_)     ;    a
   .byte  <(var_)     ;    b
   .byte  <(var_)     ;    c
   .byte  <(var_)     ;    d
   .byte  <(var_)     ;    e
   .byte  <(var_)     ;    f
   .byte  <(var_)     ;    g
   .byte  <(var_)     ;    h
   .byte  <(var_)     ;    i
   .byte  <(var_)     ;    j
   .byte  <(var_)     ;    k
   .byte  <(var_)     ;    l
   .byte  <(var_)     ;    m
   .byte  <(var_)     ;    n
   .byte  <(var_)     ;    o
   .byte  <(var_)     ;    p
   .byte  <(var_)     ;    q
   .byte  <(var_)     ;    r
   .byte  <(var_)     ;    s
   .byte  <(var_)     ;    t
   .byte  <(var_)     ;    u
   .byte  <(var_)     ;    v
   .byte  <(var_)     ;    w
   .byte  <(var_)     ;    x
   .byte  <(var_)     ;    y
   .byte  <(var_)     ;    z
   .byte  <(shl_)     ;    {
   .byte  <(or_)      ;    |
   .byte  <(shr_)     ;    }
   .byte  <(inv_)     ;    ~
   .byte  <(nop_)     ;    backspace

optcodeshi:
   .byte  >(exit_)    ;   NUL
   .byte  >(nop_)     ;   SOH
   .byte  >(nop_)     ;   STX
   .byte  >(etx_)     ;   ETX
   .byte  >(nop_)     ;   EOT
   .byte  >(nop_)     ;   ENQ
   .byte  >(nop_)     ;   ACK
   .byte  >(nop_)     ;   BEL
   .byte  >(nop_)     ;   BS
   .byte  >(nop_)     ;   TAB
   .byte  >(nop_)     ;   LF
   .byte  >(nop_)     ;   VT
   .byte  >(nop_)     ;   FF
   .byte  >(nop_)     ;   CR
   .byte  >(nop_)     ;   SO
   .byte  >(nop_)     ;   SI
   .byte  >(nop_)     ;   DLE
   .byte  >(nop_)     ;   DC1
   .byte  >(nop_)     ;   DC2
   .byte  >(nop_)     ;   DC3
   .byte  >(nop_)     ;   DC4
   .byte  >(nop_)     ;   NAK
   .byte  >(nop_)     ;   SYN
   .byte  >(nop_)     ;   ETB
   .byte  >(nop_)     ;   CAN
   .byte  >(nop_)     ;   EM
   .byte  >(nop_)     ;   SUB
   .byte  >(nop_)     ;   ESC
   .byte  >(nop_)     ;   FS
   .byte  >(nop_)     ;   GS
   .byte  >(nop_)     ;   RS
   .byte  >(nop_)     ;   US
   .byte  >(nop_)     ;   SP
   .byte  >(store_)   ;   !
   .byte  >(dup_)     ;   "
   .byte  >(hex_)     ;    #
   .byte  >(swap_)    ;    $
   .byte  >(over_)    ;    %
   .byte  >(and_)     ;    &
   .byte  >(drop_)    ;    '
   .byte  >(begin_)   ;    (
   .byte  >(again_)   ;    )
   .byte  >(mul_)     ;    *  multiply 16x16, (multpd multpr -- LSW MSW)
   .byte  >(add_)     ;    +
   .byte  >(hdot_)    ;    ,
   .byte  >(sub_)     ;    -
   .byte  >(dot_)     ;    .
   .byte  >(div_)     ;    /  divide 16x16, (divd divs -- quo rem)
   .byte  >(dec_)     ;    0
   .byte  >(dec_)     ;    1
   .byte  >(dec_)     ;    2
   .byte  >(dec_)     ;    3
   .byte  >(dec_)     ;    4
   .byte  >(dec_)     ;    5
   .byte  >(dec_)     ;    6
   .byte  >(dec_)     ;    7
   .byte  >(dec_)     ;    8
   .byte  >(dec_)     ;    9
   .byte  >(def_)     ;    :
   .byte  >(ret_)     ;    ;
   .byte  >(lt_)      ;    <( - 1)
   .byte  >(eq_)      ;    =
   .byte  >(gt_)      ;    >
   .byte  >(getRef_)  ;    ?
   .byte  >(fetch_)   ;    @
   .byte  >(call_)    ;    A
   .byte  >(call_)    ;    B
   .byte  >(call_)    ;    C
   .byte  >(call_)    ;    D
   .byte  >(call_)    ;    E
   .byte  >(call_)    ;    F
   .byte  >(call_)    ;    G
   .byte  >(call_)    ;    H
   .byte  >(call_)    ;    I
   .byte  >(call_)    ;    J
   .byte  >(call_)    ;    K
   .byte  >(call_)    ;    L
   .byte  >(call_)    ;    M
   .byte  >(call_)    ;    N
   .byte  >(call_)    ;    O
   .byte  >(call_)    ;    P
   .byte  >(call_)    ;    Q
   .byte  >(call_)    ;    R
   .byte  >(call_)    ;    S
   .byte  >(call_)    ;    T
   .byte  >(call_)    ;    U
   .byte  >(call_)    ;    V
   .byte  >(call_)    ;    W
   .byte  >(call_)    ;    X
   .byte  >(call_)    ;    Y
   .byte  >(call_)    ;    Z
   .byte  >(arrDef_)  ;    [
   .byte  >(alt_)     ;    \
   .byte  >(arrEnd_)  ;    ]
   .byte  >(xor_)     ;    ^
   .byte  >(neg_)     ;    _
   .byte  >(str_)     ;    `
   .byte  >(var_)     ;    a
   .byte  >(var_)     ;    b
   .byte  >(var_)     ;    c
   .byte  >(var_)     ;    d
   .byte  >(var_)     ;    e
   .byte  >(var_)     ;    f
   .byte  >(var_)     ;    g
   .byte  >(var_)     ;    h
   .byte  >(var_)     ;    i
   .byte  >(var_)     ;    j
   .byte  >(var_)     ;    k
   .byte  >(var_)     ;    l
   .byte  >(var_)     ;    m
   .byte  >(var_)     ;    n
   .byte  >(var_)     ;    o
   .byte  >(var_)     ;    p
   .byte  >(var_)     ;    q
   .byte  >(var_)     ;    r
   .byte  >(var_)     ;    s
   .byte  >(var_)     ;    t
   .byte  >(var_)     ;    u
   .byte  >(var_)     ;    v
   .byte  >(var_)     ;    w
   .byte  >(var_)     ;    x
   .byte  >(var_)     ;    y
   .byte  >(var_)     ;    z
   .byte  >(shl_)     ;    {
   .byte  >(or_)      ;    |
   .byte  >(shr_)     ;    }
   .byte  >(inv_)     ;    ~
   .byte  >(nop_)     ;    backspace

;----------------------------------------------------------------------
; alternate function codes
ctlcodeslo:
altcodeslo:
   .byte  <(empty_)      ; NUL ^@
   .byte  <(empty_)      ; SOH ^A
   .byte  <(toggleBase_) ; STX ^B
   .byte  <(empty_)      ; ETX ^C
   .byte  <(empty_)      ; EOT ^D
   .byte  <(edit_)       ; ENQ ^E
   .byte  <(empty_)      ; ACK ^F
   .byte  <(empty_)      ; BEL ^G
   .byte  <(backsp_)     ; BS  ^H
   .byte  <(empty_)      ; TAB ^I
   .byte  <(reedit_)     ; LF  ^J
   .byte  <(empty_)      ; VT  ^K
   .byte  <(list_)       ; FF  ^L
   .byte  <(empty_)      ; CR  ^M
   .byte  <(empty_)      ; SO  ^N
   .byte  <(empty_)      ; SI  ^O
   .byte  <(printStack_) ; DLE ^P
   .byte  <(empty_)      ; DC1 ^Q
   .byte  <(empty_)      ; DC2 ^R
   .byte  <(empty_)      ; DC3 ^S
   .byte  <(empty_)      ; DC4 ^T
   .byte  <(empty_)      ; NAK ^U
   .byte  <(empty_)      ; SYN ^V
   .byte  <(empty_)      ; ETB ^W
   .byte  <(empty_)      ; CAN ^X
   .byte  <(empty_)      ; EM  ^Y
   .byte  <(empty_)      ; SUB ^Z
   .byte  <(empty_)      ; ESC ^[
   .byte  <(empty_)      ; FS  ^\
   .byte  <(empty_)      ; GS  ^]
   .byte  <(empty_)      ; RS  ^^
   .byte  <(empty_)      ; US  ^_)
   .byte  <(aNop_)       ; SP  ^`
   .byte  <(cstore_)     ;    !
   .byte  <(aNop_)       ;    "
   .byte  <(aNop_)       ;    #
   .byte  <(aNop_)       ;    $  ( -- adr ) text input ptr
   .byte  <(aNop_)       ;    %
   .byte  <(aNop_)       ;    &
   .byte  <(aNop_)       ;    '
   .byte  <(ifte_)       ;    (  ( b -- )
   .byte  <(aNop_)       ;    )
   .byte  <(aNop_)       ;    *
   .byte  <(incr_)       ;    +  ( adr -- ) increments variable at address
   .byte  <(aNop_)       ;    ,
   .byte  <(decr_)       ;    -  ( adr -- ) decrements variable at address
   .byte  <(aNop_)       ;    .
   .byte  <(aNop_)       ;    /
   .byte  <(aNop_)       ;    0
   .byte  <(aNop_)       ;    1
   .byte  <(aNop_)       ;    2
   .byte  <(aNop_)       ;    3
   .byte  <(aNop_)       ;    4
   .byte  <(aNop_)       ;    5
   .byte  <(aNop_)       ;    6
   .byte  <(aNop_)       ;    7
   .byte  <(aNop_)       ;    8
   .byte  <(aNop_)       ;    9
   .byte  <(aNop_)       ;    :  start defining a macro
   .byte  <(aNop_)       ;    ;
   .byte  <(aNop_)       ;    <
   .byte  <(aNop_)       ;    =
   .byte  <(aNop_)       ;    >( - 1)
   .byte  <(aNop_)       ;    ?
   .byte  <(cfetch_)     ;    @
   .byte  <(aNop_)       ;    A
   .byte  <(break_)      ;    B
   .byte  <(nop_)        ;    C
   .byte  <(depth_)      ;    D  ( -- val ) depth of data stack
   .byte  <(emit_)       ;    E  ( val -- ) emits a char to output
   .byte  <(aNop_)       ;    F
   .byte  <(go_)         ;    G  ( -- ? ) execute mint definition
   .byte  <(keyq_)       ;    H  ( verify if key hit )
   .byte  <(inPort_)     ;    I  ( port -- val )
   .byte  <(aNop_)       ;    J
   .byte  <(key_)        ;    K  ( -- val )  read a char from input
   .byte  <(aNop_)       ;    L
   .byte  <(aNop_)       ;    M
   .byte  <(newln_)      ;    N  ; prints a newline to output
   .byte  <(outPort_)    ;    O  ( val port -- )
   .byte  <(printStk_)   ;    P  ( -- ) non-destructively prints stack
   .byte  <(aNop_)       ;    Q  quits from Mint REPL
   .byte  <(rot_)        ;    R  ( a b c -- b c a )
   .byte  <(aNop_)       ;    S
   .byte  <(aNop_)       ;    T
   .byte  <(r2s_)        ;    U  S( -- w ) R( w -- )
   .byte  <(s2r_)        ;    V  S( w -- ) R( -- w )
   .byte  <(aNop_)       ;    W   ; ( b -- ) if false, skip to end of loop
   .byte  <(exec_)       ;    X
   .byte  <(aNop_)       ;    Y
   .byte  <(editDef_)    ;    Z
   .byte  <(cArrDef_)    ;    [
   .byte  <(comment_)    ;    \  comment text, skip reading until end of line
   .byte  <(aNop_)       ;    ]
   .byte  <(charCode_)   ;    ^
   .byte  <(aNop_)       ;    _
   .byte  <(aNop_)       ;    `
   .byte  <(sysVar_)     ;    a  ; start of data stack *fixed
   .byte  <(sysVar_)     ;    b  ; base16 flag
   .byte  <(sysVar_)     ;    c  ; TIBPtr variable
   .byte  <(sysVar_)     ;    d  ; vDefs variable
   .byte  <(sysVar_)     ;    e  ;
   .byte  <(sysVar_)     ;    f  ; start of return stack *fixed
   .byte  <(sysVar_)     ;    g  ; next dispatcher
   .byte  <(sysVar_)     ;    h  ; heap ptr variable
   .byte  <(i_)          ;    i  ; returns index of current loop
   .byte  <(j_)          ;    j  ; returns index of outer loop
   .byte  <(sysVar_)     ;    k
   .byte  <(sysVar_)     ;    l
   .byte  <(sysVar_)     ;    m  ( a b -- c ) return the minimum value
   .byte  <(sysVar_)     ;    n
   .byte  <(sysVar_)     ;    o
   .byte  <(sysVar_)     ;    p
   .byte  <(sysVar_)     ;    q
   .byte  <(sysVar_)     ;    r  ; return stack pointer
   .byte  <(sysVar_)     ;    s  ; data stack pointer
   .byte  <(sysVar_)     ;    t
   .byte  <(sysVar_)     ;    u
   .byte  <(sysVar_)     ;    v
   .byte  <(sysVar_)     ;    w
   .byte  <(sysVar_)     ;    x
   .byte  <(sysVar_)     ;    y
   .byte  <(sysVar_)     ;    z
   .byte  <(group_)      ;    {
   .byte  <(aNop_)       ;    |
   .byte  <(endGroup_)   ;    }
   .byte  <(aNop_)       ;    ~
   .byte  <(aNop_)       ;    BS

ctlcodeshi:
altcodeshi:
   .byte  >(empty_)      ; NUL ^@
   .byte  >(empty_)      ; SOH ^A
   .byte  >(toggleBase_) ; STX ^B
   .byte  >(empty_)      ; ETX ^C
   .byte  >(empty_)      ; EOT ^D
   .byte  >(edit_)       ; ENQ ^E
   .byte  >(empty_)      ; ACK ^F
   .byte  >(empty_)      ; BEL ^G
   .byte  >(backsp_)     ; BS  ^H
   .byte  >(empty_)      ; TAB ^I
   .byte  >(reedit_)     ; LF  ^J
   .byte  >(empty_)      ; VT  ^K
   .byte  >(list_)       ; FF  ^L
   .byte  >(empty_)      ; CR  ^M
   .byte  >(empty_)      ; SO  ^N
   .byte  >(empty_)      ; SI  ^O
   .byte  >(printStack_) ; DLE ^P
   .byte  >(empty_)      ; DC1 ^Q
   .byte  >(empty_)      ; DC2 ^R
   .byte  >(empty_)      ; DC3 ^S
   .byte  >(empty_)      ; DC4 ^T
   .byte  >(empty_)      ; NAK ^U
   .byte  >(empty_)      ; SYN ^V
   .byte  >(empty_)      ; ETB ^W
   .byte  >(empty_)      ; CAN ^X
   .byte  >(empty_)      ; EM  ^Y
   .byte  >(empty_)      ; SUB ^Z
   .byte  >(empty_)      ; ESC ^[
   .byte  >(empty_)      ; FS  ^\
   .byte  >(empty_)      ; GS  ^]
   .byte  >(empty_)      ; RS  ^^
   .byte  >(empty_)      ; US  ^_)
   .byte  >(aNop_)       ; SP  ^`
   .byte  >(cstore_)     ;    !
   .byte  >(aNop_)       ;    "
   .byte  >(aNop_)       ;    #
   .byte  >(aNop_)       ;    $  ( -- adr ) text input ptr
   .byte  >(aNop_)       ;    %
   .byte  >(aNop_)       ;    &
   .byte  >(aNop_)       ;    '
   .byte  >(ifte_)       ;    (  ( b -- )
   .byte  >(aNop_)       ;    )
   .byte  >(aNop_)       ;    *
   .byte  >(incr_)       ;    +  ( adr -- ) increments variable at address
   .byte  >(aNop_)       ;    ,
   .byte  >(decr_)       ;    -  ( adr -- ) decrements veriable at address
   .byte  >(aNop_)       ;    .
   .byte  >(aNop_)       ;    /
   .byte  >(aNop_)       ;    0
   .byte  >(aNop_)       ;    1
   .byte  >(aNop_)       ;    2
   .byte  >(aNop_)       ;    3
   .byte  >(aNop_)       ;    4
   .byte  >(aNop_)       ;    5
   .byte  >(aNop_)       ;    6
   .byte  >(aNop_)       ;    7
   .byte  >(aNop_)       ;    8
   .byte  >(aNop_)       ;    9
   .byte  >(aNop_)       ;    :  start defining a macro
   .byte  >(aNop_)       ;    ;
   .byte  >(aNop_)       ;    <( - 1)
   .byte  >(aNop_)       ;    =
   .byte  >(aNop_)       ;    >
   .byte  >(aNop_)       ;    ?
   .byte  >(cfetch_)     ;    @
   .byte  >(aNop_)       ;    A
   .byte  >(break_)      ;    B
   .byte  >(nop_)        ;    C
   .byte  >(depth_)      ;    D  ( -- val ) depth of data stack
   .byte  >(emit_)       ;    E  ( val -- ) emits a char to output
   .byte  >(aNop_)       ;    F
   .byte  >(go_)         ;    G  ( -- ? ) execute mint definition
   .byte  >(keyq_)       ;    H  ( verify if key hit )
   .byte  >(inPort_)     ;    I  ( port -- val )
   .byte  >(aNop_)       ;    J
   .byte  >(key_)        ;    K  ( -- val )  read a char from input
   .byte  >(aNop_)       ;    L
   .byte  >(aNop_)       ;    M
   .byte  >(newln_)      ;    N  ; prints a newline to output
   .byte  >(outPort_)    ;    O  ( val port -- )
   .byte  >(printStk_)   ;    P  ( -- ) non-destructively prints stack
   .byte  >(aNop_)       ;    Q  quits from Mint REPL
   .byte  >(rot_)        ;    R  ( a b c -- b c a )
   .byte  >(aNop_)       ;    S
   .byte  >(aNop_)       ;    T
   .byte  >(r2s_)        ;    U  S( -- w ) R( w -- )
   .byte  >(s2r_)        ;    V  S( w -- ) R( -- w )
   .byte  >(aNop_)       ;    W   ; ( b -- ) if false, skip to end of loop
   .byte  >(exec_)       ;    X
   .byte  >(aNop_)       ;    Y
   .byte  >(editDef_)    ;    Z
   .byte  >(cArrDef_)    ;    [
   .byte  >(comment_)    ;    \  comment text, skip reading until end of line
   .byte  >(aNop_)       ;    ]
   .byte  >(charCode_)   ;    ^
   .byte  >(aNop_)       ;    _
   .byte  >(aNop_)       ;    `
   .byte  >(sysVar_)     ;    a  ; start of data stack *fixed
   .byte  >(sysVar_)     ;    b  ; base16 flag
   .byte  >(sysVar_)     ;    c  ; TIBPtr variable
   .byte  >(sysVar_)     ;    d  ; vDefs variable
   .byte  >(sysVar_)     ;    e  ;
   .byte  >(sysVar_)     ;    f  ; start of return stack *fixed
   .byte  >(sysVar_)     ;    g  ; next dispatcher
   .byte  >(sysVar_)     ;    h  ; heap ptr variable
   .byte  >(i_)          ;    i  ; returns index of current loop
   .byte  >(j_)          ;    j  ; returns index of outer loop
   .byte  >(sysVar_)     ;    k
   .byte  >(sysVar_)     ;    l
   .byte  >(sysVar_)     ;    m  ( a b -- c ) return the minimum value
   .byte  >(sysVar_)     ;    n
   .byte  >(sysVar_)     ;    o
   .byte  >(sysVar_)     ;    p
   .byte  >(sysVar_)     ;    q
   .byte  >(sysVar_)     ;    r  ; return stack pointer
   .byte  >(sysVar_)     ;    s  ; data stack pointer
   .byte  >(sysVar_)     ;    t
   .byte  >(sysVar_)     ;    u
   .byte  >(sysVar_)     ;    v
   .byte  >(sysVar_)     ;    w
   .byte  >(sysVar_)     ;    x
   .byte  >(sysVar_)     ;    y
   .byte  >(sysVar_)     ;    z
   .byte  >(group_)      ;    {
   .byte  >(aNop_)       ;    |
   .byte  >(endGroup_)   ;    }
   .byte  >(aNop_)       ;    ~
   .byte  >(aNop_)       ;    BS


; *********************************************************************
; user function groups, each with 26 plus 6 from Z
defs:
        .res NUMGRPS * GRPSIZE, $00

; *********************************************************************
; Macros must be written in Mint and end with ;
; this code must not span pages
; *********************************************************************
macros:

backsp_:
        .asciiz "\\c@0=0=(1_\\c\\+`\\b \\b`);"

reedit_:
        .asciiz "\\e\\@\\Z;"

edit_:
        .asciiz "`?`\\K\\N`> `\\^A-\\Z;"

list_:
        .asciiz "\\N26(\\i@\\Z\\c@0>(\\N))\\N`> `;"

printStack_:
        .asciiz "`=> `\\P\\N\\N`> `;"        

toggleBase_:
        .asciiz "\\b@0=\\b!;"

empty_:
        .asciiz ";"

; heap must be here !
heap:
        .addr $0

