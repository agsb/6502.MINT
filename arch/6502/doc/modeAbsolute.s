    
;------------------------------------------------------------------------------
; using absolute address direct indirect access    
; pros_: 
;   offsets inline from a fixed reference
; cons_: 
;   can not change inline reference
; multitask and multiuser _: 
;   indexed stacks are 128 words, Charles Moore says 22 is enough. 
;   Then could split 5 stacks for users or tasks,
;   more than must exchange stacks values       
;
;
;  low memory
;   -4  LSB
;   -3  MSB
;   -2  LSB
;   -1  MSB
;    0  LSB TOS  
;   +1  MSB
;   +2  LSB NOS
;   +3  MSB
;   +4  LSB WRK
;   +5  MSB
; high memory
;------------------------------------------------------------------------------
;
; to keep code safe do not using "fall throught". 
; uses A, Y, X caller must saves.
; needs 2 levels of hardware stack
; uses 4 bytes in page zero as temporary
; uses 4 bytes in memory for internal use
;

.segment "ZERO"

; zero page 
tos:    .word $0
nos:    .word $0

.segment "CODE"

; any page

isp:    .byte $0
irp:    .byte $0

aps:    .word $0
apr:    .word $0

FALSE = 0

;------------------------------------------------------------------------------
;   data stack
;------------------------------------------------------------------------------

lose_: ; to pull 
    ; ldx isp
    inx
    inx
    stx isp
    rts

keep_: ; to push 
    ; ldx isp
    dex
    dex
    stx isp
    rts

pull_:
    ldx isp
    lda aps + 0, x
    sta tos + 0
    lda aps + 1, x
    sta tos + 1
    jmp drop_

push_:
    ldx isp
    lda tos + 0
    sta aps - 2, x
    lda tos + 1
    sta aps - 1, x
    jmp keep_
 
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
    jsr drop_
    jmp drop_
   
drop_:  
    ldx isp 
    jmp lose_

dup_:
    ldx isp
    lda aps + 0, x
    sta aps - 2 
    lda aps + 1, x
    sta aps - 1
    jmp keep_

over_:
    ldx isp
    lda aps + 2, x
    sta aps - 2 
    lda aps + 3, x
    sta aps - 1
    jmp keep_

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
    rts

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
    rts

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
    rts

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
    sta aps + 2, x
    sta aps + 3, x
    jmp drop_

false2_:
    ldx isp
    lda #(FALSE)
    beq same2_

true2_:
    ldx isp
    lda #(!FALSE)
    bne same2_

shl_:
    ldx isp
    asl aps + 0, x
    rol aps + 1, x
    rts

shr_:
    ldx isp
    lsr aps + 0, x
    ror aps + 1, x
    rts

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

goto_:
    jsr pull_
    jmp (tos)

exec_:
    pla
    sta tos + 0
    pla
    sta tos + 1
    jmp (tos)

;------------------------------------------------------------------------------
;   return stack
;------------------------------------------------------------------------------

rlose_: ; to pull 
    ; ldx isp
    inx
    inx
    stx isp
    rts

rkeep_: ; to push 
    ; ldx isp
    dex
    dex
    stx irp
    rts

rpull_:
    ldx irp
    lda apr + 0, x
    sta tos + 0
    lda apr + 1, x
    sta tos + 1
    jmp rlose_

rpush_:
    ldx irp
    lda tos + 0
    sta apr - 2, x
    lda tos + 1
    sta apr - 1, x
    jmp rkeep_
 
rshow_:
    jsr rpull_
    jsr rpush_
    jsr push_
    rts

r2s_:
    jsr rpull_
    jsr push_
    rts

s2r_:
    jsr pull_
    jsr rpush_
    rts

exit_:
    jsr rpull_
    jmp (tos)

