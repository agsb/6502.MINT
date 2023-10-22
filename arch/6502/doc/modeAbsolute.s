    
;------------------------------------------------------------------------------
; using absolute address direct indirect access    
; pros: 
;   offsets inline from a fixed reference
; cons: 
;   can not change inline reference
; multitask and multiuser : 
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

; zero page 
tos:    .word $0
nos:    .word $0

; any page

ips:    .byte $0
ipr:    .byte $0

aps:    .word $0
apr:    .word $0

;------------------------------------------------------------------------------
;   data stack
;------------------------------------------------------------------------------

lose: ; to pull 
    ; ldx isp
    inx
    inx
    stx isp
    rst

keep: ; to push 
    ; ldx isp
    dex
    dex
    stx isp
    rst

pull:
    ldx isp
    lda asp + 0, x
    sta tos + 0
    lda asp + 1, x
    sta tos + 1
    jmp drop

push:
    ldx isp
    lda tos + 0
    sta asp - 2, x
    lda tos + 1
    sta asp - 1, x
    jmp keep
 
push2:
    ldx isp
    lda nos + 0
    sta asp - 4, x
    lda nos + 1
    sta asp - 3, x
    lda tos + 0
    sta asp - 2, x
    lda tos + 1
    sta asp - 1, x
    jsr keep
    jmp keep
 
pull2:
    ldx isp
    lda asp + 0, x
    sta tos + 0
    lda asp + 1, x
    sta tos + 1
    lda asp + 2, x
    sta nos + 0
    lda asp + 3, x
    sta nos + 1
    jsr drop
    jmp drop
   
drop:  
    ldx isp 
    jmp lose

dup:
    ldx isp
    lda asp + 0, x
    sta asp - 2 
    lda asp + 1, x
    sta asp - 1
    jmp keep

over:
    ldx isp
    lda asp + 2, x
    sta asp - 2 
    lda asp + 3, x
    sta asp - 1
    jmp keep

swap:
    ldx isp
    lda asp + 0, x
    sta asp - 2 
    lda asp + 1, x
    sta asp - 1
    lda asp + 2, x
    sta asp + 0 
    lda asp + 3, x
    sta asp + 1
    lda asp - 2, x
    sta asp + 2 
    lda asp - 1, x
    sta asp + 3
    rts

rot:
    ldx isp
    lda asp + 4, x
    sta asp - 2 
    lda asp + 5, x
    sta asp - 1
    lda asp + 2, x
    sta asp + 4 
    lda asp + 3, x
    sta asp + 5
    lda asp + 0, x
    sta asp + 2 
    lda asp + 1, x
    sta asp + 3
    lda asp - 2, x
    sta asp + 0 
    lda asp - 1, x
    sta asp + 1
    rts

and:
    ldx isp
    lda asp + 0, x
    and asp + 2, x
    sta asp + 2, x 
    lda asp + 1, x 
    and asp + 3, x
    sta asp + 3, x
    jmp drop

or:
    ldx isp
    lda asp + 0, x
    ora asp + 2, x
    sta asp + 2, x
    lda asp + 1, x
    ora asp + 3, x
    sta asp + 3, x
    jmp drop

xor:
    ldx isp
    lda asp + 0, x
    eor asp + 2, x
    sta asp + 2, x
    lda asp + 1, x
    eor asp + 3, x
    sta asp + 3, x
    jmp drop

cpt:
    ldx isp
    sec
    tya
    sbc asp + 0, x
    sta asp + 0, x
    sec
    tya
    sbc asp + 1, x
    sta asp + 1, x
    rts

neg:
    lda #$00
    tay
    jmp cpt

inv: 
    lda #$FF
    tay
    jmp cpt

sub:
    ldx isp
    sec
    lda asp + 2, x
    sbc asp + 0, x
    sta asp + 2, x
    lda asp + 3, x
    sbc asp + 1, x
    sta asp + 3, x
    jmp drop

add:
    ldx isp
    clc
    lda asp + 2, x
    adc asp + 0, x
    sta asp + 2, x
    lda asp + 3, x
    adc asp + 1, x
    sta asp + 3, x
    jmp drop

cmp:
    ldx isp
    sec
    lda asp + 2, x
    sbc asp + 0, x
    lda asp + 3, x
    sbc asp + 1, x
    rts

eq:
    jsr cmp
    beq true2
    bne false2

lt:
    jsr cmp
    bmi true2
    bpl false2

gt:
    jsr cmp
    bmi false2
    beq false2
    bpl true2

same2:
    sta asp + 2, x
    sta asp + 3, x
    jmp drop

false2:
    ldx isp
    lda FALSE
    beq same2

true2:
    ldx isp
    lda FALSE - 1
    bne same2

shl:
    ldx isp
    als asp + 0, x
    rol asp + 1, x
    rts

shr:
    ldx isp
    lsr asp + 0, x
    ror asp + 1, x
    rts

cto:
    jsr take2
    ldy #0
    lda nos + 0
    sta (tos), y
    rts

to:
    jsr cto
    iny
    lda nos + 1
    sta (tos), y
    rts

cat:
    ldx isp
    lda asp + 0, x
    sta tos + 0
    lda asp + 1, x
    sta tos + 1
    ldy #0
    lda (tos), y
    sta asp + 0, x
    rts

at:
    jsr cat
    iny 
    lda (tos), y
    sta asp + 1, x
    rts

goto:
    jsr pull
    jmp (tos)

exec:
    pla
    sta tos + 0
    pla
    sta tos + 1
    jmp (tos)

;------------------------------------------------------------------------------
;   return stack
;------------------------------------------------------------------------------

rlose: ; to pull 
    ; ldx isp
    inx
    inx
    stx isp
    rst


rkeep: ; to push 
    ; ldx isp
    dex
    dex
    stx irp
    rst

rpull:
    ldx irp
    lda arp + 0, x
    sta tos + 0
    lda arp + 1, x
    sta tos + 1
    jmp rlose

rpush:
    ldx irp
    lda tos + 0
    sta arp - 2, x
    lda tos + 1
    sta arp - 1, x
    jmp rkeep
 
rshow:
    jsr rpull
    jsr rpush
    jsr spush
    rts

r2s:
    jsr rpull
    jsr spush
    rts

s2r:
    jsr spull
    jsr rpush
    rts

exit:
    jsr rpull
    jmp (tos)

    
