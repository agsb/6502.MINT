spt: .byte $0
rpt: .byte $0

vsp: .word $0
vrp: .word $0

tos: .word $0
nos: .word $0
wrk: .word $0
tmp: .word $0

rpull: ; 40 cc
    ldy rpt 
    lda (vrp), y 
    sta tos + 1 
    iny 
    lda (vrp), y
    sta tos + 0
    iny 
    sty rpt 
    rts 
 
rpush:
    ldy rpt
    dey
    lda tos + 0
    sta (vrp), y
    dey
    lda tos + 1
    sta (vrp), y
    sty rpt
    rts
 
spull:
    ldy spt
    lda (vsp), y
    sta tos + 1
    iny
    lda (vsp), y
    sta tos + 0
    iny
    sty spt
    rts

spush:
    ldy spt
    dey
    lda tos + 0
    sta (vsp), y
    dey
    lda tos + 1
    sta (vsp), y
    sty spt
    rts
 
 
spush2:
    ldy spt
    dey
    lda tos + 0
    sta (vsp), y
    dey
    lda tos + 1
    sta (vsp), y
    sty spt
    dey
    lda nos + 0
    sta (vsp), y
    dey
    lda nos + 1
    sta (vsp), y
    sty spt
    rts
 
spull2:
    ldy spt
    lda (vsp), y
    sta tos + 1
    iny
    lda (vsp), y
    sta tos + 0
    iny
    lda (vsp), y
    sta nos + 1
    iny
    lda (vsp), y
    sta nos + 0
    iny
    sty spt
    rts
 
drop:
    ldy spt
    iny
    iny
    sty spt
    rts

dup:
    jsr spull
    jsr spush
    jsr spush
    rts

swap:
    jsr spull2
    lda tos + 0
    sta wrk + 0
    lda tos + 1
    sta wrk + 1
    lda nos + 0
    sta tos + 0
    lda nos + 1
    sta tos + 1
    lda wrk + 0
    sta nos + 0
    lda wrk + 1
    sta nos + 1
    jsr spush2

over:
    jsr spull2
    jsr spush2
    lda nos + 0
    sta tos + 0
    lda nos + 1
    sta tos + 1
    jsr spush
    rts

rot:
    jsr spull2
    lda tos + 0
    sta wrk + 0
    lda tos + 1
    sta wrk + 1
    jsr spull
    lda tos + 0
    sta tmp + 0
    lda tos + 1
    sta tmp + 1

    lda nos + 0
    sta tos + 0
    lda nos + 1
    sta tos + 1
    jsr spush
    lda wrk + 0
    sta tos + 0
    lda wrk + 1
    sta tos + 1
    jsr spush
    lda tmp + 0
    sta tos + 0
    lda tmp + 1
    sta tos + 1
    jsr spush
    rts



    
