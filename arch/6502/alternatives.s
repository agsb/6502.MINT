spt: .byte $0
rpt: .byte $0

vsp: .word $0
vrp: .word $0
tos: .word $0
nos: .word $0
wrk: .word $0
tmp: .word $0

rpull: ; 40 cc
    ; jsr           ;6
    ldy rpt         ;3
    lda (vrp), y    ;6
    sta tos + 1     ;3
    iny             ;2
    lda (vrp), y    ;6
    sta tos + 0     ;3
    iny             ;2
    sty rpt         ;3
    rts             ;6
    
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
    


