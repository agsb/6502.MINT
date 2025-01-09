
.ifndef EMULATOR

;----------------------------------------------------------------------
;    depends on hardware, ACIA 6551 common
;----------------------------------------------------------------------
        CIA       =  $E000   ; The base address of the 6551 ACIA.
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

