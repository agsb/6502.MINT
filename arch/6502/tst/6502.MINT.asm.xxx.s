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
; *********************************************************************** 
iSysVars: 
        .word dStack               ; a vS0 
        .word FALSE                ; b vBase16 
        .word tib                  ; c vTIBPtr 
        .word defs                 ; d vDEFS 
        .word 0                    ; e vEdited the last command to be edited 
        .word rStack               ; f vR0 **** 
        .word 0                    ; g reserved **** 
        .word HEAP                 ; h vHeapPtr 
 
; ********************************************************************* 
; Initialisation 
; ********************************************************************* 
 
; ROM code 
.segment "ONCE" 
 
; start: 
 
mint: 
    jsr  initialize 
    jsr  printStr 
    .asciiz  "MINT 6502 V1.0\r\n" 
    jmp interpret 
 
initialize: 
 
    ;z    LD HL,iSysVars 
    ;z    LD DE,sysVars 
    ;z    LD BC,8 * 2 
    ;z    LDIR 
 
; defaults in parallel 
    lda #<vars 
    sta tos + 0 
    lda #>vars 
    sta tos + 1 
    lda #<vsys 
    sta nos + 0 
    lda #>vsys 
    sta nos + 1 
    lda #<defs 
    sta wrk + 0 
    lda #>defs 
    sta wrk + 1 
    ldy #$00 
@loop: 
    lda #$00 
    sta (tos), y 
    sta (nos), y 
    lda #<empty_ 
    sta (wrk), y 
    iny 
    lda #$00 
    sta (tos), y 
    sta (nos), y 
    lda #>empty_ 
    sta (wrk), y 
    iny 
    cpy #52 ; 26 words 
    bne @loop 
 
; wise 
    lda #$00 
    tax 
    tay 
    sta ib 
    sta ns 
 
    lda #<next_ 
    sta nxt + 0 
    lda #>next_ 
    sta nxt + 1 
 
; done 
    rts 
 
;---------------------------------------------------------------------- 
; full address table 
; ???? 
macro: 
    asl 
    tay 
    lda (ctlcodes), y 
    sta tos + 0 
    iny 
    lda (ctlcodes), y 
    sta tos + 1 
    jsr rpush_ 
    jsr enter_ 
    .asciiz "\\G" 
    ;    LD BC,(vTIBPtr) 
    jmp interpret2 
 
;---------------------------------------------------------------------- 
interpret: 
    jsr  enter_ 
    .asciiz "\\N`> `" 
 
interpret1:                     ; used by tests 
    lda #$00 
    sta ips + 0 
    sta ips + 1 
    ;    LD (vTIBPtr),BC 
 
interpret2:                     ; calc nesting (a macro might have changed it) 
    dex 
    dex 
    lda ib 
    sta spz + 0, x 
    lda #0 
    sta spz + 1, x 
    sta ns 
    tay 
    clc 
    bcc @cast 
@loop: 
    lda tib, y 
    iny 
    jsr  nesting            ; update nesting value 
@cast: 
    cpy #0 
    bne @loop 
 
    ; ??324?    POP BC                  ; restore offset into TIB 
 
; ******************************************************************************** 
; 
; Dispatch Routine. 
; 
; Get the next character and form a 1 byte jump address 
; 
; This target jump address is loaded into HL, and using jmp (HL) to quickly 
; jump to the selected function. 
; 
; Individual handler routines will deal with each category: 
; 
; 1. Detect characters A-Z and jump to the User Command handler routine 
; 
; 2. Detect characters a-z and jump to the variable handler routine 
; 
; 3. All other characters are punctuation and cause a jump to the associated 
; primitive code. 
; 
; Instruction Pointer IP BC is incremented 
; 
; ********************************************************************************* 
 
 
; ********************************************************************** 
; 
; (not yet) routines are ordered to occupy pages of 256 bytes 
; 
; ********************************************************************** 
 
;---------------------------------------------------------------------- 
; hook for debug 
exec_: 
    jsr spull_ 
    jmp (tos) 
 
;---------------------------------------------------------------------- 
aNop_: 
nop_: 
    jmp next_ 
 
;---------------------------------------------------------------------- 
def_: 
    jmp def2_ 
 
;---------------------------------------------------------------------- 
; define a byte array 
carrDef_:                   
    lda TRUE 
    jmp arrDef 
 
;---------------------------------------------------------------------- 
; define a word array 
arrDef_: 
    lda FALSE 
    jmp arrDef
 
;---------------------------------------------------------------------- 
arrDef: 
    ; wire next 
    lda #<compNEXT 
    sta nxt + 0 
    lda #>compNEXT 
    sta nxt + 1 
 
    sta vByteMode 
 
   ; save start of array \[  \] 
    lda vHeapPtr + 0 
    sta tos + 0 
    lda vHeapPtr + 1 
    sta tos + 1 
    jsr rpush_ 
    jmp next_
 
;---------------------------------------------------------------------- 
; ARRAY compilation routine 
compNEXT: 
    lda vHeapPtr + 0 
    sta wrk + 0 
    lda vHeapPtr + 1 
    sta wrk + 1

    jsr spull_ 
    ldy #$00 
    lda tos + 0 
    sta (wrk), y 
    
    lda vByteMode 
    cmp TRUE 
    beq @iseq 
    iny 
    lda tos + 1 
    sta (wrk), y 

@iseq: 
    lda wrk + 0 
    sta vHeapPtr + 0 
    lda wrk + 1 
    sta vHeapPtr + 1 
    jmp next_ 

;---------------------------------------------------------------------- 
arrEnd_: 
    ; start of array 
    jsr rpull_               
    jsr spush_ 

    lda vheapPtr + 0
    sta nos + 0
    lda vheapPtr + 1
    sta nos + 1

    ; subtract
    sec
    lda nos + 0
    sbc tos + 0
    sta tos + 0
    lda nos + 1
    sbc tos + 1
    sta tos + 1

    lda vByteMode
    cmp TRUE 
    beq @iseq 
    
    ; words
    lsr tos + 1
    ror tos + 0

@iseq: 
    jsr spush_ 
    lda #<next_ 
    sta nxt + 0 
    lda #>next_ 
    sta nxt + 1 
    jmp next_ 

;---------------------------------------------------------------------- 
call_: 
    ; push ips 
    ldy yp 
    dey 
    dey 
    lda ips + 0 
    sta rpz + 0, y 
    lda ips + 1 
    sta rpz + 1, y 
    sty yp 
    ; read ch 
    jsr ldaps_ 
    jsr lookupDef1 
    jmp next_ 
 
;---------------------------------------------------------------------- 
etx_: 
etx: 
        LD HL,-DSTACK 
        ADD HL,SP 
        JR NC,etx1 
        LD SP,DSTACK 
etx1: 
        jmp interpret 
 
 
;---------------------------------------------------------------------- 
exit_: 
        INC BC 
        LD DE,BC 
        jsr  rpull               ; Restore Instruction pointer 
        LD BC,HL 
        EX DE,HL 
        jmp (HL) 
 
;---------------------------------------------------------------------- 
ret_: 
    ldy yp 
    lda rpz + 0, y 
    sta ips + 0 
    lda rpz + 1, y 
    sta ips + 1 
    iny 
    iny 
    sty yp 
    jmp next_ 
 
;---------------------------------------------------------------------- 
getRef_: 
getRef:                         ;= 8 
        INC BC LD A,(BC) 
        jsr  lookupDef 
        jmp fetch1 
 
;---------------------------------------------------------------------- 
xalt:                                ;= 11 
        INC BC LD A,(BC) 
        LD HL,altcodes 
        ADD A,L 
        LD L,A 
        LD L,(HL)                   ; 7t    get low jump address 
        LD H, msb(page6)            ; Load H with the 5th page address 
        jmp (HL)                    ; 4t    Jump to routine 
 
; ************************************************************************** 
; def is used to create a colon definition 
; When a colon is detected, the next character (usually uppercase alpha) 
; is looked up in the vector table to get its associated code field address 
; This CFA is updated to point to the character after uppercase alpha 
; The remainder of the characters are then skipped until after a semicolon 
; is found. 
; *************************************************************************** 
;---------------------------------------------------------------------- 
def:                        ; Create a colon definition 
        INC BC LD  A,(BC)          ; Get the next character 
        INC BC 
        jsr  lookupDef 
        LD DE,(vHeapPtr)    ; start of defintion 
        LD (HL),E           ; Save low byte of address in CFA 
        INC HL 
        LD (HL),D           ; Save high byte of address in CFA+1 
 
def1:                   ; Skip to end of definition 
        LD A,(BC)           ; Get the next character 
        INC BC              ; Point to next character 
        LD (DE),A 
        INC DE 
        CP ";"                  ; Is it a semicolon 
        JR Z, def2           ; end the definition 
        JR  def1            ; get the next element 
 
def2: 
        DEC BC 
 
def3: 
        LD (vHeapPtr),DE        ; bump heap ptr to after definiton 
        jmp next_ 
 
;----------------------------------------------------------------------
; Left parentesis ( begins a loop 
begin_: 
begin: 

    jsr spull_
    lda tos + 0
    or  tos + 1
    beq begin1
 
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
    xor ns
    bne @loop
    jmp next_ 
 
;----------------------------------------------------------------------
; Right parentesis ) again a loop 
again_: 
again: 
    ldy yp
    ; counter
    lda rpz + 0, y
    sta wrk + 0
    lda rpz + 1, y
    sta wrk + 1

    ; check if IFTEMode $FFFF

    lda wkr + 0
    and wrk + 1
    sta ac
    inc ac
    bne again1
    
    ; push FALSE
    lda ac
    sta tos + 0
    sta tos + 1
    jsr spush_

    ; drop IFTEMmode
    lda yp
    clc
    adc #2
    beq @ends
 
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

    ; ends
    lda yp
    clc
    adc #6
    beq @ends

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

@ends: 
    ; drop loop vars
    sta yp
    jmp next_ 
 
;----------------------------------------------------------------------
anop_: 
    jmp next_        ; 8t 
 
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
ifte_: 
    jsr spull_
    lda tos + 0
    ora tos + 1
    bne @noeq
    ; skip to closing ) works with \) too 
    inc tos + 0
    jsr spush_
    jmp begin1                   
@noeq: 
    lda #$FF
    sta tos + 0
    sta tos + 1
    jsr rpush 
    jmp next_ 
 
    jsr exec1 
    jmp next_ 

;---------------------------------------------------------------------- 
; jump nos
exec_: 
exec1: 
    jsr spull_
    jmp (tos)
 
;---------------------------------------------------------------------- 
endGroup_: 
    jsr rpull_

endgrp:    
    
    lda tos + 0
    sta vDEFS + 0
    lda tos + 1
    sta vDEFS + 1
    jmp next_ 
 
group_: 
    ; rpush vDEFS
    lda vDEFS + 0
    sta tos + 0
    lda vDEFS + 1
    sta tos + 1
    jsr rpush_

    ; spull offset
    jsr spull_

    ; value * 64
    lda tos + 0
    sta tos + 1
    eor tos + 0
    sta tos + 0
    
    asl tos + 1
    rol tos + 0
    asl tos + 1
    rol tos + 0
    
    ; add offset
    clc
    lda DEFS + 0
    adc tos + 0
    sta tos + 0
    lda DEFS + 1
    adc tos + 1
    sta tos + 1
    
    jmp endgrp
 
;---------------------------------------------------------------------- 
sysVar_: 
        LD A,(BC) 
        SUB "a" - ((sysVars - mintVars)/2) 
        ADD A,A 
        LD L,A 
        LD H,msb(mintVars) 
        PUSH HL 
        jmp next_                ; Execute code from User def 
 
;---------------------------------------------------------------------- 
i_: 
        PUSH IX 
        jmp next_ 
 
;---------------------------------------------------------------------- 
j_: 
        PUSH IX 
        POP HL 
        LD DE,6 
        ADD HL,DE 
        PUSH HL 
        jmp next_ 
 
;---------------------------------------------------------------------- 
key_: 
    jsr getchar 
        LD L,A 
        LD H,0 
    jsr spush_
    jmp next_ 
 
;---------------------------------------------------------------------- 
break_: 
        POP HL 
        LD A,L                      ; zero? 
        OR H 
        JR NZ,break1 
        jmp next_ 
break1: 
        LD DE,6                     ; drop loop frame 
        ADD IX,DE 
        jmp begin1                   ; skip to end of loop 
 
printStk_: 
        JR printStk 
 
editDef_: 
 
; ************************************************************************** 
; Page 6 primitive routines 
; ************************************************************************** 
        ; falls through 
; ************************************************************************** 
; copy definition to text input buffer 
; update TIBPtr 
; ************************************************************************** 
 
editDef:                    ; lookup up def based on number 
        LD A,"A" 
        POP DE 
        ADD A,E 
        EX AF,AF' 
        LD HL,(vDEFS) 
        ADD HL,DE 
        ADD HL,DE 
        LD E,(HL) 
        INC HL 
        LD D,(HL) 
        EX DE,HL 
        LD A,(HL) 
        CP ";" 
        LD DE,TIB 
        JR Z,editDef3 
        LD A,":" 
        jsr  writeChar 
        EX AF,AF' 
        jsr  writeChar 
        JR editDef2 
editDef1: 
        INC HL 
editDef2: 
        LD A,(HL) 
        jsr  writeChar 
        CP ";" 
        JR NZ,editDef1 
editDef3: 
        LD HL,TIB 
        EX DE,HL 
        OR A 
        SBC HL,DE 
        LD (vTIBPtr),HL 
        jmp next_ 
 
;******************************************************************* 
; Page 5 primitive routines continued 
;******************************************************************* 
 
;******************************************************************* 
; Subroutines 
;******************************************************************* 
 
;---------------------------------------------------------------------- 
lookupDef:                          ;=20 
    sec 
    sbc #'A' 
    sta vEdited 
    jmp lookupDef2 
 
lookupDef1: 
    sec 
    sbc #'A' 
 
lookupDef2: 
    tay 
    lda #<defs 
    sta nos + 0 
    lda #>defs 
    sta nos + 1 
    lda (nos), y 
    sta ips + 0 
    iny 
    lda (nos), y 
    sta ips + 1 
    rts 
 
 
