; *********************************************************************
;
;       MINT Minimal Interpreter 
;
;       GNU GENERAL PUBLIC LICENSE              Version 3, 29 June 2007
;
;       see the LICENSE file in this repo for more information 
;
;		original for the Z80, by Ken Boak, John Hardy and Craig Jones. 
;
;		adapted for the 6502, by Alvaro G. S. Barcellos, 10/2023
;						  (with some code adapted from FIG_Forth)
;
; *********************************************************************


;        DSIZE       = $80
;        RSIZE       = $80

    TIBSIZE     = $100
    TRUE        = 1
    FALSE       = 0

    NUMGRPS     = 5
    GRPSIZE     = $40

    CELL    = 2

;----------------------------------------------------------------------
; page 0, reserved cells

    ; just 128 cells deep and round-robin
    yp = $80    ; hold return stack pointer
    xp = $81    ; hold parameter stack pointer

    wk = $82    ; work register 
    us = $84    ; user register
    lk = $86    ; link register
    ch = $8a    ; char buffer

    opcs = optcodes
    alts = altcodes
    ctrs = ctlcodes

;----------------------------------------------------------------------
;   data stack indexed by x
;   return stack indexed by y
    spz = $600   ; absolute address for data stack
    rpz = $700   ; absolute address for parameter stack

; ***********************************************************************
; Initial values for user mintVars		
; ***********************************************************************		
iSysVars:
        .word dStack               ; a vS0
        .word FALSE                ; b vBase16
        .word 0                    ; c vTIBPtr
        .word DEFS                 ; d vDEFS
        .word 0                    ; e vEdited the last command to be edited
        .word 0                    ; f 
        .word 0                    ; g 
        .word HEAP                 ; h vHeapPtr

; *********************************************************************
; Page 0  Initialisation
; *********************************************************************		

.segment "CODE"

start:

mint:
        ; there is no stack at system stack! LD SP,DSTACK
        jsr  initialize
        jsr  printStr
        .asciiz  "MINT V1.0\r\n"
        jmp interpret

initialize:
        lda #$00
        tax
        tay

        LD HL,iSysVars
        LD DE,sysVars
        LD BC,8 * 2
        LDIR
        
        LD HL,DEFS
        LD B,GRPSIZE/2 * NUMGRPS
init1:
        LD (HL),lsb(empty_)
        INC HL
        LD (HL),msb(empty_)
        INC HL
        DJNZ init1
        RET

macro:                          ;=25
        LD (vTIBPtr),BC
        LD HL,ctlcodes
        ADD A,L
        LD L,A
        LD E,(HL)
        LD D,msb(macros)
        PUSH DE
        jsr  ENTER
        .asciiz  "\\G"
        LD BC,(vTIBPtr)
        JR interpret2

interpret:
        jsr  ENTER
        .asciiz  "\\N`> `"

interpret1:                     ; used by tests
        LD BC,0                 ; load BC with offset into TIB         
        LD (vTIBPtr),BC

interpret2:                     ; calc nesting (a macro might have changed it)
        LD E,0                  ; initilize nesting value
        PUSH BC                 ; save offset into TIB, 
                                ; BC is also the count of chars in TIB
        LD HL,TIB               ; HL is start of TIB
        JR interpret4

interpret3:
        LD A,(HL)               ; A = char in TIB
        INC HL                  ; inc pointer into TIB
        DEC BC                  ; dec count of chars in TIB
        jsr  nesting            ; update nesting value

interpret4:
        LD A,C                  ; is count zero?
        OR B
        JR NZ, interpret3          ; if not loop
        POP BC                  ; restore offset into TIB
; *******************************************************************         
; Wait for a character from the serial input (keyboard) 
; and store it in the text buffer. Keep accepting characters,
; increasing the instruction pointer BC - until a newline received.
; *******************************************************************

waitchar:   
        jsr  getchar            ; loop around waiting for character
        CP $20
        JR NC,waitchar1
        CP $0                   ; is it end of string?
        JR Z,waitchar4
        CP '\r'                 ; carriage return?
        JR Z,waitchar3
        LD D,0
        JR macro    

waitchar1:
        LD HL,TIB
        ADD HL,BC
        LD (HL),A               ; store the character in textbuf
        INC BC
        jsr  putchar            ; echo character to screen
        jsr  nesting
        JR  waitchar            ; wait for next character

waitchar3:
        LD HL,TIB
        ADD HL,BC
        LD (HL),"\r"            ; store the crlf in textbuf
        INC HL
        LD (HL),"\n"            
        INC HL                  ; ????
        INC BC
        INC BC
        jsr  crlf               ; echo character to screen
        LD A,E                  ; if zero nesting append and ETX after \r
        OR A
        JR NZ,waitchar
        LD (HL),$03             ; store end of text ETX in text buffer 
        INC BC

waitchar4:    
        LD (vTIBPtr),BC
        LD BC,TIB               ; Instructions stored on heap at address HERE
        DEC BC
        jmp NEXT

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

NEXT:                               ; 9 
        INC BC                      ; 6t    Increment the IP
        LD A, (BC)                  ; 7t    Get the next character and dispatch
        LD L,A                      ; 4t    Index into table
        LD H,msb(optcodes)           ; 7t    Start address of jump table         
        LD L,(HL)                   ; 7t    get low jump address
        LD H,msb(page4)             ; 7t    Load H with the 1st page address
        jmp (HL)                     ; 4t    Jump to routine

; ARRAY compilation routine
compNEXT:                       ;=20
        POP DE          ; DE = return address
        LD HL,(vHeapPtr)    ; load heap ptr
        LD (HL),E       ; store lsb
        LD A,(vByteMode)
        INC HL          
        OR A
        JR NZ,compNext1
        LD (HL),D
        INC HL
compNext1:
        LD (vHeapPtr),HL    ; save heap ptr
        JR NEXT

; **************************************************************************             
; calculate nesting value
; A is char to be tested, 
; E is the nesting value (initially 0)
; E is increased by ( and [ 
; E is decreased by ) and ]
; E has its bit 7 toggled by `
; limited to 127 levels
; **************************************************************************             

zzzz

nesting_:                        ;= 44
        CP '`'
        bne nesting1
        BIT 7,E
        beq nesting1a
        RES 7,E
        rts

nesting1a: 

nesting1:
        BIT 7,E             
        rts NZ             
nests_:
        cmp '`'
        beq @nestogg
        cmp ':'
        beq @nestinc
        cmp '['
        beq @nestinc
        cmp '('
        beq @nestinc
        cmp ';'
        beq @nestdec
        cmp ']'
        beq @nestdec
        cmp ')'
        beq @nestdec

@nonest:
        rts 

@nestinc:
        inc ch + 1
        rts

@nestdec:
        dec ch + 1
        rts

@nestogg:
        lda #$80
        eoa ch + 1
        sta ch + 1
        rts


nesting:                        ;= 44
        CP '`'
        JR NZ,nesting1
        BIT 7,E
        JR Z,nesting1a
        RES 7,E
        RET
nesting1a: 
        SET 7,E
        RET
nesting1:
        BIT 7,E             
        rts NZ             
        CP ':'
        JR Z,nesting2
        CP '['
        JR Z,nesting2
        CP '('
        JR NZ,nesting3
nesting2:
        INC E
        RET
nesting3:
        CP ';'
        JR Z,nesting4
        CP ']'
        JR Z,nesting4
        CP ')'
        rts NZ
nesting4:
        DEC E
        rts 
        
prompt:                             ;=9
        jsr  printStr
        .asciiz  "\r\n> "
        RET

; **************************************************************************
; Macros must be written in Mint and end with ; 
; this code must not span pages
; **************************************************************************
macros:

.include "MINT-macros.asm"


; **************************************************************************
; Page 2  Jump Tables
; **************************************************************************
        .align $100
optcodes:
; ***********************************************************************
; Initial values for user mintVars		
; ***********************************************************************		
       .word (exit_)    ;   NUL 
       .word (nop_)     ;   SOH 
       .word (nop_)     ;   STX 
       .word (etx_)     ;   ETX 
       .word (nop_)     ;   EOT 
       .word (nop_)     ;   ENQ 
       .word (nop_)     ;   ACK 
       .word (nop_)     ;   BEL 
       .word (nop_)     ;   BS  
       .word (nop_)     ;   TAB 
       .word (nop_)     ;   LF  
       .word (nop_)     ;   VT  
       .word (nop_)     ;   FF  
       .word (nop_)     ;   CR  
       .word (nop_)     ;   SO  
       .word (nop_)     ;   SI  
       .word (nop_)     ;   DLE 
       .word (nop_)     ;   DC1 
       .word (nop_)     ;   DC2 
       .word (nop_)     ;   DC3 
       .word (nop_)     ;   DC4 
       .word (nop_)     ;   NAK 
       .word (nop_)     ;   SYN 
       .word (nop_)     ;   ETB 
       .word (nop_)     ;   CAN 
       .word (nop_)     ;   EM  
       .word (nop_)     ;   SUB 
       .word (nop_)     ;   ESC 
       .word (nop_)     ;   FS  
       .word (nop_)     ;   GS  
       .word (nop_)     ;   RS  
       .word (nop_)     ;   US  
       .word (nop_)     ;   SP
       .word (store_)   ;   !            
       .word (dup_)     ;   "
       .word (hex_)    ;    #
       .word (swap_)   ;    $            
       .word (over_)   ;    %            
       .word (and_)    ;    &
       .word (drop_)   ;    '
       .word (begin_)  ;    (        
       .word (again_)  ;    )
       .word (mul_)    ;    *            
       .word (add_)    ;    +
       .word (hdot_)   ;    ,            
       .word (sub_)    ;    -
       .word (dot_)    ;    .
       .word (div_)    ;    /
       .word (num_)    ;    0            
       .word (num_)    ;    1        
       .word (num_)    ;    2            
       .word (num_)    ;    3
       .word (num_)    ;    4            
       .word (num_)    ;    5            
       .word (num_)    ;    6            
       .word (num_)    ;    7
       .word (num_)    ;    8            
       .word (num_)    ;    9        
       .word (def_)    ;    :        
       .word (ret_)    ;    ;
       .word (lt_)     ;    <
       .word (eq_)     ;    =            
       .word (gt_)     ;    >            
       .word (getRef_) ;    ?
       .word (fetch_)  ;    @    
       .word (jsr _)   ;    A    
       .word (jsr _)   ;    B
       .word (jsr _)   ;    C
       .word (jsr _)   ;    D    
       .word (jsr _)   ;    E
       .word (jsr _)   ;    F
       .word (jsr _)   ;    G
       .word (jsr _)   ;    H
       .word (jsr _)   ;    I
       .word (jsr _)   ;    J
       .word (jsr _)   ;    K
       .word (jsr _)   ;    L
       .word (jsr _)   ;    M
       .word (jsr _)   ;    N
       .word (jsr _)   ;    O
       .word (jsr _)   ;    P
       .word (jsr _)   ;    Q
       .word (jsr _)   ;    R
       .word (jsr _)   ;    S
       .word (jsr _)   ;    T
       .word (jsr _)   ;    U
       .word (jsr _)   ;    V
       .word (jsr _)   ;    W
       .word (jsr _)   ;    X
       .word (jsr _)   ;    Y
       .word (jsr _)   ;    Z
       .word (arrDef_) ;    [
       .word (alt_)    ;    \
       .word (arrEnd_) ;    ]
       .word (xor_)    ;    ^
       .word (neg_)    ;    _
       .word (str_)    ;    `            
       .word (var_)    ;    a
       .word (var_)    ;    b
       .word (var_)    ;    c
       .word (var_)    ;    d
       .word (var_)    ;    e
       .word (var_)    ;    f
       .word (var_)    ;    g
       .word (var_)    ;    h
       .word (var_)    ;    i            
       .word (var_)    ;    j
       .word (var_)    ;    k
       .word (var_)    ;    l
       .word (var_)    ;    m
       .word (var_)    ;    n
       .word (var_)    ;    o
       .word (var_)    ;    p
       .word (var_)    ;    q            
       .word (var_)    ;    r
       .word (var_)    ;    s    
       .word (var_)    ;    t
       .word (var_)    ;    u
       .word (var_)    ;    v
       .word (var_)    ;    w
       .word (var_)    ;    x
       .word (var_)    ;    y
       .word (var_)    ;    z
       .word (shl_)    ;    {
       .word (or_)     ;    |            
       .word (shr_)    ;    }            
       .word (inv_)    ;    ~            
       .word (nop_)    ;    backspace

        
; ***********************************************************************
; Alternate function codes		
; ***********************************************************************		
ctlcodes:
altcodes:
       .word (empty_)      ; NUL ^@
       .word (empty_)      ; SOH ^A
       .word (toggleBase_) ; STX ^B
       .word (empty_)      ; ETX ^C
       .word (empty_)      ; EOT ^D
       .word (edit_)       ; ENQ ^E
       .word (empty_)      ; ACK ^F
       .word (empty_)      ; BEL ^G
       .word (backsp_)     ; BS  ^H
       .word (empty_)      ; TAB ^I
       .word (reedit_)     ; LF  ^J
       .word (empty_)      ; VT  ^K
       .word (list_)       ; FF  ^L
       .word (empty_)      ; CR  ^M
       .word (empty_)      ; SO  ^N
       .word (empty_)      ; SI  ^O
       .word (printStack_) ; DLE ^P
       .word (empty_)      ; DC1 ^Q
       .word (empty_)      ; DC2 ^R
       .word (empty_)      ; DC3 ^S
       .word (empty_)      ; DC4 ^T
       .word (empty_)      ; NAK ^U
       .word (empty_)      ; SYN ^V
       .word (empty_)      ; ETB ^W
       .word (empty_)      ; CAN ^X
       .word (empty_)      ; EM  ^Y
       .word (empty_)      ; SUB ^Z
       .word (empty_)      ; ESC ^[
       .word (empty_)      ; FS  ^\
       .word (empty_)      ; GS  ^]
       .word (empty_)      ; RS  ^^
       .word (empty_)      ; US  ^_)
       .word (aNop_)       ; SP  ^`
       .word (cStore_)     ;    !            
       .word (aNop_)       ;    "
       .word (aNop_)       ;    #
       .word (aNop_)       ;    $  ( -- adr ) text input ptr           
       .word (aNop_)       ;    %            
       .word (aNop_)       ;    &
       .word (aNop_)       ;    '
       .word (ifte_)       ;    (  ( b -- )              
       .word (aNop_)       ;    )
       .word (aNop_)       ;    *            
       .word (incr_)       ;    +  ( adr -- ) decrements variable at address
       .word (aNop_)       ;    ,            
       .word (aNop_)       ;    -  
       .word (aNop_)       ;    .  
       .word (aNop_)       ;    /
       .word (aNop_)       ;    0           
       .word (aNop_)       ;    1  
       .word (aNop_)       ;    2            
       .word (aNop_)       ;    3  
       .word (aNop_)       ;    4            
       .word (aNop_)       ;    5            
       .word (aNop_)       ;    6            
       .word (aNop_)       ;    7
       .word (aNop_)       ;    8            
       .word (aNop_)       ;    9        
       .word (aNop_)       ;    :  start defining a macro        
       .word (aNop_)       ;    ;  
       .word (aNop_)       ;    <
       .word (aNop_)       ;    =            
       .word (aNop_)       ;    >            
       .word (aNop_)       ;    ?
       .word (cFetch_)     ;    @      
       .word (aNop_)       ;    A    
       .word (break_)      ;    B
       .word (nop_)        ;    C
       .word (depth_)      ;    D  ( -- val ) depth of data stack  
       .word (emit_)       ;    E   ( val -- ) emits a char to output
       .word (aNop_)       ;    F
       .word (go_)         ;    G   ( -- ? ) execute mint definition
       .word (aNop_)       ;    H  
       .word (inPort_)     ;    I  ( port -- val )   
       .word (aNop_)       ;    J
       .word (key_)        ;    K  ( -- val )  read a char from input
       .word (aNop_)       ;    L  
       .word (aNop_)       ;    M  
       .word (newln_)      ;    N   ; prints a newline to output
       .word (outPort_)    ;    O  ( val port -- )
       .word (printStk_)   ;    P  ( -- ) non-destructively prints stack
       .word (aNop_)       ;    Q  quits from Mint REPL
       .word (rot_)        ;    R  ( a b c -- b c a )
       .word (aNop_)       ;    S
       .word (aNop_)       ;    T
       .word (aNop_)       ;    U
       .word (aNop_)       ;    V
       .word (aNop_)       ;    W   ; ( b -- ) if false, skip to end of loop
       .word (exec_)       ;    X
       .word (aNop_)       ;    Y
       .word (editDef_)    ;    Z
       .word (cArrDef_)    ;    [
       .word (comment_)    ;    \  comment text, skips reading until end of line
       .word (aNop_)       ;    ]
       .word (charCode_)   ;    ^
       .word (aNop_)       ;    _ 
       .word (aNop_)       ;    `            
       .word (sysVar_)     ;    a  ; start of data stack variable
       .word (sysVar_)     ;    b  ; base16 variable
       .word (sysVar_)     ;    c  ; TIBPtr variable
       .word (sysVar_)     ;    d  
       .word (sysVar_)     ;    e  
       .word (sysVar_)     ;    f
       .word (sysVar_)     ;    g  
       .word (sysVar_)     ;    h  ; heap ptr variable
       .word (i_)          ;    i  ; returns index variable of current loop          
       .word (j_)          ;    j  ; returns index variable of outer loop
       .word (sysVar_)     ;    k  
       .word (sysVar_)     ;    l
       .word (sysVar_)     ;    m  ( a b -- c ) return the minimum value
       .word (sysVar_)     ;    n  
       .word (sysVar_)     ;    o
       .word (sysVar_)     ;    p  
       .word (sysVar_)     ;    q           
       .word (sysVar_)     ;    r
       .word (sysVar_)     ;    s 
       .word (sysVar_)     ;    t
       .word (sysVar_)     ;    u
       .word (sysVar_)     ;    v   
       .word (sysVar_)     ;    w   
       .word (sysVar_)     ;    x
       .word (sysVar_)     ;    y
       .word (sysVar_)     ;    z
       .word (group_)      ;    {
       .word (aNop_)       ;    |            
       .word (endGroup_)   ;    }            
       .word (aNop_)       ;    ~           
       .word (aNop_)       ;    BS		


; **********************************************************************			 
;
; the routines are ordered to occupy pages of 256 bytes 
; all rotines must end with: jmp link_ or jmp drop_ or a jmp / branch
;
; **********************************************************************			 

; **********************************************************************			 
; Page 4 primitive routines 
; **********************************************************************
        .align $100
page4:

alt_:        
        jmp alt

; emit a byte of terminal
emit_:
        jsr pull_
        lda wk + 0
        jsr putchar
        jmp link_

; receive a byte of terminal
key_:
        jsr getchar
        sta wk + 0
        jsr push_
        jmp link_

; pull wk from stack
pull_:
    lta spz + 0, x
    sda wk + 0
    lta spz + 1, x
    sda wk + 1
    inx
    inx
    rts

; push wk into stack
push_:
    dex
    dex
    lda wk + 0
    sta spz + 0, x
    lda wk + 1
    sta spz + 1, x
    rts

; NEGate the value on top of stack (2's complement)
neg_:
    sec
    lda #0
    sbc spz + 1, x
    sta spz + 1, x
    lda #0
    sbc spz + 0, x
    sta spz + 0, x
    jmp link_
   
; Bitwise INVert the top member of the stack (1's complement)
inv_:    
    lda #$FF
    eor spz + 0, x
    sta spz + 0, x
    eor spz + 1, x
    sta spz + 1, x
    jmp link_

; Drop the top member of the stack
; a b c -- a b 
drop_:
	inx
	inx
	jmp link_

; Duplicate the top member of the stack
; a b c -- a b c c 
dup_:
	dex
	dex
	lda spz + 2, x
	sta spz + 0, x
	lda spz + 3, x
	sta spz + 1, x
    jmp link_

; Duplicate 2nd element of the stack
; a b c -- a b c b 
over_:
    dex
    dex
    lda spz + 4, x
    sta spz + 0, x
    lda spz + 5, x
    sta spz + 1, x
    jmp link_

; Rotate 3 elements at stack
; a b c -- b c a
rot_:
    ; c -> w
    lda spz + 0, x
    sta wk + 0
    lda spz + 0, x
    sta wk + 1
    ; b -> u
    lda spz + 2, x
    sta us + 0
    lda spz + 3, x
    sta us + 1
    ; a -> c
    lda spz + 4, x
    sta spz + 0, x
    lda spz + 5, x
    sta spz + 1, x
    ; u -> a
    lda us + 0
    sta spz + 4, x
    lda us + 1
    sta spz + 5, x
    ; w -> b
    lda wk + 0
    sta spz + 2, x
    lda wk + 1
    sta spz + 3, x
    jmp link_

; Swap 2nd and 1st elements of the stack
; a b c -- a c b
swap_:
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
    jmp link_

;  Left shift { is multply by 2		
shl_:
    asl spz + 0, x
    rol spz + 1, x
    jmp link_

;  Right shift } is a divide by 2		
shr_:
    lsr spz + 0, x
    ror spz + 1, x
    jmp link_

;  Bitwise AND the top 2 elements of the stack
and_:        
    lda spz + 2, x
    and spz + 0, x
    sta spz + 2, x
    lda spz + 3, x
    and spz + 1, x
    sta spz + 3, x
    jmp drop_

;  Bitwise OR the top 2 elements of the stack
or_:    
    lda spz + 2, x
    ora spz + 0, x
    sta spz + 2, x
    lda spz + 3, x
    ora spz + 1, x
    sta spz + 3, x
    jmp drop_

;  Bitwise XOR the top 2 elements of the stack
xor_:    
    lda spz + 2, x
    eor spz + 0, x
    sta spz + 2, x
    lda spz + 3, x
    eor spz + 1, x
    sta spz + 3, x
    jmp drop_

; Add the top 2 members of the stack
; a b c -- a (b+c) 
add_:                          
    clc
    lda spz + 2, x
    adc spz + 0, x
    sta spz + 2, x
    lda spz + 3, x
    adc spz + 1, x
    sta spz + 3, x
    jmp drop_
   
; Subtract the top 2 members of the stack
; a b c -- a (b-c)
sub_:                          
    sec
    lda spz + 2, x
    sub spz + 0, x
    sta spz + 2, x
    lda spz + 3, x
    sub spz + 1, x
    sta spz + 3, x
	jmp drop_

; Divide the top 2 members of the stack
; a b c -- a (b / c)r (b /c)d
div_:   
     jmp divt_

; Multiply the top 2 members of the stack
; a b c -- a (b * c)h (b * c)l
mul_:   
     jmp mult_      

; false
false2_:
    lda #$00
	sta spz + 2, x
	sta spz + 3, x
	jmp drop_

; false
true2_:
    lda #$01
	sta spz + 2, x
    lda #$00
	sta spz + 3, x
	jmp drop_

; subtract for compare
cmp_:
    sec
    lda spz + 2, x
    sbc spz + 0, x
    lda spz + 3, x
    sbc spz + 1, x
    rts

; signed equal than
eq_:
    jsr cmp_
    bne false2
    beq true2

; signed less than
lt_:
    jsr cmp_
    bmi true2_
    bpl false2_

; signed greather than
gt_:
    jsr cmp_
    bmi false2_
    beq false2_
    bpl true2_

   
; Fetch the value from the address placed on the top of the stack      
fetch_:                     
	
; Store the value into the address placed on the top of the stack
store_:

hex_:   
    jmp hex2_

nop_:   
    jmp next_                ; hardwire white space to always go to NEXT (important for arrays)

num_:   
    jmp  num2_

def_:   
    jmp def2_

ZZZZZZ:

arrDef_:    
arrDef:                     ;= 18
        LD A,FALSE
arrDef1:      
        LD IY,compNEXT
        LD (vByteMode),A
        LD HL,(vHeapPtr)    ; HL = heap ptr
        jsr  rpush          ; save start of array \[  \]
        jmp NEXT         ; hardwired to NEXT

arrEnd_:    jmp arrEnd
begin_:     jmp begin                   
call _:
        LD HL,BC
        jsr  rpush              ; save Instruction Pointer
        LD A,(BC)
        jsr  lookupDef1
        LD C,(HL)
        INC HL
        LD B,(HL)
        DEC BC
        jmp  (IY)                ; Execute code from User def



hdot_:                              ; print hexadecimal
        POP     HL
        jsr     printhex
        JR   dot2
dot_:       
        POP HL
        jsr  printdec
dot2:
        LD A,' '           
        jsr  writeChar1
        jmp (IY)

etx_:
etx:
        LD HL,-DSTACK
        ADD HL,SP
        JR NC,etx1
        LD SP,DSTACK
etx1:
        jmp interpret

        
exit_:
        INC BC
        LD DE,BC                
        jsr  rpop               ; Restore Instruction pointer
        LD BC,HL
        EX DE,HL
        jmp (HL)
        
ret_:
        jsr  rpop               ; Restore Instruction pointer
        LD BC,HL                
        jmp (IY)             

        POP    HL           ; 10t
        POP    DE           ; 10t
        LD     (HL),E       ; 7t
        INC    HL           ; 6t
        LD     (HL),D       ; 7t
        jmp     (IY)         ; 8t
                            ; 48t
    
getRef_:    
        jmp getRef

var_:
        LD A,(BC)
        
        SUB "a" - ((VARS - mintVars)/2)  
        ADD A,A
        LD L,A
        LD H,msb(mintVars)
        
        PUSH HL
        jmp (IY)
        
again_:     
        jmp again
str_:                       
str:                                ;= 15
        INC BC
        
nextchar:            
        LD A, (BC)
        INC BC
        CP "`"              ; ` is the string terminator
        JR Z,str2
        jsr  putchar
        JR nextchar

str2:  
        DEC BC
        jmp   (IY) 

;*******************************************************************
; Page 5 primitive routines 
;*******************************************************************
        ;falls through 

getRef:                         ;= 8
        INC BC
        LD A,(BC)
        jsr  lookupDef
        jmp fetch1

alt:                                ;= 11
        INC BC
        LD A,(BC)
        LD HL,altcodes
        ADD A,L
        LD L,A
        LD L,(HL)                   ; 7t    get low jump address
        LD H, msb(page6)            ; Load H with the 5th page address
        jmp (HL)                    ; 4t    Jump to routine

; ********************************************************************
; 16-bit multiply  
mul:                        ;=19
        POP  DE             ; get first value
        POP  HL
        PUSH BC             ; Preserve the IP
        LD B,H              ; BC = 2nd value
        LD C,L
        
        LD HL,0
        LD A,16
Mul_Loop_1:
        ADD HL,HL
        RL E
        RL D
        JR NC,$+6
        ADD HL,BC
        JR NC,$+3
        INC DE
        DEC A
        JR NZ,Mul_Loop_1
		
		POP BC				; Restore the IP
		PUSH HL             ; Put the product on the stack - stack bug fixed 2/12/21
		JP (IY)

; ********************************************************************
; 16-bit division subroutine.
;
; BC: divisor, DE: dividend, HL: remainder

; *********************************************************************            
; This divides DE by BC, storing the result in DE, remainder in HL
; *********************************************************************

; 1382 cycles
; 35 bytes (reduced from 48)
		

div:                        ;=24
        POP  DE             ; get first value
        POP  HL             ; get 2nd value
        PUSH BC             ; Preserve the IP
        LD B,H              ; BC = 2nd value
        LD C,L		
		
        ld hl,0    	        ; Zero the remainder
        ld a,16    	        ; Loop counter

div_loop:		            ;shift the bits from BC (numerator) into HL (accumulator)
        sla c
        rl b
        adc hl,hl

        sbc hl,de			;Check if remainder >= denominator (HL>=DE)
        jr c,div_adjust
        inc c
        jr div_done

div_adjust:		            ; remainder is not >= denominator, so we have to add DE back to HL
        add hl,de

div_done:
        dec a
        jr nz,div_loop
        
        LD D,B              ; Result from BC to DE
        LD E,C
        
div_end:    
        POP  BC             ; Restore the IP
   
        PUSH DE             ; Push Result
        PUSH HL             ; Push remainder             

        jmp (IY)

; **************************************************************************             
; def is used to create a colon definition
; When a colon is detected, the next character (usually uppercase alpha)
; is looked up in the vector table to get its associated code field address
; This CFA is updated to point to the character after uppercase alpha
; The remainder of the characters are then skipped until after a semicolon  
; is found.
; ***************************************************************************
                            ;= 31
def:                        ; Create a colon definition
        INC BC
        LD  A,(BC)          ; Get the next character
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
        jmp (IY)       

; ********************************************************************************
; Number Handling Routine - converts numeric ascii string to a 16-bit number in HL
; Read the first character. 
;			
; Number characters ($30 to $39) are converted to digits by subtracting $30
; and then added into the L register. (HL forms a 16-bit accumulator)
; Fetch the next character, if it is a number, multiply contents of HL by 10
; and then add in the next digit. Repeat this until a non-number character is 
; detected. Add in the final digit so that HL contains the converted number.
; Push HL onto the stack and proceed to the dispatch routine.
; ********************************************************************************
         
num2_:
    lda #$00
    sta wk + 0
    sta wk + 1
@loop:
    jsr @mul10_
    lda ch + 0
    jsr @nums
    bcs @ends
    lda wk + 0
    adc ch + 0
    bcc @loop
@ends:
    jmp push_

; always base 10
@nums_:
    lda ch + 0
    cmp '0' + 0
    bcc nak_
    cmp '9' + 1
    bcs nak_
    sbc #'0'
    sta ch + 0
ack_:    
    clc
    rts
nak_:
    sec
    rts

; multiply by ten
mul10_:
    clc
    rol wk + 0
    rol wk + 1
    lda wk + 0
    sta us + 0
    lda wk + 1
    sta us + 1
    clc 
    rol wk + 0
    rol wk + 1
    clc
    rol wk + 0
    rol wk + 1
    clc
    lda wk + 0
    adc us + 0
    sta wk + 0
    lda wk + 1
    adc us + 1
    sta wk + 1
    clc
    rts
                
; *************************************
; Loop Handling Code
; *************************************
        	                    ;= 23                     

; Left parentesis begins a loop
begin:                          
        POP HL
        LD A,L                  ; zero?
        OR H
        JR Z,begin1
        
        DEC HL
        LD DE,-6
        ADD IX,DE
        LD (IX+0),0             ; loop var
        LD (IX+1),0                 
        LD (IX+2),L             ; loop limit
        LD (IX+3),H                 
        LD (IX+4),C             ; loop address
        LD (IX+5),B                 

        jmp (IY)
begin1:
        LD E,1
begin2:
        INC BC
        LD A,(BC)
        jsr  nesting
        XOR A
        OR E
        JR NZ,begin2
begin3:
        jmp (IY)

again:   
        LD E,(IX+0)                 ; peek loop var
        LD D,(IX+1)                 
        
        LD A,D                      ; check if IFTEMode
        AND E
        INC A
        JR NZ,again1
        INC DE
        PUSH DE                     ; push FALSE condition
        LD DE,2
        JR again3                   ; drop IFTEMode

again1:
        LD L,(IX+2)                 ; peek loop limit
        LD H,(IX+3)                 
        OR A
        SBC HL,DE
        JR Z,again2
        INC DE
        LD (IX+0),E                 ; poke loop var
        LD (IX+1),D                 
        LD C,(IX+4)                 ; peek loop address
        LD B,(IX+5)                 
        jmp (IY)
again2:   
        LD DE,6                     ; drop loop frame
again3:
        ADD IX,DE
        jmp (IY)

; **************************************************************************
; Page 6 Alt primitives
; **************************************************************************
        .align $100
page6:

cArrDef_:                   ; define a byte array
        LD A,TRUE
        jmp arrDef1

cFetch_:
        POP     HL          ; 10t
        LD      D,0         ; 7t
        LD      E,(HL)      ; 7t
        PUSH    DE          ; 11t
anop_:
        jmp      (IY)        ; 8t
                            ; 49t 
charCode_:
        INC BC
        LD A,(BC)
        LD H,0
        LD L,A
        PUSH HL
        jmp (IY)

comment_:
        INC BC              ; point to next char
        LD A,(BC)
        CP "\r"             ; terminate at cr 
        JR NZ,comment_
        ; CP "\n"             ; terminate at lf 
        ; JR NZ,comment_
        DEC BC
        jmp   (IY) 

cStore_:	  
        POP    HL           ; 10t
        POP    DE           ; 10t
        LD     (HL),E       ; 7t
        jmp     (IY)         ; 8t
                            ; 48t
depth_:
        LD HL,0
        ADD HL,SP
        EX DE,HL
        LD HL,DSTACK
        OR A
        SBC HL,DE
        jmp shr1

ifte_:
        POP DE
        LD A,E
        OR D
        JR NZ,ifte1
        INC DE
        PUSH DE                     ; push TRUE on stack for else clause
        jmp begin1                   ; skip to closing ) works with \) too 
ifte1:
        LD HL,-1                    ; push -1 on return stack to indicate IFTEMode
        jsr  rpush
        jmp (IY)

exec_:
        jsr  exec1
        jmp (IY)
exec1:
        POP HL
        EX (SP),HL
        jmp (HL)

go_:
        LD HL,BC
        jsr  rpush              ; save Instruction Pointer
        POP BC
        DEC BC
        jmp  (IY)                ; Execute code from User def

endGroup_:
        jsr  rpop
        LD (vDEFS),HL
        jmp (IY)

group_:
        POP DE
        LD D,E
        LD E,0
        SRL D
        RR E
        SRL D
        RR E
        LD HL,(vDEFS)
        jsr  rpush
        LD HL,DEFS
        ADD HL,DE
        LD (vDEFS),HL
        jmp  (IY)                ; Execute code from User def

sysVar_:
        LD A,(BC)
        SUB "a" - ((sysVars - mintVars)/2) 
        ADD A,A
        LD L,A
        LD H,msb(mintVars)
        PUSH HL
        jmp  (IY)                ; Execute code from User def

i_:
        PUSH IX
        jmp (IY)

; \+    a b -- [b]+a            ; increment variable at b by a
incr_:
        POP HL
        POP DE
        LD A,E
        ADD A,(HL)
        LD (HL),A
        INC HL
        LD A,D
        ADC A,(HL)
        LD (HL),A
        jmp (IY)

inPort_:
        POP HL
        LD A,C
        LD C,L
        IN L,(C)
        LD H,0
        LD C,A
        PUSH HL
        jmp (IY)        

j_:
        PUSH IX
        POP HL
        LD DE,6
        ADD HL,DE
        PUSH HL
        jmp (IY)

key_:
        jsr  getchar
        LD L,A
        LD H,0
        PUSH HL
        jmp (IY)

newln_:
        jsr  crlf
        jmp (IY)        

outPort_:
        POP HL
        LD E,C
        LD C,L
        POP HL
        OUT (C),L
        LD C,E
        jmp (IY)        

break_:
        POP HL
        LD A,L                      ; zero?
        OR H
        JR NZ,break1
        jmp (IY)
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
        jmp (IY)

printStk:                   ;= 40
        jsr  ENTER
        .asciiz  "\\a@2-\\D1-(",$22,"@\\b@\\(,)(.)2-)'"             
        jmp (IY)

;*******************************************************************
; Page 5 primitive routines continued
;*******************************************************************

arrEnd:                     ;= 27
        jsr  rpop               ; DE = start of array
        PUSH HL
        EX DE,HL
        LD HL,(vHeapPtr)        ; HL = heap ptr
        OR A
        SBC HL,DE               ; bytes on heap 
        LD A,(vByteMode)
        OR A
        JR NZ,arrEnd2
        SRL H           ; BC = m words
        RR L
arrEnd2:
        PUSH HL 
        LD IY,NEXT
        jmp (IY)         ; hardwired to NEXT

hex:                            ;= 26
	    LD HL,0		    		; 10t Clear HL to accept the number
hex1:
        INC BC
        LD A,(BC)				; 7t  Get the character which is a numeral
        BIT 6,A                 ; 7t    is it uppercase alpha?
        JR Z, hex2              ; no a decimal
        SUB 7                   ; sub 7  to make $A - $F
hex2:
        SUB $30                 ; 7t    Form decimal digit
        jmp C,endnum
        CP $0F+1
        jmp NC,endnum
        ADD HL,HL               ; 11t    2X ; Multiply digit(s) in HL by 16
        ADD HL,HL               ; 11t    4X
        ADD HL,HL               ; 11t    8X
        ADD HL,HL               ; 11t   16X     
        ADD A,L                 ; 4t    Add into bottom of HL
        LD  L,A                 ; 4t
        JR  hex1

;*******************************************************************
; Subroutines
;*******************************************************************

crlf:                               ;=7
        jsr  printStr
        .asciiz  "\r\n"
        RET

enter:                          ; 9
        LD HL,BC
        jsr  rpush              ; save Instruction Pointer
        POP BC
        DEC BC
        jmp  (IY)                ; Execute code from User def

lookupDef:                          ;=20
        SUB "A"  
        LD (vEdited),A      
        JR lookupDef2
lookupDef1:
        SUB "A"  
lookupDef2:
        ADD A,A
        LD E,A
        LD D,0
        LD HL,(vDEFS)
        ADD HL,DE
        RET

printStr:                           ;=14
        EX (SP),HL
        JR printStr2

printStr1:
        jsr  putchar
        INC HL

printStr2:
        LD A,(HL)
        OR A
        JR NZ,printStr1
        INC HL
        EX (SP),HL
        RET

printdec:

;Number in hl to decimal ASCII

;inputs:	hl = number to ASCII
;example: hl=300 outputs '00300'
;destroys: af, de, hl
DispHL:                         ;= 36
        ld	de,-10000
        jsr 	Num1
        ld	de,-1000
        jsr 	Num1
        ld	de,-100
        jsr 	Num1
        ld	e,-10
        jsr 	Num1
        ld	e,-1
Num1:	    
        ld	a,'0'-1
Num2:	    
        inc	a
        add	hl,de
        jr	c,Num2
        sbc	hl,de
        jmp putchar

; Print an 8-bit HEX number  - shortened KB 25/11/21
; A: Number to print
Print_Hex8:		                ;= 20
        LD	C,A
		RRA 
		RRA 
		RRA 
		RRA 
	    jsr  conv
	    LD A,C
conv:		
        AND	0x0F
		ADD	A,0x90
		DAA
		ADC	A,0x40
		DAA
		JP putchar

printhex:                       ;= 11  
                                ; Display HL as a 16-bit number in hex.
        PUSH BC                 ; preserve the IP
        LD A,H
        jsr  Print_Hex8
        LD A,L
        jsr  Print_Hex8
        POP BC
        RET

rpush:                              ;=11
        DEC IX                  
        LD (IX+0),H
        DEC IX
        LD (IX+0),L
        RET

rpop:                               ;=11
        LD L,(IX+0)         
        INC IX              
        LD H,(IX+0)
        INC IX                  
        RET
        
writeChar:
        LD (DE),A
        INC DE
        
writeChar1:
        jmp putchar


