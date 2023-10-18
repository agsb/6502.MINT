; ************************************************************************** 
; Jump Tables, not optmized 
; ************************************************************************** 
; .align $100 
 
;---------------------------------------------------------------------- 
optcodes: 
       .word (exit_)    ;   NUL 
       .word (nop_)     ;   SOH 
       .word (nop_)     ;   STX 
       .word (etx_)     ;   ETX 
       .word (nop_)     ;   EOT 
       .word (nop_)     ;   ENQ 
       .word (nop_)     ;   apK 
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
       .word (nop_)     ;   nos 
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
       .word (call_)    ;    A 
       .word (call_)    ;    B 
       .word (call_)    ;    C 
       .word (call_)    ;    D 
       .word (call_)    ;    E 
       .word (call_)    ;    F 
       .word (call_)    ;    G 
       .word (call_)    ;    H 
       .word (call_)    ;    I 
       .word (call_)    ;    J 
       .word (call_)    ;    K 
       .word (call_)    ;    L 
       .word (call_)    ;    M 
       .word (call_)    ;    N 
       .word (call_)    ;    O 
       .word (call_)    ;    P 
       .word (call_)    ;    Q 
       .word (call_)    ;    R 
       .word (call_)    ;    S 
       .word (call_)    ;    T 
       .word (call_)    ;    U 
       .word (call_)    ;    V 
       .word (call_)    ;    W 
       .word (call_)    ;    X 
       .word (call_)    ;    Y 
       .word (call_)    ;    Z 
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
 
;---------------------------------------------------------------------- 
; alternate function codes 
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
       .word (empty_)      ; nos  ^_) 
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
vS0:        .word (sysVar_)     ;    a  ; start of data stack variable 
vBase16:    .word (sysVar_)     ;    b  ; base16 variable 
vTIBPtr:    .word (sysVar_)     ;    c  ; TIBPtr variable 
vDefs:      .word (sysVar_)     ;    d 
vEdited:    .word (sysVar_)     ;    e 
vR0:        .word (sysVar_)     ;    f  ; start of return stack variable 
vNext:      .word (sysVar_)     ;    g  ; next routine dispatcher 
vHeapPtr:   .word (sysVar_)     ;    h  ; heap ptr variable 
       .word (i_)          ;    i  ; returns index variable of current loop 
       .word (j_)          ;    j  ; returns index variable of outer loop 
       .word (sysVar_)     ;    k 
       .word (sysVar_)     ;    l 
       .word (sysVar_)     ;    m  ( a b -- c ) return the minimum value 
       .word (sysVar_)     ;    n 
       .word (sysVar_)     ;    o 
       .word (sysVar_)     ;    p 
       .word (sysVar_)     ;    q 
       .word (sysVar_)     ;    r  ; return stack pointer
       .word (sysVar_)     ;    s  ; data stack pointer
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
 
; ********************************************************************* 
; Macros must be written in Mint and end with ; 
; this code must not span pages 
; ********************************************************************* 
macros: 
 
.include "6502.MINT.macros.asm" 
 
; heap must be here ! 
 
heap: 
 
