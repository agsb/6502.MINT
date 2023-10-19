; ************************************************************************** 
; Jump Tables, not optmized 
; ************************************************************************** 
; .align $100 
 
;---------------------------------------------------------------------- 
optcodeslo: 
       .byte  <exit_    ;   NUL 
       .byte  <nop_     ;   SOH 
       .byte  <nop_     ;   STX 
       .byte  <etx_     ;   ETX 
       .byte  <nop_     ;   EOT 
       .byte  <nop_     ;   ENQ 
       .byte  <nop_     ;   apK 
       .byte  <nop_     ;   BEL 
       .byte  <nop_     ;   BS 
       .byte  <nop_     ;   TAB 
       .byte  <nop_     ;   LF 
       .byte  <nop_     ;   VT 
       .byte  <nop_     ;   FF 
       .byte  <nop_     ;   CR 
       .byte  <nop_     ;   SO 
       .byte  <nop_     ;   SI 
       .byte  <nop_     ;   DLE 
       .byte  <nop_     ;   DC1 
       .byte  <nop_     ;   DC2 
       .byte  <nop_     ;   DC3 
       .byte  <nop_     ;   DC4 
       .byte  <nop_     ;   NAK 
       .byte  <nop_     ;   SYN 
       .byte  <nop_     ;   ETB 
       .byte  <nop_     ;   CAN 
       .byte  <nop_     ;   EM 
       .byte  <nop_     ;   SUB 
       .byte  <nop_     ;   ESC 
       .byte  <nop_     ;   FS 
       .byte  <nop_     ;   GS 
       .byte  <nop_     ;   RS 
       .byte  <nop_     ;   nos 
       .byte  <nop_     ;   SP 
       .byte  <store_   ;   ! 
       .byte  <dup_     ;   " 
       .byte  <hex_    ;    # 
       .byte  <swap_   ;    $ 
       .byte  <over_   ;    % 
       .byte  <and_    ;    & 
       .byte  <drop_   ;    ' 
       .byte  <begin_  ;    ( 
       .byte  <again_  ;    ) 
       .byte  <mul_    ;    * 
       .byte  <add_    ;    + 
       .byte  <hdot_   ;    , 
       .byte  <sub_    ;    - 
       .byte  <dot_    ;    . 
       .byte  <div_    ;    / 
       .byte  <num_    ;    0 
       .byte  <num_    ;    1 
       .byte  <num_    ;    2 
       .byte  <num_    ;    3 
       .byte  <num_    ;    4 
       .byte  <num_    ;    5 
       .byte  <num_    ;    6 
       .byte  <num_    ;    7 
       .byte  <num_    ;    8 
       .byte  <num_    ;    9 
       .byte  <def_    ;    : 
       .byte  <ret_    ;    ; 
       .byte  <lt_     ;    < 
       .byte  <eq_     ;    = 
       .byte  <gt_     ;    > 
       .byte  <getRef_ ;    ? 
       .byte  <fetch_  ;    @ 
       .byte  <call_    ;    A 
       .byte  <call_    ;    B 
       .byte  <call_    ;    C 
       .byte  <call_    ;    D 
       .byte  <call_    ;    E 
       .byte  <call_    ;    F 
       .byte  <call_    ;    G 
       .byte  <call_    ;    H 
       .byte  <call_    ;    I 
       .byte  <call_    ;    J 
       .byte  <call_    ;    K 
       .byte  <call_    ;    L 
       .byte  <call_    ;    M 
       .byte  <call_    ;    N 
       .byte  <call_    ;    O 
       .byte  <call_    ;    P 
       .byte  <call_    ;    Q 
       .byte  <call_    ;    R 
       .byte  <call_    ;    S 
       .byte  <call_    ;    T 
       .byte  <call_    ;    U 
       .byte  <call_    ;    V 
       .byte  <call_    ;    W 
       .byte  <call_    ;    X 
       .byte  <call_    ;    Y 
       .byte  <call_    ;    Z 
       .byte  <arrDef_ ;    [ 
       .byte  <alt_    ;    \ 
       .byte  <arrEnd_ ;    ] 
       .byte  <xor_    ;    ^ 
       .byte  <neg_    ;    _ 
       .byte  <str_    ;    ` 
       .byte  <var_    ;    a 
       .byte  <var_    ;    b 
       .byte  <var_    ;    c 
       .byte  <var_    ;    d 
       .byte  <var_    ;    e 
       .byte  <var_    ;    f 
       .byte  <var_    ;    g 
       .byte  <var_    ;    h 
       .byte  <var_    ;    i 
       .byte  <var_    ;    j 
       .byte  <var_    ;    k 
       .byte  <var_    ;    l 
       .byte  <var_    ;    m 
       .byte  <var_    ;    n 
       .byte  <var_    ;    o 
       .byte  <var_    ;    p 
       .byte  <var_    ;    q 
       .byte  <var_    ;    r 
       .byte  <var_    ;    s 
       .byte  <var_    ;    t 
       .byte  <var_    ;    u 
       .byte  <var_    ;    v 
       .byte  <var_    ;    w 
       .byte  <var_    ;    x 
       .byte  <var_    ;    y 
       .byte  <var_    ;    z 
       .byte  <shl_    ;    { 
       .byte  <or_     ;    | 
       .byte  <shr_    ;    } 
       .byte  <inv_    ;    ~ 
       .byte  <nop_    ;    backspace 
 
optcodeshi: 
       .byte  >exit_    ;   NUL 
       .byte  >nop_     ;   SOH 
       .byte  >nop_     ;   STX 
       .byte  >etx_     ;   ETX 
       .byte  >nop_     ;   EOT 
       .byte  >nop_     ;   ENQ 
       .byte  >nop_     ;   apK 
       .byte  >nop_     ;   BEL 
       .byte  >nop_     ;   BS 
       .byte  >nop_     ;   TAB 
       .byte  >nop_     ;   LF 
       .byte  >nop_     ;   VT 
       .byte  >nop_     ;   FF 
       .byte  >nop_     ;   CR 
       .byte  >nop_     ;   SO 
       .byte  >nop_     ;   SI 
       .byte  >nop_     ;   DLE 
       .byte  >nop_     ;   DC1 
       .byte  >nop_     ;   DC2 
       .byte  >nop_     ;   DC3 
       .byte  >nop_     ;   DC4 
       .byte  >nop_     ;   NAK 
       .byte  >nop_     ;   SYN 
       .byte  >nop_     ;   ETB 
       .byte  >nop_     ;   CAN 
       .byte  >nop_     ;   EM 
       .byte  >nop_     ;   SUB 
       .byte  >nop_     ;   ESC 
       .byte  >nop_     ;   FS 
       .byte  >nop_     ;   GS 
       .byte  >nop_     ;   RS 
       .byte  >nop_     ;   nos 
       .byte  >nop_     ;   SP 
       .byte  >store_   ;   ! 
       .byte  >dup_     ;   " 
       .byte  >hex_    ;    # 
       .byte  >swap_   ;    $ 
       .byte  >over_   ;    % 
       .byte  >and_    ;    & 
       .byte  >drop_   ;    ' 
       .byte  >begin_  ;    ( 
       .byte  >again_  ;    ) 
       .byte  >mul_    ;    * 
       .byte  >add_    ;    + 
       .byte  >hdot_   ;    , 
       .byte  >sub_    ;    - 
       .byte  >dot_    ;    . 
       .byte  >div_    ;    / 
       .byte  >num_    ;    0 
       .byte  >num_    ;    1 
       .byte  >num_    ;    2 
       .byte  >num_    ;    3 
       .byte  >num_    ;    4 
       .byte  >num_    ;    5 
       .byte  >num_    ;    6 
       .byte  >num_    ;    7 
       .byte  >num_    ;    8 
       .byte  >num_    ;    9 
       .byte  >def_    ;    : 
       .byte  >ret_    ;    ; 
       .byte  >lt_     ;    < 
       .byte  >eq_     ;    = 
       .byte  >gt_     ;    > 
       .byte  >getRef_ ;    ? 
       .byte  >fetch_  ;    @ 
       .byte  >call_    ;    A 
       .byte  >call_    ;    B 
       .byte  >call_    ;    C 
       .byte  >call_    ;    D 
       .byte  >call_    ;    E 
       .byte  >call_    ;    F 
       .byte  >call_    ;    G 
       .byte  >call_    ;    H 
       .byte  >call_    ;    I 
       .byte  >call_    ;    J 
       .byte  >call_    ;    K 
       .byte  >call_    ;    L 
       .byte  >call_    ;    M 
       .byte  >call_    ;    N 
       .byte  >call_    ;    O 
       .byte  >call_    ;    P 
       .byte  >call_    ;    Q 
       .byte  >call_    ;    R 
       .byte  >call_    ;    S 
       .byte  >call_    ;    T 
       .byte  >call_    ;    U 
       .byte  >call_    ;    V 
       .byte  >call_    ;    W 
       .byte  >call_    ;    X 
       .byte  >call_    ;    Y 
       .byte  >call_    ;    Z 
       .byte  >arrDef_ ;    [ 
       .byte  >alt_    ;    \ 
       .byte  >arrEnd_ ;    ] 
       .byte  >xor_    ;    ^ 
       .byte  >neg_    ;    _ 
       .byte  >str_    ;    ` 
       .byte  >var_    ;    a 
       .byte  >var_    ;    b 
       .byte  >var_    ;    c 
       .byte  >var_    ;    d 
       .byte  >var_    ;    e 
       .byte  >var_    ;    f 
       .byte  >var_    ;    g 
       .byte  >var_    ;    h 
       .byte  >var_    ;    i 
       .byte  >var_    ;    j 
       .byte  >var_    ;    k 
       .byte  >var_    ;    l 
       .byte  >var_    ;    m 
       .byte  >var_    ;    n 
       .byte  >var_    ;    o 
       .byte  >var_    ;    p 
       .byte  >var_    ;    q 
       .byte  >var_    ;    r 
       .byte  >var_    ;    s 
       .byte  >var_    ;    t 
       .byte  >var_    ;    u 
       .byte  >var_    ;    v 
       .byte  >var_    ;    w 
       .byte  >var_    ;    x 
       .byte  >var_    ;    y 
       .byte  >var_    ;    z 
       .byte  >shl_    ;    { 
       .byte  >or_     ;    | 
       .byte  >shr_    ;    } 
       .byte  >inv_    ;    ~ 
       .byte  >nop_    ;    backspace 
 
;---------------------------------------------------------------------- 
; alternate function codes 
ctlcodeslo: 
altcodeslo: 
       .byte  <empty_      ; NUL ^@ 
       .byte  <empty_      ; SOH ^A 
       .byte  <toggleBase_ ; STX ^B 
       .byte  <empty_      ; ETX ^C 
       .byte  <empty_      ; EOT ^D 
       .byte  <edit_       ; ENQ ^E 
       .byte  <empty_      ; ACK ^F 
       .byte  <empty_      ; BEL ^G 
       .byte  <backsp_     ; BS  ^H 
       .byte  <empty_      ; TAB ^I 
       .byte  <reedit_     ; LF  ^J 
       .byte  <empty_      ; VT  ^K 
       .byte  <list_       ; FF  ^L 
       .byte  <empty_      ; CR  ^M 
       .byte  <empty_      ; SO  ^N 
       .byte  <empty_      ; SI  ^O 
       .byte  <printStack_ ; DLE ^P 
       .byte  <empty_      ; DC1 ^Q 
       .byte  <empty_      ; DC2 ^R 
       .byte  <empty_      ; DC3 ^S 
       .byte  <empty_      ; DC4 ^T 
       .byte  <empty_      ; NAK ^U 
       .byte  <empty_      ; SYN ^V 
       .byte  <empty_      ; ETB ^W 
       .byte  <empty_      ; CAN ^X 
       .byte  <empty_      ; EM  ^Y 
       .byte  <empty_      ; SUB ^Z 
       .byte  <empty_      ; ESC ^[ 
       .byte  <empty_      ; FS  ^\ 
       .byte  <empty_      ; GS  ^] 
       .byte  <empty_      ; RS  ^^ 
       .byte  <empty_      ; nos  ^_) 
       .byte  <aNop_       ; SP  ^` 
       .byte  <cStore_     ;    ! 
       .byte  <aNop_       ;    " 
       .byte  <aNop_       ;    # 
       .byte  <aNop_       ;    $  ( -- adr ) text input ptr 
       .byte  <aNop_       ;    % 
       .byte  <aNop_       ;    & 
       .byte  <aNop_       ;    ' 
       .byte  <ifte_       ;    (  ( b -- ) 
       .byte  <aNop_       ;    ) 
       .byte  <aNop_       ;    * 
       .byte  <incr_       ;    +  ( adr -- ) decrements variable at address 
       .byte  <aNop_       ;    , 
       .byte  <aNop_       ;    - 
       .byte  <aNop_       ;    . 
       .byte  <aNop_       ;    / 
       .byte  <aNop_       ;    0 
       .byte  <aNop_       ;    1 
       .byte  <aNop_       ;    2 
       .byte  <aNop_       ;    3 
       .byte  <aNop_       ;    4 
       .byte  <aNop_       ;    5 
       .byte  <aNop_       ;    6 
       .byte  <aNop_       ;    7 
       .byte  <aNop_       ;    8 
       .byte  <aNop_       ;    9 
       .byte  <aNop_       ;    :  start defining a macro 
       .byte  <aNop_       ;    ; 
       .byte  <aNop_       ;    < 
       .byte  <aNop_       ;    = 
       .byte  <aNop_       ;    > 
       .byte  <aNop_       ;    ? 
       .byte  <cFetch_     ;    @ 
       .byte  <aNop_       ;    A 
       .byte  <break_      ;    B 
       .byte  <nop_        ;    C 
       .byte  <depth_      ;    D  ( -- val ) depth of data stack 
       .byte  <emit_       ;    E  ( val -- ) emits a char to output 
       .byte  <aNop_       ;    F 
       .byte  <go_         ;    G  ( -- ? ) execute mint definition 
       .byte  <keyq_       ;    H  ( verify if key hit )
       .byte  <inPort_     ;    I  ( port -- val ) 
       .byte  <aNop_       ;    J 
       .byte  <key_        ;    K  ( -- val )  read a char from input 
       .byte  <aNop_       ;    L 
       .byte  <aNop_       ;    M 
       .byte  <newln_      ;    N  ; prints a newline to output 
       .byte  <outPort_    ;    O  ( val port -- ) 
       .byte  <printStk_   ;    P  ( -- ) non-destructively prints stack 
       .byte  <aNop_       ;    Q  quits from Mint REPL 
       .byte  <rot_        ;    R  ( a b c -- b c a ) 
       .byte  <aNop_       ;    S 
       .byte  <aNop_       ;    T 
       .byte  <R2S_       ;    U  S( -- w ) R( w -- ) 
       .byte  <S2R_       ;    V  S( w -- ) R( -- w )
       .byte  <aNop_       ;    W   ; ( b -- ) if false, skip to end of loop 
       .byte  <exec_       ;    X 
       .byte  <aNop_       ;    Y 
       .byte  <editDef_    ;    Z 
       .byte  <cArrDef_    ;    [ 
       .byte  <comment_    ;    \  comment text, skips reading until end of line 
       .byte  <aNop_       ;    ] 
       .byte  <charCode_   ;    ^ 
       .byte  <aNop_       ;    _ 
       .byte  <aNop_       ;    ` 
       .byte  <sysVar_     ;    a  ; start of data stack *fixed 
       .byte  <sysVar_     ;    b  ; base16 flag 
       .byte  <sysVar_     ;    c  ; TIBPtr variable 
       .byte  <sysVar_     ;    d  ; vDefs variable
       .byte  <sysVar_     ;    e  ; 
       .byte  <sysVar_     ;    f  ; start of return stack *fixed
       .byte  <sysVar_     ;    g  ; next dispatcher 
       .byte  <sysVar_     ;    h  ; heap ptr variable 
       .byte  <i_          ;    i  ; returns index of current loop 
       .byte  <j_          ;    j  ; returns index of outer loop 
       .byte  <sysVar_     ;    k 
       .byte  <sysVar_     ;    l 
       .byte  <sysVar_     ;    m  ( a b -- c ) return the minimum value 
       .byte  <sysVar_     ;    n 
       .byte  <sysVar_     ;    o 
       .byte  <sysVar_     ;    p 
       .byte  <sysVar_     ;    q 
       .byte  <sysVar_     ;    r  ; return stack pointer
       .byte  <sysVar_     ;    s  ; data stack pointer
       .byte  <sysVar_     ;    t 
       .byte  <sysVar_     ;    u 
       .byte  <sysVar_     ;    v 
       .byte  <sysVar_     ;    w 
       .byte  <sysVar_     ;    x 
       .byte  <sysVar_     ;    y 
       .byte  <sysVar_     ;    z 
       .byte  <group_      ;    { 
       .byte  <aNop_       ;    | 
       .byte  <endGroup_   ;    } 
       .byte  <aNop_       ;    ~ 
       .byte  <aNop_       ;    BS 
 
ctlcodeshi: 
altcodeshi: 
       .byte  >empty_      ; NUL ^@ 
       .byte  >empty_      ; SOH ^A 
       .byte  >toggleBase_ ; STX ^B 
       .byte  >empty_      ; ETX ^C 
       .byte  >empty_      ; EOT ^D 
       .byte  >edit_       ; ENQ ^E 
       .byte  >empty_      ; ACK ^F 
       .byte  >empty_      ; BEL ^G 
       .byte  >backsp_     ; BS  ^H 
       .byte  >empty_      ; TAB ^I 
       .byte  >reedit_     ; LF  ^J 
       .byte  >empty_      ; VT  ^K 
       .byte  >list_       ; FF  ^L 
       .byte  >empty_      ; CR  ^M 
       .byte  >empty_      ; SO  ^N 
       .byte  >empty_      ; SI  ^O 
       .byte  >printStack_ ; DLE ^P 
       .byte  >empty_      ; DC1 ^Q 
       .byte  >empty_      ; DC2 ^R 
       .byte  >empty_      ; DC3 ^S 
       .byte  >empty_      ; DC4 ^T 
       .byte  >empty_      ; NAK ^U 
       .byte  >empty_      ; SYN ^V 
       .byte  >empty_      ; ETB ^W 
       .byte  >empty_      ; CAN ^X 
       .byte  >empty_      ; EM  ^Y 
       .byte  >empty_      ; SUB ^Z 
       .byte  >empty_      ; ESC ^[ 
       .byte  >empty_      ; FS  ^\ 
       .byte  >empty_      ; GS  ^] 
       .byte  >empty_      ; RS  ^^ 
       .byte  >empty_      ; nos  ^_) 
       .byte  >aNop_       ; SP  ^` 
       .byte  >cStore_     ;    ! 
       .byte  >aNop_       ;    " 
       .byte  >aNop_       ;    # 
       .byte  >aNop_       ;    $  ( -- adr ) text input ptr 
       .byte  >aNop_       ;    % 
       .byte  >aNop_       ;    & 
       .byte  >aNop_       ;    ' 
       .byte  >ifte_       ;    (  ( b -- ) 
       .byte  >aNop_       ;    ) 
       .byte  >aNop_       ;    * 
       .byte  >incr_       ;    +  ( adr -- ) decrements variable at address 
       .byte  >aNop_       ;    , 
       .byte  >aNop_       ;    - 
       .byte  >aNop_       ;    . 
       .byte  >aNop_       ;    / 
       .byte  >aNop_       ;    0 
       .byte  >aNop_       ;    1 
       .byte  >aNop_       ;    2 
       .byte  >aNop_       ;    3 
       .byte  >aNop_       ;    4 
       .byte  >aNop_       ;    5 
       .byte  >aNop_       ;    6 
       .byte  >aNop_       ;    7 
       .byte  >aNop_       ;    8 
       .byte  >aNop_       ;    9 
       .byte  >aNop_       ;    :  start defining a macro 
       .byte  >aNop_       ;    ; 
       .byte  >aNop_       ;    < 
       .byte  >aNop_       ;    = 
       .byte  >aNop_       ;    > 
       .byte  >aNop_       ;    ? 
       .byte  >cFetch_     ;    @ 
       .byte  >aNop_       ;    A 
       .byte  >break_      ;    B 
       .byte  >nop_        ;    C 
       .byte  >depth_      ;    D  ( -- val ) depth of data stack 
       .byte  >emit_       ;    E  ( val -- ) emits a char to output 
       .byte  >aNop_       ;    F 
       .byte  >go_         ;    G  ( -- ? ) execute mint definition 
       .byte  >keyq_       ;    H  ( verify if key hit )
       .byte  >inPort_     ;    I  ( port -- val ) 
       .byte  >aNop_       ;    J 
       .byte  >key_        ;    K  ( -- val )  read a char from input 
       .byte  >aNop_       ;    L 
       .byte  >aNop_       ;    M 
       .byte  >newln_      ;    N  ; prints a newline to output 
       .byte  >outPort_    ;    O  ( val port -- ) 
       .byte  >printStk_   ;    P  ( -- ) non-destructively prints stack 
       .byte  >aNop_       ;    Q  quits from Mint REPL 
       .byte  >rot_        ;    R  ( a b c -- b c a ) 
       .byte  >aNop_       ;    S 
       .byte  >aNop_       ;    T 
       .byte  >R2S_       ;    U  S( -- w ) R( w -- ) 
       .byte  >S2R_       ;    V  S( w -- ) R( -- w )
       .byte  >aNop_       ;    W   ; ( b -- ) if false, skip to end of loop 
       .byte  >exec_       ;    X 
       .byte  >aNop_       ;    Y 
       .byte  >editDef_    ;    Z 
       .byte  >cArrDef_    ;    [ 
       .byte  >comment_    ;    \  comment text, skips reading until end of line 
       .byte  >aNop_       ;    ] 
       .byte  >charCode_   ;    ^ 
       .byte  >aNop_       ;    _ 
       .byte  >aNop_       ;    ` 
       .byte  >sysVar_     ;    a  ; start of data stack *fixed 
       .byte  >sysVar_     ;    b  ; base16 flag 
       .byte  >sysVar_     ;    c  ; TIBPtr variable 
       .byte  >sysVar_     ;    d  ; vDefs variable
       .byte  >sysVar_     ;    e  ; 
       .byte  >sysVar_     ;    f  ; start of return stack *fixed
       .byte  >sysVar_     ;    g  ; next dispatcher 
       .byte  >sysVar_     ;    h  ; heap ptr variable 
       .byte  >i_          ;    i  ; returns index of current loop 
       .byte  >j_          ;    j  ; returns index of outer loop 
       .byte  >sysVar_     ;    k 
       .byte  >sysVar_     ;    l 
       .byte  >sysVar_     ;    m  ( a b -- c ) return the minimum value 
       .byte  >sysVar_     ;    n 
       .byte  >sysVar_     ;    o 
       .byte  >sysVar_     ;    p 
       .byte  >sysVar_     ;    q 
       .byte  >sysVar_     ;    r  ; return stack pointer
       .byte  >sysVar_     ;    s  ; data stack pointer
       .byte  >sysVar_     ;    t 
       .byte  >sysVar_     ;    u 
       .byte  >sysVar_     ;    v 
       .byte  >sysVar_     ;    w 
       .byte  >sysVar_     ;    x 
       .byte  >sysVar_     ;    y 
       .byte  >sysVar_     ;    z 
       .byte  >group_      ;    { 
       .byte  >aNop_       ;    | 
       .byte  >endGroup_   ;    } 
       .byte  >aNop_       ;    ~ 
       .byte  >aNop_       ;    BS 
 
; ********************************************************************* 
; Macros must be written in Mint and end with ; 
; this code must not span pages 
; ********************************************************************* 
macros: 
 
.include "6502.MINT.macros.asm" 
 
; heap must be here ! 
 
heap: 
 
