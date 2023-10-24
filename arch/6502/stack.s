
;--------------------------------------------------------
;
;   specifc for ca65 assembler
;
;--------------------------------------------------------

; enable listing

.list on

; identifiers

.case +

; debug

.debuginfo -

; enable features

.feature c_comments

.feature string_escapes

.feature org_per_seg

.feature dollar_is_pc

.feature pc_assignment

; enable 6502 mode

.p02

;--------------------------------------------------------

.segment "ZERO"

* = $0000

ptz:    .byte $0
ivi:    .byte $0

lsb:    .byte $0
msb:    .byte $0

ind:    .byte $0
lmt:    .byte $0

;--------------------------------------------------------

.segment "CODE"

ptr:    .word $0

ptr_lo: .word $0

ptr_hi: .word $0

;--------------------------------------------------------
.segment "VECTORS"

.word init
.word init
.word init

;--------------------------------------------------------

.segment "ONCE"

; ### at hardware stack

push_SP:
  LDA ind
  TSX
  STX ind
  TAX
  TXS
  LDA lsb
  PHA
  LDA msb
  PHA
  LDA ind
  TSX
  STX ind
  TAX
  TXS

pull_SP:
  LDA ind
  TSX
  STX ind
  TAX
  TXS
  PLA
  STA msb
  PLA
  STA lsb
  LDA ind
  TSX
  STX ind
  TAX
  TXS

;## at page zero indexed by X

push_ZX:
  LDX ind
  DEX
  LDA msb
  STA ptz, X
  DEX
  LDA lsb
  STA ptz, X
  STX ind

pull_ZX:
  LDX ind
  LDA ptz, X
  STA msb
  INX
  LDA ptz, X
  STA lsb
  INX
  STX ind

;## indirect by page zero indexed by Y

push_ZY:
  LDY ind
  DEY
  LDA msb
  STA (ptz), Y
  DEY
  LDA lsb
  STA (ptz), Y
  STY ind

pull_ZY:
  LDY ind
  LDA (ptz), Y
  STA msb
  INY
  LDA (ptz), Y
  STA lsb
  INY
  STY ind

;## absolute address indexed by Y

push_IY:
  LDY ind
  LDA msb
  STA ptr - 1, Y
  LDA lsb
  STA ptr - 2, Y
  DEY
  DEY
  STY ind

pull_IY:
  LDY ind
  LDA ptr + 0, Y
  STA lsb
  LDA ptr + 1, Y
  STA msb
  INY
  INY
  STY ind

;## split absolute addres indexed by Y

push_IY2: 
  LDY ind
  LDA msb
  STA ptr_lo - 1, Y
  LDA lsb
  STA ptr_hi - 1, Y
  DEY
  STY ind

pull_IY2: 
  LDY ind
  LDA ptr_lo + 0, Y
  STA lsb
  LDA ptr_hi + 0, Y
  STA msb
  INY
  STY ind

;## direct address with indirect access

push_DI:
  LDY #0
  LDA msb
  STA (ptz), Y
  INC ptr + 0
  BNE :+ 
  INC ptr + 1
: 
  LDA lsb
  STA (ptz), Y
  INC ptr + 0
  BNE :+ 
  INC ptr + 1

pull_DI:
  LDY #0
  LDA ptr + 0
  BNE :+ 
  DEC ptr + 1
: 
  DEC ptr + 0
  LDA (ptz), Y
  STA msb
  LDA ptr + 0
  BNE :+ 
  DEC ptr + 1
: 
  DEC ptr + 0
  LDA (ptz), Y
  STA lsb

init:
    jmp init
