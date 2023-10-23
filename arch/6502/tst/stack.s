
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

ptr:    .word $0

ptr_lo: .word $0
ptr_hi: .word $0

lsb:    .byte $0
msb:    .byte $0

ind:    .byte $0
lmt:    .byte $0

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
  STA ptr, X
  DEX
  LDA lsb
  STA ptr, X
  STX ind

pull_ZX:
  LDX ind
  LDA ptr, X
  STA msb
  INX
  LDA ptr, X
  STA lsb
  INX
  STX ind

;## indirect by page zero indexed by Y

push_ZY:
  LDY ind
  DEY
  LDA msb
  STA (ptr), Y
  DEY
  LDA lsb
  STA (ptr), Y
  STY ind

pull_ZY:
  LDY ind
  LDA (ptr), Y
  STA msb
  INY
  LDA (ptr), Y
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
  STA (ptr), Y
  INC ptr + 0
  BNE :+ 
  INC ptr + 1
: 
  LDA lsb
  STA (ptr), Y
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
  LDA (ptr), Y
  STA msb
  LDA ptr + 0
  BNE :+ 
  DEC ptr + 1
: 
  DEC ptr + 0
  LDA (ptr), Y
  STA lsb


