ca65 V2.19 - Git 7979f8a41
Main file   : main.s
Current file: main.s

000000r 1               ; vim: filetype=asm sw=4 ts=4 autoindent expandtab shiftwidth=4 et:
000000r 1               
000000r 1               ; *********************************************************************
000000r 1               ;
000000r 1               ;  MINT Minimal Interpreter
000000r 1               ;
000000r 1               ;  GNU GENERAL PUBLIC LICENSE              Version 3, 29 June 2007
000000r 1               ;
000000r 1               ;  see the LICENSE file in this repo for more information
000000r 1               ;
000000r 1               ;  original for the Z80, by Ken Boak, John Hardy and Craig Jones.
000000r 1               ;
000000r 1               ;  adapted for the 6502, by Alvaro G. S. Barcellos, 10/2023
000000r 1               ;  (some code from 6502.org forum and FIG_Forth)
000000r 1               ;  (some code from https://www.nesdev.org/wiki/Programming_guide)
000000r 1               
000000r 1               ;  star(tm) date 10/10/2023
000000r 1               ; *********************************************************************
000000r 1               
000000r 1               ;--------------------------------------------------------
000000r 1               ;
000000r 1               ;   specifc for ca65 assembler
000000r 1               ;
000000r 1               ;--------------------------------------------------------
000000r 1               ; enable listing
000000r 1               
000000r 1               .list on
000000r 1               
000000r 1               ; identifiers
000000r 1               
000000r 1               .case +
000000r 1               
000000r 1               ; debug
000000r 1               
000000r 1               .debuginfo -
000000r 1               
000000r 1               ; enable features
000000r 1               
000000r 1               .feature c_comments
000000r 1               
000000r 1               .feature string_escapes
000000r 1               
000000r 1               .feature org_per_seg
000000r 1               
000000r 1               .feature dollar_is_pc
000000r 1               
000000r 1               .feature pc_assignment
000000r 1               
000000r 1               ; enable 6502 mode
000000r 1               
000000r 1               .p02
000000r 1               
000000r 1               
000000r 1               ;--------------------------------------------------------
000000r 1               ;
000000r 1               ;   constants, must be.
000000r 1               ;
000000r 1               ;--------------------------------------------------------
000000r 1               
000000r 1                   TRUE        = 1
000000r 1                   FALSE       = 0
000000r 1               
000000r 1                   CR  = 13
000000r 1                   LF  = 10
000000r 1                   BS  = 9
000000r 1                   ETX = 3
000000r 1                   NUL = 0
000000r 1               
000000r 1                   ; size page
000000r 1                   PAGE = $100
000000r 1               
000000r 1                   ; group size
000000r 1                   GRPSIZE = $40
000000r 1               
000000r 1                   ; groups for defs, could be more
000000r 1                   NUMGRPS = 5
000000r 1               
000000r 1               ;----------------------------------------------------------------------
000000r 1               .segment "ZERO"
000000r 1               ; offset
000000r 1               * = $00F0
0000F0  1               
0000F0  1               ; instruction pointer
0000F0  1  00 00        ipt:    .addr $0
0000F2  1               ; index for data stack
0000F2  1  00           isp:    .byte $0
0000F3  1               ; index for return stack
0000F3  1  00           irp:    .byte $0
0000F4  1               ; copycat for registers
0000F4  1  00           yp: 	.byte $0
0000F5  1  00           xp: 	.byte $0
0000F6  1  00           ap: 	.byte $0
0000F7  1  00           ns: 	.byte $0
0000F8  1               ; pseudo registers
0000F8  1  00 00        tos:    .word $0
0000FA  1  00 00        nos:    .word $0
0000FC  1  00 00        wrk:    .word $0
0000FE  1  00 00        tmp:    .word $0
000100  1               
000100  1               ;----------------------------------------------------------------------
000100  1               .segment "VECTORS"
000000r 1               
000000r 1  rr rr        .word init
000002r 1  rr rr        .word init
000004r 1  rr rr        .word init
000006r 1               
000006r 1               ;----------------------------------------------------------------------
000006r 1               .segment "CODE"
000000r 1               
000000r 1               ; start of RAM
000000r 1               
000000r 1               ;.align $100
000000r 1               
000000r 1               VOID:
000000r 1               
000000r 1               ; data stack
000000r 1  00 00 00 00      .res PAGE, $00
000004r 1  00 00 00 00  
000008r 1  00 00 00 00  
000100r 1               aps:
000100r 1               
000100r 1               ; return stack
000100r 1  00 00 00 00      .res PAGE, $00
000104r 1  00 00 00 00  
000108r 1  00 00 00 00  
000200r 1               apr:
000200r 1               
000200r 1               ; terminal input buffer
000200r 1               tib:
000200r 1  00 00 00 00      .res PAGE, $00
000204r 1  00 00 00 00  
000208r 1  00 00 00 00  
000300r 1               
000300r 1               ; mint variables, 26 plus 6 from z
000300r 1               vsys:
000300r 1  00 00 00 00      .res GRPSIZE, $00
000304r 1  00 00 00 00  
000308r 1  00 00 00 00  
000340r 1               
000340r 1               ; user variable, 26 plus 6 from z
000340r 1               vars:
000340r 1  00 00 00 00      .res GRPSIZE, $00
000344r 1  00 00 00 00  
000348r 1  00 00 00 00  
000380r 1               
000380r 1               ; user function groups, each with 26 plus 6 from Z
000380r 1               defs:
000380r 1  00 00 00 00      .res GRPSIZE * NUMGRPS, $00
000384r 1  00 00 00 00  
000388r 1  00 00 00 00  
0004C0r 1               
0004C0r 1               ; internals
0004C0r 1               
0004C0r 1               vEdited:
0004C0r 1  00               .byte $0
0004C1r 1               
0004C1r 1               vByteMode:
0004C1r 1  00               .byte $0
0004C2r 1               
0004C2r 1               ; heap must be here !
0004C2r 1               heap:
0004C2r 1  00 00            .addr $0
0004C4r 1               
0004C4r 1               ;----------------------------------------------------------------------
0004C4r 1               ; aliases
0004C4r 1               
0004C4r 1               vS0      =  vsys + $00     ;    a  ; start of data stack
0004C4r 1               vBase16  =  vsys + $02     ;    b  ; base16 flag
0004C4r 1               vTIBPtr  =  vsys + $04     ;    c  ; TIBPtr variable
0004C4r 1               vDefs    =  vsys + $08     ;    d  ; reference for group user functions
0004C4r 1               ;        =  vsys + $0a     ;    e  ;
0004C4r 1               vR0      =  vsys + $0c     ;    f  ; start of return stack
0004C4r 1               vNext    =  vsys + $0e     ;    g  ; next routine dispatcher
0004C4r 1               vHeap =  vsys + $10     ;    h  ; heap ptr variable
0004C4r 1               
0004C4r 1               ; the address of stacks are hardcoded, any change do no apply
0004C4r 1               dStack = vS0
0004C4r 1               rStack = vR0
0004C4r 1               ; any change will cause unexpected behavior
0004C4r 1               HEAP = heap
0004C4r 1               DEFS = defs
0004C4r 1               
0004C4r 1               ;----------------------------------------------------------------------
0004C4r 1               .segment "ONCE"
000000r 1               
000000r 1               ; start of ROM
000000r 1               
000000r 1               init:
000000r 1  4C rr rr         jmp mint_
000003r 1  4D 49 4E 54      .asciiz "MINT@6502"
000007r 1  40 36 35 30  
00000Br 1  32 00        
00000Dr 1               
00000Dr 1               ;----------------------------------------------------------------------
00000Dr 1               ;    depends on hardware
00000Dr 1               ;----------------------------------------------------------------------
00000Dr 1               putchar:
00000Dr 1  18               clc
00000Er 1  60               rts
00000Fr 1               
00000Fr 1               getchar:
00000Fr 1  18               clc
000010r 1  60               rts
000011r 1               
000011r 1               hitchar:
000011r 1  18               clc
000012r 1  60               rts
000013r 1               
000013r 1               ;----------------------------------------------------------------------
000013r 1               ; get a char
000013r 1               key_:
000013r 1  20 rr rr         jsr getchar
000016r 1               keyk:
000016r 1  85 F8            sta tos + 0
000018r 1  20 rr rr         jsr spush
00001Br 1                   ; next
00001Br 1  6C rr rr         jmp (vNext)
00001Er 1               
00001Er 1               ;----------------------------------------------------------------------
00001Er 1               ; put a char
00001Er 1               emit_:
00001Er 1  20 rr rr         jsr spull
000021r 1  A5 F8            lda tos + 0
000023r 1  20 rr rr         jsr putchar
000026r 1                   ; next
000026r 1  6C rr rr         jmp (vNext)
000029r 1               
000029r 1               ;----------------------------------------------------------------------
000029r 1               ; hit a char ?
000029r 1               keyq_:
000029r 1  20 rr rr         jsr hitchar
00002Cr 1  B8               clv
00002Dr 1  50 E7            bvc keyk
00002Fr 1               
00002Fr 1               ;------------------------------------------------------------------------------
00002Fr 1               ;   data stack stuff
00002Fr 1               
00002Fr 1               keep_: ; to push
00002Fr 1                   ; ldx isp
00002Fr 1  CA               dex
000030r 1  CA               dex
000031r 1  86 F2            stx isp
000033r 1  60               rts
000034r 1               
000034r 1               lose_: ; to pull
000034r 1                   ; ldx isp
000034r 1  E8               inx
000035r 1  E8               inx
000036r 1  86 F2            stx isp
000038r 1  60               rts
000039r 1               
000039r 1               spush:
000039r 1               push_:
000039r 1  A6 F2            ldx isp
00003Br 1  A5 F8            lda tos + 0
00003Dr 1  9D rr rr         sta aps - 2, x
000040r 1  A5 F9            lda tos + 1
000042r 1  9D rr rr         sta aps - 1, x
000045r 1  4C rr rr         jmp keep_
000048r 1               
000048r 1               spull:
000048r 1               pull_:
000048r 1  A6 F2            ldx isp
00004Ar 1  BD rr rr         lda aps + 0, x
00004Dr 1  85 F8            sta tos + 0
00004Fr 1  BD rr rr         lda aps + 1, x
000052r 1  85 F9            sta tos + 1
000054r 1  4C rr rr         jmp lose_
000057r 1               
000057r 1               push2_:
000057r 1  A6 F2            ldx isp
000059r 1  A5 FA            lda nos + 0
00005Br 1  9D rr rr         sta aps - 4, x
00005Er 1  A5 FB            lda nos + 1
000060r 1  9D rr rr         sta aps - 3, x
000063r 1  A5 F8            lda tos + 0
000065r 1  9D rr rr         sta aps - 2, x
000068r 1  A5 F9            lda tos + 1
00006Ar 1  9D rr rr         sta aps - 1, x
00006Dr 1  20 rr rr         jsr keep_
000070r 1  4C rr rr         jmp keep_
000073r 1               
000073r 1               take2:
000073r 1               pull2_:
000073r 1  A6 F2            ldx isp
000075r 1  BD rr rr         lda aps + 0, x
000078r 1  85 F8            sta tos + 0
00007Ar 1  BD rr rr         lda aps + 1, x
00007Dr 1  85 F9            sta tos + 1
00007Fr 1  BD rr rr         lda aps + 2, x
000082r 1  85 FA            sta nos + 0
000084r 1  BD rr rr         lda aps + 3, x
000087r 1  85 FB            sta nos + 1
000089r 1  20 rr rr         jsr lose_
00008Cr 1  4C rr rr         jmp lose_
00008Fr 1               
00008Fr 1               drop_:
00008Fr 1  A6 F2            ldx isp
000091r 1  20 rr rr         jsr lose_
000094r 1                   ; rts
000094r 1  6C rr rr         jmp (vNext)
000097r 1               
000097r 1               dup_:
000097r 1  A6 F2            ldx isp
000099r 1  BD rr rr         lda aps + 0, x
00009Cr 1  8D rr rr         sta aps - 2
00009Fr 1  BD rr rr         lda aps + 1, x
0000A2r 1  8D rr rr         sta aps - 1
0000A5r 1  20 rr rr         jsr keep_
0000A8r 1                   ; rts
0000A8r 1  6C rr rr         jmp (vNext)
0000ABr 1               
0000ABr 1               over_:
0000ABr 1  A6 F2            ldx isp
0000ADr 1  BD rr rr         lda aps + 2, x
0000B0r 1  8D rr rr         sta aps - 2
0000B3r 1  BD rr rr         lda aps + 3, x
0000B6r 1  8D rr rr         sta aps - 1
0000B9r 1  20 rr rr         jsr keep_
0000BCr 1                   ; rts
0000BCr 1  6C rr rr         jmp (vNext)
0000BFr 1               
0000BFr 1               swap_:
0000BFr 1  A6 F2            ldx isp
0000C1r 1  BD rr rr         lda aps + 0, x
0000C4r 1  8D rr rr         sta aps - 2
0000C7r 1  BD rr rr         lda aps + 1, x
0000CAr 1  8D rr rr         sta aps - 1
0000CDr 1  BD rr rr         lda aps + 2, x
0000D0r 1  8D rr rr         sta aps + 0
0000D3r 1  BD rr rr         lda aps + 3, x
0000D6r 1  8D rr rr         sta aps + 1
0000D9r 1  BD rr rr         lda aps - 2, x
0000DCr 1  8D rr rr         sta aps + 2
0000DFr 1  BD rr rr         lda aps - 1, x
0000E2r 1  8D rr rr         sta aps + 3
0000E5r 1                   ; rts
0000E5r 1  6C rr rr         jmp (vNext)
0000E8r 1               
0000E8r 1               rot_:
0000E8r 1  A6 F2            ldx isp
0000EAr 1  BD rr rr         lda aps + 4, x
0000EDr 1  8D rr rr         sta aps - 2
0000F0r 1  BD rr rr         lda aps + 5, x
0000F3r 1  8D rr rr         sta aps - 1
0000F6r 1  BD rr rr         lda aps + 2, x
0000F9r 1  8D rr rr         sta aps + 4
0000FCr 1  BD rr rr         lda aps + 3, x
0000FFr 1  8D rr rr         sta aps + 5
000102r 1  BD rr rr         lda aps + 0, x
000105r 1  8D rr rr         sta aps + 2
000108r 1  BD rr rr         lda aps + 1, x
00010Br 1  8D rr rr         sta aps + 3
00010Er 1  BD rr rr         lda aps - 2, x
000111r 1  8D rr rr         sta aps + 0
000114r 1  BD rr rr         lda aps - 1, x
000117r 1  8D rr rr         sta aps + 1
00011Ar 1                   ; rts
00011Ar 1  6C rr rr         jmp (vNext)
00011Dr 1               
00011Dr 1               and_:
00011Dr 1  A6 F2            ldx isp
00011Fr 1  BD rr rr         lda aps + 0, x
000122r 1  3D rr rr         and aps + 2, x
000125r 1  9D rr rr         sta aps + 2, x
000128r 1  BD rr rr         lda aps + 1, x
00012Br 1  3D rr rr         and aps + 3, x
00012Er 1  9D rr rr         sta aps + 3, x
000131r 1  4C rr rr         jmp drop_
000134r 1               
000134r 1               or_:
000134r 1  A6 F2            ldx isp
000136r 1  BD rr rr         lda aps + 0, x
000139r 1  1D rr rr         ora aps + 2, x
00013Cr 1  9D rr rr         sta aps + 2, x
00013Fr 1  BD rr rr         lda aps + 1, x
000142r 1  1D rr rr         ora aps + 3, x
000145r 1  9D rr rr         sta aps + 3, x
000148r 1  4C rr rr         jmp drop_
00014Br 1               
00014Br 1               xor_:
00014Br 1  A6 F2            ldx isp
00014Dr 1  BD rr rr         lda aps + 0, x
000150r 1  5D rr rr         eor aps + 2, x
000153r 1  9D rr rr         sta aps + 2, x
000156r 1  BD rr rr         lda aps + 1, x
000159r 1  5D rr rr         eor aps + 3, x
00015Cr 1  9D rr rr         sta aps + 3, x
00015Fr 1  4C rr rr         jmp drop_
000162r 1               
000162r 1               cpt_:
000162r 1  A6 F2            ldx isp
000164r 1  38               sec
000165r 1  98               tya
000166r 1  FD rr rr         sbc aps + 0, x
000169r 1  9D rr rr         sta aps + 0, x
00016Cr 1  38               sec
00016Dr 1  98               tya
00016Er 1  FD rr rr         sbc aps + 1, x
000171r 1  9D rr rr         sta aps + 1, x
000174r 1                   ; rts
000174r 1  6C rr rr         jmp (vNext)
000177r 1               
000177r 1               neg_:
000177r 1  A9 00            lda #$00
000179r 1  A8               tay
00017Ar 1  4C rr rr         jmp cpt_
00017Dr 1               
00017Dr 1               inv_:
00017Dr 1  A9 FF            lda #$FF
00017Fr 1  A8               tay
000180r 1  4C rr rr         jmp cpt_
000183r 1               
000183r 1               sub_:
000183r 1  A6 F2            ldx isp
000185r 1  38               sec
000186r 1  BD rr rr         lda aps + 2, x
000189r 1  FD rr rr         sbc aps + 0, x
00018Cr 1  9D rr rr         sta aps + 2, x
00018Fr 1  BD rr rr         lda aps + 3, x
000192r 1  FD rr rr         sbc aps + 1, x
000195r 1  9D rr rr         sta aps + 3, x
000198r 1  4C rr rr         jmp drop_
00019Br 1               
00019Br 1               add_:
00019Br 1  A6 F2            ldx isp
00019Dr 1  18               clc
00019Er 1  BD rr rr         lda aps + 2, x
0001A1r 1  7D rr rr         adc aps + 0, x
0001A4r 1  9D rr rr         sta aps + 2, x
0001A7r 1  BD rr rr         lda aps + 3, x
0001AAr 1  7D rr rr         adc aps + 1, x
0001ADr 1  9D rr rr         sta aps + 3, x
0001B0r 1  4C rr rr         jmp drop_
0001B3r 1               
0001B3r 1               cmp_:
0001B3r 1  A6 F2            ldx isp
0001B5r 1  38               sec
0001B6r 1  BD rr rr         lda aps + 2, x
0001B9r 1  FD rr rr         sbc aps + 0, x
0001BCr 1  BD rr rr         lda aps + 3, x
0001BFr 1  FD rr rr         sbc aps + 1, x
0001C2r 1  60               rts
0001C3r 1               
0001C3r 1               eq_:
0001C3r 1  20 rr rr         jsr cmp_
0001C6r 1  F0 21            beq true2_
0001C8r 1  D0 1B            bne false2_
0001CAr 1               
0001CAr 1               lt_:
0001CAr 1  20 rr rr         jsr cmp_
0001CDr 1  30 1A            bmi true2_
0001CFr 1  10 14            bpl false2_
0001D1r 1               
0001D1r 1               gt_:
0001D1r 1  20 rr rr         jsr cmp_
0001D4r 1  30 0F            bmi false2_
0001D6r 1  F0 0D            beq false2_
0001D8r 1  10 0F            bpl true2_
0001DAr 1               
0001DAr 1               same2_:
0001DAr 1  A6 F2            ldx isp
0001DCr 1  9D rr rr         sta aps + 2, x
0001DFr 1  9D rr rr         sta aps + 3, x
0001E2r 1  4C rr rr         jmp drop_
0001E5r 1               
0001E5r 1               false2_:
0001E5r 1  A9 00            lda #(FALSE)
0001E7r 1  F0 F1            beq same2_
0001E9r 1               
0001E9r 1               true2_:
0001E9r 1  A9 01            lda #(!FALSE)
0001EBr 1  D0 ED            bne same2_
0001EDr 1               
0001EDr 1               shl_:
0001EDr 1  A6 F2            ldx isp
0001EFr 1  1E rr rr         asl aps + 0, x
0001F2r 1  3E rr rr         rol aps + 1, x
0001F5r 1                   ; rts
0001F5r 1  6C rr rr         jmp (vNext)
0001F8r 1               
0001F8r 1               shr_:
0001F8r 1  A6 F2            ldx isp
0001FAr 1  5E rr rr         lsr aps + 0, x
0001FDr 1  7E rr rr         ror aps + 1, x
000200r 1                   ; rts
000200r 1  6C rr rr         jmp (vNext)
000203r 1               
000203r 1               cto_:
000203r 1  20 rr rr         jsr pull2_
000206r 1  A0 00            ldy #0
000208r 1  A5 FA            lda nos + 0
00020Ar 1  91 F8            sta (tos), y
00020Cr 1  60               rts
00020Dr 1               
00020Dr 1               to_:
00020Dr 1  20 rr rr         jsr cto_
000210r 1  C8               iny
000211r 1  A5 FB            lda nos + 1
000213r 1  91 F8            sta (tos), y
000215r 1  60               rts
000216r 1               
000216r 1               cStore_:
000216r 1  20 rr rr         jsr cto_
000219r 1                   ; rts
000219r 1  6C rr rr         jmp (vNext)
00021Cr 1               
00021Cr 1               store_:
00021Cr 1  20 rr rr         jsr to_
00021Fr 1                   ; rts
00021Fr 1  6C rr rr         jmp (vNext)
000222r 1               
000222r 1               cat_:
000222r 1  A6 F2            ldx isp
000224r 1  BD rr rr         lda aps + 0, x
000227r 1  85 F8            sta tos + 0
000229r 1  BD rr rr         lda aps + 1, x
00022Cr 1  85 F9            sta tos + 1
00022Er 1  A0 00            ldy #0
000230r 1  B1 F8            lda (tos), y
000232r 1  9D rr rr         sta aps + 0, x
000235r 1  60               rts
000236r 1               
000236r 1               at_:
000236r 1  20 rr rr         jsr cat_
000239r 1  C8               iny
00023Ar 1  B1 F8            lda (tos), y
00023Cr 1  9D rr rr         sta aps + 1, x
00023Fr 1  60               rts
000240r 1               
000240r 1               cFetch_:
000240r 1  20 rr rr         jsr cat_
000243r 1                   ; rts
000243r 1  6C rr rr         jmp (vNext)
000246r 1               
000246r 1               fetch_:
000246r 1  20 rr rr         jsr at_
000249r 1                   ; rts
000249r 1  6C rr rr         jmp (vNext)
00024Cr 1               
00024Cr 1               incr_:
00024Cr 1  A6 F2            ldx isp
00024Er 1  FE rr rr         inc aps + 0, x
000251r 1  D0 03            bne @ends
000253r 1  FE rr rr         inc aps + 1, x
000256r 1               @ends:
000256r 1                   ; rts
000256r 1  6C rr rr         jmp (vNext)
000259r 1               
000259r 1               decr_:
000259r 1  A6 F2            ldx isp
00025Br 1  BD rr rr         lda aps + 0, x
00025Er 1  D0 03            bne @ends
000260r 1  DE rr rr         dec aps + 1, x
000263r 1               @ends:
000263r 1  DE rr rr         dec aps + 0, x
000266r 1                   ; rts
000266r 1  6C rr rr         jmp (vNext)
000269r 1               
000269r 1               .if 0
000269r 1               addto_:
000269r 1                   jsr pull2_
000269r 1                   ldy NUL
000269r 1                   clc
000269r 1                   lda (tos), y
000269r 1                   adc nos + 0
000269r 1                   sta (tos), y
000269r 1                   iny
000269r 1                   lda (tos), y
000269r 1                   adc nos + 1
000269r 1                   sta (tos), y
000269r 1               	jmp (vNext)
000269r 1               
000269r 1               subto_:
000269r 1                   jsr pull2_
000269r 1                   ldy NUL
000269r 1                   sec
000269r 1                   lda (tos), y
000269r 1                   sbc nos + 0
000269r 1                   sta (tos), y
000269r 1                   iny
000269r 1                   lda (tos), y
000269r 1                   sbc nos + 1
000269r 1                   sta (tos), y
000269r 1               	jmp (vNext)
000269r 1               .endif
000269r 1               
000269r 1               ;------------------------------------------------------------------------------
000269r 1               ;   return stack stuff
000269r 1               
000269r 1               rpush:
000269r 1               rpush_:
000269r 1  A6 F2            ldx isp
00026Br 1  A5 F8            lda tos + 0
00026Dr 1  9D rr rr         sta apr - 2, x
000270r 1  A5 F9            lda tos + 1
000272r 1  9D rr rr         sta apr - 1, x
000275r 1  CA               dex
000276r 1  CA               dex
000277r 1  86 F2            stx isp
000279r 1  60               rts
00027Ar 1               
00027Ar 1               rpull:
00027Ar 1               rpull_:
00027Ar 1  A6 F2            ldx isp
00027Cr 1  BD rr rr         lda apr + 0, x
00027Fr 1  85 F8            sta tos + 0
000281r 1  BD rr rr         lda apr + 1, x
000284r 1  85 F9            sta tos + 1
000286r 1  E8               inx
000287r 1  E8               inx
000288r 1  86 F2            stx isp
00028Ar 1  60               rts
00028Br 1               
00028Br 1               .if 0
00028Br 1               rshow_:
00028Br 1                   ldx isp
00028Br 1                   lda apr + 0, x
00028Br 1                   sta tos + 0
00028Br 1                   lda apr + 1, x
00028Br 1                   sta tos + 1
00028Br 1                   jsr push_
00028Br 1                   ; rts
00028Br 1                   jmp (vNext)
00028Br 1               .endif
00028Br 1               
00028Br 1               r2s_:
00028Br 1  20 rr rr         jsr rpull_
00028Er 1  20 rr rr         jsr push_
000291r 1                   ; rts
000291r 1  6C rr rr         jmp (vNext)
000294r 1               
000294r 1               s2r_:
000294r 1  20 rr rr         jsr pull_
000297r 1  20 rr rr         jsr rpush_
00029Ar 1                   ; rts
00029Ar 1  6C rr rr         jmp (vNext)
00029Dr 1               
00029Dr 1               ;----------------------------------------------------------------------
00029Dr 1               ; prepare for mult or divd
00029Dr 1               opin:
00029Dr 1  A6 F2            ldx isp
00029Fr 1                   ; pseudo tos
00029Fr 1  BD rr rr         lda aps + 0, x
0002A2r 1  85 FC            sta wrk + 0
0002A4r 1  BD rr rr         lda aps + 1, x
0002A7r 1  85 FD            sta wrk + 1
0002A9r 1                   ; pseudo nos
0002A9r 1  BD rr rr         lda aps + 2, x
0002ACr 1  85 FE            sta tmp + 0
0002AEr 1  BD rr rr         lda aps + 3, x
0002B1r 1  85 FF            sta tmp + 1
0002B3r 1                   ; clear results
0002B3r 1  A5 00            lda NUL
0002B5r 1  85 F8            sta tos + 0
0002B7r 1  85 F9            sta tos + 1
0002B9r 1  85 FA            sta nos + 0
0002BBr 1  85 FB            sta nos + 1
0002BDr 1                   ; countdown
0002BDr 1  A0 10            ldy #16
0002BFr 1  60               rts
0002C0r 1               
0002C0r 1               ;----------------------------------------------------------------------
0002C0r 1               ; resume from mult or divd
0002C0r 1               opout:
0002C0r 1                   ; copy results
0002C0r 1  A6 F2            ldx isp
0002C2r 1  A5 FA            lda nos + 0
0002C4r 1  9D rr rr         sta aps + 0, x
0002C7r 1  A5 FB            lda nos + 1
0002C9r 1  9D rr rr         sta aps + 1, x
0002CCr 1  A5 F8            lda tos + 0
0002CEr 1  9D rr rr         sta aps + 2, x
0002D1r 1  A5 F9            lda tos + 1
0002D3r 1  9D rr rr         sta aps + 3, x
0002D6r 1  6C rr rr         jmp (vNext)
0002D9r 1               
0002D9r 1               ;----------------------------------------------------------------------
0002D9r 1               ; Divide the top 2 cell of the stack
0002D9r 1               ; http://codebase64.org/doku.php?id=base:16bit_division_16-bit_result
0002D9r 1               ; dividend divisor -- result remainder
0002D9r 1               ; ( tmp wrk -- nos tos )
0002D9r 1               div_:
0002D9r 1  20 rr rr         jsr opin
0002DCr 1               @loop:
0002DCr 1  06 FE            asl tmp + 0
0002DEr 1  26 FF            rol tmp + 1
0002E0r 1  26 F8            rol tos + 0
0002E2r 1  26 F9            rol tos + 1
0002E4r 1               
0002E4r 1  38               sec
0002E5r 1  A5 F8            lda tos + 0
0002E7r 1  E5 FC            sbc wrk + 0
0002E9r 1  85 FA            sta nos + 0
0002EBr 1  A5 F9            lda tos + 1
0002EDr 1  E5 FD            sbc wrk + 1
0002EFr 1  90 08            bcc @skip
0002F1r 1               
0002F1r 1  85 F9            sta tos + 1
0002F3r 1  A5 FA            lda nos + 0
0002F5r 1  85 F8            sta tos + 0
0002F7r 1  E6 FE            inc tmp + 0
0002F9r 1               
0002F9r 1               @skip:
0002F9r 1                   ; countdown
0002F9r 1  88               dey
0002FAr 1  D0 E0            bne @loop
0002FCr 1                   ; results
0002FCr 1  A5 FE            lda tmp + 0
0002FEr 1  85 FA            sta nos + 0
000300r 1  A5 FF            lda tmp + 1
000302r 1  85 FB            sta nos + 1
000304r 1                   ; ends
000304r 1  4C rr rr         jmp opout
000307r 1               
000307r 1               ;----------------------------------------------------------------------
000307r 1               ; 16-bit multiply 16x16, 32 result
000307r 1               ; http://codebase64.org/doku.php?id=base:16bit_multiplication_32-bit_product
000307r 1               ; ( multiplicand multiplier -- resultMSW resultLSW )
000307r 1               ; ( tmp wrk -- nos tos )
000307r 1               mul_:
000307r 1  20 rr rr         jsr opin
00030Ar 1               @shift_r:
00030Ar 1                   ; divide by 2
00030Ar 1  46 FD            lsr wrk + 1
00030Cr 1  66 FC            ror wrk + 0
00030Er 1  90 0B            bcc @rotate_r
000310r 1                   ; add multiplicand to upper half product
000310r 1  18               clc
000311r 1  A5 F8            lda tos + 0
000313r 1  65 FE            adc tmp + 0
000315r 1  85 F8            sta tos + 0
000317r 1  A5 FF            lda tmp + 1
000319r 1  65 F9            adc tos + 1
00031Br 1               @rotate_r:
00031Br 1                   ; rotate partial product upper to low
00031Br 1  6A               ror
00031Cr 1  85 F9            sta tos + 1
00031Er 1  66 F8            ror tos + 0
000320r 1  66 FB            ror nos + 1
000322r 1  66 FA            ror nos + 0
000324r 1                   ; countdown
000324r 1  88               dey
000325r 1  D0 E3            bne @shift_r
000327r 1                   ; ends
000327r 1  4C rr rr         jmp opout
00032Ar 1               
00032Ar 1               ;----------------------------------------------------------------------
00032Ar 1               ;   MINT
00032Ar 1               ;----------------------------------------------------------------------
00032Ar 1               ; NOOP
00032Ar 1               aNop_:
00032Ar 1               nop_:
00032Ar 1                   ; next
00032Ar 1  4C rr rr         jmp next
00032Dr 1               
00032Dr 1               ;----------------------------------------------------------------------
00032Dr 1               ; add a byte offset to instruction pointer
00032Dr 1               add2ps:
00032Dr 1               ; update ip
00032Dr 1  18               clc
00032Er 1  65 F0            adc ipt + 0
000330r 1  85 F0            sta ipt + 0
000332r 1  90 02            bcc @ends
000334r 1  E6 F1            inc ipt + 1
000336r 1               @ends:
000336r 1                   ; next
000336r 1  6C rr rr         jmp (vNext)
000339r 1               
000339r 1               ;----------------------------------------------------------------------
000339r 1               pushps:
000339r 1  A6 F3            ldx irp
00033Br 1  A5 F0            lda ipt + 0
00033Dr 1  9D rr rr         sta apr - 2, x
000340r 1  A5 F1            lda ipt + 1
000342r 1  9D rr rr         sta apr - 1, x
000345r 1  CA               dex
000346r 1  CA               dex
000347r 1  86 F3            stx irp
000349r 1  60               rts
00034Ar 1               
00034Ar 1               ;----------------------------------------------------------------------
00034Ar 1               pullps:
00034Ar 1  A6 F3            ldx irp
00034Cr 1  BD rr rr         lda apr + 0, x
00034Fr 1  85 F0            sta ipt + 0
000351r 1  BD rr rr         lda apr + 1, x
000354r 1  85 F1            sta ipt + 1
000356r 1  E8               inx
000357r 1  E8               inx
000358r 1  86 F3            stx irp
00035Ar 1  60               rts
00035Br 1               
00035Br 1               ;----------------------------------------------------------------------
00035Br 1               seekps:
00035Br 1  A4 00            ldy NUL
00035Dr 1  B1 F0            lda (ipt), y
00035Fr 1  E6 F0            inc ipt + 0
000361r 1  D0 02            bne @ends
000363r 1  E6 F1            inc ipt + 1
000365r 1               @ends:
000365r 1  60               rts
000366r 1               
000366r 1               ;----------------------------------------------------------------------
000366r 1               heap2nos:
000366r 1                   ; array start
000366r 1  AD rr rr         lda vHeap + 0
000369r 1  85 FA            sta nos + 0
00036Br 1  AD rr rr         lda vHeap + 1
00036Er 1  85 FB            sta nos + 1
000370r 1  60               rts
000371r 1               
000371r 1               ;----------------------------------------------------------------------
000371r 1               add2heap:
000371r 1  18               clc
000372r 1  6D rr rr         adc vHeap + 0
000375r 1  8D rr rr         sta vHeap + 0
000378r 1  90 03            bcc @ends
00037Ar 1  EE rr rr         inc vHeap + 1
00037Dr 1               @ends:
00037Dr 1  60               rts
00037Er 1               
00037Er 1               ;----------------------------------------------------------------------
00037Er 1               inctos:
00037Er 1  E6 F8            inc tos + 0
000380r 1  90 02            bcc @ends
000382r 1  E6 F9            inc tos + 1
000384r 1               @ends:
000384r 1  60               rts
000385r 1               
000385r 1               ;----------------------------------------------------------------------
000385r 1               tib2tos:
000385r 1  A9 rr            lda #<tib
000387r 1  85 F8            sta tos + 0
000389r 1  A9 rr            lda #>tib
00038Br 1  85 F9            sta tos + 1
00038Dr 1  60               rts
00038Er 1               
00038Er 1               ;----------------------------------------------------------------------
00038Er 1               ; puts a string
00038Er 1               str_:
00038Er 1  A4 00            ldy NUL
000390r 1               @loop:
000390r 1  B1 F0            lda (ipt), y
000392r 1  F0 0A            beq @ends       ; NUL
000394r 1  C9 60            cmp #'`'        ; ` is the string terminator
000396r 1  F0 06            beq @ends
000398r 1  20 rr rr         jsr putchar
00039Br 1  C8               iny
00039Cr 1  D0 F2            bne @loop       ; limit 255
00039Er 1               @ends:
00039Er 1                   ; next
00039Er 1  6C rr rr         jmp  (vNext)
0003A1r 1               
0003A1r 1               ;----------------------------------------------------------------------
0003A1r 1               ; $00 to $1F, reserved for macros
0003A1r 1               ; macros could no call macros.
0003A1r 1               macro:
0003A1r 1  8C rr rr         sty vTIBPtr + 0
0003A4r 1  A8               tay
0003A5r 1  B9 rr rr         lda ctlcodeslo, y
0003A8r 1  85 F8            sta tos + 0
0003AAr 1  B9 rr rr         lda ctlcodeshi, y
0003ADr 1  85 F9            sta tos + 1
0003AFr 1  20 rr rr         jsr spush
0003B2r 1  20 rr rr         jsr enter
0003B5r 1  5C 47 00         .asciiz "\\G"
0003B8r 1  AC rr rr         ldy vTIBPtr + 0
0003BBr 1  4C rr rr         jmp interpret2
0003BEr 1               
0003BEr 1               ;----------------------------------------------------------------------
0003BEr 1               interpret:
0003BEr 1  20 rr rr         jsr enter
0003C1r 1  5C 4E 60 3E      .asciiz "\\N`> `"
0003C5r 1  20 60 00     
0003C8r 1                   ; fall throught
0003C8r 1               
0003C8r 1               ; used by tests
0003C8r 1               interpret1:
0003C8r 1  A5 00            lda NUL
0003CAr 1  8D rr rr         sta vTIBPtr + 0
0003CDr 1               
0003CDr 1               interpret2:
0003CDr 1  A5 00            lda NUL
0003CFr 1  85 F7            sta ns
0003D1r 1  A8               tay
0003D2r 1  F0 07            beq @isnest
0003D4r 1               
0003D4r 1               ; calc nesting (a macro might have changed it)
0003D4r 1               @loop:
0003D4r 1  B9 rr rr         lda tib, y
0003D7r 1  C8               iny
0003D8r 1  20 rr rr         jsr nesting            ; update nesting value
0003DBr 1               
0003DBr 1               @isnest:
0003DBr 1  C4 00            cpy NUL
0003DDr 1  D0 F5            bne @loop
0003DFr 1                   ; fall throught
0003DFr 1               
0003DFr 1               ;----------------------------------------------------------------------
0003DFr 1               ; loop around waiting for character
0003DFr 1               ; get a line into tib
0003DFr 1               waitchar:
0003DFr 1  20 rr rr         jsr tib2tos
0003E2r 1  20 rr rr         jsr spush
0003E5r 1                   ; fall throught
0003E5r 1               
0003E5r 1               ;----------------------------------------------------------------------
0003E5r 1               ; get a line into buffer pointer by TOS
0003E5r 1               gets_:
0003E5r 1                   ; already
0003E5r 1  A4 00            ldy NUL
0003E7r 1  20 rr rr         jsr spull
0003EAr 1               @loop:
0003EAr 1                   ; limit 254
0003EAr 1  C0 FE            cpy #$FE
0003ECr 1  F0 2F            beq @endstr
0003EEr 1               
0003EEr 1  20 rr rr         jsr getchar
0003F1r 1               
0003F1r 1                   ; ge space ?
0003F1r 1  C9 20            cmp #32
0003F3r 1  B0 0F            bcs @ischar
0003F5r 1                   ; is it end of string ?
0003F5r 1  C9 00            cmp #$0
0003F7r 1  F0 24            beq @endstr
0003F9r 1                   ; windows CRLF, linux CR, Mac LF
0003F9r 1  C5 0D            cmp CR                 ; carriage return ?
0003FBr 1  F0 10            beq @iscrlf
0003FDr 1  C5 0A            cmp LF                 ; line feed ?
0003FFr 1  F0 0C            beq @iscrlf
000401r 1               
000401r 1               @ismacro:
000401r 1                   ; $00 to $1F
000401r 1  4C rr rr         jmp macro
000404r 1               
000404r 1               @ischar:
000404r 1  20 rr rr         jsr @echo
000407r 1                   ; nest ?
000407r 1  20 rr rr         jsr nesting
00040Ar 1                   ; wait for next character
00040Ar 1  18               clc
00040Br 1  90 DD            bcc @loop
00040Dr 1               
00040Dr 1               @iscrlf:
00040Dr 1  A5 0D            lda CR
00040Fr 1  20 rr rr         jsr @echo
000412r 1  A5 0A            lda LF
000414r 1  20 rr rr         jsr @echo
000417r 1                   ; pending nest ?
000417r 1  A5 F7            lda ns
000419r 1  C5 00            cmp NUL
00041Br 1  F0 CD            beq @loop
00041Dr 1               
00041Dr 1               ; mark end with etx,
00041Dr 1               @endstr:
00041Dr 1                   ; mark ETX
00041Dr 1  A5 03            lda ETX
00041Fr 1  91 F8            sta (tos), y
000421r 1  C8               iny
000422r 1               
000422r 1                   ; mark NUL
000422r 1                   ;lda NUL
000422r 1                   ;sta (tos), y
000422r 1               
000422r 1  A5 F8            lda tos + 0
000424r 1  85 F0            sta ipt + 0
000426r 1  A5 F9            lda tos + 1
000428r 1  85 F1            sta ipt + 1
00042Ar 1                   ; next
00042Ar 1  4C rr rr         jmp next
00042Dr 1               
00042Dr 1               ; maximum 254 chars
00042Dr 1               @echo:
00042Dr 1                   ; echo
00042Dr 1  20 rr rr         jsr putchar
000430r 1                   ; store
000430r 1  91 F8            sta (tos), y
000432r 1  C8               iny
000433r 1  60               rts
000434r 1               
000434r 1               ;----------------------------------------------------------------------
000434r 1               ; calculate nesting value
000434r 1               nesting:
000434r 1  C9 60            cmp #'`'
000436r 1  D0 07            bne @nests
000438r 1                   ; clear bit 7
000438r 1  A9 80            lda #$80
00043Ar 1  45 F7            eor ns
00043Cr 1  85 F7            sta ns
00043Er 1  60               rts
00043Fr 1               @nests:
00043Fr 1  24 F7            bit ns
000441r 1  30 18            bmi @nonest
000443r 1  C9 3A            cmp #':'
000445r 1  F0 15            beq @nestinc
000447r 1  C9 5B            cmp #'['
000449r 1  F0 11            beq @nestinc
00044Br 1  C9 28            cmp #'('
00044Dr 1  F0 0D            beq @nestinc
00044Fr 1  C9 3B            cmp #';'
000451r 1  F0 0C            beq @nestdec
000453r 1  C9 5D            cmp #']'
000455r 1  F0 08            beq @nestdec
000457r 1  C9 29            cmp #')'
000459r 1  F0 04            beq @nestdec
00045Br 1               @nonest:
00045Br 1  60               rts
00045Cr 1               @nestinc:
00045Cr 1  E6 F7            inc ns
00045Er 1  60               rts
00045Fr 1               @nestdec:
00045Fr 1  C6 F7            dec ns
000461r 1  60               rts
000462r 1               
000462r 1               ;----------------------------------------------------------------------
000462r 1               ; prints a asciiz, refered by hardware stack
000462r 1               printStr:
000462r 1                   ; reference
000462r 1  68               pla
000463r 1  85 F9            sta tos + 1
000465r 1  68               pla
000466r 1  85 F8            sta tos + 0
000468r 1  20 rr rr         jsr putstr
00046Br 1                   ; offset
00046Br 1  18               clc
00046Cr 1  65 F8            adc tos + 0
00046Er 1  48               pha
00046Fr 1  65 F9            adc tos + 1
000471r 1  48               pha
000472r 1  60               rts
000473r 1               
000473r 1               ;----------------------------------------------------------------------
000473r 1               ; prints a asciiz line from a buffer pointer by tos
000473r 1               puts_:
000473r 1  20 rr rr         jsr spull
000476r 1                   ; fall throught
000476r 1               
000476r 1               ;----------------------------------------------------------------------
000476r 1               ; prints a asciiz
000476r 1               putstr:
000476r 1  A4 00            ldy NUL
000478r 1               @loop:
000478r 1  B1 F8            lda (tos), y
00047Ar 1  F0 06            beq @ends   ; limit NUL
00047Cr 1  20 rr rr         jsr putchar
00047Fr 1  C8               iny
000480r 1  D0 F6            bne @loop   ; limit 256
000482r 1               @ends:
000482r 1  98               tya
000483r 1  60               rts
000484r 1               
000484r 1               ;----------------------------------------------------------------------
000484r 1               ; prints number in tos to decimal ASCII
000484r 1               ; ps. putchar ends with rts
000484r 1               printdec:
000484r 1  A9 10            lda #<10000
000486r 1  85 FA            sta nos + 0
000488r 1  A9 27            lda #>10000
00048Ar 1  85 FB            sta nos + 1
00048Cr 1  20 rr rr         jsr @nums
00048Fr 1  A9 E8            lda #<1000
000491r 1  85 FA            sta nos + 0
000493r 1  A9 03            lda #>1000
000495r 1  85 FB            sta nos + 1
000497r 1  20 rr rr         jsr @nums
00049Ar 1  A9 64            lda #<100
00049Cr 1  85 FA            sta nos + 0
00049Er 1  A9 00            lda #>100
0004A0r 1  85 FB            sta nos + 1
0004A2r 1  20 rr rr         jsr @nums
0004A5r 1  A9 0A            lda #<10
0004A7r 1  85 FA            sta nos + 0
0004A9r 1  A9 00            lda #>10
0004ABr 1  85 FB            sta nos + 1
0004ADr 1               @nums:
0004ADr 1  A0 2F            ldy #'0'-1
0004AFr 1               @loop:
0004AFr 1  C8               iny
0004B0r 1  38               sec
0004B1r 1  A5 F8            lda tos + 0
0004B3r 1  E5 FA            sbc nos + 0
0004B5r 1  85 F8            sta tos + 0
0004B7r 1  A5 F9            lda tos + 1
0004B9r 1  E5 FB            sbc nos + 1
0004BBr 1  85 F9            sta tos + 1
0004BDr 1  90 F0            bcc @loop
0004BFr 1  18               clc
0004C0r 1  A5 F8            lda tos + 0
0004C2r 1  65 FA            adc nos + 0
0004C4r 1  85 F8            sta tos + 0
0004C6r 1  A5 F9            lda tos + 1
0004C8r 1  65 FB            adc nos + 1
0004CAr 1  85 F9            sta tos + 1
0004CCr 1  98               tya
0004CDr 1  4C rr rr         jmp putchar
0004D0r 1               
0004D0r 1               ;----------------------------------------------------------------------
0004D0r 1               ; prints number in tos to hexadecimal ASCII
0004D0r 1               printhex:
0004D0r 1  A5 F9            lda tos + 1
0004D2r 1  20 rr rr         jsr printhex8
0004D5r 1  A5 F8            lda tos + 0
0004D7r 1  20 rr rr         jsr printhex8
0004DAr 1  60               rts
0004DBr 1               
0004DBr 1               ;----------------------------------------------------------------------
0004DBr 1               ; print a 8-bit HEX
0004DBr 1               printhex8:
0004DBr 1  85 F6            sta ap
0004DDr 1  4A               lsr
0004DEr 1  6A               ror
0004DFr 1  6A               ror
0004E0r 1  6A               ror
0004E1r 1  20 rr rr         jsr @conv
0004E4r 1  A5 F6            lda ap
0004E6r 1               @conv:
0004E6r 1  29 0F            and #$0F
0004E8r 1  18               clc
0004E9r 1  09 30            ora #$30
0004EBr 1  C9 3A            cmp #$3A
0004EDr 1  90 02            bcc @ends
0004EFr 1  69 06            adc #$06
0004F1r 1               @ends:
0004F1r 1  4C rr rr         jmp putchar
0004F4r 1               
0004F4r 1               ;----------------------------------------------------------------------
0004F4r 1               prenum:
0004F4r 1  A5 00            lda NUL
0004F6r 1  85 F8            sta tos + 0
0004F8r 1  85 F9            sta tos + 1
0004FAr 1  60               rts
0004FBr 1               
0004FBr 1               ;----------------------------------------------------------------------
0004FBr 1               ; convert a decimal value to binary
0004FBr 1               num_:
0004FBr 1  20 rr rr         jsr prenum
0004FEr 1               @loop:
0004FEr 1  20 rr rr         jsr seekps
000501r 1  C9 30            cmp #'0' + 0
000503r 1  90 18            bcc @ends
000505r 1  C9 3A            cmp #'9' + 1
000507r 1  B0 14            bcs @ends
000509r 1               @cv10:
000509r 1  38               sec
00050Ar 1  E9 30            sbc #'0'
00050Cr 1               @uval:
00050Cr 1  18               clc
00050Dr 1  65 F8            adc tos + 0
00050Fr 1  85 F8            sta tos + 0
000511r 1  A5 00            lda NUL
000513r 1  65 F9            adc tos + 1
000515r 1  85 F9            sta tos + 1
000517r 1  20 rr rr         jsr mul10
00051Ar 1  18               clc
00051Br 1  90 E1            bcc @loop
00051Dr 1               @ends:
00051Dr 1  20 rr rr         jsr spush
000520r 1                   ; next
000520r 1  6C rr rr         jmp (vNext)
000523r 1               
000523r 1               ;----------------------------------------------------------------------
000523r 1               ; multiply by ten
000523r 1               ; 2x + 8x
000523r 1               mul10:
000523r 1                   ; 2x
000523r 1  06 F8            asl tos + 0
000525r 1  85 F8            sta tos + 0
000527r 1  85 FA            sta nos + 0
000529r 1  26 F9            rol tos + 1
00052Br 1  85 F9            sta tos + 1
00052Dr 1  85 FB            sta nos + 1
00052Fr 1                   ; 2x
00052Fr 1  06 F8            asl tos + 0
000531r 1  85 F8            sta tos + 0
000533r 1  26 F9            rol tos + 1
000535r 1  85 F9            sta tos + 1
000537r 1                   ; 2x
000537r 1  06 F8            asl tos + 0
000539r 1  85 F8            sta tos + 0
00053Br 1  26 F9            rol tos + 1
00053Dr 1  85 F9            sta tos + 1
00053Fr 1                   ; 2x + 8x
00053Fr 1  18               clc
000540r 1  A5 F8            lda tos + 0
000542r 1  65 FA            adc nos + 0
000544r 1  85 F8            sta tos + 0
000546r 1  A5 F9            lda tos + 1
000548r 1  65 FB            adc nos + 1
00054Ar 1  85 F9            sta tos + 1
00054Cr 1  60               rts
00054Dr 1               
00054Dr 1               ;----------------------------------------------------------------------
00054Dr 1               ; convert a hexadecimal value to binary
00054Dr 1               hex_:
00054Dr 1  20 rr rr         jsr prenum
000550r 1               @loop:
000550r 1  20 rr rr         jsr seekps
000553r 1               @isd:
000553r 1  C9 30            cmp #'0'
000555r 1  90 27            bcc @ends
000557r 1  C9 3A            cmp #'9' + 1
000559r 1  B0 05            bcs @ish
00055Br 1               @cv10:
00055Br 1  38               sec
00055Cr 1  E9 30            sbc #'0'
00055Er 1  90 0D            bcc @uval
000560r 1               @ish:
000560r 1                   ; to upper, clear bit-6
000560r 1  29 DF            and #%11011111
000562r 1  C5 41            cmp 'A'
000564r 1  90 18            bcc @ends
000566r 1  C5 47            cmp 'F' + 1
000568r 1  B0 14            bcs @ends
00056Ar 1               @cv16:
00056Ar 1  38               sec
00056Br 1  E9 37            sbc #'A' - 10
00056Dr 1               @uval:
00056Dr 1  18               clc
00056Er 1  65 F8            adc tos + 0
000570r 1  85 F8            sta tos + 0
000572r 1  A5 00            lda NUL
000574r 1  65 F9            adc tos + 1
000576r 1  85 F9            sta tos + 1
000578r 1  20 rr rr         jsr mul16
00057Br 1  B8               clv
00057Cr 1  50 D2            bvc @loop
00057Er 1               @ends:
00057Er 1  20 rr rr         jsr spush
000581r 1                   ; next
000581r 1  6C rr rr         jmp (vNext)
000584r 1               
000584r 1               ;----------------------------------------------------------------------
000584r 1               ; multiply by sixteen
000584r 1               mul16:
000584r 1  A0 04            ldy #4
000586r 1               @loop:
000586r 1  06 F8            asl tos + 0
000588r 1  85 F8            sta tos + 0
00058Ar 1  26 F9            rol tos + 1
00058Cr 1  85 F9            sta tos + 1
00058Er 1  88               dey
00058Fr 1  D0 F5            bne @loop
000591r 1  60               rts
000592r 1               
000592r 1               ;----------------------------------------------------------------------
000592r 1               ; skip to eol
000592r 1               comment_:
000592r 1  A4 00            ldy NUL
000594r 1               @loop:
000594r 1  C8               iny
000595r 1  F0 06            beq @ends   ; limit 256
000597r 1  B1 F0            lda (ipt), y
000599r 1  C5 0D            cmp CR
00059Br 1  D0 F7            bne @loop
00059Dr 1                   ; skip \r ?
00059Dr 1                   ; iny
00059Dr 1                   ; skip \n ?
00059Dr 1                   ; iny
00059Dr 1                   ; offset
00059Dr 1               @ends:
00059Dr 1  98               tya
00059Er 1  4C rr rr         jmp add2ps
0005A1r 1               
0005A1r 1               ;----------------------------------------------------------------------
0005A1r 1               depth_:
0005A1r 1                   ; limit to 255 bytes
0005A1r 1  38               sec
0005A2r 1  A9 FF            lda #$FF
0005A4r 1  E5 F2            sbc isp
0005A6r 1                   ; words
0005A6r 1  4A               lsr
0005A7r 1  85 F8            sta tos + 0
0005A9r 1  A5 00            lda NUL
0005ABr 1  85 F9            sta tos + 1
0005ADr 1  20 rr rr         jsr spush
0005B0r 1                   ; next
0005B0r 1  6C rr rr         jmp (vNext)
0005B3r 1               
0005B3r 1               ;----------------------------------------------------------------------
0005B3r 1               ; print hexadecimal
0005B3r 1               hdot_:
0005B3r 1  20 rr rr         jsr spull
0005B6r 1  20 rr rr         jsr printhex
0005B9r 1  4C rr rr         jmp dotsp
0005BCr 1               
0005BCr 1               ;----------------------------------------------------------------------
0005BCr 1               ; print decimal
0005BCr 1               dot_:
0005BCr 1  20 rr rr         jsr spull
0005BFr 1  20 rr rr         jsr printdec
0005C2r 1  4C rr rr         jmp dotsp
0005C5r 1               
0005C5r 1               ;----------------------------------------------------------------------
0005C5r 1               ; print space
0005C5r 1               dotsp:
0005C5r 1  A9 20            lda #' '
0005C7r 1  20 rr rr         jsr putchar
0005CAr 1                   ; next
0005CAr 1  6C rr rr         jmp (vNext)
0005CDr 1               
0005CDr 1               ;----------------------------------------------------------------------
0005CDr 1               newln_:
0005CDr 1  20 rr rr         jsr crlf
0005D0r 1                   ; next
0005D0r 1  6C rr rr         jmp (vNext)
0005D3r 1               
0005D3r 1               ;----------------------------------------------------------------------
0005D3r 1               crlf:
0005D3r 1  20 rr rr         jsr printStr
0005D6r 1  0D 0A 00         .asciiz "\r\n"
0005D9r 1  60               rts
0005DAr 1               
0005DAr 1               ;----------------------------------------------------------------------
0005DAr 1               prompt:
0005DAr 1  20 rr rr         jsr printStr
0005DDr 1  0D 0A 3E 20      .asciiz "\r\n> "
0005E1r 1  00           
0005E2r 1  60               rts
0005E3r 1               
0005E3r 1               ;----------------------------------------------------------------------
0005E3r 1               ; how many ? 14
0005E3r 1               printStk_:
0005E3r 1  20 rr rr         jsr enter
0005E6r 1                   ;.asciiz  "\\a@2-\\D1-(",$22,"@\\b@\\(,)(.)2-)'"
0005E6r 1  5C 61 40 32      .asciiz  "\\a@2-\\D1-(14@\\b@\\(,)(.)2-)'"
0005EAr 1  2D 5C 44 31  
0005EEr 1  2D 28 31 34  
000602r 1                   ; next
000602r 1  6C rr rr         jmp (vNext)
000605r 1               
000605r 1               ;----------------------------------------------------------------------
000605r 1               ; 6502 is memory mapped IO, just read
000605r 1               inPort_:
000605r 1  4C rr rr         jmp cFetch_
000608r 1               
000608r 1               ;----------------------------------------------------------------------
000608r 1               ; 6502 is memory mapped IO, just write
000608r 1               outPort_:
000608r 1  4C rr rr         jmp cStore_
00060Br 1               
00060Br 1               ;----------------------------------------------------------------------
00060Br 1               ; ascii code
00060Br 1               charCode_:
00060Br 1  20 rr rr         jsr seekps
00060Er 1  85 F8            sta tos + 0
000610r 1  A5 00            lda NUL
000612r 1  85 F9            sta tos + 1
000614r 1  20 rr rr         jsr spush
000617r 1                   ; next
000617r 1  6C rr rr         jmp (vNext)
00061Ar 1               
00061Ar 1               ;----------------------------------------------------------------------
00061Ar 1               ; copy and update
00061Ar 1               compNext:
00061Ar 1                   ; array start
00061Ar 1  20 rr rr         jsr heap2nos
00061Dr 1               
00061Dr 1  20 rr rr         jsr spull
000620r 1               
000620r 1                   ; byte
000620r 1  A4 00            ldy NUL
000622r 1  A5 F8            lda tos + 0
000624r 1  91 FA            sta (nos), y
000626r 1  C8               iny
000627r 1               
000627r 1  AD rr rr         lda vByteMode + 0
00062Ar 1  D0 05            bne @isbm
00062Cr 1               
00062Cr 1                   ; word
00062Cr 1  A5 F9            lda tos + 1
00062Er 1  91 FA            sta (nos), y
000630r 1  C8               iny
000631r 1               @isbm:
000631r 1               
000631r 1  98               tya
000632r 1  20 rr rr         jsr add2heap
000635r 1                   ; fall throught
000635r 1               
000635r 1               ;----------------------------------------------------------------------
000635r 1               ; Execute next opcode
000635r 1               next:
000635r 1               opt_:
000635r 1  20 rr rr         jsr seekps
000638r 1  A8               tay
000639r 1  B9 rr rr         lda optcodeslo, y
00063Cr 1  85 FC            sta wrk + 0
00063Er 1  B9 rr rr         lda optcodeshi, y
000641r 1  85 FD            sta wrk + 1
000643r 1  6C FC 00         jmp (wrk)
000646r 1               
000646r 1               ;----------------------------------------------------------------------
000646r 1               ; Execute next alt opcode
000646r 1               alt_:
000646r 1  20 rr rr         jsr seekps
000649r 1  A8               tay
00064Ar 1  B9 rr rr         lda altcodeslo, y
00064Dr 1  85 FC            sta wrk + 0
00064Fr 1  B9 rr rr         lda altcodeshi, y
000652r 1  85 FD            sta wrk + 1
000654r 1  6C FC 00         jmp (wrk)
000657r 1               
000657r 1               ;----------------------------------------------------------------------
000657r 1               ; Parse inline code, must be asciiz
000657r 1               enter:
000657r 1               ; pull from system stack
000657r 1  68               pla
000658r 1  85 F0            sta ipt + 0
00065Ar 1  68               pla
00065Br 1  85 F1            sta ipt + 1
00065Dr 1                   ; next
00065Dr 1  6C rr rr         jmp (vNext)
000660r 1               
000660r 1               ;----------------------------------------------------------------------
000660r 1               ; char 0, Continue from enter
000660r 1               exit_:
000660r 1  6C F0 00         jmp (ipt)
000663r 1               
000663r 1               ;----------------------------------------------------------------------
000663r 1               ; Execute code from data stack
000663r 1               ;
000663r 1               exec_:
000663r 1  20 rr rr         jsr spull
000666r 1  6C F8 00         jmp (tos)
000669r 1               
000669r 1               ;----------------------------------------------------------------------
000669r 1               ret_:
000669r 1  20 rr rr         jsr pullps
00066Cr 1                   ; next
00066Cr 1  6C rr rr         jmp (vNext)
00066Fr 1               
00066Fr 1               ;----------------------------------------------------------------------
00066Fr 1               ; Interpret code from data stack
00066Fr 1               go_:
00066Fr 1  20 rr rr         jsr pushps
000672r 1               ; pull ps from data stack
000672r 1  A6 F2            ldx isp
000674r 1  BD rr rr         lda aps + 0, x
000677r 1  85 F0            sta ipt + 0
000679r 1  BD rr rr         lda aps + 1, x
00067Cr 1  85 F1            sta ipt + 1
00067Er 1  E8               inx
00067Fr 1  E8               inx
000680r 1  86 F2            stx isp
000682r 1                   ; next
000682r 1  6C rr rr         jmp (vNext)
000685r 1               
000685r 1               ;----------------------------------------------------------------------
000685r 1               ; Execute code from a user function
000685r 1               call_:
000685r 1  85 F6            sta ap
000687r 1               
000687r 1  20 rr rr         jsr pushps
00068Ar 1               
00068Ar 1  20 rr rr         jsr lookupDefs
00068Dr 1               
00068Dr 1                   ; update instruction pointer
00068Dr 1  A4 00            ldy NUL
00068Fr 1  B1 F8            lda (tos), y
000691r 1  85 F0            sta ipt + 0
000693r 1  C8               iny
000694r 1  B1 F8            lda (tos), y
000696r 1  85 F1            sta ipt + 1
000698r 1               
000698r 1                   ; next
000698r 1  6C rr rr         jmp (vNext)
00069Br 1               
00069Br 1               ;----------------------------------------------------------------------
00069Br 1               lookupDeft:
00069Br 1  A5 F6            lda ap
00069Dr 1  8D rr rr         sta vEdited
0006A0r 1                   ; fall throught
0006A0r 1               
0006A0r 1               lookupDefs:
0006A0r 1  38               sec
0006A1r 1  A5 F6            lda ap
0006A3r 1  E5 41            sbc 'A'
0006A5r 1  0A               asl
0006A6r 1  A8               tay
0006A7r 1                   ; offset
0006A7r 1  18               clc
0006A8r 1  6D rr rr         adc vDefs + 0
0006ABr 1  85 F8            sta tos + 0
0006ADr 1  A5 00            lda NUL
0006AFr 1  6D rr rr         adc vDefs + 1
0006B2r 1  85 F9            sta tos + 1
0006B4r 1  60               rts
0006B5r 1               
0006B5r 1               ;----------------------------------------------------------------------
0006B5r 1               ; Copy a user macro to tib
0006B5r 1               ; lookup up def based on a number at data stack
0006B5r 1               ;
0006B5r 1               editDef_:
0006B5r 1                   ; which one
0006B5r 1  20 rr rr         jsr spull
0006B8r 1               
0006B8r 1                   ; toChar
0006B8r 1  18               clc
0006B9r 1  A9 41            lda #'A'
0006BBr 1  65 F8            adc tos + 0
0006BDr 1  85 F6            sta ap
0006BFr 1               
0006BFr 1  20 rr rr         jsr lookupDeft
0006C2r 1               
0006C2r 1                   ; origin
0006C2r 1  B1 F8            lda (tos), y
0006C4r 1  85 FA            sta nos + 0
0006C6r 1  C8               iny
0006C7r 1  B1 F8            lda (tos), y
0006C9r 1  85 FB            sta nos + 1
0006CBr 1               
0006CBr 1  A4 00            ldy NUL
0006CDr 1                   ; empty ?
0006CDr 1  B1 FA            lda (nos), y
0006CFr 1  F0 26            beq editDef3    ; is NUL ?
0006D1r 1  C9 3B            cmp #';'        ; is end ?
0006D3r 1  F0 22            beq editDef3
0006D5r 1               
0006D5r 1                   ; else
0006D5r 1                   ; ldy NUL
0006D5r 1               
0006D5r 1                   ; destiny
0006D5r 1  20 rr rr         jsr tib2tos
0006D8r 1               
0006D8r 1  A9 3A            lda #':'
0006DAr 1  20 rr rr         jsr writeChar
0006DDr 1  20 rr rr         jsr inctos
0006E0r 1               
0006E0r 1  A5 F6            lda ap
0006E2r 1  20 rr rr         jsr writeChar
0006E5r 1  20 rr rr         jsr inctos
0006E8r 1               
0006E8r 1  B8               clv
0006E9r 1  50 03            bvc editDef2
0006EBr 1               
0006EBr 1               editDef1:
0006EBr 1  C8               iny
0006ECr 1  F0 09            beq editDef3
0006EEr 1               
0006EEr 1               editDef2:
0006EEr 1  B1 FA            lda (nos), y
0006F0r 1  20 rr rr         jsr writeChar
0006F3r 1  C9 3B            cmp #';'
0006F5r 1  D0 F4            bne editDef1
0006F7r 1               
0006F7r 1               editDef3:
0006F7r 1  A9 rr            lda #<tib
0006F9r 1  8D rr rr         sta vTIBPtr + 0
0006FCr 1  A9 rr            lda #>tib
0006FEr 1  8D rr rr         sta vTIBPtr + 1
000701r 1                   ; next
000701r 1  6C rr rr         jmp (vNext)
000704r 1               
000704r 1               ;----------------------------------------------------------------------
000704r 1               writeChar:
000704r 1  91 F8            sta (tos), y
000706r 1  4C rr rr         jmp putchar
000709r 1               
000709r 1               ;----------------------------------------------------------------------
000709r 1               ; push an user variable
000709r 1               var_:
000709r 1  85 F6            sta ap
00070Br 1  A9 rr            lda #<vars
00070Dr 1  85 F8            sta tos + 0
00070Fr 1  A9 rr            lda #>vars
000711r 1  85 F9            sta tos + 1
000713r 1  4C rr rr         jmp a2z
000716r 1               
000716r 1               ;----------------------------------------------------------------------
000716r 1               ; push a mint variable
000716r 1               sysVar_:
000716r 1  85 F6            sta ap
000718r 1  A9 rr            lda #<vsys
00071Ar 1  85 F8            sta tos + 0
00071Cr 1  A9 rr            lda #>vsys
00071Er 1  85 F9            sta tos + 1
000720r 1  4C rr rr         jmp a2z
000723r 1               
000723r 1               ;----------------------------------------------------------------------
000723r 1               ; push a reference into stack
000723r 1               a2z:
000723r 1  38               sec
000724r 1  A5 F6            lda ap
000726r 1  E9 61            sbc #'a'
000728r 1  0A               asl
000729r 1  18               clc
00072Ar 1  65 F8            adc tos + 0
00072Cr 1  90 02            bcc @iscc
00072Er 1  E6 F9            inc tos + 1
000730r 1               @iscc:
000730r 1  20 rr rr         jsr spush
000733r 1                   ; next
000733r 1  6C rr rr         jmp (vNext)
000736r 1               
000736r 1               ;----------------------------------------------------------------------
000736r 1               ; skip spaces
000736r 1               nosp:
000736r 1  20 rr rr         jsr seekps
000739r 1  C9 20            cmp #' '
00073Br 1  F0 F9            beq nosp
00073Dr 1  60               rts
00073Er 1               
00073Er 1               ;----------------------------------------------------------------------
00073Er 1               group_:
00073Er 1  20 rr rr         jsr spull
000741r 1                   ; multiply by GROUP (64)
000741r 1                   ; swap byte
000741r 1  A5 F8            lda tos + 0
000743r 1  85 FB            sta nos + 1
000745r 1  A5 00            lda NUL
000747r 1  85 FA            sta nos + 0
000749r 1                   ; group is 64 bytes
000749r 1  46 FB            lsr nos + 1
00074Br 1  66 FA            ror nos + 0
00074Dr 1  46 FB            lsr nos + 1
00074Fr 1  66 FA            ror nos + 0
000751r 1                   ; save last group
000751r 1  AD rr rr         lda vDefs + 0
000754r 1  85 F8            sta tos + 0
000756r 1  AD rr rr         lda vDefs + 1
000759r 1  85 F9            sta tos + 1
00075Br 1  20 rr rr         jsr rpush
00075Er 1                   ; update group
00075Er 1  AD rr rr         lda defs + 0
000761r 1  18               clc
000762r 1  65 FA            adc nos + 0
000764r 1  8D rr rr         sta vDefs + 0
000767r 1  AD rr rr         lda defs + 1
00076Ar 1  65 FB            adc nos + 1
00076Cr 1  8D rr rr         sta vDefs + 1
00076Fr 1                   ; next
00076Fr 1  6C rr rr         jmp (vNext)
000772r 1               
000772r 1               ;----------------------------------------------------------------------
000772r 1               endGroup_:
000772r 1                   ; load last group
000772r 1  20 rr rr         jsr rpull
000775r 1  A5 F8            lda tos + 0
000777r 1  8D rr rr         sta vDefs + 0
00077Ar 1  A5 F9            lda tos + 1
00077Cr 1  8D rr rr         sta vDefs + 1
00077Fr 1                   ; next
00077Fr 1  6C rr rr         jmp (vNext)
000782r 1               
000782r 1               ;----------------------------------------------------------------------
000782r 1               getRef_:
000782r 1  20 rr rr         jsr nosp
000785r 1  85 F6            sta ap
000787r 1  20 rr rr         jsr lookupDefs
00078Ar 1  4C rr rr         jmp fetch_
00078Dr 1               
00078Dr 1               ;----------------------------------------------------------------------
00078Dr 1               arrDef_:
00078Dr 1  A5 00            lda FALSE
00078Fr 1  F0 02            beq arrDefs
000791r 1               
000791r 1               ;----------------------------------------------------------------------
000791r 1               cArrDef_:
000791r 1  A5 01            lda TRUE
000793r 1                   ; fall throught
000793r 1               
000793r 1               ;----------------------------------------------------------------------
000793r 1               arrDefs:
000793r 1                   ; save array mode
000793r 1  8D rr rr         sta vByteMode
000796r 1               
000796r 1                   ; save array start
000796r 1  A6 F2            ldx isp
000798r 1  CA               dex
000799r 1  CA               dex
00079Ar 1  AD rr rr         lda vHeap + 0
00079Dr 1  9D rr rr         sta apr + 0, x
0007A0r 1  AD rr rr         lda vHeap + 1
0007A3r 1  9D rr rr         sta apr + 1, x
0007A6r 1  86 F2            stx isp
0007A8r 1               
0007A8r 1                   ; array next
0007A8r 1  A9 rr            lda #<compNext
0007AAr 1  8D rr rr         sta vNext + 0
0007ADr 1  A9 rr            lda #>compNext
0007AFr 1  8D rr rr         sta vNext + 1
0007B2r 1               
0007B2r 1                   ; next
0007B2r 1  4C rr rr         jmp next
0007B5r 1               
0007B5r 1               ;----------------------------------------------------------------------
0007B5r 1               arrEnd_:
0007B5r 1               
0007B5r 1                   ; start of array
0007B5r 1  20 rr rr         jsr rpull
0007B8r 1  20 rr rr         jsr spush
0007BBr 1               
0007BBr 1                   ; bytes
0007BBr 1  38               sec
0007BCr 1  AD rr rr         lda vHeap + 0
0007BFr 1  E5 F8            sbc tos + 0
0007C1r 1  85 F8            sta tos + 0
0007C3r 1  AD rr rr         lda vHeap + 1
0007C6r 1  E5 F9            sbc tos + 1
0007C8r 1  85 F9            sta tos + 1
0007CAr 1               
0007CAr 1  AD rr rr         lda vByteMode
0007CDr 1  D0 04            bne @isne
0007CFr 1                   ; words
0007CFr 1  46 F8            lsr tos + 0
0007D1r 1  66 F9            ror tos + 1
0007D3r 1               @isne:
0007D3r 1                   ; size of array
0007D3r 1  20 rr rr         jsr spush
0007D6r 1               
0007D6r 1                   ; common next
0007D6r 1  A9 rr            lda #<next
0007D8r 1  8D rr rr         sta vNext + 0
0007DBr 1  A9 rr            lda #>next
0007DDr 1  8D rr rr         sta vNext + 1
0007E0r 1               
0007E0r 1                   ; next
0007E0r 1  4C rr rr         jmp next
0007E3r 1               
0007E3r 1               ;----------------------------------------------------------------------
0007E3r 1               def_:
0007E3r 1                   ; must be a A-Z
0007E3r 1  20 rr rr         jsr nosp
0007E6r 1               
0007E6r 1                   ; get slot at list
0007E6r 1  85 F6            sta ap
0007E8r 1  20 rr rr         jsr lookupDefs
0007EBr 1               
0007EBr 1                   ; get heap
0007EBr 1  20 rr rr         jsr heap2nos
0007EEr 1               
0007EEr 1                   ; put heap at list
0007EEr 1  A5 FA            lda nos + 0
0007F0r 1  91 F8            sta (tos), y
0007F2r 1  C8               iny
0007F3r 1  A5 FB            lda nos + 1
0007F5r 1  91 F8            sta (tos), y
0007F7r 1               
0007F7r 1                   ; copy to heap
0007F7r 1  A4 00            ldy NUL
0007F9r 1               @loop:
0007F9r 1  B1 F0            lda (ipt), y
0007FBr 1  91 FA            sta (nos), y
0007FDr 1  F0 07            beq @ends
0007FFr 1  C8               iny
000800r 1  F0 04            beq @ends
000802r 1  C9 3B            cmp #';'
000804r 1  D0 F3            bne @loop
000806r 1               @ends:
000806r 1                   ; update heap
000806r 1  98               tya
000807r 1  85 F6            sta ap
000809r 1  20 rr rr         jsr add2heap
00080Cr 1                   ; update instruction pointer
00080Cr 1  A5 F6            lda ap
00080Er 1  4C rr rr         jmp add2ps
000811r 1               
000811r 1               ;----------------------------------------------------------------------
000811r 1               ; skip while nest
000811r 1               skipnest:
000811r 1  A9 01            lda #$01
000813r 1  85 F7            sta ns
000815r 1               @loop:
000815r 1  20 rr rr         jsr seekps
000818r 1  20 rr rr         jsr nesting
00081Br 1  A5 F7            lda ns
00081Dr 1  D0 F6            bne @loop
00081Fr 1                   ; next
00081Fr 1  6C rr rr         jmp (vNext)
000822r 1               
000822r 1               ;----------------------------------------------------------------------
000822r 1               break_:
000822r 1  20 rr rr         jsr spull
000825r 1  A5 F8            lda tos + 0
000827r 1  D0 03            bne @isne
000829r 1                   ; parse frame
000829r 1  6C rr rr         jmp (vNext)
00082Cr 1               @isne:
00082Cr 1                   ; skip frame
00082Cr 1  18               clc
00082Dr 1  A5 F3            lda irp
00082Fr 1  69 06            adc #6
000831r 1  85 F3            sta irp
000833r 1               @iscc:
000833r 1  4C rr rr         jmp skipnest
000836r 1               
000836r 1               ;----------------------------------------------------------------------
000836r 1               ; Left parentesis ( begins a loop
000836r 1               begin_:
000836r 1               
000836r 1                   ; tos is zero ?
000836r 1  20 rr rr         jsr spull
000839r 1  A5 F8            lda tos + 0
00083Br 1  F0 D4            beq skipnest
00083Dr 1               
00083Dr 1                   ; alloc a frame
00083Dr 1  38               sec
00083Er 1  A5 F3            lda irp
000840r 1  E9 06            sbc #6
000842r 1  85 F3            sta irp
000844r 1               
000844r 1                   ; make a frame
000844r 1  A6 F3            ldx irp
000846r 1                   ; counter
000846r 1  A5 00            lda NUL
000848r 1  9D rr rr         sta apr + 0, x
00084Br 1  9D rr rr         sta apr + 1, x
00084Er 1                   ; limit
00084Er 1  A5 F8            lda tos + 0
000850r 1  9D rr rr         sta apr + 2, x
000853r 1  A5 F9            lda tos + 1
000855r 1  9D rr rr         sta apr + 3, x
000858r 1                   ; pointer
000858r 1  A5 F0            lda ipt + 0
00085Ar 1  9D rr rr         sta apr + 4, x
00085Dr 1  A5 F1            lda ipt + 1
00085Fr 1  9D rr rr         sta apr + 5, x
000862r 1                   ; next
000862r 1  6C rr rr         jmp (vNext)
000865r 1               
000865r 1               ;----------------------------------------------------------------------
000865r 1               ; Right parentesis ) again a loop
000865r 1               again_:
000865r 1                   ; check if IFTEMode $FFFF
000865r 1  BD rr rr         lda apr + 0, x
000868r 1  3D rr rr         and apr + 1, x
00086Br 1  C9 FF            cmp #$FF
00086Dr 1  D0 13            bne again1
00086Fr 1               
00086Fr 1                   ; push FALSE
00086Fr 1  A5 00            lda FALSE
000871r 1  85 F8            sta tos + 0
000873r 1  85 F9            sta tos + 1
000875r 1  20 rr rr         jsr spush
000878r 1               
000878r 1                   ; drop IFTEMmode
000878r 1  18               clc
000879r 1  A5 F3            lda irp
00087Br 1  69 02            adc #2
00087Dr 1  85 F3            sta irp
00087Fr 1                   ; next
00087Fr 1  6C rr rr         jmp (vNext)
000882r 1               
000882r 1               again1:
000882r 1                   ; test end
000882r 1  A6 F3            ldx irp
000884r 1  BD rr rr         lda apr + 2, x
000887r 1  DD rr rr         cmp apr + 0, x
00088Ar 1  D0 12            bne @noend
00088Cr 1  BD rr rr         lda apr + 3, x
00088Fr 1  DD rr rr         cmp apr + 1, x
000892r 1  D0 0A            bne @noend
000894r 1               
000894r 1                   ; end of loop
000894r 1                   ; drop frame
000894r 1  18               clc
000895r 1  A5 F3            lda irp
000897r 1  69 06            adc #6
000899r 1  85 F3            sta irp
00089Br 1                   ; next
00089Br 1  6C rr rr         jmp (vNext)
00089Er 1               
00089Er 1               @noend:
00089Er 1                   ; increase counter
00089Er 1  FE rr rr         inc apr + 0, x
0008A1r 1  D0 03            bne @novr
0008A3r 1  FE rr rr         inc apr + 1, x
0008A6r 1               @novr:
0008A6r 1               
0008A6r 1                   ; return at begin
0008A6r 1  BD rr rr         lda apr + 4, x
0008A9r 1  85 F0            sta ipt + 0
0008ABr 1  BD rr rr         lda apr + 5, x
0008AEr 1  85 F1            sta ipt + 1
0008B0r 1               
0008B0r 1                   ; next
0008B0r 1  6C rr rr         jmp (vNext)
0008B3r 1               
0008B3r 1               ;----------------------------------------------------------------------
0008B3r 1               j_:
0008B3r 1  38               sec
0008B4r 1  A5 F3            lda irp
0008B6r 1  E9 06            sbc #6
0008B8r 1  AA               tax
0008B9r 1  4C rr rr         jmp indx
0008BCr 1               
0008BCr 1               ;----------------------------------------------------------------------
0008BCr 1               i_:
0008BCr 1  A6 F3            ldx irp
0008BEr 1                   ; fall through
0008BEr 1               
0008BEr 1               ;----------------------------------------------------------------------
0008BEr 1               indx:
0008BEr 1  BD rr rr         lda aps + 0, x
0008C1r 1  85 F8            sta tos + 0
0008C3r 1  BD rr rr         lda aps + 1, x
0008C6r 1  85 F9            sta tos + 1
0008C8r 1  20 rr rr         jsr spush
0008CBr 1                   ; next
0008CBr 1  6C rr rr         jmp (vNext)
0008CEr 1               
0008CEr 1               ;----------------------------------------------------------------------
0008CEr 1               ifte_:
0008CEr 1  20 rr rr         jsr spull
0008D1r 1  A5 F8            lda tos + 0
0008D3r 1  05 F9            ora tos + 1
0008D5r 1  D0 08            bne @istrue
0008D7r 1  E6 F8            inc tos + 0
0008D9r 1  20 rr rr         jsr spush
0008DCr 1  4C rr rr         jmp skipnest
0008DFr 1               @istrue:
0008DFr 1  A9 FF            lda #$FF
0008E1r 1  85 F8            sta tos + 0
0008E3r 1  85 F9            sta tos + 1
0008E5r 1  20 rr rr         jsr rpush
0008E8r 1                   ; next
0008E8r 1  6C rr rr         jmp (vNext)
0008EBr 1               
0008EBr 1               ;----------------------------------------------------------------------
0008EBr 1               ; 6502 stack is fixed and round robin
0008EBr 1               ; no need control deep
0008EBr 1               etx_:
0008EBr 1  4C rr rr         jmp interpret
0008EEr 1               
0008EEr 1               ;----------------------------------------------------------------------
0008EEr 1               iSysVars:
0008EEr 1  rr rr            .word  dStack               ; a vS0
0008F0r 1  00 00            .word  FALSE                ; b vBase16
0008F2r 1  rr rr            .word  tib                  ; c vTIBPtr
0008F4r 1  rr rr            .word  defs                 ; d vDEFS
0008F6r 1  00 00            .word  FALSE                ; e vEdited
0008F8r 1  rr rr            .word  rStack               ; f vR0
0008FAr 1  rr rr            .word  next                 ; g dispatcher
0008FCr 1  rr rr            .word  heap                 ; h vHeap
0008FEr 1               fSysVars:
0008FEr 1               
0008FEr 1               dysys = fSysVars - iSysVars
0008FEr 1               
0008FEr 1               ;----------------------------------------------------------------------
0008FEr 1               mint_:
0008FEr 1               
0008FEr 1               ; wise
0008FEr 1               
0008FEr 1  78               sei
0008FFr 1  A9 FF            lda #$FF
000901r 1  A8               tay
000902r 1  AA               tax
000903r 1  9A               txs
000904r 1  A5 00            lda NUL
000906r 1  85 F7            sta ns
000908r 1  D8               cld
000909r 1  58               cli
00090Ar 1               
00090Ar 1  20 rr rr         jsr initialize
00090Dr 1               
00090Dr 1               ; default system values
00090Dr 1  AC 10 00         ldy dysys
000910r 1               @loop:
000910r 1  B9 rr rr         lda iSysVars, y
000913r 1  99 rr rr         sta vsys, y
000916r 1  88               dey
000917r 1  D0 F7            bne @loop
000919r 1               
000919r 1               ; safe
000919r 1  A9 rr            lda #<next
00091Br 1  8D rr rr         sta vNext + 0
00091Er 1  A9 rr            lda #>next
000920r 1  8D rr rr         sta vNext + 1
000923r 1               
000923r 1  20 rr rr         jsr printStr
000926r 1  4D 49 4E 54      .asciiz "MINT 6502 V1.0\r\n"
00092Ar 1  20 36 35 30  
00092Er 1  32 20 56 31  
000937r 1               
000937r 1                   ; auto reset if stack overflows
000937r 1  20 rr rr         jsr interpret
00093Ar 1  4C rr rr         jmp mint_
00093Dr 1               
00093Dr 1               ;----------------------------------------------------------------------
00093Dr 1               initialize:
00093Dr 1               
00093Dr 1               ; defaults values
00093Dr 1  A9 rr            lda #<vars
00093Fr 1  85 F8            sta tos + 0
000941r 1  A9 rr            lda #>vars
000943r 1  85 F9            sta tos + 1
000945r 1  A9 rr            lda #<vsys
000947r 1  85 FA            sta nos + 0
000949r 1  A9 rr            lda #>vsys
00094Br 1  85 FB            sta nos + 1
00094Dr 1  A4 40            ldy GRPSIZE
00094Fr 1  A5 00            lda NUL
000951r 1               @loop1:
000951r 1  91 F8            sta (tos), y
000953r 1  91 FA            sta (nos), y
000955r 1  88               dey
000956r 1  91 F8            sta (tos), y
000958r 1  91 FA            sta (nos), y
00095Ar 1  88               dey
00095Br 1  D0 F4            bne @loop1
00095Dr 1               
00095Dr 1               ; default function
00095Dr 1  A9 40            lda #<(GRPSIZE * NUMGRPS)
00095Fr 1  85 FA            sta nos + 0
000961r 1  A9 01            lda #>(GRPSIZE * NUMGRPS)
000963r 1  85 FB            sta nos + 1
000965r 1  A9 rr            lda #<defs
000967r 1  85 F8            sta tos + 0
000969r 1  A9 rr            lda #>defs
00096Br 1  85 F9            sta tos + 1
00096Dr 1               
00096Dr 1               @loop2:
00096Dr 1                   ; default
00096Dr 1  A4 00            ldy NUL
00096Fr 1  A9 rr            lda #<empty_
000971r 1  91 F8            sta (tos), y
000973r 1  C8               iny
000974r 1  A9 rr            lda #>empty_
000976r 1  91 F8            sta (tos), y
000978r 1               
000978r 1                   ; increment
000978r 1  18               clc
000979r 1  A5 F8            lda tos + 0
00097Br 1  69 02            adc #2
00097Dr 1  85 F8            sta tos + 0
00097Fr 1  A5 F9            lda tos + 1
000981r 1  69 00            adc #0
000983r 1  85 F9            sta tos + 1
000985r 1               
000985r 1                   ; decrement
000985r 1  38               sec
000986r 1  A5 FA            lda nos + 0
000988r 1  E9 02            sbc #2
00098Ar 1  85 FA            sta nos + 0
00098Cr 1  A5 FB            lda nos + 1
00098Er 1  E9 00            sbc #0
000990r 1  85 FB            sta nos + 1
000992r 1               
000992r 1                   ; ends ?
000992r 1  05 FA            ora nos + 0
000994r 1  D0 D7            bne @loop2
000996r 1               
000996r 1  60               rts
000997r 1               
000997r 1  AD DE        .word $DEAD
000999r 1               
000999r 1               ;----------------------------------------------------------------------
000999r 1               ;optcodes: parsed by opt_ (next)
000999r 1               ;altcodes: parsed by alt_
000999r 1               ;ctlcodes: maybe in a future...
000999r 1               
000999r 1               ; *********************************************************************
000999r 1               ; Jump Tables, optmized for single index
000999r 1               ; *********************************************************************
000999r 1               
000999r 1               ; .align $100
000999r 1               
000999r 1               ;----------------------------------------------------------------------
000999r 1               optcodeslo:
000999r 1  rr              .byte  <exit_    ;   NUL
00099Ar 1  rr              .byte  <nop_     ;   SOH
00099Br 1  rr              .byte  <nop_     ;   STX
00099Cr 1  rr              .byte  <etx_     ;   ETX
00099Dr 1  rr              .byte  <nop_     ;   EOT
00099Er 1  rr              .byte  <nop_     ;   ENQ
00099Fr 1  rr              .byte  <nop_     ;   ACK
0009A0r 1  rr              .byte  <nop_     ;   BEL
0009A1r 1  rr              .byte  <nop_     ;   BS
0009A2r 1  rr              .byte  <nop_     ;   TAB
0009A3r 1  rr              .byte  <nop_     ;   LF
0009A4r 1  rr              .byte  <nop_     ;   VT
0009A5r 1  rr              .byte  <nop_     ;   FF
0009A6r 1  rr              .byte  <nop_     ;   CR
0009A7r 1  rr              .byte  <nop_     ;   SO
0009A8r 1  rr              .byte  <nop_     ;   SI
0009A9r 1  rr              .byte  <nop_     ;   DLE
0009AAr 1  rr              .byte  <nop_     ;   DC1
0009ABr 1  rr              .byte  <nop_     ;   DC2
0009ACr 1  rr              .byte  <nop_     ;   DC3
0009ADr 1  rr              .byte  <nop_     ;   DC4
0009AEr 1  rr              .byte  <nop_     ;   NAK
0009AFr 1  rr              .byte  <nop_     ;   SYN
0009B0r 1  rr              .byte  <nop_     ;   ETB
0009B1r 1  rr              .byte  <nop_     ;   CAN
0009B2r 1  rr              .byte  <nop_     ;   EM
0009B3r 1  rr              .byte  <nop_     ;   SUB
0009B4r 1  rr              .byte  <nop_     ;   ESC
0009B5r 1  rr              .byte  <nop_     ;   FS
0009B6r 1  rr              .byte  <nop_     ;   GS
0009B7r 1  rr              .byte  <nop_     ;   RS
0009B8r 1  rr              .byte  <nop_     ;   US
0009B9r 1  rr              .byte  <nop_     ;   SP
0009BAr 1  rr              .byte  <store_   ;   !
0009BBr 1  rr              .byte  <dup_     ;   "
0009BCr 1  rr              .byte  <hex_     ;    #
0009BDr 1  rr              .byte  <swap_    ;    $
0009BEr 1  rr              .byte  <over_    ;    %
0009BFr 1  rr              .byte  <and_     ;    &
0009C0r 1  rr              .byte  <drop_    ;    '
0009C1r 1  rr              .byte  <begin_   ;    (
0009C2r 1  rr              .byte  <again_   ;    )
0009C3r 1  rr              .byte  <mul_     ;    * multiply 16x16
0009C4r 1  rr              .byte  <add_     ;    +
0009C5r 1  rr              .byte  <hdot_    ;    ,
0009C6r 1  rr              .byte  <sub_     ;    -
0009C7r 1  rr              .byte  <dot_     ;    .
0009C8r 1  rr              .byte  <div_     ;    / divide 16x16
0009C9r 1  rr              .byte  <num_     ;    0
0009CAr 1  rr              .byte  <num_     ;    1
0009CBr 1  rr              .byte  <num_     ;    2
0009CCr 1  rr              .byte  <num_     ;    3
0009CDr 1  rr              .byte  <num_     ;    4
0009CEr 1  rr              .byte  <num_     ;    5
0009CFr 1  rr              .byte  <num_     ;    6
0009D0r 1  rr              .byte  <num_     ;    7
0009D1r 1  rr              .byte  <num_     ;    8
0009D2r 1  rr              .byte  <num_     ;    9
0009D3r 1  rr              .byte  <def_     ;    :
0009D4r 1  rr              .byte  <ret_     ;    ;
0009D5r 1  rr              .byte  <lt_      ;    <
0009D6r 1  rr              .byte  <eq_      ;    =
0009D7r 1  rr              .byte  <gt_      ;    >
0009D8r 1  rr              .byte  <getRef_  ;    ?
0009D9r 1  rr              .byte  <fetch_   ;    @
0009DAr 1  rr              .byte  <call_    ;    A
0009DBr 1  rr              .byte  <call_    ;    B
0009DCr 1  rr              .byte  <call_    ;    C
0009DDr 1  rr              .byte  <call_    ;    D
0009DEr 1  rr              .byte  <call_    ;    E
0009DFr 1  rr              .byte  <call_    ;    F
0009E0r 1  rr              .byte  <call_    ;    G
0009E1r 1  rr              .byte  <call_    ;    H
0009E2r 1  rr              .byte  <call_    ;    I
0009E3r 1  rr              .byte  <call_    ;    J
0009E4r 1  rr              .byte  <call_    ;    K
0009E5r 1  rr              .byte  <call_    ;    L
0009E6r 1  rr              .byte  <call_    ;    M
0009E7r 1  rr              .byte  <call_    ;    N
0009E8r 1  rr              .byte  <call_    ;    O
0009E9r 1  rr              .byte  <call_    ;    P
0009EAr 1  rr              .byte  <call_    ;    Q
0009EBr 1  rr              .byte  <call_    ;    R
0009ECr 1  rr              .byte  <call_    ;    S
0009EDr 1  rr              .byte  <call_    ;    T
0009EEr 1  rr              .byte  <call_    ;    U
0009EFr 1  rr              .byte  <call_    ;    V
0009F0r 1  rr              .byte  <call_    ;    W
0009F1r 1  rr              .byte  <call_    ;    X
0009F2r 1  rr              .byte  <call_    ;    Y
0009F3r 1  rr              .byte  <call_    ;    Z
0009F4r 1  rr              .byte  <arrDef_  ;    [
0009F5r 1  rr              .byte  <alt_     ;    \
0009F6r 1  rr              .byte  <arrEnd_  ;    ]
0009F7r 1  rr              .byte  <xor_     ;    ^
0009F8r 1  rr              .byte  <neg_     ;    _
0009F9r 1  rr              .byte  <str_     ;    `
0009FAr 1  rr              .byte  <var_     ;    a
0009FBr 1  rr              .byte  <var_     ;    b
0009FCr 1  rr              .byte  <var_     ;    c
0009FDr 1  rr              .byte  <var_     ;    d
0009FEr 1  rr              .byte  <var_     ;    e
0009FFr 1  rr              .byte  <var_     ;    f
000A00r 1  rr              .byte  <var_     ;    g
000A01r 1  rr              .byte  <var_     ;    h
000A02r 1  rr              .byte  <var_     ;    i
000A03r 1  rr              .byte  <var_     ;    j
000A04r 1  rr              .byte  <var_     ;    k
000A05r 1  rr              .byte  <var_     ;    l
000A06r 1  rr              .byte  <var_     ;    m
000A07r 1  rr              .byte  <var_     ;    n
000A08r 1  rr              .byte  <var_     ;    o
000A09r 1  rr              .byte  <var_     ;    p
000A0Ar 1  rr              .byte  <var_     ;    q
000A0Br 1  rr              .byte  <var_     ;    r
000A0Cr 1  rr              .byte  <var_     ;    s
000A0Dr 1  rr              .byte  <var_     ;    t
000A0Er 1  rr              .byte  <var_     ;    u
000A0Fr 1  rr              .byte  <var_     ;    v
000A10r 1  rr              .byte  <var_     ;    w
000A11r 1  rr              .byte  <var_     ;    x
000A12r 1  rr              .byte  <var_     ;    y
000A13r 1  rr              .byte  <var_     ;    z
000A14r 1  rr              .byte  <shl_     ;    {
000A15r 1  rr              .byte  <or_      ;    |
000A16r 1  rr              .byte  <shr_     ;    }
000A17r 1  rr              .byte  <inv_     ;    ~
000A18r 1  rr              .byte  <nop_     ;    backspace
000A19r 1               
000A19r 1               optcodeshi:
000A19r 1  rr              .byte  >exit_    ;   NUL
000A1Ar 1  rr              .byte  >nop_     ;   SOH
000A1Br 1  rr              .byte  >nop_     ;   STX
000A1Cr 1  rr              .byte  >etx_     ;   ETX
000A1Dr 1  rr              .byte  >nop_     ;   EOT
000A1Er 1  rr              .byte  >nop_     ;   ENQ
000A1Fr 1  rr              .byte  >nop_     ;   ACK
000A20r 1  rr              .byte  >nop_     ;   BEL
000A21r 1  rr              .byte  >nop_     ;   BS
000A22r 1  rr              .byte  >nop_     ;   TAB
000A23r 1  rr              .byte  >nop_     ;   LF
000A24r 1  rr              .byte  >nop_     ;   VT
000A25r 1  rr              .byte  >nop_     ;   FF
000A26r 1  rr              .byte  >nop_     ;   CR
000A27r 1  rr              .byte  >nop_     ;   SO
000A28r 1  rr              .byte  >nop_     ;   SI
000A29r 1  rr              .byte  >nop_     ;   DLE
000A2Ar 1  rr              .byte  >nop_     ;   DC1
000A2Br 1  rr              .byte  >nop_     ;   DC2
000A2Cr 1  rr              .byte  >nop_     ;   DC3
000A2Dr 1  rr              .byte  >nop_     ;   DC4
000A2Er 1  rr              .byte  >nop_     ;   NAK
000A2Fr 1  rr              .byte  >nop_     ;   SYN
000A30r 1  rr              .byte  >nop_     ;   ETB
000A31r 1  rr              .byte  >nop_     ;   CAN
000A32r 1  rr              .byte  >nop_     ;   EM
000A33r 1  rr              .byte  >nop_     ;   SUB
000A34r 1  rr              .byte  >nop_     ;   ESC
000A35r 1  rr              .byte  >nop_     ;   FS
000A36r 1  rr              .byte  >nop_     ;   GS
000A37r 1  rr              .byte  >nop_     ;   RS
000A38r 1  rr              .byte  >nop_     ;   US
000A39r 1  rr              .byte  >nop_     ;   SP
000A3Ar 1  rr              .byte  >store_   ;   !
000A3Br 1  rr              .byte  >dup_     ;   "
000A3Cr 1  rr              .byte  >hex_     ;    #
000A3Dr 1  rr              .byte  >swap_    ;    $
000A3Er 1  rr              .byte  >over_    ;    %
000A3Fr 1  rr              .byte  >and_     ;    &
000A40r 1  rr              .byte  >drop_    ;    '
000A41r 1  rr              .byte  >begin_   ;    (
000A42r 1  rr              .byte  >again_   ;    )
000A43r 1  rr              .byte  >mul_     ;    *  multiply 16x16
000A44r 1  rr              .byte  >add_     ;    +
000A45r 1  rr              .byte  >hdot_    ;    ,
000A46r 1  rr              .byte  >sub_     ;    -
000A47r 1  rr              .byte  >dot_     ;    .
000A48r 1  rr              .byte  >div_     ;    /  divide 16x16
000A49r 1  rr              .byte  >num_     ;    0
000A4Ar 1  rr              .byte  >num_     ;    1
000A4Br 1  rr              .byte  >num_     ;    2
000A4Cr 1  rr              .byte  >num_     ;    3
000A4Dr 1  rr              .byte  >num_     ;    4
000A4Er 1  rr              .byte  >num_     ;    5
000A4Fr 1  rr              .byte  >num_     ;    6
000A50r 1  rr              .byte  >num_     ;    7
000A51r 1  rr              .byte  >num_     ;    8
000A52r 1  rr              .byte  >num_     ;    9
000A53r 1  rr              .byte  >def_     ;    :
000A54r 1  rr              .byte  >ret_     ;    ;
000A55r 1  rr              .byte  >lt_      ;    <
000A56r 1  rr              .byte  >eq_      ;    =
000A57r 1  rr              .byte  >gt_      ;    >
000A58r 1  rr              .byte  >getRef_  ;    ?
000A59r 1  rr              .byte  >fetch_   ;    @
000A5Ar 1  rr              .byte  >call_    ;    A
000A5Br 1  rr              .byte  >call_    ;    B
000A5Cr 1  rr              .byte  >call_    ;    C
000A5Dr 1  rr              .byte  >call_    ;    D
000A5Er 1  rr              .byte  >call_    ;    E
000A5Fr 1  rr              .byte  >call_    ;    F
000A60r 1  rr              .byte  >call_    ;    G
000A61r 1  rr              .byte  >call_    ;    H
000A62r 1  rr              .byte  >call_    ;    I
000A63r 1  rr              .byte  >call_    ;    J
000A64r 1  rr              .byte  >call_    ;    K
000A65r 1  rr              .byte  >call_    ;    L
000A66r 1  rr              .byte  >call_    ;    M
000A67r 1  rr              .byte  >call_    ;    N
000A68r 1  rr              .byte  >call_    ;    O
000A69r 1  rr              .byte  >call_    ;    P
000A6Ar 1  rr              .byte  >call_    ;    Q
000A6Br 1  rr              .byte  >call_    ;    R
000A6Cr 1  rr              .byte  >call_    ;    S
000A6Dr 1  rr              .byte  >call_    ;    T
000A6Er 1  rr              .byte  >call_    ;    U
000A6Fr 1  rr              .byte  >call_    ;    V
000A70r 1  rr              .byte  >call_    ;    W
000A71r 1  rr              .byte  >call_    ;    X
000A72r 1  rr              .byte  >call_    ;    Y
000A73r 1  rr              .byte  >call_    ;    Z
000A74r 1  rr              .byte  >arrDef_  ;    [
000A75r 1  rr              .byte  >alt_     ;    \
000A76r 1  rr              .byte  >arrEnd_  ;    ]
000A77r 1  rr              .byte  >xor_     ;    ^
000A78r 1  rr              .byte  >neg_     ;    _
000A79r 1  rr              .byte  >str_     ;    `
000A7Ar 1  rr              .byte  >var_     ;    a
000A7Br 1  rr              .byte  >var_     ;    b
000A7Cr 1  rr              .byte  >var_     ;    c
000A7Dr 1  rr              .byte  >var_     ;    d
000A7Er 1  rr              .byte  >var_     ;    e
000A7Fr 1  rr              .byte  >var_     ;    f
000A80r 1  rr              .byte  >var_     ;    g
000A81r 1  rr              .byte  >var_     ;    h
000A82r 1  rr              .byte  >var_     ;    i
000A83r 1  rr              .byte  >var_     ;    j
000A84r 1  rr              .byte  >var_     ;    k
000A85r 1  rr              .byte  >var_     ;    l
000A86r 1  rr              .byte  >var_     ;    m
000A87r 1  rr              .byte  >var_     ;    n
000A88r 1  rr              .byte  >var_     ;    o
000A89r 1  rr              .byte  >var_     ;    p
000A8Ar 1  rr              .byte  >var_     ;    q
000A8Br 1  rr              .byte  >var_     ;    r
000A8Cr 1  rr              .byte  >var_     ;    s
000A8Dr 1  rr              .byte  >var_     ;    t
000A8Er 1  rr              .byte  >var_     ;    u
000A8Fr 1  rr              .byte  >var_     ;    v
000A90r 1  rr              .byte  >var_     ;    w
000A91r 1  rr              .byte  >var_     ;    x
000A92r 1  rr              .byte  >var_     ;    y
000A93r 1  rr              .byte  >var_     ;    z
000A94r 1  rr              .byte  >shl_     ;    {
000A95r 1  rr              .byte  >or_      ;    |
000A96r 1  rr              .byte  >shr_     ;    }
000A97r 1  rr              .byte  >inv_     ;    ~
000A98r 1  rr              .byte  >nop_     ;    backspace
000A99r 1               
000A99r 1               ;----------------------------------------------------------------------
000A99r 1               ; alternate function codes
000A99r 1               ctlcodeslo:
000A99r 1               altcodeslo:
000A99r 1  rr              .byte  <empty_      ; NUL ^@
000A9Ar 1  rr              .byte  <empty_      ; SOH ^A
000A9Br 1  rr              .byte  <toggleBase_ ; STX ^B
000A9Cr 1  rr              .byte  <empty_      ; ETX ^C
000A9Dr 1  rr              .byte  <empty_      ; EOT ^D
000A9Er 1  rr              .byte  <edit_       ; ENQ ^E
000A9Fr 1  rr              .byte  <empty_      ; ACK ^F
000AA0r 1  rr              .byte  <empty_      ; BEL ^G
000AA1r 1  rr              .byte  <backsp_     ; BS  ^H
000AA2r 1  rr              .byte  <empty_      ; TAB ^I
000AA3r 1  rr              .byte  <reedit_     ; LF  ^J
000AA4r 1  rr              .byte  <empty_      ; VT  ^K
000AA5r 1  rr              .byte  <list_       ; FF  ^L
000AA6r 1  rr              .byte  <empty_      ; CR  ^M
000AA7r 1  rr              .byte  <empty_      ; SO  ^N
000AA8r 1  rr              .byte  <empty_      ; SI  ^O
000AA9r 1  rr              .byte  <printStack_ ; DLE ^P
000AAAr 1  rr              .byte  <empty_      ; DC1 ^Q
000AABr 1  rr              .byte  <empty_      ; DC2 ^R
000AACr 1  rr              .byte  <empty_      ; DC3 ^S
000AADr 1  rr              .byte  <empty_      ; DC4 ^T
000AAEr 1  rr              .byte  <empty_      ; NAK ^U
000AAFr 1  rr              .byte  <empty_      ; SYN ^V
000AB0r 1  rr              .byte  <empty_      ; ETB ^W
000AB1r 1  rr              .byte  <empty_      ; CAN ^X
000AB2r 1  rr              .byte  <empty_      ; EM  ^Y
000AB3r 1  rr              .byte  <empty_      ; SUB ^Z
000AB4r 1  rr              .byte  <empty_      ; ESC ^[
000AB5r 1  rr              .byte  <empty_      ; FS  ^\
000AB6r 1  rr              .byte  <empty_      ; GS  ^]
000AB7r 1  rr              .byte  <empty_      ; RS  ^^
000AB8r 1  rr              .byte  <empty_      ; US  ^_)
000AB9r 1  rr              .byte  <aNop_       ; SP  ^`
000ABAr 1  rr              .byte  <cStore_     ;    !
000ABBr 1  rr              .byte  <aNop_       ;    "
000ABCr 1  rr              .byte  <aNop_       ;    #
000ABDr 1  rr              .byte  <aNop_       ;    $  ( -- adr ) text input ptr
000ABEr 1  rr              .byte  <aNop_       ;    %
000ABFr 1  rr              .byte  <aNop_       ;    &
000AC0r 1  rr              .byte  <aNop_       ;    '
000AC1r 1  rr              .byte  <ifte_       ;    (  ( b -- )
000AC2r 1  rr              .byte  <aNop_       ;    )
000AC3r 1  rr              .byte  <aNop_       ;    *
000AC4r 1  rr              .byte  <incr_       ;    +  ( adr -- ) increments variable at address
000AC5r 1  rr              .byte  <aNop_       ;    ,
000AC6r 1  rr              .byte  <decr_       ;    -  ( adr -- ) decrements variable at address
000AC7r 1  rr              .byte  <aNop_       ;    .
000AC8r 1  rr              .byte  <aNop_       ;    /
000AC9r 1  rr              .byte  <aNop_       ;    0
000ACAr 1  rr              .byte  <aNop_       ;    1
000ACBr 1  rr              .byte  <aNop_       ;    2
000ACCr 1  rr              .byte  <aNop_       ;    3
000ACDr 1  rr              .byte  <aNop_       ;    4
000ACEr 1  rr              .byte  <aNop_       ;    5
000ACFr 1  rr              .byte  <aNop_       ;    6
000AD0r 1  rr              .byte  <aNop_       ;    7
000AD1r 1  rr              .byte  <aNop_       ;    8
000AD2r 1  rr              .byte  <aNop_       ;    9
000AD3r 1  rr              .byte  <aNop_       ;    :  start defining a macro
000AD4r 1  rr              .byte  <aNop_       ;    ;
000AD5r 1  rr              .byte  <aNop_       ;    <
000AD6r 1  rr              .byte  <aNop_       ;    =
000AD7r 1  rr              .byte  <aNop_       ;    >
000AD8r 1  rr              .byte  <aNop_       ;    ?
000AD9r 1  rr              .byte  <cFetch_     ;    @
000ADAr 1  rr              .byte  <aNop_       ;    A
000ADBr 1  rr              .byte  <break_      ;    B
000ADCr 1  rr              .byte  <nop_        ;    C
000ADDr 1  rr              .byte  <depth_      ;    D  ( -- val ) depth of data stack
000ADEr 1  rr              .byte  <emit_       ;    E  ( val -- ) emits a char to output
000ADFr 1  rr              .byte  <aNop_       ;    F
000AE0r 1  rr              .byte  <go_         ;    G  ( -- ? ) execute mint definition
000AE1r 1  rr              .byte  <keyq_       ;    H  ( verify if key hit )
000AE2r 1  rr              .byte  <inPort_     ;    I  ( port -- val )
000AE3r 1  rr              .byte  <aNop_       ;    J
000AE4r 1  rr              .byte  <key_        ;    K  ( -- val )  read a char from input
000AE5r 1  rr              .byte  <aNop_       ;    L
000AE6r 1  rr              .byte  <aNop_       ;    M
000AE7r 1  rr              .byte  <newln_      ;    N  ; prints a newline to output
000AE8r 1  rr              .byte  <outPort_    ;    O  ( val port -- )
000AE9r 1  rr              .byte  <printStk_   ;    P  ( -- ) non-destructively prints stack
000AEAr 1  rr              .byte  <aNop_       ;    Q  quits from Mint REPL
000AEBr 1  rr              .byte  <rot_        ;    R  ( a b c -- b c a )
000AECr 1  rr              .byte  <aNop_       ;    S
000AEDr 1  rr              .byte  <aNop_       ;    T
000AEEr 1  rr              .byte  <r2s_        ;    U  S( -- w ) R( w -- )
000AEFr 1  rr              .byte  <s2r_        ;    V  S( w -- ) R( -- w )
000AF0r 1  rr              .byte  <aNop_       ;    W   ; ( b -- ) if false, skip to end of loop
000AF1r 1  rr              .byte  <exec_       ;    X
000AF2r 1  rr              .byte  <aNop_       ;    Y
000AF3r 1  rr              .byte  <editDef_    ;    Z
000AF4r 1  rr              .byte  <cArrDef_    ;    [
000AF5r 1  rr              .byte  <comment_    ;    \  comment text, skip reading until end of line
000AF6r 1  rr              .byte  <aNop_       ;    ]
000AF7r 1  rr              .byte  <charCode_   ;    ^
000AF8r 1  rr              .byte  <aNop_       ;    _
000AF9r 1  rr              .byte  <aNop_       ;    `
000AFAr 1  rr              .byte  <sysVar_     ;    a  ; start of data stack *fixed
000AFBr 1  rr              .byte  <sysVar_     ;    b  ; base16 flag
000AFCr 1  rr              .byte  <sysVar_     ;    c  ; TIBPtr variable
000AFDr 1  rr              .byte  <sysVar_     ;    d  ; vDefs variable
000AFEr 1  rr              .byte  <sysVar_     ;    e  ;
000AFFr 1  rr              .byte  <sysVar_     ;    f  ; start of return stack *fixed
000B00r 1  rr              .byte  <sysVar_     ;    g  ; next dispatcher
000B01r 1  rr              .byte  <sysVar_     ;    h  ; heap ptr variable
000B02r 1  rr              .byte  <i_          ;    i  ; returns index of current loop
000B03r 1  rr              .byte  <j_          ;    j  ; returns index of outer loop
000B04r 1  rr              .byte  <sysVar_     ;    k
000B05r 1  rr              .byte  <sysVar_     ;    l
000B06r 1  rr              .byte  <sysVar_     ;    m  ( a b -- c ) return the minimum value
000B07r 1  rr              .byte  <sysVar_     ;    n
000B08r 1  rr              .byte  <sysVar_     ;    o
000B09r 1  rr              .byte  <sysVar_     ;    p
000B0Ar 1  rr              .byte  <sysVar_     ;    q
000B0Br 1  rr              .byte  <sysVar_     ;    r  ; return stack pointer
000B0Cr 1  rr              .byte  <sysVar_     ;    s  ; data stack pointer
000B0Dr 1  rr              .byte  <sysVar_     ;    t
000B0Er 1  rr              .byte  <sysVar_     ;    u
000B0Fr 1  rr              .byte  <sysVar_     ;    v
000B10r 1  rr              .byte  <sysVar_     ;    w
000B11r 1  rr              .byte  <sysVar_     ;    x
000B12r 1  rr              .byte  <sysVar_     ;    y
000B13r 1  rr              .byte  <sysVar_     ;    z
000B14r 1  rr              .byte  <group_      ;    {
000B15r 1  rr              .byte  <aNop_       ;    |
000B16r 1  rr              .byte  <endGroup_   ;    }
000B17r 1  rr              .byte  <aNop_       ;    ~
000B18r 1  rr              .byte  <aNop_       ;    BS
000B19r 1               
000B19r 1               ctlcodeshi:
000B19r 1               altcodeshi:
000B19r 1  rr              .byte  >empty_      ; NUL ^@
000B1Ar 1  rr              .byte  >empty_      ; SOH ^A
000B1Br 1  rr              .byte  >toggleBase_ ; STX ^B
000B1Cr 1  rr              .byte  >empty_      ; ETX ^C
000B1Dr 1  rr              .byte  >empty_      ; EOT ^D
000B1Er 1  rr              .byte  >edit_       ; ENQ ^E
000B1Fr 1  rr              .byte  >empty_      ; ACK ^F
000B20r 1  rr              .byte  >empty_      ; BEL ^G
000B21r 1  rr              .byte  >backsp_     ; BS  ^H
000B22r 1  rr              .byte  >empty_      ; TAB ^I
000B23r 1  rr              .byte  >reedit_     ; LF  ^J
000B24r 1  rr              .byte  >empty_      ; VT  ^K
000B25r 1  rr              .byte  >list_       ; FF  ^L
000B26r 1  rr              .byte  >empty_      ; CR  ^M
000B27r 1  rr              .byte  >empty_      ; SO  ^N
000B28r 1  rr              .byte  >empty_      ; SI  ^O
000B29r 1  rr              .byte  >printStack_ ; DLE ^P
000B2Ar 1  rr              .byte  >empty_      ; DC1 ^Q
000B2Br 1  rr              .byte  >empty_      ; DC2 ^R
000B2Cr 1  rr              .byte  >empty_      ; DC3 ^S
000B2Dr 1  rr              .byte  >empty_      ; DC4 ^T
000B2Er 1  rr              .byte  >empty_      ; NAK ^U
000B2Fr 1  rr              .byte  >empty_      ; SYN ^V
000B30r 1  rr              .byte  >empty_      ; ETB ^W
000B31r 1  rr              .byte  >empty_      ; CAN ^X
000B32r 1  rr              .byte  >empty_      ; EM  ^Y
000B33r 1  rr              .byte  >empty_      ; SUB ^Z
000B34r 1  rr              .byte  >empty_      ; ESC ^[
000B35r 1  rr              .byte  >empty_      ; FS  ^\
000B36r 1  rr              .byte  >empty_      ; GS  ^]
000B37r 1  rr              .byte  >empty_      ; RS  ^^
000B38r 1  rr              .byte  >empty_      ; US  ^_)
000B39r 1  rr              .byte  >aNop_       ; SP  ^`
000B3Ar 1  rr              .byte  >cStore_     ;    !
000B3Br 1  rr              .byte  >aNop_       ;    "
000B3Cr 1  rr              .byte  >aNop_       ;    #
000B3Dr 1  rr              .byte  >aNop_       ;    $  ( -- adr ) text input ptr
000B3Er 1  rr              .byte  >aNop_       ;    %
000B3Fr 1  rr              .byte  >aNop_       ;    &
000B40r 1  rr              .byte  >aNop_       ;    '
000B41r 1  rr              .byte  >ifte_       ;    (  ( b -- )
000B42r 1  rr              .byte  >aNop_       ;    )
000B43r 1  rr              .byte  >aNop_       ;    *
000B44r 1  rr              .byte  >incr_       ;    +  ( adr -- ) increments variable at address
000B45r 1  rr              .byte  >aNop_       ;    ,
000B46r 1  rr              .byte  >decr_       ;    -  ( adr -- ) decrements veriable at address
000B47r 1  rr              .byte  >aNop_       ;    .
000B48r 1  rr              .byte  >aNop_       ;    /
000B49r 1  rr              .byte  >aNop_       ;    0
000B4Ar 1  rr              .byte  >aNop_       ;    1
000B4Br 1  rr              .byte  >aNop_       ;    2
000B4Cr 1  rr              .byte  >aNop_       ;    3
000B4Dr 1  rr              .byte  >aNop_       ;    4
000B4Er 1  rr              .byte  >aNop_       ;    5
000B4Fr 1  rr              .byte  >aNop_       ;    6
000B50r 1  rr              .byte  >aNop_       ;    7
000B51r 1  rr              .byte  >aNop_       ;    8
000B52r 1  rr              .byte  >aNop_       ;    9
000B53r 1  rr              .byte  >aNop_       ;    :  start defining a macro
000B54r 1  rr              .byte  >aNop_       ;    ;
000B55r 1  rr              .byte  >aNop_       ;    <
000B56r 1  rr              .byte  >aNop_       ;    =
000B57r 1  rr              .byte  >aNop_       ;    >
000B58r 1  rr              .byte  >aNop_       ;    ?
000B59r 1  rr              .byte  >cFetch_     ;    @
000B5Ar 1  rr              .byte  >aNop_       ;    A
000B5Br 1  rr              .byte  >break_      ;    B
000B5Cr 1  rr              .byte  >nop_        ;    C
000B5Dr 1  rr              .byte  >depth_      ;    D  ( -- val ) depth of data stack
000B5Er 1  rr              .byte  >emit_       ;    E  ( val -- ) emits a char to output
000B5Fr 1  rr              .byte  >aNop_       ;    F
000B60r 1  rr              .byte  >go_         ;    G  ( -- ? ) execute mint definition
000B61r 1  rr              .byte  >keyq_       ;    H  ( verify if key hit )
000B62r 1  rr              .byte  >inPort_     ;    I  ( port -- val )
000B63r 1  rr              .byte  >aNop_       ;    J
000B64r 1  rr              .byte  >key_        ;    K  ( -- val )  read a char from input
000B65r 1  rr              .byte  >aNop_       ;    L
000B66r 1  rr              .byte  >aNop_       ;    M
000B67r 1  rr              .byte  >newln_      ;    N  ; prints a newline to output
000B68r 1  rr              .byte  >outPort_    ;    O  ( val port -- )
000B69r 1  rr              .byte  >printStk_   ;    P  ( -- ) non-destructively prints stack
000B6Ar 1  rr              .byte  >aNop_       ;    Q  quits from Mint REPL
000B6Br 1  rr              .byte  >rot_        ;    R  ( a b c -- b c a )
000B6Cr 1  rr              .byte  >aNop_       ;    S
000B6Dr 1  rr              .byte  >aNop_       ;    T
000B6Er 1  rr              .byte  >r2s_        ;    U  S( -- w ) R( w -- )
000B6Fr 1  rr              .byte  >s2r_        ;    V  S( w -- ) R( -- w )
000B70r 1  rr              .byte  >aNop_       ;    W   ; ( b -- ) if false, skip to end of loop
000B71r 1  rr              .byte  >exec_       ;    X
000B72r 1  rr              .byte  >aNop_       ;    Y
000B73r 1  rr              .byte  >editDef_    ;    Z
000B74r 1  rr              .byte  >cArrDef_    ;    [
000B75r 1  rr              .byte  >comment_    ;    \  comment text, skip reading until end of line
000B76r 1  rr              .byte  >aNop_       ;    ]
000B77r 1  rr              .byte  >charCode_   ;    ^
000B78r 1  rr              .byte  >aNop_       ;    _
000B79r 1  rr              .byte  >aNop_       ;    `
000B7Ar 1  rr              .byte  >sysVar_     ;    a  ; start of data stack *fixed
000B7Br 1  rr              .byte  >sysVar_     ;    b  ; base16 flag
000B7Cr 1  rr              .byte  >sysVar_     ;    c  ; TIBPtr variable
000B7Dr 1  rr              .byte  >sysVar_     ;    d  ; vDefs variable
000B7Er 1  rr              .byte  >sysVar_     ;    e  ;
000B7Fr 1  rr              .byte  >sysVar_     ;    f  ; start of return stack *fixed
000B80r 1  rr              .byte  >sysVar_     ;    g  ; next dispatcher
000B81r 1  rr              .byte  >sysVar_     ;    h  ; heap ptr variable
000B82r 1  rr              .byte  >i_          ;    i  ; returns index of current loop
000B83r 1  rr              .byte  >j_          ;    j  ; returns index of outer loop
000B84r 1  rr              .byte  >sysVar_     ;    k
000B85r 1  rr              .byte  >sysVar_     ;    l
000B86r 1  rr              .byte  >sysVar_     ;    m  ( a b -- c ) return the minimum value
000B87r 1  rr              .byte  >sysVar_     ;    n
000B88r 1  rr              .byte  >sysVar_     ;    o
000B89r 1  rr              .byte  >sysVar_     ;    p
000B8Ar 1  rr              .byte  >sysVar_     ;    q
000B8Br 1  rr              .byte  >sysVar_     ;    r  ; return stack pointer
000B8Cr 1  rr              .byte  >sysVar_     ;    s  ; data stack pointer
000B8Dr 1  rr              .byte  >sysVar_     ;    t
000B8Er 1  rr              .byte  >sysVar_     ;    u
000B8Fr 1  rr              .byte  >sysVar_     ;    v
000B90r 1  rr              .byte  >sysVar_     ;    w
000B91r 1  rr              .byte  >sysVar_     ;    x
000B92r 1  rr              .byte  >sysVar_     ;    y
000B93r 1  rr              .byte  >sysVar_     ;    z
000B94r 1  rr              .byte  >group_      ;    {
000B95r 1  rr              .byte  >aNop_       ;    |
000B96r 1  rr              .byte  >endGroup_   ;    }
000B97r 1  rr              .byte  >aNop_       ;    ~
000B98r 1  rr              .byte  >aNop_       ;    BS
000B99r 1               
000B99r 1               ; *********************************************************************
000B99r 1               ; Macros must be written in Mint and end with ;
000B99r 1               ; this code must not span pages
000B99r 1               ; *********************************************************************
000B99r 1               macros:
000B99r 1               
000B99r 1               .include "MINT.macros.asm"
000B99r 2               empty_:
000B99r 2  3B 00            .asciiz ";"
000B9Br 2               
000B9Br 2               backsp_:
000B9Br 2  5C 63 40 30      .asciiz "\\c@0=0=(1_\\c\\+`\\b \\b`);"
000B9Fr 2  3D 30 3D 28  
000BA3r 2  31 5F 5C 63  
000BB3r 2               
000BB3r 2               reedit_:
000BB3r 2  5C 65 5C 40      .asciiz "\\e\\@\\Z;"
000BB7r 2  5C 5A 3B 00  
000BBBr 2               
000BBBr 2               edit_:
000BBBr 2  60 3F 60 5C      .asciiz "`?`\\K\\N`> `\\^A-\\Z;"
000BBFr 2  4B 5C 4E 60  
000BC3r 2  3E 20 60 5C  
000BCEr 2               
000BCEr 2               list_:
000BCEr 2  5C 4E 32 36      .asciiz "\\N26(\\i@\\Z\\c@0>(\\N))\\N`> `;"
000BD2r 2  28 5C 69 40  
000BD6r 2  5C 5A 5C 63  
000BEAr 2               
000BEAr 2               printStack_:
000BEAr 2  60 3D 3E 20      .asciiz "`=> `\\P\\N\\N`> `;"
000BEEr 2  60 5C 50 5C  
000BF2r 2  4E 5C 4E 60  
000BFBr 2               
000BFBr 2               toggleBase_:
000BFBr 2  5C 62 40 30      .asciiz "\\b@0=\\b!;"
000BFFr 2  3D 5C 62 21  
000C03r 2  3B 00        
000C05r 2               
000C05r 2               
000C05r 1               
000C05r 1  DE AD        .word $ADDE
000C05r 1               
