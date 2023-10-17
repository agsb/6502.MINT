ca65 V2.19 - Git 7979f8a41
Main file   : main.s
Current file: main.s

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
000000r 1                   DSIZE       = $80
000000r 1                   RSIZE       = $80
000000r 1               
000000r 1                   TIBSIZE     = $100
000000r 1                   TRUE        = 1
000000r 1                   FALSE       = 0
000000r 1               
000000r 1                   NUMGRPS     = 5
000000r 1                   GRPSIZE     = $40
000000r 1               
000000r 1               ;----------------------------------------------------------------------
000000r 1               ; notes 6502 version:
000000r 1               ;
000000r 1               ;   code is for RAM use.
000000r 1               ;
000000r 1               ;   caller must save a, x, y and
000000r 1               ;   reserve 32 (?) words at hardware stack
000000r 1               ;
000000r 1               ;   stacks are from absolute address.
000000r 1               ;   data stack indexed by x
000000r 1               ;   return stack indexed by y
000000r 1               ;   terminal input buffer
000000r 1               ;   all just 128 cells deep and round-robin
000000r 1               ;   a cell is 16-bit
000000r 1               ;   16-bit jump table
000000r 1               ;   extense use of post-indexed indirect addressing
000000r 1               ;
000000r 1               ;   alt-a used for vS0, start of data stack
000000r 1               ;   alt-f used for vR0, start of return stack ****
000000r 1               ;   alt-g reserved, copycat of references ****
000000r 1               ;
000000r 1               ;   all rotines must end with:
000000r 1               ;   jmp next_ or jmp drop_ or a jmp / branch
000000r 1               ;----------------------------------------------------------------------
000000r 1               .macro spull addr
000000r 1                   ldx xp
000000r 1                   lda spz, x
000000r 1                   sta addr, 0
000000r 1                   lda spz, x
000000r 1                   sta addr, 1
000000r 1                   inx
000000r 1                   inx
000000r 1                   stx xp
000000r 1               .endmacro
000000r 1               
000000r 1               .macro spush addr
000000r 1                   ldx xp
000000r 1                   dex
000000r 1                   dex
000000r 1                   lda addr, x
000000r 1                   sta spz, 0
000000r 1                   lda addr, x
000000r 1                   sta spz, 1
000000r 1                   stx xp
000000r 1               .endmacro
000000r 1               
000000r 1               .macro rpull addr
000000r 1                   ldy yp
000000r 1                   lda rpz, y
000000r 1                   sta addr, 0
000000r 1                   lda rpz, y
000000r 1                   sta addr, 1
000000r 1                   iny
000000r 1                   iny
000000r 1                   sty yp
000000r 1               .endmacro
000000r 1               
000000r 1               .macro rpush addr
000000r 1                   ldy yp
000000r 1                   dey
000000r 1                   dey
000000r 1                   lda addr, y
000000r 1                   sta rpz, 0
000000r 1                   lda addr, y
000000r 1                   sta rpz, 1
000000r 1                   sty yp
000000r 1               .endmacro
000000r 1               
000000r 1               ;----------------------------------------------------------------------
000000r 1               ; page 0, reserved cells
000000r 1                   zpage = $f0
000000r 1               
000000r 1               ; copycat
000000r 1                   yp = zpage + $0  ; y index, return stack pointer,
000000r 1                   xp = zpage + $1  ; x index, parameter stack pointer,
000000r 1                   ap = zpage + $2  ; accumulator
000000r 1                   ns = zpage + $3  ; nests
000000r 1               
000000r 1               ; pseudos
000000r 1                   tos = zpage + $4  ; tos  register
000000r 1                   nos = zpage + $6  ; nos  register
000000r 1                   wrk = zpage + $8  ; work register
000000r 1                   tmp = zpage + $a  ; work register
000000r 1               
000000r 1               ; holds
000000r 1                   nxt = zpage + $c  ; next pointer
000000r 1                   ips = zpage + $e  ; instruction pointer
000000r 1               
000000r 1               ; all in RAM, better put tables at end of code ?
000000r 1               
000000r 1                   start = $200
000000r 1                   tib = start  ; terminal input buffer, upwards
000000r 1                   spz = start + $1FF  ; absolute data stack, backwards
000000r 1                   rpz = start + $2FF  ; absolute parameter stack, backwards
000000r 1               
000000r 1                   vars = start + $300  ;   26 words
000000r 1                   vsys = start + $336  ;   26 words
000000r 1                   defs = start + $36C  ;   26 words
000000r 1                   tmps = start + $3D8  ;   14 words
000000r 1               
000000r 1                   free = start + $400  ; free ram start
000000r 1               
000000r 1               ;----------------------------------------------------------------------
000000r 1               .segment "VECTORS"
000000r 1  rr rr        .word init
000002r 1  rr rr        .word init
000004r 1  rr rr        .word init
000006r 1               
000006r 1               ;----------------------------------------------------------------------
000006r 1               .segment "ONCE"
000000r 1               
000000r 1               ; **********************************************************************
000000r 1               ;
000000r 1               ; (not yet) routines are ordered to occupy pages of 256 bytes
000000r 1               ;
000000r 1               ; **********************************************************************
000000r 1               
000000r 1               putchar:
000000r 1  18               clc
000001r 1  60               rts
000002r 1               
000002r 1               getchar:
000002r 1  18               clc
000003r 1  60               rts
000004r 1               
000004r 1               hitchar:
000004r 1  18               clc
000005r 1  60               rts
000006r 1               
000006r 1               ;----------------------------------------------------------------------
000006r 1               nop_:
000006r 1  18               clc
000007r 1  4C rr rr         jmp next_
00000Ar 1               
00000Ar 1               ;----------------------------------------------------------------------
00000Ar 1               ; increase instruction pointer
00000Ar 1               incps_:
00000Ar 1  E6 FE            inc ips + 0
00000Cr 1  D0 02            bne @noeq
00000Er 1  E6 FF            inc ips + 1
000010r 1               @noeq:
000010r 1  60               rts
000011r 1               
000011r 1               ;----------------------------------------------------------------------
000011r 1               ; decrease instruction pointer
000011r 1               decps_:
000011r 1  A5 FE            lda ips + 0
000013r 1  D0 02            bne @noeq
000015r 1  C6 FF            dec ips + 1
000017r 1               @noeq:
000017r 1  C6 FE            dec ips + 0
000019r 1  60               rts
00001Ar 1               
00001Ar 1               ;----------------------------------------------------------------------
00001Ar 1               ; load char at instruction pointer
00001Ar 1               ldaps_:
00001Ar 1  A0 00            ldy #$00
00001Cr 1  B1 FE            lda (ips), y
00001Er 1  60               rts
00001Fr 1               
00001Fr 1               ;----------------------------------------------------------------------
00001Fr 1               pushps_:
00001Fr 1               ; push ps into RS
00001Fr 1  A4 F0            ldy yp
000021r 1  88               dey
000022r 1  88               dey
000023r 1  A5 FE            lda ips + 0
000025r 1  99 FF 04         sta rpz + 0, y
000028r 1  A5 FF            lda ips + 1
00002Ar 1  99 00 05         sta rpz + 1, y
00002Dr 1  84 F0            sty yp
00002Fr 1  60               rts
000030r 1               
000030r 1               ;----------------------------------------------------------------------
000030r 1               pullps_:
000030r 1               ; pull ps from RS
000030r 1  A4 F0            ldy yp
000032r 1  B9 FF 04         lda rpz + 0, y
000035r 1  85 FE            sta ips + 0
000037r 1  B9 00 05         lda rpz + 1, y
00003Ar 1  85 FF            sta ips + 1
00003Cr 1  C8               iny
00003Dr 1  C8               iny
00003Er 1  84 F0            sty yp
000040r 1  60               rts
000041r 1               
000041r 1               ;----------------------------------------------------------------------
000041r 1               ; push tos into return stack
000041r 1               rpush_:
000041r 1  A4 F0            ldy yp
000043r 1  88               dey
000044r 1  88               dey
000045r 1  A5 F4            lda tos + 0
000047r 1  99 FF 04         sta rpz + 0, y
00004Ar 1  A5 F5            lda tos + 1
00004Cr 1  B9 00 05         lda rpz + 1, y
00004Fr 1  84 F0            sty yp
000051r 1  60               rts
000052r 1               
000052r 1               ;----------------------------------------------------------------------
000052r 1               ; push tos from return stack
000052r 1               rpull_:
000052r 1  A4 F0            ldy yp
000054r 1  B9 FF 04         lda rpz + 0, y
000057r 1  85 F4            sta tos + 0
000059r 1  B9 00 05         lda rpz + 1, y
00005Cr 1  85 F5            sta tos + 1
00005Er 1  C8               iny
00005Fr 1  C8               iny
000060r 1  84 F0            sty yp
000062r 1  60               rts
000063r 1               
000063r 1               ;----------------------------------------------------------------------
000063r 1               ; push tos into stack
000063r 1               spush_:
000063r 1                   ; ldx xp
000063r 1  CA               dex
000064r 1  CA               dex
000065r 1  A5 F4            lda tos + 0
000067r 1  9D FF 03         sta spz + 0, x
00006Ar 1  A5 F5            lda tos + 1
00006Cr 1  9D 00 04         sta spz + 1, x
00006Fr 1                   ; stx xp
00006Fr 1  60               rts
000070r 1               
000070r 1               ;----------------------------------------------------------------------
000070r 1               ; pull tos from stack
000070r 1               spull_:
000070r 1                   ; ldx xp
000070r 1  BD FF 03         lda spz + 0, x
000073r 1  85 F4            sta tos + 0
000075r 1  BD 00 04         lda spz + 1, x
000078r 1  85 F5            sta tos + 1
00007Ar 1  E8               inx
00007Br 1  E8               inx
00007Cr 1                   ; stx xp
00007Cr 1  60               rts
00007Dr 1               
00007Dr 1               ;----------------------------------------------------------------------
00007Dr 1               ; take two from stack
00007Dr 1               take2_:
00007Dr 1                   ; ldx xp
00007Dr 1  BD FF 03         lda spz + 0, x
000080r 1  85 F4            sta tos + 0
000082r 1  BD 00 04         lda spz + 1, x
000085r 1  85 F5            sta tos + 1
000087r 1  BD 01 04         lda spz + 2, x
00008Ar 1  85 F8            sta nos + 2
00008Cr 1  BD 02 04         lda spz + 3, x
00008Fr 1  85 F9            sta nos + 3
000091r 1  E8               inx
000092r 1  E8               inx
000093r 1  E8               inx
000094r 1  E8               inx
000095r 1                   ; stx xp
000095r 1  60               rts
000096r 1               
000096r 1               ;----------------------------------------------------------------------
000096r 1               ; NEGate the value on top of stack (2's complement)
000096r 1               neg_:
000096r 1                   ; ldx xp
000096r 1  38               sec
000097r 1  A9 00            lda #0
000099r 1  FD 00 04         sbc spz + 1, x
00009Cr 1  9D 00 04         sta spz + 1, x
00009Fr 1  38               sec
0000A0r 1  A9 00            lda #0
0000A2r 1  FD FF 03         sbc spz + 0, x
0000A5r 1  9D FF 03         sta spz + 0, x
0000A8r 1                   ; stx xp
0000A8r 1  4C rr rr         jmp next_
0000ABr 1               
0000ABr 1               ;----------------------------------------------------------------------
0000ABr 1               ; Bitwise INVert the top member of the stack (1's complement)
0000ABr 1               inv_:
0000ABr 1                   ; ldx xp
0000ABr 1  A9 FF            lda #$FF
0000ADr 1  5D FF 03         eor spz + 0, x
0000B0r 1  9D FF 03         sta spz + 0, x
0000B3r 1  5D 00 04         eor spz + 1, x
0000B6r 1  9D 00 04         sta spz + 1, x
0000B9r 1                   ; stx xp
0000B9r 1  4C rr rr         jmp next_
0000BCr 1               
0000BCr 1               ;----------------------------------------------------------------------
0000BCr 1               ; Duplicate the top member of the stack
0000BCr 1               ; a b c -- a b c c
0000BCr 1               dup_:
0000BCr 1                   ; ldx xp
0000BCr 1  CA               dex
0000BDr 1  CA               dex
0000BEr 1  BD 01 04         lda spz + 2, x
0000C1r 1  9D FF 03         sta spz + 0, x
0000C4r 1  BD 02 04         lda spz + 3, x
0000C7r 1  9D 00 04         sta spz + 1, x
0000CAr 1                   ; stx xp
0000CAr 1  4C rr rr         jmp next_
0000CDr 1               
0000CDr 1               ;----------------------------------------------------------------------
0000CDr 1               ; Duplicate 2nd element of the stack
0000CDr 1               ; a b c -- a b c b
0000CDr 1               over_:
0000CDr 1                   ; ldx xp
0000CDr 1  CA               dex
0000CEr 1  CA               dex
0000CFr 1  BD 03 04         lda spz + 4, x
0000D2r 1  9D FF 03         sta spz + 0, x
0000D5r 1  BD 04 04         lda spz + 5, x
0000D8r 1  9D 00 04         sta spz + 1, x
0000DBr 1                   ; stx xp
0000DBr 1  4C rr rr         jmp next_
0000DEr 1               
0000DEr 1               ;----------------------------------------------------------------------
0000DEr 1               ; Rotate 3 elements at stack
0000DEr 1               ; a b c -- b c a
0000DEr 1               rot_:
0000DEr 1                   ; ldx xp
0000DEr 1                   ; c -> w
0000DEr 1  BD FF 03         lda spz + 0, x
0000E1r 1  85 F4            sta tos + 0
0000E3r 1  BD FF 03         lda spz + 0, x
0000E6r 1  85 F5            sta tos + 1
0000E8r 1                   ; b -> u
0000E8r 1  BD 01 04         lda spz + 2, x
0000EBr 1  85 F6            sta nos + 0
0000EDr 1  BD 02 04         lda spz + 3, x
0000F0r 1  85 F7            sta nos + 1
0000F2r 1                   ; a -> c
0000F2r 1  BD 03 04         lda spz + 4, x
0000F5r 1  9D FF 03         sta spz + 0, x
0000F8r 1  BD 04 04         lda spz + 5, x
0000FBr 1  9D 00 04         sta spz + 1, x
0000FEr 1                   ; u -> a
0000FEr 1  A5 F6            lda nos + 0
000100r 1  9D 03 04         sta spz + 4, x
000103r 1  A5 F7            lda nos + 1
000105r 1  9D 04 04         sta spz + 5, x
000108r 1                   ; w -> b
000108r 1  A5 F4            lda tos + 0
00010Ar 1  9D 01 04         sta spz + 2, x
00010Dr 1  A5 F5            lda tos + 1
00010Fr 1  9D 02 04         sta spz + 3, x
000112r 1                   ; stx xp
000112r 1  4C rr rr         jmp next_
000115r 1               
000115r 1               ;----------------------------------------------------------------------
000115r 1               ; Swap 2nd and 1st elements of the stack
000115r 1               ; a b c -- a c b
000115r 1               swap_:
000115r 1                   ; ldx xp
000115r 1                   ; b -> w
000115r 1  BD 01 04         lda spz + 2, x
000118r 1  85 F8            sta wrk + 0
00011Ar 1  BD 02 04         lda spz + 3, x
00011Dr 1  85 F9            sta wrk + 1
00011Fr 1                   ; a -> b
00011Fr 1  BD FF 03         lda spz + 0, x
000122r 1  9D 01 04         sta spz + 2, x
000125r 1  BD 00 04         lda spz + 1, x
000128r 1  9D 02 04         sta spz + 3, x
00012Br 1                   ; w -> a
00012Br 1  A5 F8            lda wrk + 0
00012Dr 1  9D FF 03         sta spz + 0, x
000130r 1  A5 F9            lda wrk + 1
000132r 1  9D 00 04         sta spz + 1, x
000135r 1                   ; stx xp
000135r 1  4C rr rr         jmp next_
000138r 1               
000138r 1               ;----------------------------------------------------------------------
000138r 1               ;  Left shift { is multply by 2
000138r 1               shl_:
000138r 1                   ; ldx xp
000138r 1  1E FF 03         asl spz + 0, x
00013Br 1  3E 00 04         rol spz + 1, x
00013Er 1                   ; stx xp
00013Er 1  4C rr rr         jmp next_
000141r 1               
000141r 1               ;----------------------------------------------------------------------
000141r 1               ;  Right shift } is a divide by 2
000141r 1               shr_:
000141r 1                   ; ldx xp
000141r 1  5E FF 03         lsr spz + 0, x
000144r 1  7E 00 04         ror spz + 1, x
000147r 1                   ; stx xp
000147r 1  4C rr rr         jmp next_
00014Ar 1               
00014Ar 1               ;----------------------------------------------------------------------
00014Ar 1               ; Drop the top member of the stack
00014Ar 1               ; a b c -- a b
00014Ar 1               drop_:
00014Ar 1                   ; ldx xp
00014Ar 1  E8               inx
00014Br 1  E8               inx
00014Cr 1                   ; stx xp
00014Cr 1  4C rr rr         jmp next_
00014Fr 1               
00014Fr 1               ;----------------------------------------------------------------------
00014Fr 1               ;  Bitwise AND the top 2 elements of the stack
00014Fr 1               and_:
00014Fr 1                   ; ldx xp
00014Fr 1  BD 01 04         lda spz + 2, x
000152r 1  3D FF 03         and spz + 0, x
000155r 1  9D 01 04         sta spz + 2, x
000158r 1  BD 02 04         lda spz + 3, x
00015Br 1  3D 00 04         and spz + 1, x
00015Er 1  9D 02 04         sta spz + 3, x
000161r 1                   ; stx xp
000161r 1  4C rr rr         jmp drop_
000164r 1               
000164r 1               ;----------------------------------------------------------------------
000164r 1               ;  Bitwise OR the top 2 elements of the stack
000164r 1               or_:
000164r 1                   ; ldx xp
000164r 1  BD 01 04         lda spz + 2, x
000167r 1  1D FF 03         ora spz + 0, x
00016Ar 1  9D 01 04         sta spz + 2, x
00016Dr 1  BD 02 04         lda spz + 3, x
000170r 1  1D 00 04         ora spz + 1, x
000173r 1  9D 02 04         sta spz + 3, x
000176r 1                   ; stx xp
000176r 1  4C rr rr         jmp drop_
000179r 1               
000179r 1               ;----------------------------------------------------------------------
000179r 1               ;  Bitwise XOR the top 2 elements of the stack
000179r 1               xor_:
000179r 1                   ; ldx xp
000179r 1  BD 01 04         lda spz + 2, x
00017Cr 1  5D FF 03         eor spz + 0, x
00017Fr 1  9D 01 04         sta spz + 2, x
000182r 1  BD 02 04         lda spz + 3, x
000185r 1  5D 00 04         eor spz + 1, x
000188r 1  9D 02 04         sta spz + 3, x
00018Br 1                   ; stx xp
00018Br 1  4C rr rr         jmp drop_
00018Er 1               
00018Er 1               ;----------------------------------------------------------------------
00018Er 1               ; Add the top 2 members of the stack
00018Er 1               ; a b c -- a (b+c)
00018Er 1               add_:
00018Er 1                   ; ldx xp
00018Er 1  18               clc
00018Fr 1  BD 01 04         lda spz + 2, x
000192r 1  7D FF 03         adc spz + 0, x
000195r 1  9D 01 04         sta spz + 2, x
000198r 1  BD 02 04         lda spz + 3, x
00019Br 1  7D 00 04         adc spz + 1, x
00019Er 1  9D 02 04         sta spz + 3, x
0001A1r 1                   ; stx xp
0001A1r 1  4C rr rr         jmp drop_
0001A4r 1               
0001A4r 1               ;----------------------------------------------------------------------
0001A4r 1               ; Subtract the top 2 members of the stack
0001A4r 1               ; a b c -- a (b-c)
0001A4r 1               sub_:
0001A4r 1                   ; ldx xp
0001A4r 1  38               sec
0001A5r 1  BD 01 04         lda spz + 2, x
0001A8r 1  FD FF 03         sbc spz + 0, x
0001ABr 1  9D 01 04         sta spz + 2, x
0001AEr 1  BD 02 04         lda spz + 3, x
0001B1r 1  FD 00 04         sbc spz + 1, x
0001B4r 1  9D 02 04         sta spz + 3, x
0001B7r 1                   ; stx xp
0001B7r 1  4C rr rr         jmp drop_
0001BAr 1               
0001BAr 1               ;----------------------------------------------------------------------
0001BAr 1               opin:
0001BAr 1  A4 F0            ldy yp
0001BCr 1                   ; pseudo tos
0001BCr 1  B9 FF 03         lda spz + 0, y
0001BFr 1  85 F8            sta wrk + 0
0001C1r 1  B9 00 04         lda spz + 1, y
0001C4r 1  85 F9            sta wrk + 1
0001C6r 1                   ; pseudo nos
0001C6r 1  B9 01 04         lda spz + 2, y
0001C9r 1  85 FA            sta tmp + 0
0001CBr 1  B9 02 04         lda spz + 3, y
0001CEr 1  85 FB            sta tmp + 1
0001D0r 1  A9 00            lda #0
0001D2r 1                   ; clear results
0001D2r 1  85 F4            sta tos + 0
0001D4r 1  85 F5            sta tos + 1
0001D6r 1  85 F6            sta nos + 0
0001D8r 1  85 F6            sta nos + 0
0001DAr 1  60               rts
0001DBr 1               
0001DBr 1               ;----------------------------------------------------------------------
0001DBr 1               opout:
0001DBr 1                   ; copy results
0001DBr 1  A4 F0            ldy yp
0001DDr 1  A5 F6            lda nos + 0
0001DFr 1  99 FF 03         sta spz + 0, y
0001E2r 1  A5 F7            lda nos + 1
0001E4r 1  99 00 04         sta spz + 1, y
0001E7r 1  A5 F4            lda tos + 0
0001E9r 1  99 01 04         sta spz + 2, y
0001ECr 1  A5 F5            lda tos + 1
0001EEr 1  99 02 04         sta spz + 3, y
0001F1r 1  60               rts
0001F2r 1               
0001F2r 1               ;----------------------------------------------------------------------
0001F2r 1               ; Divide the top 2 members of the stack
0001F2r 1               ; divisor dividend -- quontient remainder
0001F2r 1               div_:
0001F2r 1  20 rr rr         jsr divd
0001F5r 1  4C rr rr         jmp next_
0001F8r 1               
0001F8r 1               divd:
0001F8r 1  20 rr rr         jsr opin
0001FBr 1                   ; countdown
0001FBr 1  A0 10            ldy #16
0001FDr 1               @loop:
0001FDr 1  06 FA            asl tmp + 0
0001FFr 1  26 FB            rol tmp + 1
000201r 1  26 F4            rol tos + 0
000203r 1  26 F5            rol tos + 1
000205r 1  38               sec
000206r 1  A5 F6            lda nos + 0
000208r 1  E5 F4            sbc tos + 0
00020Ar 1  85 F4            sta tos + 0
00020Cr 1  A5 F7            lda nos + 1
00020Er 1  E5 F5            sbc tos + 1
000210r 1  85 F5            sta tos + 1
000212r 1  90 0D            bcc @iscc
000214r 1  18               clc
000215r 1  A5 F6            lda nos + 0
000217r 1  65 F4            adc tos + 0
000219r 1  85 F4            sta tos + 0
00021Br 1  A5 F7            lda nos + 1
00021Dr 1  65 F5            adc tos + 1
00021Fr 1  85 F5            sta tos + 1
000221r 1               @iscc:
000221r 1                   ; countdown
000221r 1  88               dey
000222r 1  D0 D9            bne @loop
000224r 1  20 rr rr         jsr opout
000227r 1  4C rr rr         jmp next_
00022Ar 1               
00022Ar 1               ;----------------------------------------------------------------------
00022Ar 1               ; 16-bit multiply 16x16, 32 result
00022Ar 1               ; multiplier multiplicand -- resultLSW resultMSW
00022Ar 1               ;
00022Ar 1               mul_:
00022Ar 1  20 rr rr         jsr mult
00022Dr 1  4C rr rr         jmp next_
000230r 1               
000230r 1               mult:
000230r 1  20 rr rr         jsr opin
000233r 1                   ; countdown
000233r 1  A0 10            ldy #16
000235r 1               @loop:
000235r 1  06 F4            asl tos + 0
000237r 1  26 F5            rol tos + 1
000239r 1  26 F6            rol nos + 0
00023Br 1  26 F6            rol nos + 0
00023Dr 1  90 13            bcc @iscc
00023Fr 1  18               clc
000240r 1  A5 FA            lda tmp + 0
000242r 1  65 F4            adc tos + 0
000244r 1  85 F4            sta tos + 0
000246r 1  A5 FB            lda tmp + 1
000248r 1  65 F5            adc tos + 1
00024Ar 1  85 F5            sta tos + 1
00024Cr 1  A9 00            lda #0
00024Er 1  65 F6            adc nos + 0
000250r 1  85 F6            sta nos + 0
000252r 1               @iscc:
000252r 1                   ; countdown
000252r 1  88               dey
000253r 1  D0 E0            bne @loop
000255r 1  20 rr rr         jsr opout
000258r 1  4C rr rr         jmp next_
00025Br 1               
00025Br 1               ;----------------------------------------------------------------------
00025Br 1               ; \+    a b c -- a ; [c]+b  ; increment variable at c by b
00025Br 1               incr_:
00025Br 1  20 rr rr         jsr take2_
00025Er 1  18               clc
00025Fr 1  A0 00            ldy #$00
000261r 1  B1 F4            lda (tos), y
000263r 1  65 F6            adc nos + 0
000265r 1  91 F4            sta (tos), y
000267r 1  C8               iny
000268r 1  B1 F4            lda (tos), y
00026Ar 1  65 F7            adc nos + 1
00026Cr 1  91 F4            sta (tos), y
00026Er 1  4C rr rr         jmp next_
000271r 1               
000271r 1               ;----------------------------------------------------------------------
000271r 1               ; \-    a b c -- a ; [c]-b  ; decrement variable at c by b
000271r 1               decr_:
000271r 1  20 rr rr         jsr take2_
000274r 1  38               sec
000275r 1  A0 00            ldy #$00
000277r 1  B1 F4            lda (tos), y
000279r 1  E5 F6            sbc nos + 0
00027Br 1  91 F4            sta (tos), y
00027Dr 1  C8               iny
00027Er 1  B1 F4            lda (tos), y
000280r 1  E5 F7            sbc nos + 1
000282r 1  91 F4            sta (tos), y
000284r 1  4C rr rr         jmp next_
000287r 1               
000287r 1               ;----------------------------------------------------------------------
000287r 1               ; false
000287r 1               false2:
000287r 1                   ; ldx xp
000287r 1  A9 00            lda #$00
000289r 1  9D 01 04         sta spz + 2, x
00028Cr 1  9D 02 04         sta spz + 3, x
00028Fr 1                   ; stx xp
00028Fr 1  4C rr rr         jmp drop_
000292r 1               
000292r 1               ;----------------------------------------------------------------------
000292r 1               ; true
000292r 1               true2:
000292r 1                   ; ldx xp
000292r 1  A9 01            lda #$01
000294r 1  9D 01 04         sta spz + 2, x
000297r 1  A9 00            lda #$00
000299r 1  9D 02 04         sta spz + 3, x
00029Cr 1                   ; stx xp
00029Cr 1  4C rr rr         jmp drop_
00029Fr 1               
00029Fr 1               ;----------------------------------------------------------------------
00029Fr 1               ; subtract for compare
00029Fr 1               cmp_:
00029Fr 1                   ; ldx xp
00029Fr 1  38               sec
0002A0r 1  BD 01 04         lda spz + 2, x
0002A3r 1  FD FF 03         sbc spz + 0, x
0002A6r 1  BD 02 04         lda spz + 3, x
0002A9r 1  FD 00 04         sbc spz + 1, x
0002ACr 1                   ; stx xp
0002ACr 1  60               rts
0002ADr 1               
0002ADr 1               ;----------------------------------------------------------------------
0002ADr 1               ; signed equal than
0002ADr 1               eq_:
0002ADr 1  20 rr rr         jsr cmp_
0002B0r 1  D0 D5            bne false2
0002B2r 1  F0 DE            beq true2
0002B4r 1               
0002B4r 1               ;----------------------------------------------------------------------
0002B4r 1               ; signed less than
0002B4r 1               lt_:
0002B4r 1  20 rr rr         jsr cmp_
0002B7r 1  30 D9            bmi true2
0002B9r 1  10 CC            bpl false2
0002BBr 1               
0002BBr 1               ;----------------------------------------------------------------------
0002BBr 1               ; signed greather than
0002BBr 1               ; must be in that order, bpl is non negative flag
0002BBr 1               gt_:
0002BBr 1  20 rr rr         jsr cmp_
0002BEr 1  30 C7            bmi false2
0002C0r 1  F0 C5            beq false2
0002C2r 1  10 CE            bpl true2
0002C4r 1               
0002C4r 1               ;----------------------------------------------------------------------
0002C4r 1               ; fetch the value from the address placed on the top of the stack
0002C4r 1               ; a b c - a b (c)
0002C4r 1               ; fetch a byte
0002C4r 1               cfetch_:
0002C4r 1  A9 00            lda #$00
0002C6r 1  85 F5            sta tos + 1
0002C8r 1  38               sec
0002C9r 1  4C rr rr         jmp isfetch_
0002CCr 1               
0002CCr 1               ;----------------------------------------------------------------------
0002CCr 1               ; fetch a word
0002CCr 1               fetch_:
0002CCr 1  18               clc
0002CDr 1  4C rr rr         jmp isfetch_
0002D0r 1               
0002D0r 1               ;----------------------------------------------------------------------
0002D0r 1               isfetch_:
0002D0r 1                   ; ldx xp
0002D0r 1                   ; load the reference
0002D0r 1  BD FF 03         lda spz + 0, x
0002D3r 1  85 F6            sta nos + 0
0002D5r 1  BD 00 04         lda spz + 1, x
0002D8r 1  85 F7            sta nos + 1
0002DAr 1                   ; then the value
0002DAr 1  A0 00            ldy #$00
0002DCr 1  B1 F6            lda (nos), y
0002DEr 1  85 F4            sta tos + 0
0002E0r 1  B0 05            bcs @iset
0002E2r 1  C8               iny
0002E3r 1  B1 F6            lda (nos), y
0002E5r 1  85 F5            sta tos + 1
0002E7r 1               @iset:
0002E7r 1                   ; save the value
0002E7r 1  A5 F4            lda tos + 0
0002E9r 1  9D FF 03         sta spz + 0, x
0002ECr 1  A5 F5            lda tos + 1
0002EEr 1  9D 00 04         sta spz + 1, x
0002F1r 1                   ; next
0002F1r 1                   ; stx xp
0002F1r 1  4C rr rr         jmp next_
0002F4r 1               
0002F4r 1               ;----------------------------------------------------------------------
0002F4r 1               ; store the value into the address placed on the top of the stack
0002F4r 1               ; a b c -- a
0002F4r 1               ; store a byte
0002F4r 1               cstore_:
0002F4r 1  38               sec
0002F5r 1  4C rr rr         jmp isstore_
0002F8r 1               
0002F8r 1               ;----------------------------------------------------------------------
0002F8r 1               ; store a word
0002F8r 1               store_:
0002F8r 1  18               clc
0002F9r 1  4C rr rr         jmp isstore_
0002FCr 1               
0002FCr 1               ;----------------------------------------------------------------------
0002FCr 1               isstore_:
0002FCr 1  20 rr rr         jsr take2_
0002FFr 1                   ; copy the value
0002FFr 1  A0 00            ldy #$00
000301r 1  A5 F6            lda nos + 0
000303r 1  91 F4            sta (tos), y
000305r 1  B0 05            bcs @iset
000307r 1  C8               iny
000308r 1  A5 F7            lda nos + 1
00030Ar 1  91 F4            sta (tos), y
00030Cr 1                   ; next
00030Cr 1               @iset:
00030Cr 1                   ; stx xp
00030Cr 1  4C rr rr         jmp next_
00030Fr 1               
00030Fr 1               ;----------------------------------------------------------------------
00030Fr 1               ; hook for debug
00030Fr 1               exec_:
00030Fr 1  20 rr rr         jsr spull_
000312r 1  6C F4 00         jmp (tos)
000315r 1               
000315r 1               ;----------------------------------------------------------------------
000315r 1               empty_:
000315r 1  20 rr rr         jsr printStr
000318r 1  56 4F 49 44      .asciiz  "void define\r\n"
00031Cr 1  20 44 45 46  
000320r 1  49 4E 45 BF  
000328r 1  4C rr rr         jmp next_
00032Br 1               
00032Br 1               ;----------------------------------------------------------------------
00032Br 1               ; puts a string
00032Br 1               str_:
00032Br 1  A0 00            ldy #$00
00032Dr 1               @loop:
00032Dr 1  C8               iny
00032Er 1  B1 FE            lda (ips), y
000330r 1  C9 AD            cmp #'`'              ; ` is the string terminator
000332r 1  F0 05            beq @ends
000334r 1  20 rr rr         jsr putchar
000337r 1  90 F4            bcc @loop
000339r 1                   ; error in putchar
000339r 1               @ends:
000339r 1  4C rr rr         jmp next_
00033Cr 1               
00033Cr 1               ;----------------------------------------------------------------------
00033Cr 1               ; loop around waiting for character
00033Cr 1               waitchar:
00033Cr 1  A0 00            ldy #$00
00033Er 1               @loop:
00033Er 1  20 rr rr         jsr getchar
000341r 1  C9 20            cmp #32                 ; ge space ?
000343r 1  B0 0F            bcs @ischar
000345r 1  C9 00            cmp #$0                 ; is it end of string ?
000347r 1  F0 29            beq @endstr
000349r 1  C9 0D            cmp #13                 ; carriage return ?
00034Br 1  F0 10            beq @iscrlf
00034Dr 1  C9 0A            cmp #10                 ; line feed ?
00034Fr 1  F0 0C            beq @iscrlf
000351r 1               @ismacro:
000351r 1  4C rr rr         jmp macro
000354r 1               @ischar:
000354r 1  20 rr rr         jsr @echo
000357r 1                   ; nest ?
000357r 1  20 rr rr         jsr nesting
00035Ar 1  4C rr rr         jmp @loop            ; wait for next character
00035Dr 1               @iscrlf:
00035Dr 1                   ; CR
00035Dr 1  A9 0D            lda #13
00035Fr 1  20 rr rr         jsr @echo
000362r 1                   ; LF
000362r 1  A9 0A            lda #10
000364r 1  20 rr rr         jsr @echo
000367r 1                   ; pending nest ?
000367r 1  A5 F3            lda ns
000369r 1  C9 00            cmp #$00
00036Br 1  F0 D1            beq @loop
00036Dr 1               @isend:
00036Dr 1                   ; ETX
00036Dr 1  A9 03            lda #$03
00036Fr 1  20 rr rr         jsr @inbuff
000372r 1               ; mark end
000372r 1               @endstr:
000372r 1  A9 00            lda #$00
000374r 1  99 00 02         sta tib, y
000377r 1  4C rr rr         jmp next_
00037Ar 1               ; maximum 255 chars
00037Ar 1               @echo:
00037Ar 1                   ; echo
00037Ar 1  20 rr rr         jsr putchar
00037Dr 1               @inbuff:
00037Dr 1                   ; store
00037Dr 1  99 00 02         sta tib, y
000380r 1  C8               iny
000381r 1  C0 FE            cpy #$FE
000383r 1  F0 ED            beq @endstr
000385r 1  60               rts
000386r 1               
000386r 1               ;----------------------------------------------------------------------
000386r 1               ; calculate nesting value
000386r 1               nesting:
000386r 1  C9 AD            cmp #'`'
000388r 1  D0 07            bne @nests
00038Ar 1  A9 80            lda #$80
00038Cr 1  45 F3            eor ns
00038Er 1  85 F3            sta ns
000390r 1  60               rts
000391r 1               @nests:
000391r 1  24 F3            bit ns
000393r 1  30 18            bmi @nonest
000395r 1  C9 3A            cmp #':'
000397r 1  F0 15            beq @nestinc
000399r 1  C9 5B            cmp #'['
00039Br 1  F0 11            beq @nestinc
00039Dr 1  C9 28            cmp #'('
00039Fr 1  F0 0D            beq @nestinc
0003A1r 1  C9 3B            cmp #';'
0003A3r 1  F0 0C            beq @nestdec
0003A5r 1  C9 5D            cmp #']'
0003A7r 1  F0 08            beq @nestdec
0003A9r 1  C9 29            cmp #')'
0003ABr 1  F0 04            beq @nestdec
0003ADr 1               @nonest:
0003ADr 1  60               rts
0003AEr 1               @nestinc:
0003AEr 1  E6 F3            inc ns
0003B0r 1  60               rts
0003B1r 1               @nestdec:
0003B1r 1  C6 F3            dec ns
0003B3r 1  60               rts
0003B4r 1               
0003B4r 1               ;----------------------------------------------------------------------
0003B4r 1               ; prints a asciiz, refered by hardware stack
0003B4r 1               printStr:
0003B4r 1  68               pla
0003B5r 1  85 F9            sta wrk + 1
0003B7r 1  68               pla
0003B8r 1  85 F8            sta wrk + 0
0003BAr 1  20 rr rr         jsr puts_
0003BDr 1  A5 F8            lda wrk + 0
0003BFr 1  48               pha
0003C0r 1  A5 F9            lda wrk + 1
0003C2r 1  48               pha
0003C3r 1  60               rts
0003C4r 1               
0003C4r 1               ;----------------------------------------------------------------------
0003C4r 1               ; prints a asciiz, refered by wrk
0003C4r 1               puts_:
0003C4r 1  A0 00            ldy #$00
0003C6r 1  20 rr rr         jsr @isne
0003C9r 1               @loop:
0003C9r 1  20 rr rr         jsr putchar
0003CCr 1  E6 F8            inc wrk + 0
0003CEr 1  D0 02            bne @isne
0003D0r 1  E6 F9            inc wrk + 1
0003D2r 1               @isne:
0003D2r 1  B1 F8            lda (wrk), y
0003D4r 1  D0 F3            bne @loop
0003D6r 1               @ends:
0003D6r 1  60               rts
0003D7r 1               
0003D7r 1               ;----------------------------------------------------------------------
0003D7r 1               ; prints number in tos to decimal ASCII
0003D7r 1               printdec:
0003D7r 1  A9 10            lda #<10000
0003D9r 1  85 F6            sta nos + 0
0003DBr 1  A9 27            lda #>10000
0003DDr 1  85 F7            sta nos + 1
0003DFr 1  20 rr rr         jsr @nums
0003E2r 1  A9 E8            lda #<1000
0003E4r 1  85 F6            sta nos + 0
0003E6r 1  A9 03            lda #>1000
0003E8r 1  85 F7            sta nos + 1
0003EAr 1  20 rr rr         jsr @nums
0003EDr 1  A9 64            lda #<100
0003EFr 1  85 F6            sta nos + 0
0003F1r 1  A9 00            lda #>100
0003F3r 1  85 F7            sta nos + 1
0003F5r 1  20 rr rr         jsr @nums
0003F8r 1  A9 0A            lda #<10
0003FAr 1  85 F6            sta nos + 0
0003FCr 1  A9 00            lda #>10
0003FEr 1  85 F7            sta nos + 1
000400r 1  20 rr rr         jsr @nums
000403r 1  A9 0A            lda #<10
000405r 1  85 F6            sta nos + 0
000407r 1  A9 00            lda #>10
000409r 1  85 F7            sta nos + 1
00040Br 1               @nums:
00040Br 1  A0 2F            ldy #'0'-1
00040Dr 1               @loop:
00040Dr 1  C8               iny
00040Er 1  38               sec
00040Fr 1  A5 F4            lda tos + 0
000411r 1  E5 F6            sbc nos + 0
000413r 1  85 F4            sta tos + 0
000415r 1  A5 F5            lda tos + 1
000417r 1  E5 F7            sbc nos + 1
000419r 1  85 F5            sta tos + 1
00041Br 1  90 F0            bcc @loop
00041Dr 1  18               clc
00041Er 1  A5 F4            lda tos + 0
000420r 1  65 F6            adc nos + 0
000422r 1  85 F4            sta tos + 0
000424r 1  A5 F5            lda tos + 1
000426r 1  65 F7            adc nos + 1
000428r 1  85 F5            sta tos + 1
00042Ar 1  98               tya
00042Br 1  4C rr rr         jmp putchar
00042Er 1  60               rts
00042Fr 1               
00042Fr 1               ;----------------------------------------------------------------------
00042Fr 1               ; prints number in tos to hexadecimal ASCII
00042Fr 1               printhex:
00042Fr 1  A5 F5            lda tos + 1
000431r 1  20 rr rr         jsr printhex8
000434r 1  A5 F4            lda tos + 0
000436r 1  20 rr rr         jsr printhex8
000439r 1  60               rts
00043Ar 1               
00043Ar 1               ;----------------------------------------------------------------------
00043Ar 1               ; print a 8-bit HEX
00043Ar 1               printhex8:
00043Ar 1  85 F2            sta ap
00043Cr 1  18               clc
00043Dr 1  6A               ror
00043Er 1  6A               ror
00043Fr 1  6A               ror
000440r 1  6A               ror
000441r 1  20 rr rr         jsr @conv
000444r 1  A5 F2            lda ap
000446r 1               @conv:
000446r 1  18               clc
000447r 1  29 0F            and #$0F
000449r 1  69 30            adc #$30
00044Br 1  C9 3A            cmp #$3A
00044Dr 1  90 02            bcc @ends
00044Fr 1  69 06            adc #$06
000451r 1               @ends:
000451r 1  20 rr rr         jsr putchar
000454r 1  60               rts
000455r 1               
000455r 1               ;----------------------------------------------------------------------
000455r 1               ; convert a decimal value to binary
000455r 1               numd_:
000455r 1  A9 00            lda #$00
000457r 1  85 F4            sta tos + 0
000459r 1  85 F5            sta tos + 1
00045Br 1               @loop:
00045Br 1                   ; get a char from buffer
00045Br 1                   ; zzzz lda ch
00045Br 1  C5 30            cmp '0' + 0
00045Dr 1  90 10            bcc @ends
00045Fr 1  C5 3A            cmp '9' + 1
000461r 1  B0 0C            bcs @ends
000463r 1               @cv10:
000463r 1  38               sec
000464r 1  E9 30            sbc #'0'
000466r 1               @uval:
000466r 1                   ; zzzz sta ch
000466r 1  20 rr rr         jsr mul10_
000469r 1  A5 F4            lda tos + 0
00046Br 1                   ; zzzz adc ch
00046Br 1  85 F4            sta tos + 0
00046Dr 1  90 EC            bcc @loop
00046Fr 1               @ends:
00046Fr 1  4C rr rr         jmp spush_
000472r 1               
000472r 1               ;----------------------------------------------------------------------
000472r 1               ; convert a hexadecimal value to binary
000472r 1               numh_:
000472r 1  A9 00            lda #$00
000474r 1  85 F4            sta tos + 0
000476r 1  85 F5            sta tos + 1
000478r 1               @loop:
000478r 1                   ; get a char from buffer
000478r 1                   ; zzzz lda ch
000478r 1               @isd:
000478r 1  C9 30            cmp #'0'
00047Ar 1  90 22            bcc @ends
00047Cr 1  C9 3A            cmp #'9' + 1
00047Er 1  B0 06            bcs @ish
000480r 1               @cv10:
000480r 1  38               sec
000481r 1  E9 30            sbc #'0'
000483r 1  18               clc
000484r 1  90 0F            bcc @uval
000486r 1               @ish:
000486r 1  29 DF            and #%11011111
000488r 1  C5 C7            cmp 'F' + 1
00048Ar 1  B0 12            bcs @ends
00048Cr 1  C5 C1            cmp 'A'
00048Er 1  90 0E            bcc @ends
000490r 1               @cv16:
000490r 1  38               sec
000491r 1  E9 B7            sbc #'A' - 10
000493r 1  90 00            bcc @uval
000495r 1               @uval:
000495r 1                   ; zzzzz sta ch
000495r 1  20 rr rr         jsr mul16_
000498r 1  A5 F4            lda tos + 0
00049Ar 1                   ; zzzz adc ch
00049Ar 1  85 F4            sta tos + 0
00049Cr 1  90 DA            bcc @loop
00049Er 1               @ends:
00049Er 1  4C rr rr         jmp spush_
0004A1r 1               
0004A1r 1               ;----------------------------------------------------------------------
0004A1r 1               ; multiply by ten
0004A1r 1               ; 2x + 8x
0004A1r 1               mul10_:
0004A1r 1  06 F4            asl tos + 0
0004A3r 1  85 F6            sta nos + 0
0004A5r 1  26 F5            rol tos + 1
0004A7r 1  85 F7            sta nos + 1
0004A9r 1  06 F4            asl tos + 0
0004ABr 1  26 F5            rol tos + 1
0004ADr 1  06 F4            asl tos + 0
0004AFr 1  26 F5            rol tos + 1
0004B1r 1  18               clc
0004B2r 1  A5 F4            lda tos + 0
0004B4r 1  65 F6            adc nos + 0
0004B6r 1  85 F4            sta tos + 0
0004B8r 1  A5 F5            lda tos + 1
0004BAr 1  65 F7            adc nos + 1
0004BCr 1  85 F5            sta tos + 1
0004BEr 1  60               rts
0004BFr 1               
0004BFr 1               ;----------------------------------------------------------------------
0004BFr 1               ; multiply by sixteen
0004BFr 1               mul16_:
0004BFr 1  A0 04            ldy #04
0004C1r 1               @loop:
0004C1r 1  06 F4            asl tos + 0
0004C3r 1  85 F4            sta tos + 0
0004C5r 1  26 F5            rol tos + 1
0004C7r 1  85 F5            sta tos + 1
0004C9r 1  88               dey
0004CAr 1  D0 F5            bne @loop
0004CCr 1  60               rts
0004CDr 1               
0004CDr 1               ;----------------------------------------------------------------------
0004CDr 1               ; skip to eol
0004CDr 1               comment_:
0004CDr 1  A0 00            ldy #$00
0004CFr 1               @loop:
0004CFr 1  C8               iny
0004D0r 1  B1 FE            lda (ips), y
0004D2r 1  C9 0D            cmp #13
0004D4r 1  D0 F9            bne @loop
0004D6r 1                   ; skip \r
0004D6r 1  C8               iny
0004D7r 1                   ; skip \n
0004D7r 1  C8               iny
0004D8r 1                   ; offset
0004D8r 1  98               tya
0004D9r 1  18               clc
0004DAr 1  65 FE            adc ips + 0
0004DCr 1  90 02            bcc @iscc
0004DEr 1  E6 FF            inc ips + 1
0004E0r 1               @iscc:
0004E0r 1  4C rr rr         jmp next_
0004E3r 1               
0004E3r 1               ;----------------------------------------------------------------------
0004E3r 1               depth_:
0004E3r 1                   ; limit to 255 bytes
0004E3r 1  A9 FF            lda #$FF
0004E5r 1  38               sec
0004E6r 1  E5 F1            sbc xp
0004E8r 1                   ; words
0004E8r 1  4A               lsr
0004E9r 1  85 F4            sta tos + 0
0004EBr 1  A9 00            lda #00
0004EDr 1  85 F5            sta tos + 1
0004EFr 1  20 rr rr         jsr spush_
0004F2r 1  4C rr rr         jmp next_
0004F5r 1               
0004F5r 1               ;----------------------------------------------------------------------
0004F5r 1               ; print hexadecimal
0004F5r 1               hdot_:
0004F5r 1  20 rr rr         jsr spull_
0004F8r 1  20 rr rr         jsr printhex
0004FBr 1  4C rr rr         jmp dotsp
0004FEr 1               
0004FEr 1               ;----------------------------------------------------------------------
0004FEr 1               ; print decimal
0004FEr 1               dot_:
0004FEr 1  20 rr rr         jsr spull_
000501r 1  20 rr rr         jsr printdec
000504r 1  4C rr rr         jmp dotsp
000507r 1               
000507r 1               ;----------------------------------------------------------------------
000507r 1               ; print space
000507r 1               dotsp:
000507r 1  A9 20            lda #' '
000509r 1  20 rr rr         jsr  writeChar1
00050Cr 1  4C rr rr         jmp next_
00050Fr 1               
00050Fr 1               ;----------------------------------------------------------------------
00050Fr 1               writeChar:
00050Fr 1  20 rr rr         jsr ldaps_
000512r 1               
000512r 1               ;----------------------------------------------------------------------
000512r 1               writeChar1:
000512r 1  4C rr rr         jmp putchar
000515r 1  60               rts
000516r 1               
000516r 1               ;----------------------------------------------------------------------
000516r 1               enter_:
000516r 1  4C rr rr         jmp next_
000519r 1               
000519r 1               ;----------------------------------------------------------------------
000519r 1               macro:
000519r 1  4C rr rr         jmp next_
00051Cr 1               
00051Cr 1               ;----------------------------------------------------------------------
00051Cr 1               crlf:
00051Cr 1  20 rr rr         jsr printStr
00051Fr 1  BF 52 BF 4E      .asciiz "\r\n"
000523r 1  00           
000524r 1  60               rts
000525r 1               
000525r 1               ;----------------------------------------------------------------------
000525r 1               prompt:
000525r 1  20 rr rr         jsr printStr
000528r 1  BF 52 BF 4E      .asciiz "\r\n> "
00052Cr 1  3E 20 00     
00052Fr 1  60               rts
000530r 1               
000530r 1               ;----------------------------------------------------------------------
000530r 1               printStk:
000530r 1  20 rr rr         jsr enter
000533r 1                   ;.asciiz  "\\a@2-\\D1-(",$22,"@\\b@\\(,)(.)2-)'"
000533r 1  BF BF 41 40      .asciiz  "\\a@2-\\D1-(34@\\b@\\(,)(.)2-)'"
000537r 1  32 2D BF BF  
00053Br 1  C4 31 2D 28  
000553r 1  4C rr rr         jmp next_
000556r 1               
000556r 1               
000556r 1               ;----------------------------------------------------------------------
000556r 1               ; Execute next opcode
000556r 1               next_:
000556r 1                   ; using full jump table
000556r 1  20 rr rr         jsr incps_
000559r 1  20 rr rr         jsr ldaps_
00055Cr 1  0A               asl
00055Dr 1  A8               tay
00055Er 1  B9 rr rr         lda optcodes, y
000561r 1  85 F8            sta wrk + 0
000563r 1  C8               iny
000564r 1  B9 rr rr         lda optcodes, y
000567r 1  85 F9            sta wrk + 1
000569r 1  6C F8 00         jmp (wrk)
00056Cr 1               
00056Cr 1               ;----------------------------------------------------------------------
00056Cr 1               ; Execute code inline
00056Cr 1               enter:
00056Cr 1  20 rr rr         jsr pushps_
00056Fr 1               ; pull from system stack
00056Fr 1  68               pla
000570r 1  85 FE            sta ips + 0
000572r 1  68               pla
000573r 1  85 FF            sta ips + 1
000575r 1  20 rr rr         jsr decps_
000578r 1  4C rr rr         jmp next_
00057Br 1               
00057Br 1               ;----------------------------------------------------------------------
00057Br 1               ; Execute code from data stack
00057Br 1               go_:
00057Br 1  20 rr rr         jsr pushps_
00057Er 1               ; pull ps from data stack
00057Er 1                   ; ldx xp
00057Er 1  BD FF 03         lda spz + 0, x
000581r 1  85 FE            sta ips + 0
000583r 1  BD 00 04         lda spz + 1, x
000586r 1  85 FF            sta ips + 1
000588r 1  E8               inx
000589r 1  E8               inx
00058Ar 1                   ; stx xp
00058Ar 1  20 rr rr         jsr decps_
00058Dr 1  4C rr rr         jmp next_
000590r 1               
000590r 1               ;----------------------------------------------------------------------
000590r 1               init:
000590r 1               
000590r 1               ;----------------------------------------------------------------------
000590r 1               optcodes:
000590r 1               altcodes:
000590r 1               ctrcodes:
000590r 1               
