;
; MD201705
;

; Code, graphics and music by T.M.R/Cosine
; Original Protracker module by Maza


; This source code is formatted for the ACME cross assembler from
; http://sourceforge.net/projects/acme-crossass/
; Compression is handled with Exomizer which can be downloaded at
; http://csdb.dk/release/?id=141402

; build.bat will call both to create an assembled file and then the
; crunched release version.


; Select an output filename
		!to "md201705.prg",cbm


; Yank in binary data
		* = $2008
		!binary "data/effect.chr"

		* = $2400
		!binary "data/sprites.spr"

		* = $2800
		!binary "data/charset.chr"

		* = $9000
music		!binary "data/atominus.prg",,2


; Constants
rstr1p		= $30
rstr2p		= $e1


; Label assignments
rn		= $50

char_width	= $51		; for the scroller
d016_mirror	= $52
scroll_x	= $53
scroll_spd	= $54

preset_cnt	= $57
cos_at_1	= $58
cos_speed_1	= $59
cos_offset_1	= $5a

cos_at_2	= $5b
cos_speed_2	= $5c
cos_offset_2	= $5d

cos_at_3	= $5e
cos_speed_3	= $5f
cos_offset_3	= $60

cos_at_4	= $61
cos_speed_4	= $62
cos_offset_4	= $63

cos_work	= $64		; $26 bytes used

screen_ram	= $0400


; Add a BASIC startline
		* = $0801
		!word entry-2
		!byte $00,$00,$9e
		!text "16384"
		!byte $00,$00,$00


; Entry point at $0812
		* = $4000
entry		sei

; Turn off ROMs and set up interrupts
		lda #$35
		sta $01

		lda #<nmi
		sta $fffa
		lda #>nmi
		sta $fffb

		lda #<int
		sta $fffe
		lda #>int
		sta $ffff

		lda #$7f
		sta $dc0d
		sta $dd0d

		lda $dc0d
		lda $dd0d

		lda #rstr1p
		sta $d012

		lda #$1b
		sta $d011
		lda #$01
		sta $d019
		sta $d01a

; Clear label space and set some specific labels
		ldx #$50
		lda #$00
nuke_zp		sta $00,x
		inx
		bne nuke_zp

		lda #$01
		sta rn

; Set a few fixed video registers
		lda #$0f
		sta $d020

; Clear the screen RAM
		ldx #$00
screen_clear	lda #$00
		sta screen_ram+$000,x
		sta screen_ram+$100,x
		sta screen_ram+$200,x
		sta screen_ram+$2e8,x
		lda #$0f
		sta $d800,x
		sta $d900,x
		sta $da00,x
		sta $dae8,x
		inx
		bne screen_clear

; Reset the scrolling message
		jsr reset
		lda #$03
		sta scroll_spd
		lda #$01
		sta char_width
		sta d016_mirror

; Rest the preset system
		jsr preset_fetch

; Initialise the music
		lda #$00
		jsr music+$00

		cli

; Infinite loop - runtime isn't used after this point
		jmp *


; IRQ interrupt
int		pha
		txa
		pha
		tya
		pha

		lda $d019
		and #$01
		sta $d019
		bne ya
		jmp ea31

ya		lda rn
		cmp #$02
		bne *+$05
		jmp rout2


; Raster split 1
rout1		lda #$04
		sta $d021
		lda #$0e
		sta $d022
		lda #$0d
		sta $d023

		lda #$18
		ldx #$1b
		ldy #$17
		sta $d018
		stx $d011
		sty $d016

; Set up copyright symbol sprites
		lda #$ff
		sta $d015
		lda #$00
		sta $d01b
		sta $d01d

		ldx #$00
logo_spr_set	lda logo_spr_pos,x
		sta $d000,x
		inx
		cpx #$11
		bne logo_spr_set

		ldx #$00
logo_sdp_set	lda logo_spr_dp,x
		sta screen_ram+$3f8,x
		lda #$00
		sta $d027,x
		inx
		cpx #$08
		bne logo_sdp_set

; Clone the effect area to get the last six lines
!set char_cnt=$00
!do {
		lda screen_ram+$050+char_cnt
		sta screen_ram+$2a8+char_cnt
		sta screen_ram+$370+char_cnt
		lda screen_ram+$078+char_cnt
		sta screen_ram+$2d0+char_cnt
		lda screen_ram+$0a0+char_cnt
		sta screen_ram+$2f8+char_cnt
		lda screen_ram+$000+char_cnt
		sta screen_ram+$320+char_cnt
		lda screen_ram+$028+char_cnt
		sta screen_ram+$348+char_cnt


		lda $d850+char_cnt
		sta $daa8+char_cnt
		sta $db70+char_cnt
		lda $d878+char_cnt
		sta $dad0+char_cnt
		lda $d8a0+char_cnt
		sta $daf8+char_cnt
		lda $d800+char_cnt
		sta $db20+char_cnt
		lda $d828+char_cnt
		sta $db48+char_cnt

		!set char_cnt=char_cnt+$01
} until char_cnt=$26

; Update positions for the effect area
		lda cos_at_1
		clc
		adc cos_speed_1
		sta cos_at_1
		tax

		lda cos_at_2
		clc
		adc cos_speed_2
		sta cos_at_2
		tay

		lda effect_cosinus,x
		clc
		adc effect_cosinus,y
		sta cos_work

!set column_cnt=$01
!do {
		txa
		clc
		adc cos_offset_1
		tax

		tya
		clc
		adc cos_offset_2
		tay

		lda effect_cosinus,x
		clc
		adc effect_cosinus,y
		sta cos_work+column_cnt

		!set column_cnt=column_cnt+$01
} until column_cnt=$26


		lda cos_at_3
		clc
		adc cos_speed_3
		sta cos_at_3
		tax

		lda cos_at_4
		clc
		adc cos_speed_4
		sta cos_at_4
		tay

		lda cos_work
		clc
		adc effect_cosinus,x
		clc
		adc effect_cosinus,y
		sta cos_work

!set column_cnt=$01
!do {
		txa
		clc
		adc cos_offset_3
		tax

		tya
		clc
		adc cos_offset_4
		tay

		lda cos_work+column_cnt
		clc
		adc effect_cosinus,x
		clc
		adc effect_cosinus,y
		sta cos_work+column_cnt

		!set column_cnt=column_cnt+$01
} until column_cnt=$26

; Move the scrolling message
		ldy scroll_spd
scroll_upd	ldx scroll_x
		inx
		cpx #$08
		beq *+$05
		jmp sx_xb

; Shift the character lines
!set char_cnt=$00
!do {
		lda screen_ram+$399+char_cnt
		sta screen_ram+$398+char_cnt
		ora #$80
		sta screen_ram+$3c0+char_cnt

		!set char_cnt=char_cnt+$01
} until char_cnt=$26

		dec char_width
		beq mread

; Bump the current character value by one
		lda screen_ram+$398+$26
		clc
		adc #$01
		sta screen_ram+$398+$26
		ora #$80
		sta screen_ram+$3c0+$26
		jmp no_fetch

; Fetch a new character
mread		ldx scroll_text
		bne okay
		jsr reset
		jmp mread

okay		cpx #$81
		bcc okay_2
		txa
		and #$0f
		sta scroll_spd
		ldx #$20

okay_2		txa
		asl
		sta screen_ram+$398+$26
		ora #$80
		sta screen_ram+$3c0+$26
		lda char_width_dcd,x
		sta char_width

		inc mread+$01
		bne *+$05
		inc mread+$02

no_fetch	ldx #$00
sx_xb		stx scroll_x

		dey
		beq *+$05
		jmp scroll_upd

		txa
		and #$07
		eor #$07
		sta d016_mirror

; Check to see if a preset is going to be loaded
		inc preset_cnt
		bne preset_skip

		jsr preset_fetch

preset_skip

; Play the music
		jsr music+$03

; Set up for the second raster split
		lda #$02
		sta rn
		lda #rstr2p
		sta $d012

		jmp ea31


		* = ((*/$100)+$01)*$100

; Raster split 2
rout2

; Set up sprites for the dissolve
		lda #$ff
		sta $d01b
		lda #$12
		sta $d01d

		ldx #$00
scroll_spr_set	lda scroll_spr_pos,x
		sta $d000,x
		inx
		cpx #$11
		bne scroll_spr_set

		ldx #$00
scroll_sdp_set	lda scroll_spr_dp,x
		sta screen_ram+$3f8,x
		lda scroll_spr_col,x
		sta $d027,x
		inx
		cpx #$08
		bne scroll_sdp_set

; Set up video registers for the scroller
		ldx #$08
		dex
		bne *-$01

		lda #$1a
		ldx #$0b
		ldy d016_mirror
		sta $d018
		sty $d016
		stx $d021

; Render the effect area
!set column_cnt=$00
!do {
		ldy cos_work+column_cnt

		lda map_data+$00,y
		sta screen_ram+$000+column_cnt
		sta screen_ram+$0c8+column_cnt
		sta screen_ram+$190+column_cnt
		sta screen_ram+$258+column_cnt
		lda map_col_data+$00,y
		sta $d800+$000+column_cnt
		sta $d800+$0c8+column_cnt
		sta $d800+$190+column_cnt
		sta $d800+$258+column_cnt

		lda map_data+$08,y
		sta screen_ram+$028+column_cnt
		sta screen_ram+$0f0+column_cnt
		sta screen_ram+$1b8+column_cnt
		sta screen_ram+$280+column_cnt
		lda map_col_data+$08,y
		sta $d800+$028+column_cnt
		sta $d800+$0f0+column_cnt
		sta $d800+$1b8+column_cnt
		sta $d800+$280+column_cnt

		lda map_data+$10,y
		sta screen_ram+$050+column_cnt
		sta screen_ram+$118+column_cnt
		sta screen_ram+$1e0+column_cnt
		lda map_col_data+$10,y
		sta $d800+$050+column_cnt
		sta $d800+$118+column_cnt
		sta $d800+$1e0+column_cnt

		lda map_data+$18,y
		sta screen_ram+$078+column_cnt
		sta screen_ram+$140+column_cnt
		sta screen_ram+$208+column_cnt
		lda map_col_data+$18,y
		sta $d800+$078+column_cnt
		sta $d800+$140+column_cnt
		sta $d800+$208+column_cnt

		lda map_data+$20,y
		sta screen_ram+$0a0+column_cnt
		sta screen_ram+$168+column_cnt
		sta screen_ram+$230+column_cnt
		lda map_col_data+$20,y
		sta $d800+$0a0+column_cnt
		sta $d800+$168+column_cnt
		sta $d800+$230+column_cnt


		!set column_cnt=column_cnt+$01
} until column_cnt=$26

; Set up for the first raster split
		lda #$01
		sta rn
		lda #rstr1p
		sta $d012

; Exit interrupt
ea31		pla
		tay
		pla
		tax
		pla
nmi		rti

; Reset the self mod for the scroller
reset		lda #<scroll_text
		sta mread+$01
		lda #>scroll_text
		sta mread+$02
		rts

; Preset self mod and reset code
preset_mread	lda preset_data
		inc preset_mread+$01
		bne *+$05
		inc preset_mread+$02
		rts

preset_reset	lda #<preset_data_2
		sta preset_mread+$01
		lda #>preset_data_2
		sta preset_mread+$02
		rts


; Effect preset reader
; $80 in the first byte is a "skip" command to retain the current value,
; $81 wraps to preset_data_2
preset_fetch	jsr preset_mread
		cmp #$81
		bne pf_write

		jsr preset_reset	; first byte of a preset is $81 so reset
		jmp preset_fetch

; First oscillator
pf_write	cmp #$80		; skip the write if it's $80
		beq *+$04
		sta cos_at_1

		jsr preset_mread
		sta cos_speed_1

		jsr preset_mread
		sta cos_offset_1

; Second oscillator
		jsr preset_mread
		cmp #$80		; skip the write if it's $80
		beq *+$04
		sta cos_at_2

		jsr preset_mread
		sta cos_speed_2

		jsr preset_mread
		sta cos_offset_2

; Third oscillator
		jsr preset_mread
		cmp #$80		; skip the write if it's $80
		beq *+$04
		sta cos_at_3

		jsr preset_mread
		sta cos_speed_3

		jsr preset_mread
		sta cos_offset_3

; Fourth oscillator
		jsr preset_mread
		cmp #$80		; skip the write if it's $80
		beq *+$04
		sta cos_at_4

		jsr preset_mread
		sta cos_speed_4

		jsr preset_mread
		sta cos_offset_4

		rts

; Screen data for the effect
		* = ((*/$100)+$01)*$100
map_data	!byte $01,$02,$03,$04,$05,$06,$07,$08
		!byte $09,$0a,$0b,$0c,$0d,$0e,$0f,$10
		!byte $11,$12,$13,$14,$15,$16,$17,$18
		!byte $19,$1a,$1b,$1c,$1d,$1e,$1f,$20
		!byte $21,$22,$23,$24,$25,$26,$27,$28

		!byte $01,$02,$03,$04,$05,$06,$07,$08
		!byte $09,$0a,$0b,$0c,$0d,$0e,$0f,$10
		!byte $11,$12,$13,$14,$15,$16,$17,$18
		!byte $19,$1a,$1b,$1c,$1d,$1e,$1f,$20
		!byte $21,$22,$23,$24,$25,$26,$27,$28

		!byte $01,$02,$03,$04,$05,$06,$07,$08
		!byte $09,$0a,$0b,$0c,$0d,$0e,$0f,$10
		!byte $11,$12,$13,$14,$15,$16,$17,$18
		!byte $19,$1a,$1b,$1c,$1d,$1e,$1f,$20
		!byte $21,$22,$23,$24,$25,$26,$27,$28

		!byte $01,$02,$03,$04,$05,$06,$07,$08
		!byte $09,$0a,$0b,$0c,$0d,$0e,$0f,$10
		!byte $11,$12,$13,$14,$15,$16,$17,$18
		!byte $19,$1a,$1b,$1c,$1d,$1e,$1f,$20
		!byte $21,$22,$23,$24,$25,$26,$27,$28

		!byte $01,$02,$03,$04,$05,$06,$07,$08
		!byte $09,$0a,$0b,$0c,$0d,$0e,$0f,$10
		!byte $11,$12,$13,$14,$15,$16,$17,$18
		!byte $19,$1a,$1b,$1c,$1d,$1e,$1f,$20
		!byte $21,$22,$23,$24,$25,$26,$27,$28

		!byte $01,$02,$03,$04,$05,$06,$07,$08
		!byte $09,$0a,$0b,$0c,$0d,$0e,$0f,$10
		!byte $11,$12,$13,$14,$15,$16,$17,$18
		!byte $19,$1a,$1b,$1c,$1d,$1e,$1f,$20
		!byte $21,$22,$23,$24,$25,$26,$27,$28

		* = ((*/$100)+$01)*$100
map_col_data	!byte $0e,$0e,$0e,$0e,$0e,$0b,$0b,$0b
		!byte $0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b
		!byte $0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b
		!byte $0b,$0b,$0b,$0b,$0b,$0b,$0e,$0e
		!byte $0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e

		!byte $0e,$0e,$0e,$0e,$0e,$0b,$0b,$0b
		!byte $0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b
		!byte $0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b
		!byte $0b,$0b,$0b,$0b,$0b,$0b,$0e,$0e
		!byte $0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e

		!byte $0e,$0e,$0e,$0e,$0e,$0b,$0b,$0b
		!byte $0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b
		!byte $0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b
		!byte $0b,$0b,$0b,$0b,$0b,$0b,$0e,$0e
		!byte $0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e

		!byte $0e,$0e,$0e,$0e,$0e,$0b,$0b,$0b
		!byte $0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b
		!byte $0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b
		!byte $0b,$0b,$0b,$0b,$0b,$0b,$0e,$0e
		!byte $0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e

		!byte $0e,$0e,$0e,$0e,$0e,$0b,$0b,$0b
		!byte $0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b
		!byte $0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b
		!byte $0b,$0b,$0b,$0b,$0b,$0b,$0e,$0e
		!byte $0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e

		!byte $0e,$0e,$0e,$0e,$0e,$0b,$0b,$0b
		!byte $0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b
		!byte $0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b
		!byte $0b,$0b,$0b,$0b,$0b,$0b,$0e,$0e
		!byte $0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e

		* = ((*/$100)+$01)*$100
effect_cosinus	!byte $27,$27,$27,$27,$27,$27,$27,$27
		!byte $27,$27,$27,$27,$27,$26,$26,$26
		!byte $26,$26,$26,$25,$25,$25,$25,$24
		!byte $24,$24,$24,$23,$23,$23,$22,$22
		!byte $22,$21,$21,$21,$20,$20,$1f,$1f
		!byte $1f,$1e,$1e,$1d,$1d,$1c,$1c,$1c
		!byte $1b,$1b,$1a,$1a,$19,$19,$18,$18
		!byte $17,$17,$16,$16,$15,$15,$14,$14

		!byte $13,$13,$12,$12,$12,$11,$11,$10
		!byte $10,$0f,$0f,$0e,$0e,$0d,$0d,$0c
		!byte $0c,$0b,$0b,$0a,$0a,$0a,$09,$09
		!byte $08,$08,$08,$07,$07,$06,$06,$06
		!byte $05,$05,$05,$04,$04,$04,$03,$03
		!byte $03,$03,$02,$02,$02,$02,$01,$01
		!byte $01,$01,$01,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00

		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$01,$01,$01
		!byte $01,$01,$01,$02,$02,$02,$02,$03
		!byte $03,$03,$03,$04,$04,$04,$05,$05
		!byte $05,$06,$06,$07,$07,$07,$08,$08
		!byte $08,$09,$09,$0a,$0a,$0b,$0b,$0b
		!byte $0c,$0c,$0d,$0d,$0e,$0e,$0f,$0f
		!byte $10,$10,$11,$11,$12,$12,$13,$13

		!byte $14,$14,$15,$15,$16,$16,$17,$17
		!byte $18,$18,$18,$19,$19,$1a,$1a,$1b
		!byte $1b,$1c,$1c,$1d,$1d,$1d,$1e,$1e
		!byte $1f,$1f,$20,$20,$20,$21,$21,$21
		!byte $22,$22,$22,$23,$23,$23,$24,$24
		!byte $24,$24,$25,$25,$25,$25,$26,$26
		!byte $26,$26,$26,$27,$27,$27,$27,$27
		!byte $27,$27,$27,$27,$27,$27,$27,$27


; Sprite positions and data pointers for the copyright symbol
logo_spr_pos	!byte $20,$3a,$38,$3a,$50,$3a,$20,$4f
		!byte $50,$4f,$20,$64,$38,$64,$50,$64
		!byte $00

logo_spr_dp	!byte $90,$91,$92,$93,$94,$95,$96,$97

; Sprite positions, colours and data pointers for the scroll mask
scroll_spr_pos	!byte $18,$ec,$18,$ec,$48,$ec,$38,$ec
		!byte $20,$ec,$08,$ec,$00,$00,$00,$00
		!byte $38

scroll_spr_dp	!byte $98,$99,$98,$9a,$99,$9a,$99,$99

scroll_spr_col	!byte $0f,$0c,$0c,$0f,$0c,$0c,$00,$00


; Width for each character
char_width_dcd	!byte $01,$02,$02,$02,$02,$02,$02,$02		; @ to G
		!byte $02,$01,$02,$02,$02,$02,$02,$02		; H to O
		!byte $02,$02,$02,$02,$02,$02,$02,$02		; P to W
		!byte $02,$02,$02,$01,$01,$01,$01,$01		; X to Z, 5 * punct.
		!byte $01,$01,$01,$01,$01,$01,$01,$01		; space to '
		!byte $02,$02,$01,$02,$01,$02,$01,$02		; ( to /
		!byte $02,$01,$02,$02,$02,$02,$02,$02		; 0 to 7
		!byte $02,$02,$01,$01,$01,$02,$01,$02		; 8 to ?

; Effect preset data
; Values are curve start position, speed and offset, one block per counter
; $80 in the position byte is a "skip" command to retain the current value,
; $81 in the first byte wraps to preset_data_2
preset_data	!byte $00,$00,$10
		!byte $00,$03,$00
		!byte $00,$00,$00
		!byte $00,$00,$00

		!byte $80,$fd,$10
		!byte $80,$03,$00
		!byte $80,$02,$00
		!byte $80,$00,$00

		!byte $80,$03,$10
		!byte $80,$fe,$03
		!byte $80,$01,$00
		!byte $80,$00,$00

		!byte $80,$fd,$10
		!byte $80,$02,$03
		!byte $80,$01,$0c
		!byte $80,$00,$00

		!byte $80,$03,$10
		!byte $80,$fe,$03
		!byte $80,$ff,$0c
		!byte $80,$04,$11

; The restart point (everything above only appears once)
preset_data_2	!byte $00,$fd,$03
		!byte $6f,$fd,$fd
		!byte $00,$02,$00
		!byte $00,$03,$00

		!byte $00,$03,$0c
		!byte $00,$02,$f9
		!byte $00,$fd,$0f
		!byte $00,$03,$04

		!byte $00,$02,$83
		!byte $00,$fb,$04
		!byte $00,$fe,$00
		!byte $00,$00,$00

		!byte $00,$fd,$0c
		!byte $10,$02,$f9
		!byte $20,$f8,$0f
		!byte $30,$06,$0c

		!byte $80,$fd,$04
		!byte $40,$fb,$07
		!byte $00,$f9,$0a
		!byte $00,$02,$00

		!byte $00,$fe,$80
		!byte $00,$02,$02
		!byte $00,$04,$00
		!byte $00,$00,$00

		!byte $00,$06,$03
		!byte $6f,$06,$fd
		!byte $00,$02,$fe
		!byte $00,$03,$02

		!byte $00,$ff,$04
		!byte $80,$fe,$04
		!byte $00,$fd,$04
		!byte $80,$01,$04

		!byte $00,$01,$81
		!byte $00,$00,$81
		!byte $00,$03,$04
		!byte $00,$02,$f8

		!byte $00,$01,$06
		!byte $28,$02,$06
		!byte $50,$03,$06
		!byte $78,$04,$06

		!byte $00,$03,$08
		!byte $00,$02,$f8
		!byte $00,$00,$0c
		!byte $00,$00,$f4

		!byte $00,$03,$09
		!byte $6f,$03,$f7
		!byte $00,$04,$00
		!byte $00,$03,$00

		!byte $00,$fd,$06
		!byte $6f,$fd,$fa
		!byte $00,$fe,$00
		!byte $00,$03,$00

		!byte $48,$01,$81
		!byte $c8,$01,$7f
		!byte $00,$02,$03
		!byte $00,$03,$03

		!byte $00,$fd,$0c
		!byte $80,$02,$0a
		!byte $00,$f8,$08
		!byte $80,$06,$06

		!byte $00,$fe,$80
		!byte $40,$ff,$80
		!byte $00,$00,$07
		!byte $00,$00,$00

		!byte $00,$02,$80
		!byte $20,$00,$82
		!byte $00,$00,$12
		!byte $00,$01,$00

		!byte $81		; end of data marker


; Text for the scrolling message
scroll_text	!scr $82,"h'lo everyone and welcome to"
		!scr "    "

		!scr $81,"--- md201705 ---   ",$83
		!scr "    "

		!scr "code, graphics and music me (the magic roundabout) with "
		!scr "the latter being a cover of ''atominus'' by maza which "
		!scr "was done in an already outdated version of odie's ems "
		!scr "editor over",$82,"two decades ago...!",$83
		!scr "      "

		!scr "this effect is something i've done previously as well, "
		!scr "there's a version of it in mish mash but this time the "
		!scr "colour ram is being updated along with the screen."
		!scr "      "

		!scr "that's not a major ask in this particular case of "
		!scr "course, but it was at least entertaining to do."
		!scr "      "

		!scr $83,"i'm in something of a rush to get this released "
		!scr "in time for the end of the month again;  "
		!scr "may has been extremely busy (getting married was a "
		!scr "blast and terrifying at the same time) so, although "
		!scr "the code for this was ready just shy of two weeks "
		!scr "ago, i haven't been able to find the time for my "
		!scr "favouritest job of text writing and there still isn't "
		!scr "any inspiration now!"
		!scr "      "

		!scr $84,"so let's get on with those all-important greetings "
		!scr "so i can finish up and release!"
		!scr "      "

		!scr $82,"hello to the delightful people in..."
		!scr "      "


		!scr $85,"absence - "
		!scr "abyss connection - "
		!scr "arkanix labs - "
		!scr "artstate - "
		!scr "ate bit - "
		!scr "atlantis - "
		!scr "booze design - "
		!scr "camelot - "
		!scr "censor design - "
		!scr "chorus - "
		!scr "chrome - "
		!scr "cncd - "
		!scr "cpu - "
		!scr "crescent - "
		!scr "crest - "
		!scr "covert bitops - "
		!scr "defence force - "
		!scr "dekadence - "
		!scr "desire - "
		!scr "dac - "
		!scr "dmagic - "
		!scr "dualcrew - "
		!scr "exclusive on - "
		!scr "fairlight - "
		!scr "f4cg - "
		!scr "fire - "
		!scr "flat 3 - "
		!scr "focus - "
		!scr "french touch - "
		!scr "funkscientist productions - "
		!scr "genesis project - "
		!scr "gheymaid inc. - "
		!scr "hitmen - "
		!scr "hokuto force - "
		!scr "legion of doom - "
		!scr "level64 - "
		!scr "maniacs of noise - "
		!scr "mayday - "
		!scr "meanteam - "
		!scr "metalvotze - "
		!scr "noname - "
		!scr "nostalgia - "
		!scr "nuance - "
		!scr "offence - "
		!scr "onslaught - "
		!scr "orb - "
		!scr "oxyron - "
		!scr "padua - "
		!scr "performers - "
		!scr "plush - "
		!scr "professional protection cracking service - "
		!scr "psytronik - "
		!scr "reptilia - "
		!scr "resource - "
		!scr "rgcd - "
		!scr "secure - "
		!scr "shape - "
		!scr "side b - "
		!scr "singular - "
		!scr "slash - "
		!scr "slipstream - "
		!scr "success and trc - "
		!scr "style - "
		!scr "suicyco industries - "
		!scr "taquart - "
		!scr "tempest - "
		!scr "tek - "
		!scr "triad - "
		!scr "trsi - "
		!scr "viruz - "
		!scr "vision - "
		!scr "wow - "
		!scr "wrath "
		!scr "and xenon."
		!scr "         "

		!scr $82,"and now i can get the plug out of the way for "
		!scr "http://cosine.org.uk/ and my own blog at "
		!scr "http://jasonkelk.me.uk/ befoer signing off for the "
		!scr "night..."
		!scr "         "

		!scr $83,"this was t.m.r of cosine getting ready for a "
		!scr "wrap very late on 2017-05-31... .. .  .   ."
		!scr "                  "

		!byte $00		; end of text marker