; SPREADPOINT
; FILLUSION (AMIGA, PAL, >= OCS, >= 68000, >= 512 KB)
; (C) 2024 DEPECHE

; Build with vasm
; vasmm68k_mot -kick1hunks -Fhunkexe -o fillusion -nosym fillusion.s

AbsExecBase	equ	4
OldOpenLibrary	equ	-408
CloseLibrary	equ	-414
Write		equ	-48
Output		equ	-60
AvailMem	equ	-216
AllocMem	equ	-198
FreeMem		equ	-210
TypeOfMem	equ	-534
WaitTOF		equ	-270
Forbid		equ	-132
Permit		equ	-138
LoadView	equ	-222

custom		equ	$dff000

tilewidth	equ	10

steps		equ	23 ; (not 24)

inviswidth	equ	(steps-4+3)*tilewidth

viswidth	equ	40

pwidth		equ	viswidth+inviswidth
pheight		equ	80 ; px
psize		equ	pwidth*pheight
coverplanesize	equ	viswidth*pheight*3
center		equ	psize/2+((pwidth-inviswidth)/2)
numplanes	equ	4
numcoverplanes	equ	1

numtilesx	equ	4
numtilesy	equ	3

numvertices	equ	20

gmpwidth	equ	80 ; bytes (hires, 640px)
gmpheight	equ	39 ; px
gmpsize		equ	gmpwidth*gmpheight
gmlineheight	equ	8
gmblinkvalue	equ	50

royalwait1	equ	42
royalwait2	equ	102

; profiling
availablemem	equ	0
numbers		equ	0
testing		equ	0
timing		equ	0

; DMACON
; see http://coppershade.org/articles/Code/Reference/DMACON/
SET		equ	1<<15		; 0=clear, 1=set bits that are set to 1 below
DMAEN		equ	1<<9		; Enable all DMA below
BPLEN		equ	1<<8		; Bit plane DMA
COPEN		equ	1<<7		; Copper DMA
BLTEN		equ	1<<6		; Blitter DMA

*------	ALLOCATE MEMORY AND SAVE STATE -----------------------------------*

base	movem.l	a0-a6/d0-d7,-(a7)	;
	bsr	alloc			;
	bne	.exit			; out of memory error?

	if availablemem
	move.l	AbsExecBase.w,a6	;
	move.l	#MEMF_CHIP,d1		;
	jsr	AvailMem(a6)		;
	move.l	d0,$210.w		; Free (available) memory
	endif

	move.l	AbsExecBase.w,a6	;
	lea	.gfx(pc),a1		;
	jsr	OldOpenLibrary(a6)	; open gfx library
	move.l	d0,a6			;
	beq	.exit			; could not open gfx library
	move.l 	34(a6),-(a7)		; view
	move.l	d0,-(a7)		; gfx base
	move.l 	38(a6),-(a7)		; copper list 1
;	move.l 	50(a6),-(a7)		; copper list 2
	sub.l	a1,a1			;
	jsr	LoadView(a6)		;
	jsr	WaitTOF(a6)		;
	jsr	WaitTOF(a6)		;
	move.l	AbsExecBase.w,a6	;	
	jsr	Forbid(a6)		;
	
	lea	custom,a6		;
	bsr	waitblitter		;

	move.w	$02(a6),-(a7)		; store DMA control
	move.w	$1c(a6),-(a7)		; store interrupt enable bits
	move.l	$6c.w,-(a7)		; store irq3
	move.w	#$7fff,d0		;
	move.w	d0,$9a(a6)		;
	move.w	d0,$9c(a6)		; delete all interrupt requests
	move.w	d0,$96(a6)		; disable all DMAs

	clr.w	-(a7)			; store LED state
	btst	#1,$bfe001		;
	beq	.ledstate		;
	not.w	(a7)			;
.ledstate
	bset	#1,$bfe001		; LED dark

*------	INIT -------------------------------------------------------------*

	lea	vars(pc),a5		;

	lea	text(pc),a0		;
	move.l	a0,v_textpointer(a5)	;
	lea	playcmds(pc),a0		;
	move.l	a0,v_cmdspointer(a5)	;
	move.w	#gmblinkvalue,v_gmblinkdelay(a5) ;
	move.b	#royalwait2,v_waitvalue(a5) ; see cmd_royalwait
	move.w	#100,v_volume(a5)	;
	addq.w	#1<<actorlspplay,v_actors(a5) ;

	bsr	lspinit			;

	clr.l	v_bpltoffset(a5)	;
	move.l	#8*tilewidth,v_bplmoffset(a5)
	move.l	#16*tilewidth,v_bplboffset(a5)
	
	lea	irq3(pc),a0		;
	move.l	a0,$6c.w		;

	bsr	waitraster		; avoid flickering (?)
	move.l	b_genclist(pc),$80(a6)	;

	move.w	#SET+DMAEN+BPLEN+BLTEN+COPEN,$96(a6) ;

	move.w	#64*2,v_angles(a5)	;
	move.w	#6*2,v_deltas+2(a5)	; 288/12 = 24 (=steps minus 1) * 3200 = 76800

;	rem
	move.w	#tilewidth*8/2,v_tx(a5)	;
	clr.w	v_xoffset(a5)		;

	moveq	#steps+3-1,d7		;
.loop	move.l	d7,-(a7)		;
	bsr	mtx			;
	bsr	hiddenlines		;
	move.l	b_bitplanes(pc),a2	;
	add.l	#psize/2,a2		;
	move.l	a2,-(a7)		; used for stroke
	bsr	draw			; draw object

	move.l	b_bitplanes(pc),a0	;
	add.w	v_xoffset(a5),a0	;
	
	add.l	#psize-pwidth+tilewidth-2,a0 ; fills from bottom to top (why -2?)
	moveq	#numplanes-1,d7		;
.fill	bsr	fullfill		;
	add.l	#psize,a0		;
	dbf	d7,.fill		;

	move.l	(a7)+,a2		;
	bsr	stroke			;

	add.w	#tilewidth*8,v_tx(a5)	;
	add.w	#tilewidth,v_xoffset(a5);
	
	move.l	(a7)+,d7		;
	dbf	d7,.loop		;
;	erem

	lea	v_covertiles(a5),a1	; create address of cover tiles
	moveq	#numtilesy-1,d6		;
	move.l	b_coverplane(pc),a0	;
.tilesy	moveq	#numtilesx-1,d7		;
.tilesx	move.l	a0,(a1)+		;
	add.w	#tilewidth,a0		; next x
	dbf	d7,.tilesx		;
	add.w	#viswidth*pheight-numtilesx*tilewidth,a0 ; next y
	dbf	d6,.tilesy		;

	lea	v_covertiles(a5),a1	; create address of cover tiles
	move.b	$bfe801,d0		;
	add.l	a7,d0			; additional awesome randomness
	move.b	$bfe901,d1		;
	add.l	$4.w,d1			; additional awesome randomness
	and.l	#$ff,d0			; (do not shuffle too long. see dbf)
.shuffle
	add.b	d1,d0			;
	moveq	#0,d2			; index 1
	move.b	d0,d2			;
	and.w	#%1111,d2		; 0...15
	cmp.b	#11,d2			;
	ble	.inrange1		;
	sub.b	#6,d2			; 0...11
.inrange1
	add.b	d1,d0			;
	moveq	#0,d3			; index 2
	move.b	d0,d3			;
	and.w	#%1111,d3		; 0...15
	cmp.b	#11,d3			;
	ble	.inrange2		;
	sub.b	#6,d3			; 0...11
.inrange2
	asl.w	#2,d2			; *4 (long word offset)
	asl.w	#2,d3			; *4 (long word offset)
	move.l	(a1,d2.w),d4		;
	move.l	(a1,d3.w),d5		;
	move.l	d4,(a1,d3.w)		;
	move.l	d5,(a1,d2.w)		;
	dbf	d0,.shuffle		;

	move.w	#$c030,$9a(a6)		; enable coper and vertb interrupts

*------	IDLE LOOP --------------------------------------------------------*

	bsr	precalc			;
	
*------	RESTORE STATE AND EXIT -------------------------------------------*

	bsr	waitblitter		;
	
	tst.w	(a7)+			; restore state
	bne	.leddark		;
	bclr	#1,$bfe001		; LED bright
.leddark
	move.w	#$7fff,d0		;
	move.w	d0,$9a(a6)		;
	move.w	d0,$9c(a6)		;
	move.w	d0,$96(a6)		;

	moveq	#0,d0			; volume to zero
	move.w	d0,$a8(a6)		;
	move.w	d0,$b8(a6)		;
	move.w	d0,$c8(a6)		;
	move.w	d0,$d8(a6)		;
	
	move.l	(a7)+,$6c.w		; 
	move.w	(a7)+,d0		;
	or.w	#$c000,d0		;
	move.w	d0,$9a(a6)		;
	move.w	(a7)+,d0		;
	or.w	#$8000,d0		;
	move.w	d0,$96(a6)		;

;	move.l	(a7)+,$84(a6)		; copper list 2
	move.l	(a7)+,$80(a6)		; copper list 1
	move.l	(a7)+,a6		; gfx base
	move.l	(a7)+,a1		; view
	jsr	LoadView(a6)		;
	jsr	WaitTOF(a6)		;
	jsr	WaitTOF(a6)		;
	move.l	a6,a1			; parameter for CloseLibrary
	move.l	AbsExecBase.w,a6	;
	jsr	CloseLibrary(a6)	; close gfx library
	jsr	Permit(a6)		;

	bsr	dealloc			;
.exit	movem.l	(a7)+,a0-a6/d0-d7	;
	moveq	#0,d0			;
	rts				;

.gfx	dc.b	"graphics.library",0
	even


*------	VARS -------------------------------------------------------------*

	rsreset
v_tx		rs.w	1
v_doquit	rs.w	1	; signal quit
v_actors	rs.w	1

; copper list access addresses
v_cl_bplt	rs.l	1
v_cl_lspdmacon	rs.l	1
v_cl_bplm	rs.l	1
v_cl_bplb	rs.l	1
v_cl_colst	rs.l	1
v_cl_colsm	rs.l	1
v_cl_colsb	rs.l	1
v_cl_bplcon0	rs.l	1
v_cl_covercols	rs.l	1
v_cl_framecol	rs.l	1

v_matrix	rs.w	3*3	; 3D rotation matrix
v_angles	rs.w	3	; a b c
v_deltas	rs.w	3	; da db dc
v_vertices	rs.w	numvertices*3	; x (2d) y (2d) z (for hidden lines)

v_temp		rs.l	1
v_xoffset	rs.w	1

v_bpltoffset	rs.l	1
v_bplmoffset	rs.l	1
v_bplboffset	rs.l	1

v_textpointer	rs.l	1
v_precalccmd	rs.w	1

v_cmdspointer	rs.l	1
v_coverpointer	rs.l	1

v_covertiles	rs.l	12

v_gmplane1	rs.l	1
v_gmplane2	rs.l	1	
v_gmblinkdelay	rs.w	1
v_gmcount	rs.w	1

v_volume	rs.w	1

v_wait		rs.b	1
v_waitvalue	rs.b	1

	if numbers
v_frame		rs.w	1	; frame counter
	endif

	if numbers&testing
v_number2	rs.l	1
	endif

sizeofvars	rs.w	0

vars	ds.b	sizeofvars
	even


*------ PRINT NUMBER ----------------------------------------------------------*

; d0.l: number, d1.w: pos
	if numbers
printnumber
	move.l	b_bitplanes(pc),a0	;
	add.l	d1,a0			;

 	moveq	#8-1,d7			;
.loop	move.w	d0,d1			; number
	and.w	#$000f,d1		; mask digit out
	asl.w	#3,d1			; offset to font data
	lea	.digits(pc,d1.w),a1	;
	move.b	(a1)+,(a0)		; print digit
	move.b	(a1)+,pwidth(a0)	;
	move.b	(a1)+,2*pwidth(a0)	;
	move.b	(a1)+,3*pwidth(a0)	;
	move.b	(a1)+,4*pwidth(a0)	;
	asr.l	#4,d0			; next digit
	subq.w	#1,a0			; next x position
	dbf	d7,.loop		;
	rts				;

.digits	dc.b	%11111000	; 0
	dc.b	%10001000
	dc.b	%10001000
	dc.b	%10001000
	dc.b	%11111000
	ds.b	3

	dc.b	%00100000	; 1
	dc.b	%00100000
	dc.b	%00100000
	dc.b	%00100000
	dc.b	%00100000
	ds.b	3

	dc.b	%11111000	; 2
	dc.b	%00001000
	dc.b	%11111000
	dc.b	%10000000
	dc.b	%11111000
	ds.b	3

	dc.b	%11111000	; 3
	dc.b	%00001000
	dc.b	%11111000
	dc.b	%00001000
	dc.b	%11111000
	ds.b	3

	dc.b	%10001000	; 4
	dc.b	%10001000
	dc.b	%11111000
	dc.b	%00001000
	dc.b	%00001000
	ds.b	3

	dc.b	%11111000	; 5
	dc.b	%10000000
	dc.b	%11111000
	dc.b	%00001000
	dc.b	%11111000
	ds.b	3

	dc.b	%11111000	; 6
	dc.b	%10000000
	dc.b	%11111000
	dc.b	%10001000
	dc.b	%11111000
	ds.b	3

	dc.b	%11111000	; 7
	dc.b	%00001000
	dc.b	%00010000
	dc.b	%00100000
	dc.b	%00100000
	ds.b	3

	dc.b	%11111000	; 8
	dc.b	%10001000
	dc.b	%11111000
	dc.b	%10001000
	dc.b	%11111000
	ds.b	3

	dc.b	%11111000	; 9
	dc.b	%10001000
	dc.b	%11111000
	dc.b	%00001000
	dc.b	%11111000
	ds.b	3

	dc.b	%11111000	; A
	dc.b	%10001000
	dc.b	%11111000
	dc.b	%10001000
	dc.b	%10001000
	ds.b	3

	dc.b	%11110000	; B
	dc.b	%10001000
	dc.b	%11110000
	dc.b	%10001000
	dc.b	%11110000
	ds.b	3

	dc.b	%11111000	; C
	dc.b	%10000000
	dc.b	%10000000
	dc.b	%10000000
	dc.b	%11111000
	ds.b	3

	dc.b	%11110000	; D
	dc.b	%10001000
	dc.b	%10001000
	dc.b	%10001000
	dc.b	%11110000
	ds.b	3

	dc.b	%11111000	; E
	dc.b	%10000000
	dc.b	%11111000
	dc.b	%10000000
	dc.b	%11111000
	ds.b	3

	dc.b	%11111000	; F
	dc.b	%10000000
	dc.b	%11111000
	dc.b	%10000000
	dc.b	%10000000
	ds.b	3

	endif


*------	WAIT BLITTER ----------------------------------------------------------*

; http://amigadev.elowar.com/read/ADCD_2.1/Hardware_Manual_guide/node0123.html
waitblitter
	btst.b	#14-8,$02(a6)			; DMAB_BLTDONE = 14
.wait	btst.b	#14-8,$02(a6)			;
	bne	.wait				;
	rts					;


*------	WAIT RASTER -----------------------------------------------------------*

waitraster
	cmp.b	#1,$06(a6)			;
	bne	waitraster			;
	btst	#0,$05(a6)			;
	bne	waitraster			;
	rts					;


*------	PRECALCULATION --------------------------------------------------------*

precalc
.loop	move.w	v_precalccmd(a5),d0		;
	beq	.idle				;
	bsr	textwriter			;	
	clr.w	v_precalccmd(a5)		; signal/mark done
.idle	tst.w	v_doquit(a5)			;
	beq	.loop				;
	rts					;


*------	COPER -----------------------------------------------------------------*

coper	moveq	#$0010,d0			; delete coper request bit
	move.w	d0,$9c(a6)			;
	move.w	d0,$9c(a6)			;
	move.w	d0,$9c(a6)			;

	if timing
	move.w	#$0f00,$180(a6)			;
	endif
	
	lea	vars(pc),a5			;
	move.w	v_actors(a5),d7			; process actors
	btst	#actorlspplay,d7		;
	beq	.noplay				;
	bsr	lspplay				;

.noplay	movem.l	(a7)+,a0-a6/d0-d7		;
	rte					;


*------	IRQ3 ------------------------------------------------------------------*

irq3	movem.l	a0-a6/d0-d7,-(a7)		;
	lea	custom,a6			;
	move.w	$1e(a6),d0			; read interrupt request bits
	btst	#4,d0				;
	bne	coper				;

	lea	vars(pc),a5			;

	moveq	#$0030,d0			; delete vertb and coper request bit
	move.w	d0,$9c(a6)			; 
	move.w	d0,$9c(a6)			; 3 times? https://amycoders.org/tutorials/frametime.html
	move.w	d0,$9c(a6)			;
		
	move	#$2200,sr			; allow other (coper) level 3 interrupts

;	bsr	testdode			;
	bsr	play				;
	bsr	animate				;
	
	move.w	v_actors(a5),d7			; process actors
	btst	#actorgmblink,d7		; actor blink guru meditation border?
	beq	.act0				;
	subq.w	#1,v_gmblinkdelay(a5)		;
	bne	.act0				;
	move.l	b_gmclist(pc),a1		;
	add.w	#gmcolor2-gmclist+2,a1		;
	move.w	#$0111,d1			;
	move.w	#$0f20,d2			;
	cmp.w	(a1),d2				;
	beq	.gmcol				;
	move.w	d2,d1				;
.gmcol	move.w	d1,(a1)				;
	move.w	#gmblinkvalue,v_gmblinkdelay(a5);
	
.act0	move.w	v_actors(a5),d7			;
	btst	#actorending,d7			;
	beq	.actend				;
	subq.w	#1,v_volume(a5)			; fade out	
	move.w	v_volume(a5),d1			;
	bne	.setvol				;
	st	v_doquit+1(a5)			; quit -> +1 is important because of "seq"
	subq.w	#1<<actorending,v_actors(a5)	;
.setvol	asr.w	#1,d1				; fade out slowly
	move.w	d1,$a8(a6)			;
	move.w	d1,$b8(a6)			;
	move.w	d1,$c8(a6)			;
	move.w	d1,$d8(a6)			;
.actend

	if numbers&timing
	move.w	#$0440,$180(a6)			; dark yellow color indicates numbers consumption
	endif
	
	if numbers
	moveq	#0,d0				; number
	move.w	v_frame(a5),d0			;

;	moveq	#8-1,d1				; pos
	move.l	#2*psize+24-1,d1		; pos +8 vs 24
	bsr	printnumber			;

	moveq	#0,d0				; number
	
	if numbers&testing	
	move.l	v_number2(a5),d0		;
	endif

;	moveq	#1,d0				; value
	move.l	v_temp(a5),d0
	move.l	#(2*psize)+(10*pwidth)+24-1,d1	;
	bsr	printnumber			;
	addq.w	#1,v_frame(a5)			; advance frame number
	endif

	btst	#6,$bfe001			; left mouse button pressed?
	seq	v_doquit(a5)			; thanks MnemoTroN ;-)
	if timing
	move.w	#$0020,$180(a6)			; dark green color indicates free capacity
	endif

	movem.l	(a7)+,a0-a6/d0-d7		;
	rte					;
	

*------	ANIMATE ---------------------------------------------------------------*

animate	move.l	b_bitplanes(pc),d0		; init bitplane pointers
	add.l	v_bpltoffset(a5),d0		; top
	add.l	#tilewidth,v_bpltoffset(a5)	;
	cmp.l	#steps*tilewidth,v_bpltoffset(a5) ;
	bcs	.noreset1			;
	clr.l	v_bpltoffset(a5)		;
.noreset1
	move.l	v_cl_bplt(a5),a1		;
	moveq	#numplanes-1,d7			;
.initbplt
	move.w	d0,(a1)				;
	swap	d0				;
	move.w	d0,-4(a1)			;
	swap	d0				;
	addq.w	#8,a1				;
	add.l	#psize,d0			;
	dbf	d7,.initbplt			;

	move.l	b_bitplanes(pc),d0		; init bitplane pointers
	add.l	v_bplmoffset(a5),d0		; middle
	sub.l	#tilewidth,v_bplmoffset(a5)	;
	cmp.l	#-tilewidth,v_bplmoffset(a5)	;
	bne	.noreset2			;
	move.l	#(steps-1)*tilewidth,v_bplmoffset(a5) ;
.noreset2
	move.l	v_cl_bplm(a5),a1		;
	moveq	#numplanes-1,d7			;
.initbplm
	move.w	d0,(a1)				;
	swap	d0				;
	move.w	d0,-4(a1)			;
	swap	d0				;
	addq.w	#8,a1				;
	add.l	#psize,d0			;
	dbf	d7,.initbplm			;

	move.l	b_bitplanes(pc),d0		; init bitplane pointers
	add.l	v_bplboffset(a5),d0		; bottom
	add.l	#tilewidth,v_bplboffset(a5)	;
	cmp.l	#steps*tilewidth,v_bplboffset(a5) ;
	bcs	.noreset3			;
	clr.l	v_bplboffset(a5)		;
.noreset3
	move.l	v_cl_bplb(a5),a1		;
	add.l	#psize-pwidth,d0		; -pwidth because of mirror modulo
	moveq	#numplanes-1,d7			;
.initbplb
	move.w	d0,(a1)				;
	swap	d0				;
	move.w	d0,-4(a1)			;
	swap	d0				;
	addq.w	#8,a1				;
	add.l	#psize,d0			;
	dbf	d7,.initbplb			;
	rts					;


	rem

*------	CLEAR SCREEN ----------------------------------------------------------*

; a0 bitplane

cls	bsr	waitblitter		;
	move.l	a0,$54(a6)		; destination d
	move.l	#$01000000,$40(a6)	; bltcon0 bltcon1
	move.w	#pwidth-tilewidth,$66(a6)		; modulo d
	move.w	#pheight<<6+tilewidth>>1,$58(a6)	; bltsize and go
	rts				;


*------	TEST DODE -------------------------------------------------------------*

testdode
	move.l	b_bitplanes(pc),a0	;
	moveq	#numplanes-1,d7		;
.clear	bsr	cls			;
	add.l	#psize,a0		;
	dbf	d7,.clear		;

	bsr	mtx			;
	bsr	hiddenlines		;
	move.l	b_bitplanes(pc),a2	;
	add.l	#psize/2,a2		;
	move.l	a2,-(a7)		; used for stroke
	move.w	#tilewidth*8/2,v_tx(a5)	;
	bsr	draw			; draw object

	move.l	b_bitplanes(pc),a0	;
;	add.l	#psize-inviswidth-2,a0	; fills from bottom to top (why -2)
	add.l	#psize-pwidth+tilewidth-2,a0 ; fills from bottom to top (why -2)
	moveq	#numplanes-1,d7		;
.fill	bsr	fullfill		;
	add.l	#psize,a0		;
	dbf	d7,.fill		;

	move.l	(a7)+,a2		;
	bsr	stroke			;
	rts				;

	erem
	

*------	PLAYER ----------------------------------------------------------------*

play	tst.b	v_wait(a5)		;
	beq	.donotwait		;
	subq.b	#1,v_wait(a5)		;
	rts				;

.donotwait
	move.l	v_cmdspointer(a5),a0	;
.loop	move.b	(a0)+,d0		; cmd_wait (0)?
	bne	.1			;
	move.b	(a0)+,v_wait(a5)	; duration
	move.l	a0,v_cmdspointer(a5)	;
	rts				;

.1	subq.b	#1,d0			; cmd_showtile (1)?
	bne	.2			;
	moveq	#0,d1			;
	move.b	(a0)+,d1		;
	asl.w	#2,d1			; *4 (long word offset)
	lea	v_covertiles(a5),a1	; (move.l v_covertiles(a5,d1.w),a0 is
	move.l	a0,-(a7)		;    out of range)
	move.l	(a1,d1.w),a0		;
	bsr	cleartile		;
	move.l	(a7)+,a0		;
	bra	.loop			;

.2	subq.b	#1,d0			; cmd_colors (2)?
	bne	.3			;
	lea	colst(pc),a1		;
	move.l	v_cl_colst(a5),a2	;
	moveq	#numcols-1,d7		;
.colst	move.w	(a1)+,(a2)		;
	addq.w	#4,a2			;
	dbf	d7,.colst		;

	lea	colsm(pc),a1		;
	move.l	v_cl_colsm(a5),a2	;
	moveq	#numcols-1,d7		;
.colsm	move.w	(a1)+,(a2)		;
	addq.w	#4,a2			;
	dbf	d7,.colsm		;

	lea	colsb(pc),a1		;
	move.l	v_cl_colsb(a5),a2	;
	moveq	#numcols-1,d7		;
.colsb	move.w	(a1)+,(a2)		;
	addq.w	#4,a2			;
	dbf	d7,.colsb		;
	bra	.loop			;

.3	subq.b	#1,d0			; cmd_setclist (3)?
	bne	.4			;
	moveq	#0,d1			;
	move.b	(a0)+,d1		; var (offset) eg. v_cl_bplcon0
	move.b	(a0)+,d2		; value (upper byte)
	asl.w	#8,d2			;
	move.b	(a0)+,d2		; value (lower byte)
	move.l	(a5,d1.w),a1		; get "v_cl" address
	move.w	d2,(a1)			;
	bra	.loop			;
	
.4	subq.b	#1,d0			; cmd_whitecover (4)?
	bne	.5			;
	move.l	v_cl_covercols(a5),a1	;
	moveq	#16-1,d7		;
	move.w	#$0fff,d0		;
.white	move.w	d0,(a1)			;
	addq.w	#4,a1			;
	dbf	d7,.white		;
	bra	.loop			;

.5	subq.b	#1,d0			; cmd_preplogo (5)?
	bne	.6			;
	
	lea	logo(pc),a1		; logo w320 h40
	move.l	b_coverplane(pc),a2	;
	add.w	#76*viswidth,a2		; y position (golden ration)
	move.w	#(logoend-logo)/40-1,d7	; 240-40=200. 200/1.61=124. 76+124=200.
.clogo	
	rept 10
	move.l	(a1)+,(a2)+		;
	endr
	dbf	d7,.clogo		;
	bra	.loop			;

.6	subq.b	#1,d0			; cmd_framecol (6)?
	bne	.7			;
	move.l	v_cl_framecol(a5),a1	;
	move.w	#$0ccb,(a1)		;
	bra	.loop			;

.7	subq.b	#1,d0			; cmd_gmclist (7)?
	bne	.8			;
	move.l	b_gmclist(pc),$80(a6)	; activate guru meditation clist (next frame)
	bra	.loop			;
	
.8	subq.b	#1,d0			; cmd_gminit (8)?
	bne	.9			;
	move.w	#$0030,$180(a6)		; some color flickering
	bsr	waitblitter		;
	move.l	v_gmplane1(a5),$54(a6)	; destination d
	move.l	#$01000000,$40(a6)	; bltcon0 bltcon1
	move.w	#$0000,$66(a6)		; modulo d
	move.w	#(2*gmpheight)<<6+gmpwidth>>1,$58(a6) ; bltsize and go
	bsr	waitblitter		;
	move.w	#$00f0,$180(a6)		; some color flickering

borderweight	equ	3

	move.l	v_gmplane2(a5),a1	;
	move.l	a1,a2			;
	add.w	#(gmpheight-borderweight)*gmpwidth,a2 ;
	moveq	#-1,d1			; top/bottom border pattern
	moveq	#borderweight-1,d7	;
.bordertb	
	rept 20
	move.l	d1,(a1)+		;
	move.l	d1,(a2)+		;
	endr
	dbf	d7,.bordertb		;

	move.l	v_gmplane2(a5),a1	;
	add.w	#borderweight*gmpwidth,a1 ;
	move.l	a1,a2			;
	add.w	#gmpwidth-1,a2		;

	move.b	#%11111100,d1		; left border pattern (6 px)
	move.b	#%00111111,d2		; right border pattern (6 px)
	move.w	#gmpwidth,d3		;
	moveq	#gmpheight-(2*borderweight)-1,d7 ;
.borderlr
	move.b	d1,(a1)			;
	move.b	d2,(a2)			;
	add.w	d3,a1			;
	add.w	d3,a2			;
	dbf	d7,.borderlr		;	
	move.w	#$0060,$180(a6)		; some color flickering
	bra	.loop			;

.9	subq.b	#1,d0			; cmd_gmwrite (9)?
	bne	.10			;
	tst.w	v_precalccmd(a5)	;
	bne	.busy			;
	st	v_precalccmd(a5)	;
.busy	bra	.loop			;

shiftwidth	equ	54

.10	subq.b	#1,d0			; cmd_royalwait (10)?
	bne	.11			;
	move.b	v_waitvalue(a5),v_wait(a5) ; duration
	move.l	a0,v_cmdspointer(a5)	;
	rts				;

.11	subq.b	#1,d0			; cmd_start (11)?
	bne	.12			;
	moveq	#0,d1			;
	move.b	(a0)+,d1		; actor
	move.w	v_actors(a5),d2		;
	bset	d1,d2			;
	move.w	d2,v_actors(a5)		;
	bra	.loop			;

.12	subq.b	#1,d0			; cmd_rewind (12)
	bne	.13			;
	lea	playrewind(pc),a0	;
	bra	.loop			;

.13	subq.b	#1,d0			; cmd_gmpokecl (13)
	bne	.14			;
	moveq	#0,d2			;
	move.b	(a0)+,d2		; gmclist offset
	moveq	#0,d1			; value (e.g. color)
	move.b	(a0)+,d1		;
	asl.w	#8,d1			; merge bytes to word
	move.b	(a0)+,d1		;
	move.l	b_gmclist(pc),a1	;
	move.w	d1,(a1,d2.w)		; write to gmclist
	bra	.loop			;

.14	subq.b	#1,d0			; cmd_stop (14)?
	bne	.15			;
	moveq	#0,d1			;
	move.b	(a0)+,d1		; actor
	move.w	v_actors(a5),d2		;
	bclr	d1,d2			;
	move.w	d2,v_actors(a5)		;
	bra	.loop			;

.15	subq.b	#1,d0			; cmd_setvolume (15)?
	bne	.16			;
	moveq	#0,d1			; volume
	move.b	(a0)+,d1		;
	move.w	d1,$a8(a6)		;
	move.w	d1,$b8(a6)		;
	move.w	d1,$c8(a6)		;
	move.w	d1,$d8(a6)		;
.16	bra	.loop			;

; commands
cmd_wait	equ 	0
cmd_showtile	equ	1
cmd_colors	equ	2
cmd_setclist	equ	3
cmd_whitecover	equ	4
cmd_preplogo	equ	5
cmd_framecol	equ	6
cmd_gmclist	equ	7
cmd_gminit	equ	8
cmd_gmwrite	equ	9
cmd_royalwait	equ	10
cmd_start	equ	11
cmd_rewind	equ	12
cmd_gmpokecl	equ	13	; poke gm clist
cmd_stop	equ	14
cmd_setvolume	equ	15

; actor bits
actorgmblink	equ	0
actorlspplay	equ	1
actorending	equ	2

tilewait	equ	47
playcmds
	dc.b	cmd_wait,5*tilewait+4
		
	dc.b	cmd_showtile,3, cmd_wait,tilewait
	dc.b	cmd_showtile,9, cmd_wait,tilewait
	dc.b	cmd_showtile,0, cmd_wait,tilewait
	dc.b	cmd_showtile,5, cmd_wait,tilewait
	dc.b	cmd_showtile,1, cmd_wait,tilewait
	dc.b	cmd_showtile,11, cmd_wait,tilewait
	dc.b	cmd_showtile,7, cmd_wait,tilewait
	dc.b	cmd_showtile,8, cmd_wait,tilewait
	dc.b	cmd_showtile,2, cmd_wait,tilewait
	dc.b	cmd_showtile,10, cmd_wait,tilewait
	dc.b	cmd_showtile,4, cmd_wait,tilewait
	dc.b	cmd_showtile,6
	
	dc.b	cmd_wait,250
	dc.b	cmd_wait,135

	dc.b	cmd_framecol
	dc.b	cmd_colors
	dc.b	cmd_setclist,v_cl_bplcon0,numplanes<<4+2,$00
	dc.b	cmd_wait,2

	dc.b	cmd_whitecover, cmd_wait,2

	dc.b	cmd_preplogo, cmd_wait,250
	dc.b	cmd_wait,110
	dc.b	cmd_setclist,v_cl_bplcon0,(numplanes+numcoverplanes)<<4+2,$00
	dc.b	cmd_wait,250
	dc.b	cmd_wait,240

	dc.b	cmd_stop,actorlspplay
	dc.b	cmd_wait,1
	dc.b	cmd_setvolume,0
	dc.b	cmd_wait,1

	dc.b	cmd_gminit	
	dc.b	cmd_wait,1

	dc.b	cmd_gmpokecl,gmcolor0-gmclist+2,$08,$88
	dc.b	cmd_gmpokecl,gmcolor1-gmclist+2,$08,$88
	dc.b	cmd_gmpokecl,gmcolor1b-gmclist+2,$08,$88
	dc.b	cmd_gmpokecl,gmcolor2-gmclist+2,$08,$88
	dc.b	cmd_gmclist
	dc.b	cmd_wait,1

	dc.b	cmd_gmwrite
	dc.b	cmd_wait,2

	dc.b	cmd_gmwrite
	dc.b	cmd_wait,2

	dc.b	cmd_gmwrite
	dc.b	cmd_wait,49-3-3

	dc.b	cmd_gmpokecl,gmcolor0-gmclist+2,$0f,$ff
	dc.b	cmd_gmpokecl,gmcolor1-gmclist+2,$0f,$ff
	dc.b	cmd_gmpokecl,gmcolor1b-gmclist+2,$0f,$ff
	dc.b	cmd_gmpokecl,gmcolor2-gmclist+2,$0f,$ff
	dc.b	cmd_wait,59

	dc.b	cmd_gmpokecl,gmcolor0-gmclist+2,$01,$11
	dc.b	cmd_gmpokecl,gmcolor1-gmclist+2,$0f,$20
	dc.b	cmd_gmpokecl,gmcolor1b-gmclist+2,$0f,$20
	dc.b	cmd_gmpokecl,gmcolor2-gmclist+2,$0f,$20
	dc.b	cmd_start,actorgmblink
	dc.b	cmd_wait,190
	dc.b	cmd_wait,250

	dc.b	cmd_gmpokecl,gmcolor1b-gmclist+2,$0e,$20, cmd_wait,0
	dc.b	cmd_gmpokecl,gmcolor1b-gmclist+2,$0d,$20, cmd_wait,0
	dc.b	cmd_gmpokecl,gmcolor1b-gmclist+2,$0c,$20, cmd_wait,0
	dc.b	cmd_gmpokecl,gmcolor1b-gmclist+2,$0b,$20, cmd_wait,0
	dc.b	cmd_gmpokecl,gmcolor1b-gmclist+2,$0a,$20, cmd_wait,0
	dc.b	cmd_gmpokecl,gmcolor1b-gmclist+2,$09,$20, cmd_wait,0
	dc.b	cmd_gmpokecl,gmcolor1b-gmclist+2,$08,$10, cmd_wait,0
	dc.b	cmd_gmpokecl,gmcolor1b-gmclist+2,$07,$10, cmd_wait,0
	dc.b	cmd_gmpokecl,gmcolor1b-gmclist+2,$06,$10, cmd_wait,0
	dc.b	cmd_gmpokecl,gmcolor1b-gmclist+2,$05,$10, cmd_wait,0
	dc.b	cmd_gmpokecl,gmcolor1b-gmclist+2,$04,$10, cmd_wait,0
	dc.b	cmd_gmpokecl,gmcolor1b-gmclist+2,$03,$10, cmd_wait,0
	dc.b	cmd_gmpokecl,gmcolor1b-gmclist+2,$02,$10, cmd_wait,0
	dc.b	cmd_gmpokecl,gmcolor1b-gmclist+2,$01,$11, cmd_wait,30

	dc.b	cmd_setvolume,64
	dc.b	cmd_wait,1
	dc.b	cmd_start,actorlspplay
	dc.b	cmd_wait,1
playrewind
	dc.b	cmd_gmpokecl,gmcolor1b-gmclist+2,$01,$11
	dc.b	cmd_wait,0
	dc.b	cmd_gmwrite
	dc.b	cmd_wait,10 ; 10 frames should be enough time to write

	dc.b	cmd_gmpokecl,gmcolor1b-gmclist+2,$02,$10
	dc.b	cmd_wait,0
	dc.b	cmd_gmpokecl,gmcolor1b-gmclist+2,$04,$10
	dc.b	cmd_wait,0
	dc.b	cmd_gmpokecl,gmcolor1b-gmclist+2,$06,$10
	dc.b	cmd_wait,0
	dc.b	cmd_gmpokecl,gmcolor1b-gmclist+2,$08,$10
	dc.b	cmd_wait,0
	dc.b	cmd_gmpokecl,gmcolor1b-gmclist+2,$0a,$20
	dc.b	cmd_wait,0
	dc.b	cmd_gmpokecl,gmcolor1b-gmclist+2,$0c,$20
	dc.b	cmd_wait,0
	dc.b	cmd_gmpokecl,gmcolor1b-gmclist+2,$0f,$20

	dc.b	cmd_royalwait
	dc.b	cmd_gmpokecl,gmcolor1b-gmclist+2,$0c,$20
	dc.b	cmd_wait,0
	dc.b	cmd_gmpokecl,gmcolor1b-gmclist+2,$0a,$20
	dc.b	cmd_wait,0	
	dc.b	cmd_gmpokecl,gmcolor1b-gmclist+2,$08,$10
	dc.b	cmd_wait,0
	dc.b	cmd_gmpokecl,gmcolor1b-gmclist+2,$06,$10
	dc.b	cmd_wait,0
	dc.b	cmd_gmpokecl,gmcolor1b-gmclist+2,$04,$10
	dc.b	cmd_wait,0
	dc.b	cmd_gmpokecl,gmcolor1b-gmclist+2,$02,$10
	dc.b	cmd_wait,0
	
	dc.b	cmd_rewind
	even


*------	GURU MEDITATION COPPER INSTRUCTION LIST -------------------------------*

gmclist	dc.w	$2b01,$fffe
gmcolor0
	dc.w	$0180,$0111	; background
gmcolor1
	dc.w	$0182,$0111	; text color 1st line
gmcolor2
	dc.w	$0184,$0111	; border color
	dc.w	$0186,$0fff	; not used

	dc.w	$008e,$0581
	dc.w	$0100,$0200
	dc.w	$0104,$0024
	dc.w	$0090,$40c1
	dc.w	$0092,$003c
	dc.w	$0094,$00d4
	dc.w	$0102,$0000
	dc.w	$0108,$0000
	dc.w	$010a,$0000
	
gmbpl1	dc.w	$00e0,0
	dc.w	$00e2,0
	
gmbpl2	dc.w	$00e4,0
	dc.w	$00e6,0
	
	dc.w	$2c01,$fffe
	dc.w	$0100,$a200	; 2 hires bitplanes
	
	dc.w	$4001,$fffe
gmcolor1b
	dc.w	$0182,$0111	; text color 2nd line
	
	dc.w	$5301,$fffe	; changed from $54 to $53
	dc.w	$0100,$0200
	
	dc.w	$6007,$fffe	; LSP
	dc.w	$009c,$8010	; trigger coper interrupt
	dc.w	$6b07,$fffe ; $60+11=$6b (11 lines later)
gmlspdmacon
	dc.w	$0096,$8000

	dc.w	$ffff,$fffe
gmclistend


*------	COPPER INSTRUCTION LIST -----------------------------------------------*

clist	dc.w	$1007,$fffe	; chance for player to alter copper list in time

	dc.w	$008e,$3481 ; DIWSTRT $2c+8=$34
	dc.w	$0090,$24c1 ; DIWSTOP $34+240=$124

	dc.w	$0092,$0038 ; DDFSTRT
	dc.w	$0094,$00d0 ; DDFSTOP

	dc.w	-2,v_cl_bplcon0,2 ; store address
	dc.w	$0100,(numplanes+numcoverplanes)<<12+$0200
	dc.w	$0102,$0000
	dc.w	$0104,$0000 ; sprites have no priority

	dc.w	$0108,inviswidth
	dc.w	$010a,inviswidth

; sprites off
	dc.w	$0144,$0000,$0146,$0000 ; data 0
	dc.w	$014c,$0000,$014e,$0000 ; data 1
	dc.w	$0154,$0000,$0156,$0000 ; data 2
	dc.w	$015c,$0000,$015e,$0000 ; data 3
	dc.w	$0164,$0000,$0166,$0000 ; data 4
	dc.w	$016c,$0000,$016e,$0000 ; data 5
	dc.w	$0174,$0000,$0176,$0000	; data 6
	dc.w	$017c,$0000,$017e,$0000 ; data 7

	dc.w	-2,v_cl_covercols,2
	dc.w	$01a0,$0000,$01a2,$0000,$01a4,$0000,$01a6,$0000
	dc.w	$01a8,$0000,$01aa,$0000,$01ac,$0000,$01ae,$0000
	dc.w	$01b0,$0000,$01b2,$0000,$01b4,$0000,$01b6,$0000
	dc.w	$01b8,$0000,$01ba,$0000,$01bc,$0000,$01be,$0000

	dc.w	$0180,$0000
	dc.w	-2,v_cl_framecol,2
	dc.w	$019e,$0ddd	; color 15

	dc.w	-2,v_cl_colst,2
	dc.w	$0182,$0000
	dc.w	$0184,$0000
	dc.w	$0186,$0000
	dc.w	$0188,$0000
	dc.w	$018a,$0000
	dc.w	$018c,$0000
	dc.w	$018e,$0000
	dc.w	$0190,$0000
	dc.w	$0192,$0000
	dc.w	$0194,$0000
	dc.w	$0196,$0000
	dc.w	$0198,$0000
	dc.w	$019a,$0000
	dc.w	$019c,$0000
	
;	dc.w	$019c,$0f00	; testing
	
	dc.w	$3407,$fffe
	dc.w	-4,0
	dc.w	-2,v_cl_bplt,6 ; store address
	dc.w	$00e0,0,$00e2,0
	dc.w	$00e4,0,$00e6,0
	dc.w	$00e8,0,$00ea,0
	dc.w	$00ec,0,$00ee,0

	dc.w	-3,$35,$3f ; GENERATE

	dc.w	$4007,$fffe
	dc.w	-4,0
	dc.w	$009c,$8010	; trigger coper interrupt
	
	dc.w	-3,$41,$4a ; GENERATE

	dc.w	$4b07,$fffe ; $40+11=$4b (11 lines later)
	dc.w	-4,0
	dc.w	-2,v_cl_lspdmacon,3 ; store address
	dc.w	$0096,$8000
	
	dc.w	-3,$4c,$83 ; GENERATE

	dc.w	$8407,$fffe ; $34+80=$84
	dc.w	-4,0
	dc.w	-2,v_cl_bplm,6 ; store address
	dc.w	$00e0,0,$00e2,0
	dc.w	$00e4,0,$00e6,0
	dc.w	$00e8,0,$00ea,0
	dc.w	$00ec,0,$00ee,0

	dc.w	-2,v_cl_colsm,2
	dc.w	$0182,$0000
	dc.w	$0184,$0000
	dc.w	$0186,$0000
	dc.w	$0188,$0000
	dc.w	$018a,$0000
	dc.w	$018c,$0000
	dc.w	$018e,$0000
	dc.w	$0190,$0000
	dc.w	$0192,$0000
	dc.w	$0194,$0000
	dc.w	$0196,$0000
	dc.w	$0198,$0000
	dc.w	$019a,$0000
	dc.w	$019c,$0000

	dc.w	-3,$85,$d3 ; GENERATE

	dc.w	$d407,$fffe ; $34+80+80=$d4
	dc.w	-4,0
	dc.w	-2,v_cl_bplb,6 ; store address
	dc.w	$00e0,0,$00e2,0
	dc.w	$00e4,0,$00e6,0
	dc.w	$00e8,0,$00ea,0
	dc.w	$00ec,0,$00ee,0

	dc.w	$0108,-pwidth-viswidth ; mirrored
	dc.w	$010a,-pwidth-viswidth ; mirrored

	dc.w	-2,v_cl_colsb,2
	dc.w	$0182,$0000
	dc.w	$0184,$0000
	dc.w	$0186,$0000
	dc.w	$0188,$0000
	dc.w	$018a,$0000
	dc.w	$018c,$0000
	dc.w	$018e,$0000
	dc.w	$0190,$0000
	dc.w	$0192,$0000
	dc.w	$0194,$0000
	dc.w	$0196,$0000
	dc.w	$0198,$0000
	dc.w	$019a,$0000
	dc.w	$019c,$0000

	dc.w	-3,$d5,$123 ; GENERATE $123: $d4+80=$(1)24

	dc.w	$2407,$fffe
	dc.w	$0100,numplanes<<12+$0200

	dc.w	$ffff,$fffe
clistend

numcols	equ	(colsm-colst)/2

colst	dc.w	$0210,$0321,$0431,$0542
	dc.w	$0652,$0763,$0873,$0984
	dc.w	$0a94,$0ba5,$0cb5,$0dc6
	dc.w	$0ed6,$0fe6

colsm	dc.w	$0100,$0200,$0310,$0420
	dc.w	$0531,$0641,$0752,$0862
	dc.w	$0973,$0a83,$0b94,$0ca4
	dc.w	$0db5,$0ed6

colsb	dc.w	$0000,$0100,$0100,$0200
	dc.w	$0200,$0310,$0420,$0531
	dc.w	$0641,$0752,$0862,$0973
	dc.w	$0a83,$0b94


*------	HIDDEN LINES ----------------------------------------------------------*

hiddenlines
	lea	v_vertices(a5),a0	;
	lea	edges+4(pc),a3		; color
	lea	faces(pc),a4		;

	moveq	#12-1,d7		;
.loop	movem.w	(a4)+,d4/d5/d6		;
	movem.w	(a0,d4.w),d0/d1		;
	sub.w	(a0,d5.w),d0		; vx=p2x-p1x
	sub.w	2(a0,d5.w),d1		; vy=p2y-p1y
	movem.w	(a0,d6.w),d2/d3		;
	sub.w	(a0,d5.w),d2		; wx=p3x-p1x
	sub.w	2(a0,d5.w),d3		; wy=p3y-p1y
	muls	d0,d3			; vx*wy
	muls	d1,d2			; vy*wx
	sub.l	d2,d3			; z=vx*wy-vy*wx
	bpl	.visible		; face hidden?
	add.w	#5*2,a4			; skip 5 words
	bra	.next			;	

.visible
	rem
	move.l	v_temp(a5),d2		;
	cmp.l	d2,d3			;
	blt	.sk			;
	move.l	d3,v_temp(a5)		; $4f4
.sk
	erem
	
	divs	#96,d3			; $4f4 / 13 = 97
	addq.w	#1,d3			; 
;	move.w	#14,d3			;
	cmp.w	#14,d3			;
	ble	.c			;
	moveq	#14,d3			;
.c	asl.w	#8,d3			; *256 for btst
	
	moveq	#5-1,d0			;
.mark	move.w	(a4)+,d2		; edge
	eor.w	d3,(a3,d2.w)		; mark used egde by color
	st	2(a3,d2.w)		; stroke/frame edge
	dbf	d0,.mark		; mark all edges of face

.next	dbf	d7,.loop		;
	rts				;


*------	MATRIX ----------------------------------------------------------------*

mtx	lea	sin(pc),a0		;
	lea	cos(pc),a1		;
	lea	v_angles(a5),a2		;
	lea	v_deltas(a5),a3		;

;	move.w	#720/5*2,d7		; = 144 * 2
	move.w	#720,d7			;

	move.w	(a2),d6			;
	cmp.w	d7,d6			;
	bcs	.ca			;
	sub.w	d7,d6			;
.ca	move.w	(a0,d6.w),d0		; sin a
	move.w	(a1,d6.w),d3		; cos a
	add.w	(a3)+,d6		; step a
	move.w	d6,(a2)+		;	

	move.w	(a2),d6			;
	cmp.w	d7,d6			;
	bcs	.cb			;
	sub.w	d7,d6			;
.cb	move.w	(a0,d6.w),d1		; sin b
	move.w	(a1,d6.w),d4		; cos b
	add.w	(a3)+,d6		; step b
	move.w	d6,(a2)+		;

	move.w	(a2),d6			;
	cmp.w	d7,d6			;
	bcs	.cc			;
	sub.w	d7,d6			;
.cc	move.w	(a0,d6.w),d2		; sin c
	move.w	(a1,d6.w),d5		; cos c
	add.w	(a3),d6			; step c
	move.w	d6,(a2)			;

	lea	v_matrix(a5),a0		;
	
	move.w	d0,d6			;
	muls	d1,d6			;
	asr.l	#8,d6			;
	move.w	d6,a1			;
	move.w	d3,d7			;
	muls	d2,d7			;
	asr.l	#8,d7			;
	move.w	d7,a2			;
	muls	d5,d6			;
	asr.l	#8,d6			;
	sub.w	d7,d6			;
	move.w	d6,$0006(a0)		;
	move.w	d3,d7			;
	muls	d5,d7			;
	asr.l	#8,d7			;
	move.w	d7,a3			;
	move.w	a1,d6			;
	muls	d2,d6			;
	asr.l	#8,d6			;
	add.w	d7,d6			;
	move.w	d6,$0008(a0)		;
	move.w	a3,d6			;
	muls	d1,d6			;
	asr.l	#8,d6			;
	move.w	d0,d7			;
	muls	d2,d7			;
	asr.l	#8,d7			;
	add.w	d7,d6			;
	move.w	d6,$000c(a0)		;
	move.w	a2,d6			;
	muls	d1,d6			;
	asr.l	#8,d6			;
	move.w	d0,d7			;
	muls	d5,d7			;
	asr.l	#8,d7			;
	sub.w	d7,d6			;
	move.w	d6,$000e(a0)		;
	muls	d4,d5			;
	asr.l	#8,d5			;
	move.w	d5,(a0)			;
	muls	d4,d2			;
	asr.l	#8,d2			;
	move.w	d2,$0002(a0)		;
	muls	d4,d0			;
	asr.l	#8,d0			;
	move.w	d0,$000a(a0)		;
	muls	d4,d3			;
	asr.l	#8,d3			;
	move.w	d3,$0010(a0)		;
	neg.w	d1			;
	move.w	d1,$0004(a0)		;

	lea	vertices(pc),a3		;
	lea	v_matrix(a5),a0		;
	lea	v_vertices(a5),a2	; vertices'
	move.w	#$0900,d3		;
	move.w	(a3)+,d7		;
.loop	move.l	a0,a1			;
	movem.w	(a3)+,d0-d2		; x,y,z
	move.w	d1,d4			;
	muls	(a1)+,d4		;
	move.w	d2,d5			;
	muls	(a1)+,d5		;
	add.l	d5,d4			;
	move.w	d0,d5			;
	muls	(a1)+,d5		;
	add.l	d5,d4			;
	move.w	d1,d5			;
	muls	(a1)+,d5		;
	move.w	d2,d6			;
	muls	(a1)+,d6		;
	add.l	d6,d5			;
	move.w	d0,d6			;
	muls	(a1)+,d6		;
	add.l	d6,d5			;
	muls	(a1)+,d1		;
	muls	(a1)+,d2		;
	muls	(a1)+,d0		;
	add.l	d1,d0			;
	add.l	d2,d0			;
	asr.l	#8,d0			;
	move.w	d0,d6			;
	asr.w	#3,d6			; fix offset for (ex)colint
	add.w	d3,d0			; distance
	divs	d0,d4			;
	divs	d0,d5			;
	move.w	d4,(a2)+		; x (2d)
	move.w	d5,(a2)+		; y (2d)
	move.w	d6,(a2)+		; z
	dbf	d7,.loop		;
	rts				;


*------	DRAW FILLED FACE ------------------------------------------------------*

; a2 = bitplane
draw	lea	$52(a6),a6		;
	lea	v_vertices(a5),a1	;
	lea	edges(pc),a3		;
.loop	move.w	(a3)+,d2		;
	bge	.notdone		; done?
	lea	-$52(a6),a6		; a6 becomes $dff000 again
	rts				;
.notdone
	move.w	(a3)+,d4		;
	tst.w	(a3)			; colorless line?
	bne	.hascolor		;
	addq.w	#4,a3			; skip color and frame status
	bra	.loop			;

.hascolor
	movem.w	(a1,d2.w),d0/d1		; x1,y1
	movem.w	(a1,d4.w),d2/d3		; x2,y2
	
	add.w	v_tx(a5),d0		; very stupid. who cares.
	add.w	v_tx(a5),d2		;
	
	cmp.w	d3,d1			; compare y
	bgt	.noswap			;
	exg	d0,d2			; swap -> second larger
	exg	d1,d3			;
.noswap	moveq	#4,d7			; clears upper word too
	move.l	d7,a4			; octant code
	move.w	d0,d7			; x1
	and.w	#$000f,d7		;
	ror.w	#4,d7			; startbit of line
	or.w	#$0b4a,d7		; $0bca=or
	swap	d7			;
	sub.w	d0,d2			; x2-x1
	bpl	.rightw			;
	addq.w	#1,a4			;
	neg.w	d2			;
.rightw	sub.w	d1,d3			; y2-y1
	bpl	.upw			; upward
	addq.w	#2,a4			; =bset #1
	neg.w	d3			; d3=y
.upw	move.w	d3,d6			;
	sub.w	d2,d6			; d6=y-x
	bmi	.nsteep			; steepness <1
	exg	d2,d3			; swap x and y
	subq.w	#4,a4			; =bclr #2
	neg.w	d6			;
.nsteep	lsl.w	#6,d2			; 64 = 1<<6
	add.w	#64+2,d2		; +2 required (width)
	move.w	d6,d4			; d2=y-x
	add.w	d3,d4			; d2=2y-x
	bpl	.nosign			;
	addq.w	#8,a4			; sign of 2y-x (in oct code)
.nosign	lsl.w	#2,d3			; d3=4y
	lsl.w	#2,d6			; d6=4y-4x
	swap	d3			;
	move.w	d6,d3			;
	clr.w	d7			;
	move.b	.octs(pc,a4.w),d7	; read octant from table

	muls	#pwidth,d1		;
	move.l	a2,a4			; bitplane
	add.l	d1,a4			;
	moveq	#7,d1			;
	sub.w	d0,d1			; see bchg below
	asr.w	#3,d0			; allow neg x coords
	add.w	d0,a4			;

	btst	#14-8,$02-$52(a6)	;
.wblit1	btst	#14-8,$02-$52(a6)	;
	bne	.wblit1			;
	move.w	#pwidth,$60-$52(a6)	;
	move.w	#pwidth,$66-$52(a6)	;
	move.l	#$ffff8000,$72-$52(a6)	; texture data/index
	move.w	#$8000,$44-$52(a6)	; first word mask

	moveq	#0,d0			; bitplane index
.loopbitplanes
	btst	d0,(a3)			; draw into this bitplane?
	beq	.skip			;
	move.l	a6,a0			;
	btst	#14-8,$02-$52(a6)	;
.wblit2	btst	#14-8,$02-$52(a6)	;
	bne	.wblit2			;

	move.l	d3,$62-$52(a6)		; 4y,4y-4x	BLTB/AMOD
	move.l	d7,$40-$52(a6)		;		BLTCON0,1
	move.l	a4,$48-$52(a6)		;
	move.w	d4,(a0)+		; 2y-x		BLTAPTL
	move.l	a4,(a0)+		; start address
	bchg	d1,(a4)			; flip pixel (important for filling)
	move.w	d2,(a0)			; start
.skip	add.l	#psize,a4		;
	addq.w	#1,d0			; next bitplane
	cmp.w	#numplanes,d0		;
	bne	.loopbitplanes		;

	clr.w	(a3)+			; clear color for next iteration
	addq.w	#2,a3			; skip frame status
	bra	.loop			;

; +2 = SING Single bit per horizontal line for use with subsequent area fill
.octs	dc.b	0*4+1+2, 2*4+1+2, 1*4+1+2, 3*4+1+2
	dc.b	4*4+1+2, 5*4+1+2, 6*4+1+2, 7*4+1+2
	dc.b	0*4+65+2, 2*4+65+2, 1*4+65+2, 3*4+65+2
	dc.b	4*4+65+2, 5*4+65+2, 6*4+65+2, 7*4+65+2


*------	DRAW STROKED EDGES ----------------------------------------------------*

; a2 = bitplane
stroke	lea	$52(a6),a6		;
	lea	v_vertices(a5),a1	;
	lea	edges(pc),a3		;
.loop	move.w	(a3)+,d2		;
	bge	.notdone		; done?
	lea	-$52(a6),a6		; a6 becomes $dff000 again
	rts				;
.notdone
	move.w	(a3)+,d4		;
	addq.w	#2,a3			; skip color
	tst.w	(a3)			; stroke?
	bne	.hascolor		;
	addq.w	#2,a3			; skip stroke
	bra	.loop			;

.hascolor
	movem.w	(a1,d2.w),d0/d1		; x1,y1
	movem.w	(a1,d4.w),d2/d3		; x2,y2
	
	add.w	v_tx(a5),d0		; stupid. who cares.
	add.w	v_tx(a5),d2		;

	cmp.w	d3,d1			; compare y
	bgt	.noswap			; (draw the same way as the "filled" lines)
	exg	d0,d2			; swap -> second larger
	exg	d1,d3			;
.noswap	moveq	#4,d7			; clears upper word too
	move.l	d7,a4			; octant code
	move.w	d0,d7			; x1
	and.w	#$000f,d7		;
	ror.w	#4,d7			; startbit of line
	or.w	#$0bca,d7		; vs $0b4a (fill)  $0bca=or
	swap	d7			;
	sub.w	d0,d2			; x2-x1
	bpl	.rightw			;
	addq.w	#1,a4			;
	neg.w	d2			;
.rightw	sub.w	d1,d3			; y2-y1
	bpl	.upw			; upward
	addq.w	#2,a4			; =bset #1
	neg.w	d3			; d3=y
.upw	move.w	d3,d6			;
	sub.w	d2,d6			; d6=y-x
	bmi	.nsteep			; steepness <1
	exg	d2,d3			; swap x and y
	subq.w	#4,a4			; =bclr #2
	neg.w	d6			;
.nsteep	lsl.w	#6,d2			; 64 = 1<<6
	add.w	#64+2,d2		; +2 required (width)
	move.w	d6,d4			; d2=y-x
	add.w	d3,d4			; d2=2y-x
	bpl	.nosign			;
	addq.w	#8,a4			; sign of 2y-x (in oct code)
.nosign	lsl.w	#2,d3			; d3=4y
	lsl.w	#2,d6			; d6=4y-4x
	swap	d3			;
	move.w	d6,d3			;
	clr.w	d7			;
	move.b	.octs(pc,a4.w),d7	; read octant from table

	muls	#pwidth,d1		; not time critical - no lut
	move.l	a2,a4			; bitplane
	add.l	d1,a4			;
	asr.w	#3,d0			; allow neg x coords
	add.w	d0,a4			;

	btst	#14-8,$02-$52(a6)	;
.wblit1	btst	#14-8,$02-$52(a6)	;
	bne	.wblit1			;
	move.w	#pwidth,$60-$52(a6)	;
	move.w	#pwidth,$66-$52(a6)	;
	move.l	#$ffff8000,$72-$52(a6)	; texture data/index
	move.w	#$8000,$44-$52(a6)	; first word mask

	moveq	#numplanes-1,d0		;
.loopbitplanes
	move.l	a6,a0			;
	btst	#14-8,$02-$52(a6)	;
.wblit2	btst	#14-8,$02-$52(a6)	;
	bne	.wblit2			;

	move.l	d3,$62-$52(a6)		; 4y,4y-4x	BLTB/AMOD
	move.l	d7,$40-$52(a6)		;		BLTCON0,1
	move.l	a4,$48-$52(a6)		;
	move.w	d4,(a0)+		; 2y-x		BLTAPTL
	move.l	a4,(a0)+		; start address
	move.w	d2,(a0)			; start

	sub.l	#pwidth,a4		;
	bra	.jump			; stupid.

.octs	dc.b	0*4+1, 2*4+1, 1*4+1, 3*4+1
	dc.b	4*4+1, 5*4+1, 6*4+1, 7*4+1
	dc.b	0*4+65, 2*4+65, 1*4+65, 3*4+65
	dc.b	4*4+65, 5*4+65, 6*4+65, 7*4+65

.jump	move.l	a6,a0			;
	btst	#14-8,$02-$52(a6)	;
.wblit3	btst	#14-8,$02-$52(a6)	;
	bne	.wblit3			;

	move.l	d3,$62-$52(a6)		; 4y,4y-4x	BLTB/AMOD
	move.l	d7,$40-$52(a6)		;		BLTCON0,1
	move.l	a4,$48-$52(a6)		;
	move.w	d4,(a0)+		; 2y-x		BLTAPTL
	move.l	a4,(a0)+		; start address
	move.w	d2,(a0)			; start
	add.l	#psize+pwidth,a4	;

	dbf	d0,.loopbitplanes	;

	clr.w	(a3)+			; clear stroke for next iteration
;	addq.w	#2,a3			; skip frame status
	bra	.loop			;


*------	HEY FILL --------------------------------------------------------------*

fullfill
	move.l	#pwidth-tilewidth,d0	;
	moveq	#-1,d1			;
	move.w	#(pheight-2)<<6+tilewidth>>1,d2 ; no need to fill completely (safety)
	bsr	waitblitter		;
	move.l	a0,$50(a6)		; source A
	move.l	a0,$54(a6)		; destination D
	move.w	d0,$64(a6)		; modulo A
	move.w	d0,$66(a6)		; modulo D
	move.l	#$09f0000a,$40(a6)	; 09f00012 exlusive   09f0000a inclusive
	move.l	d1,$44(a6)		; first/last word mask
	move.w	d2,$58(a6)		; bltsize and go
	rts				;


*------	CLEAR TILE ------------------------------------------------------------*

; a0 bitplane
cleartile
	bsr	waitblitter		;
	move.l	a0,$54(a6)		; destination d
	move.l	#$01000000,$40(a6)	; bltcon0 bltcon1
	move.w	#viswidth-tilewidth,$66(a6) ; modulo d
	move.w	#pheight<<6+tilewidth>>1,$58(a6) ; bltsize and go
	rts				;


*------ TEXT WIDTH ------------------------------------------------------------*

; input a0: text
; output d0, d4
textwidth
	moveq	#0,d2			; sum up width
.loop	moveq	#0,d0			;
	move.b	(a0)+,d0		;
	ble	.done			;
	add.w	#8+2,d2			; 1 char = 8 px + 2 px padding
	bra	.loop			;
.done	move.w	#640+1,d0		; 640 + center adj (last padding)
	sub.w	d2,d0			;
	asr.w	#1,d0			; /2 (half)
	move.w	d0,d2			;
	asr.w	#3,d0			; /8 (byte)
	and.w	#%111,d2		;
	moveq	#0,d4			;
	move.b	.startmasks(pc,d2.w),d4	;
	rts				;

.startmasks
	dc.b	%10000000
	dc.b	%01000000
	dc.b	%00100000
	dc.b	%00010000
	dc.b	%00001000
	dc.b	%00000100
	dc.b	%00000010
	dc.b	%00000001


*------ PRINT TEXT ------------------------------------------------------------*

; param a0, a1, d4
printtext
.char	moveq	#0,d0			;
	move.b	(a0)+,d0		;
	ble	.done			;
	asl.w	#3,d0			; *8 (data of 1 char)
	lea	topaz-(' '*8)(pc),a2	;
	add.w	d0,a2			;
	
	moveq	#8-1,d1			; fixed width
	moveq	#7,d3			;
.col	movem.l a1-a2,-(a7)		;
	moveq	#8-1,d7			; font height
.row	move.b	(a2),d0			;
	btst	d3,d0			;
	beq	.notset			;
 	or.b	d4,(a1)			; draw pixel
.notset	addq.w	#1,a2			; next row of char
	add.w	#gmpwidth,a1		;
	dbf	d7,.row			;
	movem.l (a7)+,a1-a2		;
	subq.w	#1,d3			; next bit to test in char col

	ror.b	#1,d4			; x++
 	bcc	.x			;
 	addq.w	#1,a1			; next byte
.x	dbf	d1,.col			;

	ror.b	#1,d4			; x++
	bcc	.gap1			;
 	addq.w	#1,a1			; next byte
.gap1	ror.b	#1,d4			; x++
	bcc	.gap2			;
	addq.w	#1,a1			; next byte
.gap2	bra	.char			;
 	
.done	cmp.b	#-1,d0			; change royal wait value?
	bne	.nm1			;
	move.b	#royalwait2,v_waitvalue(a5) ;
	bra	.done2			;
.nm1	cmp.b	#-2,d0			;
	bne	.nm2			;
	move.b	#royalwait1,v_waitvalue(a5) ;
	bra	.done2			;
.nm2	cmp.b	#-3,d0			; ending?
	bne	.done2			;
	addq.w	#1<<actorending,v_actors(a5) ;
	lea	LSPVars+m_noVolume(pc),a3 ;
	st	(a3)			; LSP won't set volume from now on
.done2	rts				;


*------	TEXT WRITER -----------------------------------------------------------*

textwriter
	move.l	v_gmplane1(a5),a0	; clear area
	add.w	#24*gmpwidth,a0		; we always clear 2nd line (only)
	bsr	waitblitter		;
	move.l	a0,$54(a6)		; destination d
	move.l	#$01000000,$40(a6)	; bltcon0 bltcon1
	move.w	#0,$66(a6)		; modulo d
	move.w	#gmlineheight<<6+gmpwidth>>1,$58(a6) ; bltsize and go
	
	move.l	v_textpointer(a5),a0	;
	bsr	textwidth		;

	move.l	v_gmplane1(a5),a1	;
	
	cmp.w	#3,v_gmcount(a5)	;
	beq	.skip			;
	
	cmp.w	#0,v_gmcount(a5)	; "Software Fail..."
	bne	.not1			;
	add.w	#9*gmpwidth+4,a1	;
	moveq	#%1,d4			;
	bra	.not3			;

.not1	cmp.w	#1,v_gmcount(a5)	; "Press left..."
	bne	.not2			;
	add.w	#9*gmpwidth+29,a1	;
	moveq	#%1000,d4		;
	bra	.not3			;

.not2	cmp.w	#2,v_gmcount(a5)	; "Guru..."
	bne	.not3			;
	add.w	d0,a1			;
	add.w	#24*gmpwidth,a1		; y pos
	moveq	#%1000000,d4		;

.not3	addq.w	#1,v_gmcount(a5)	;
	bra	.forced			;
	
.skip	add.w	#24*gmpwidth,a1		; y pos
	add.w	d0,a1			; x pos see textwidth

.forced	move.l	v_textpointer(a5),a0	;
	bsr	waitblitter		;
	bsr	printtext		; a0, d4

	move.l	a0,v_textpointer(a5)	;
	rts				;


*------	TEXT ------------------------------------------------------------------*

; 1 line = max 64 chars
;                1234567890123456789012345678901234567890123456789012345678901234
text	dc.b	"Software Failure.",0
	dc.b	"Press left mouse button to continue.",0
	dc.b	"Guru Meditation #00000004.4C6F7264",0 ; = 4 Lord ;-)

	dc.b	"That was just a Fillusion.",0
	dc.b	"Last year we travelled here empty-handed.",0
	dc.b	"This year it was at least enough for a compo filler.",0
	dc.b	"Greetings to everyone at MountainBytes.",0
	dc.b	"Special greetings to:",0
	dc.b	"Mercury     Umlaut Design     Atlantis     Slipstream",-2
	dc.b	"RBBS     Vantage     Void     5711     CN112",0
	dc.b	"Batman Group     AmigaBill     Five Finger Punch",0
	dc.b	"The Electronic Knights     mcCoy     Pouet",0
	dc.b	"Melon     AttentionWhore     Desire     Hoffman",0
	dc.b	"The Mega-Mighty Swiss Cracking Association",0
	dc.b	"Scoopex     Artstate     The Twitch Elite     Dekadence",0
	dc.b	"Defekt     Software Failure     Kestra Bitworld",0
	dc.b	"TRSI     Spaceballs     Zodiac     Lemon.     Loonies",0
	dc.b	"Gasman     Lemmy     The Black Lotus     RaccoonViolet",0
	dc.b	"Up Rough     Altair     Nah-Kolor     Rebels",0
	dc.b	"Ghostown     Cosmic Orbs     Logicoma     Spacepigs",0
	dc.b	"Alcatraz     FairLight     AFWD     Shana     Binary",0
	dc.b	"Planet Jazz     Demozoo     Insane     Unlock     ne7",0
	dc.b	"Andromeda     TPOLM     ToBach     Haujobb     Elkmoose",0
	dc.b	"Sir Garbagetruck     Fnuque     Oxygene     Rabenauge",0
	dc.b	"reality404     Abyss     Big Apple     Cocoon",0
	dc.b	"Music: Starbuck     Logo: Marvin, Mike, MnemoTroN",-1
	dc.b	"Dodecahedron: Amicom     Code: Depeche",0
	dc.b	"Enjoy the party. See you soon.",-3
	dc.b	"",0
	dc.b	"",0
	dc.b	"",0
	dc.b	"",0
	
	even


*------	MEMORY MANAGEMENT -----------------------------------------------------*

BESTMEMORY	equ	0
MEMF_CHIP	equ	1<<1
MEMF_CLEAR	equ	1<<16

clistsize	equ	3400
lspbanksize	equ	lspbankend-lspbank
gmclistsize	equ	gmclistend-gmclist

memtable
b_gmclist	dc.l	0,MEMF_CHIP,gmclistsize
b_lspbank	dc.l	0,MEMF_CHIP,lspbanksize

memtable2
b_bitplanes	dc.l	0,MEMF_CHIP+MEMF_CLEAR,numplanes*psize
b_coverplane	dc.l	0,MEMF_CHIP,coverplanesize
b_genclist	dc.l	0,MEMF_CHIP+MEMF_CLEAR,clistsize

;b_testoutofmem	dc.l	0,MEMF_CHIP,600000
memtableend

entrysize 	equ	12 ; one entry in the memtable is 12 bytes large (3 longwords)
entries		equ	(memtableend-memtable)/entrysize
entrieschip	equ	(memtable2-memtable)/entrysize

alloc	lea	clist(pc),a1			;
	move.l	AbsExecBase.w,a6		;
	jsr	TypeOfMem(a6)			;
	btst	#1,d0				; chipmem?
	beq	.notchipmem			;

	lea	gmclist(pc),a0			; mark data that is in chipmen already
	lea	b_gmclist(pc),a1		;
	move.l	a0,(a1)				;

	lea	base(pc),a0			;
	add.l	#lspbank-base,a0		;
	lea	b_lspbank(pc),a1		;
	move.l	a0,(a1)				;
.notchipmem
	lea	memtable(pc),a5			;
	moveq	#entries-1,d7			;
.loop	tst.l	(a5)				; not to be allocated?
	bne	.noalloc			;
	move.l	8(a5),d0			; bytesize
	move.l	4(a5),d1			; requirements
	move.l	AbsExecBase.w,a6		;
	jsr	AllocMem(a6)			;
	move.l	d0,(a5)				;
	beq	.printerrorandfreemem		; out of memory
.noalloc	
	add.w	#entrysize,a5			; next entry
	dbf	d7,.loop			;
	bsr	initmemory			;
	moveq	#0,d0				; ok, all entries allocated
	rts					;

.printerrorandfreemem
	bsr	printoutofmemory		;
dealloc	move.l	AbsExecBase.w,a6		;
	jsr	TypeOfMem(a6)			;
	lea	memtable(pc),a5			;
	moveq	#entries-1,d7			;
	btst	#1,d0				; chipmem?
	beq	.loop				; we are not in chipmem so free all entries
	lea	memtable2(pc),a5		;
	moveq	#entries-entrieschip-1,d7	;
.loop	tst.l	(a5)				; end of memtable?
	beq	.done				;
	move.l	(a5),a1				; address of memory block
	move.l	8(a5),d0			; bytesize
	move.l	AbsExecBase.w,a6		;
	jsr	FreeMem(a6)			;
	add.w	#entrysize,a5			;
	dbf	d7,.loop			;
.done	moveq	#-1,d0				; alloc error
	rts					;

initmemory
	lea	vars(pc),a5			;

	lea	gmclist(pc),a0			; copy guru meditation clist to chip memory
	move.l	b_gmclist(pc),a1		;
	move.w	#gmclistsize-1,d7		;
.gmcopy	move.b	(a0)+,(a1)+			;
	dbf	d7,.gmcopy			;

	move.l	b_bitplanes(pc),d0		;
	move.l	d0,v_gmplane1(a5)		;
	add.l	#gmpsize,d0			;
	move.l	d0,v_gmplane2(a5)		;
	
	move.l	b_gmclist(pc),a1		; init guru meditation clist
	move.l	v_gmplane1(a5),d0		;
	move.w	d0,gmbpl1-gmclist+6(a1)		;
	swap	d0				;
	move.w	d0,gmbpl1-gmclist+2(a1)		;
	move.l	v_gmplane2(a5),d0		;
	move.w	d0,gmbpl2-gmclist+6(a1)		;
	swap	d0				;
	move.w	d0,gmbpl2-gmclist+2(a1)		;

	move.l	b_coverplane(pc),a0		; fill cover bitplane
	moveq	#-1,d1				;
	move.w	#coverplanesize/4-1,d7		;
.fill	move.l	d1,(a0)+			;
	dbf	d7,.fill			;

	lea	base(pc),a0			; copy lspbank to chip memory
	add.l	#lspbank-base,a0		;
	move.l	b_lspbank(pc),a1		;
	move.l	#lspbanksize,d0			;
.copylspbank
	move.b	(a0)+,(a1)+			;
	subq.l	#1,d0				;
	bne	.copylspbank			;
;	rts					; fall thru


*------	GENEREATE FINAL COPPER LIST -------------------------------------------*

genclist
	move.l	b_coverplane(pc),v_coverpointer(a5)

	lea	clist(pc),a0			;
	move.l	b_genclist(pc),a1		;

	move.w	#((clistend-clist)/4)-1,d7	;
.loop	movem.w	(a0)+,d0/d1			;
	cmp.w	#-2,d0				; store? e.g. -2,v_cl_bplt,6
	bne	.nstore				;
	move.w	(a0)+,d2			; offset
	ext.l	d2				;
	add.l	a1,d2				;
	move.l	d2,(a5,d1.w)			;
	bra	.next				;	

.nstore	cmp.w	#-4,d0				; emit cover bitplane pointer only?
	bne	.waits				;
	bsr	.coverpointer			;
	bra	.next				;

.waits	cmp.w	#-3,d0				; emit waits? e.g. -3,$85 (d1),$d3 (d2)
	bne	.copy				;

	move.w	(a0)+,d2			; "to"
	sub.w	d1,d2				;
.fill	move.b	d1,(a1)+			;
	move.b	#$07,(a1)+			;
	move.w	#$fffe,(a1)+			;

	bsr	.coverpointer			;

	cmp.b	#$ff,d1				;
	bne	.ntsc				;
	move.l	#$ffdffffe,(a1)+		; ntsc/pal border
.ntsc	addq.w	#1,d1				; next line

	dbf	d2,.fill			;
	bra	.next				;

.copy	move.w	d0,(a1)+			; existing clist element
	move.w	d1,(a1)+			;
.next	dbf	d7,.loop			;

	rem
	move.l	b_genclist(pc),a0		;
	sub.l	a0,a1				;
	move.l	a1,$200.w			; clistsize (approx $d34 - 3380)
	erem
	rts					;

.coverpointer
	move.l	v_coverpointer(a5),d0		;
	move.w	#$00f2,(a1)+			;
	move.w	d0,(a1)+			;
	move.w	#$00f0,(a1)+			;
	swap	d0				;
	move.w	d0,(a1)+			;
;	move.w	#$0180,(a1)+			; TEST
;	move.w	a1,(a1)+			; TEST	
	add.l	#viswidth,v_coverpointer(a5)	; next line
	rts					;


*------	PRINT OUT OF MEMORY ---------------------------------------------------*

printoutofmemory
	lea	.dos(pc),a1			;
	move.l	AbsExecBase.w,a6		;
	jsr	OldOpenLibrary(a6)		;
	move.l	d0,a6				;
	beq	.error				;
	jsr	Output(a6)			;
	move.l	d0,d1				;
	beq	.error				;
	moveq	#.textend-.text,d3		; length
	lea	.text(pc),a1			;
	move.l	a1,d2				;
	jsr	Write(a6)			;
	tst.l	d0				;
	beq	.error				;
	move.l	a6,a1				;
	move.l	AbsExecBase.w,a6		;
	jsr	CloseLibrary(a6)		;
.error	moveq	#0,d0				;
	rts					;

.dos	dc.b	"dos.library",0
.text	dc.b	"Error: Could not allocate enough memory",10
.textend
	even


*------	AMICOM'S DODECAHEDRON -------------------------------------------------*

dod_div=3000	; was 1390
dod_dv2=10000
dod_r0=607062/dod_div
dod_r1=982247/dod_div
dod_z0=794654/dod_div
dod_z1=187592/dod_div
dz1=dod_z0
dz2=dod_z1
dz3=-dod_z1
dz4=-dod_z0

dx11=  10000*dod_r0/dod_dv2
dx12=   3090*dod_r0/dod_dv2
dx13= -(8090*dod_r0/dod_dv2)
dx14= -(8090*dod_r0/dod_dv2)
dx15=   3090*dod_r0/dod_dv2

dx21=  10000*dod_r1/dod_dv2
dx22=   3090*dod_r1/dod_dv2
dx23= -(8090*dod_r1/dod_dv2)
dx24= -(8090*dod_r1/dod_dv2)
dx25=   3090*dod_r1/dod_dv2

dx31=   8090*dod_r1/dod_dv2
dx32= -(3090*dod_r1/dod_dv2)
dx33=-(10000*dod_r1/dod_dv2)
dx34= -(3090*dod_r1/dod_dv2)
dx35=   8090*dod_r1/dod_dv2

dx41=   8090*dod_r0/dod_dv2
dx42= -(3090*dod_r0/dod_dv2)
dx43=-(10000*dod_r0/dod_dv2)
dx44= -(3090*dod_r0/dod_dv2)
dx45=   8090*dod_r0/dod_dv2

dy11=      0*dod_r0/dod_dv2
dy12=   9511*dod_r0/dod_dv2
dy13=   5878*dod_r0/dod_dv2
dy14= -(5878*dod_r0/dod_dv2)
dy15= -(9511*dod_r0/dod_dv2)

dy21=      0*dod_r1/dod_dv2
dy22=   9511*dod_r1/dod_dv2
dy23=   5878*dod_r1/dod_dv2
dy24= -(5878*dod_r1/dod_dv2)
dy25= -(9511*dod_r1/dod_dv2)

dy31=   5878*dod_r1/dod_dv2
dy32=   9511*dod_r1/dod_dv2
dy33=      0*dod_r1/dod_dv2
dy34= -(9511*dod_r1/dod_dv2)
dy35= -(5878*dod_r1/dod_dv2)

dy41=   5878*dod_r0/dod_dv2
dy42=   9511*dod_r0/dod_dv2
dy43=      0*dod_r0/dod_dv2
dy44= -(9511*dod_r0/dod_dv2)
dy45= -(5878*dod_r0/dod_dv2)

v	equ	6	; size of 1 vertex

vertices
	dc.w	numvertices-1
	dc.w	dx11,dy11,dz1,	dx12,dy12,dz1,	dx13,dy13,dz1
	dc.w	dx14,dy14,dz1,	dx15,dy15,dz1
	dc.w	dx21,dy21,dz2,	dx22,dy22,dz2,	dx23,dy23,dz2
	dc.w	dx24,dy24,dz2,	dx25,dy25,dz2
	dc.w	dx31,dy31,dz3,	dx32,dy32,dz3,	dx33,dy33,dz3
	dc.w	dx34,dy34,dz3,	dx35,dy35,dz3
	dc.w	dx41,dy41,dz4,	dx42,dy42,dz4,	dx43,dy43,dz4
	dc.w	dx44,dy44,dz4,	dx45,dy45,dz4

edges	dc.w	0*v,1*v,0,0
	dc.w	1*v,2*v,0,0
	dc.w	2*v,3*v,0,0
	dc.w	3*v,4*v,0,0
	dc.w	4*v,0*v,0,0
	dc.w	5*v,10*v,0,0
	dc.w	6*v,11*v,0,0
	dc.w	7*v,12*v,0,0
	dc.w	8*v,13*v,0,0
	dc.w	9*v,14*v,0,0
	dc.w	10*v,6*v,0,0
	dc.w	11*v,7*v,0,0
	dc.w	12*v,8*v,0,0
	dc.w	13*v,9*v,0,0
	dc.w	14*v,5*v,0,0
	dc.w	15*v,16*v,0,0
	dc.w	16*v,17*v,0,0
	dc.w	17*v,18*v,0,0
	dc.w	18*v,19*v,0,0
	dc.w	19*v,15*v,0,0
	dc.w	0*v,5*v,0,0
	dc.w	1*v,6*v,0,0
	dc.w	2*v,7*v,0,0
	dc.w	3*v,8*v,0,0
	dc.w	4*v,9*v,0,0
	dc.w	15*v,10*v,0,0
	dc.w	16*v,11*v,0,0
	dc.w	17*v,12*v,0,0
	dc.w	18*v,13*v,0,0
	dc.w	19*v,14*v,0,0
	dc.w	-1

e	equ	8	; size of 1 edge

faces	dc.w	0*v,1*v,3*v		; vertices for normal vector
	dc.w	0*e,1*e,2*e,3*e,4*e	; edges

	dc.w	1*v,0*v,10*v
	dc.w	20*e,5*e,10*e,21*e,0*e

	dc.w	2*v,1*v,11*v
	dc.w	21*e,6*e,11*e,22*e,1*e

	dc.w	3*v,2*v,12*v
	dc.w	22*e,7*e,12*e,23*e,2*e

	dc.w	4*v,3*v,13*v
	dc.w	23*e,8*e,13*e,24*e,3*e

	dc.w	0*v,4*v,14*v
	dc.w	24*e,9*e,14*e,20*e,4*e

	dc.w	18*v,17*v,15*v
	dc.w	15*e,16*e,17*e,18*e,19*e

	dc.w	18*v,8*v,17*v
	dc.w	27*e,12*e,8*e,28*e,17*e

	dc.w	19*v,9*v,18*v
	dc.w	28*e,13*e,9*e,29*e,18*e

	dc.w	15*v,5*v,19*v
	dc.w	29*e,14*e,5*e,25*e,19*e

	dc.w	16*v,6*v,15*v
	dc.w	25*e,10*e,6*e,26*e,15*e

	dc.w	17*v,7*v,16*v
	dc.w	26*e,11*e,7*e,27*e,16*e


*------	SINE TABLE USING 720 ANGLE STEPS AND FACTOR 256 -----------------------*

sin	dc.w	$0000,$0002,$0004,$0007,$0009,$000b,$000d,$0010
	dc.w	$0012,$0014,$0016,$0019,$001b,$001d,$001f,$0021
	dc.w	$0024,$0026,$0028,$002a,$002c,$002f,$0031,$0033
	dc.w	$0035,$0037,$003a,$003c,$003e,$0040,$0042,$0044
	dc.w	$0047,$0049,$004b,$004d,$004f,$0051,$0053,$0055
	dc.w	$0058,$005a,$005c,$005e,$0060,$0062,$0064,$0066
	dc.w	$0068,$006a,$006c,$006e,$0070,$0072,$0074,$0076
	dc.w	$0078,$007a,$007c,$007e,$0080,$0082,$0084,$0086
	dc.w	$0088,$008a,$008b,$008d,$008f,$0091,$0093,$0095
	dc.w	$0096,$0098,$009a,$009c,$009e,$009f,$00a1,$00a3
	dc.w	$00a5,$00a6,$00a8,$00aa,$00ab,$00ad,$00af,$00b0
	dc.w	$00b2,$00b3,$00b5,$00b7,$00b8,$00ba,$00bb,$00bd
	dc.w	$00be,$00c0,$00c1,$00c3,$00c4,$00c6,$00c7,$00c8
	dc.w	$00ca,$00cb,$00cc,$00ce,$00cf,$00d0,$00d2,$00d3
	dc.w	$00d4,$00d5,$00d7,$00d8,$00d9,$00da,$00db,$00dd
	dc.w	$00de,$00df,$00e0,$00e1,$00e2,$00e3,$00e4,$00e5
	dc.w	$00e6,$00e7,$00e8,$00e9,$00ea,$00eb,$00ec,$00ed
	dc.w	$00ed,$00ee,$00ef,$00f0,$00f1,$00f1,$00f2,$00f3
	dc.w	$00f3,$00f4,$00f5,$00f5,$00f6,$00f7,$00f7,$00f8
	dc.w	$00f8,$00f9,$00f9,$00fa,$00fa,$00fb,$00fb,$00fc
	dc.w	$00fc,$00fc,$00fd,$00fd,$00fe,$00fe,$00fe,$00fe
	dc.w	$00ff,$00ff,$00ff,$00ff,$00ff,$0100,$0100,$0100
	dc.w	$0100,$0100,$0100,$0100
	
cos	dc.w	$0100,$0100,$0100,$0100
	dc.w	$0100,$0100,$0100,$0100,$00ff,$00ff,$00ff,$00ff
	dc.w	$00ff,$00fe,$00fe,$00fe,$00fe,$00fd,$00fd,$00fc
	dc.w	$00fc,$00fc,$00fb,$00fb,$00fa,$00fa,$00f9,$00f9
	dc.w	$00f8,$00f8,$00f7,$00f7,$00f6,$00f5,$00f5,$00f4
	dc.w	$00f3,$00f3,$00f2,$00f1,$00f1,$00f0,$00ef,$00ee
	dc.w	$00ed,$00ed,$00ec,$00eb,$00ea,$00e9,$00e8,$00e7
	dc.w	$00e6,$00e5,$00e4,$00e3,$00e2,$00e1,$00e0,$00df
	dc.w	$00de,$00dd,$00db,$00da,$00d9,$00d8,$00d7,$00d5
	dc.w	$00d4,$00d3,$00d2,$00d0,$00cf,$00ce,$00cc,$00cb
	dc.w	$00ca,$00c8,$00c7,$00c6,$00c4,$00c3,$00c1,$00c0
	dc.w	$00be,$00bd,$00bb,$00ba,$00b8,$00b7,$00b5,$00b3
	dc.w	$00b2,$00b0,$00af,$00ad,$00ab,$00aa,$00a8,$00a6
	dc.w	$00a5,$00a3,$00a1,$009f,$009e,$009c,$009a,$0098
	dc.w	$0096,$0095,$0093,$0091,$008f,$008d,$008b,$008a
	dc.w	$0088,$0086,$0084,$0082,$0080,$007e,$007c,$007a
	dc.w	$0078,$0076,$0074,$0072,$0070,$006e,$006c,$006a
	dc.w	$0068,$0066,$0064,$0062,$0060,$005e,$005c,$005a
	dc.w	$0058,$0055,$0053,$0051,$004f,$004d,$004b,$0049
	dc.w	$0047,$0044,$0042,$0040,$003e,$003c,$003a,$0037
	dc.w	$0035,$0033,$0031,$002f,$002c,$002a,$0028,$0026
	dc.w	$0024,$0021,$001f,$001d,$001b,$0019,$0016,$0014
	dc.w	$0012,$0010,$000d,$000b,$0009,$0007,$0004,$0002
	dc.w	$0000,$fffe,$fffc,$fff9,$fff7,$fff5,$fff3,$fff0
	dc.w	$ffee,$ffec,$ffea,$ffe7,$ffe5,$ffe3,$ffe1,$ffdf
	dc.w	$ffdc,$ffda,$ffd8,$ffd6,$ffd4,$ffd1,$ffcf,$ffcd
	dc.w	$ffcb,$ffc9,$ffc6,$ffc4,$ffc2,$ffc0,$ffbe,$ffbc
	dc.w	$ffb9,$ffb7,$ffb5,$ffb3,$ffb1,$ffaf,$ffad,$ffab
	dc.w	$ffa8,$ffa6,$ffa4,$ffa2,$ffa0,$ff9e,$ff9c,$ff9a
	dc.w	$ff98,$ff96,$ff94,$ff92,$ff90,$ff8e,$ff8c,$ff8a
	dc.w	$ff88,$ff86,$ff84,$ff82,$ff80,$ff7e,$ff7c,$ff7a
	dc.w	$ff78,$ff76,$ff75,$ff73,$ff71,$ff6f,$ff6d,$ff6b
	dc.w	$ff6a,$ff68,$ff66,$ff64,$ff62,$ff61,$ff5f,$ff5d
	dc.w	$ff5b,$ff5a,$ff58,$ff56,$ff55,$ff53,$ff51,$ff50
	dc.w	$ff4e,$ff4d,$ff4b,$ff49,$ff48,$ff46,$ff45,$ff43
	dc.w	$ff42,$ff40,$ff3f,$ff3d,$ff3c,$ff3a,$ff39,$ff38
	dc.w	$ff36,$ff35,$ff34,$ff32,$ff31,$ff30,$ff2e,$ff2d
	dc.w	$ff2c,$ff2b,$ff29,$ff28,$ff27,$ff26,$ff25,$ff23
	dc.w	$ff22,$ff21,$ff20,$ff1f,$ff1e,$ff1d,$ff1c,$ff1b
	dc.w	$ff1a,$ff19,$ff18,$ff17,$ff16,$ff15,$ff14,$ff13
	dc.w	$ff13,$ff12,$ff11,$ff10,$ff0f,$ff0f,$ff0e,$ff0d
	dc.w	$ff0d,$ff0c,$ff0b,$ff0b,$ff0a,$ff09,$ff09,$ff08
	dc.w	$ff08,$ff07,$ff07,$ff06,$ff06,$ff05,$ff05,$ff04
	dc.w	$ff04,$ff04,$ff03,$ff03,$ff02,$ff02,$ff02,$ff02
	dc.w	$ff01,$ff01,$ff01,$ff01,$ff01,$ff00,$ff00,$ff00
	dc.w	$ff00,$ff00,$ff00,$ff00,$ff00,$ff00,$ff00,$ff00
	dc.w	$ff00,$ff00,$ff00,$ff00,$ff01,$ff01,$ff01,$ff01
	dc.w	$ff01,$ff02,$ff02,$ff02,$ff02,$ff03,$ff03,$ff04
	dc.w	$ff04,$ff04,$ff05,$ff05,$ff06,$ff06,$ff07,$ff07
	dc.w	$ff08,$ff08,$ff09,$ff09,$ff0a,$ff0b,$ff0b,$ff0c
	dc.w	$ff0d,$ff0d,$ff0e,$ff0f,$ff0f,$ff10,$ff11,$ff12
	dc.w	$ff13,$ff13,$ff14,$ff15,$ff16,$ff17,$ff18,$ff19
	dc.w	$ff1a,$ff1b,$ff1c,$ff1d,$ff1e,$ff1f,$ff20,$ff21
	dc.w	$ff22,$ff23,$ff25,$ff26,$ff27,$ff28,$ff29,$ff2b
	dc.w	$ff2c,$ff2d,$ff2e,$ff30,$ff31,$ff32,$ff34,$ff35
	dc.w	$ff36,$ff38,$ff39,$ff3a,$ff3c,$ff3d,$ff3f,$ff40
	dc.w	$ff42,$ff43,$ff45,$ff46,$ff48,$ff49,$ff4b,$ff4d
	dc.w	$ff4e,$ff50,$ff51,$ff53,$ff55,$ff56,$ff58,$ff5a
	dc.w	$ff5b,$ff5d,$ff5f,$ff61,$ff62,$ff64,$ff66,$ff68
	dc.w	$ff6a,$ff6b,$ff6d,$ff6f,$ff71,$ff73,$ff75,$ff76
	dc.w	$ff78,$ff7a,$ff7c,$ff7e,$ff80,$ff82,$ff84,$ff86
	dc.w	$ff88,$ff8a,$ff8c,$ff8e,$ff90,$ff92,$ff94,$ff96
	dc.w	$ff98,$ff9a,$ff9c,$ff9e,$ffa0,$ffa2,$ffa4,$ffa6
	dc.w	$ffa8,$ffab,$ffad,$ffaf,$ffb1,$ffb3,$ffb5,$ffb7
	dc.w	$ffb9,$ffbc,$ffbe,$ffc0,$ffc2,$ffc4,$ffc6,$ffc9
	dc.w	$ffcb,$ffcd,$ffcf,$ffd1,$ffd4,$ffd6,$ffd8,$ffda
	dc.w	$ffdc,$ffdf,$ffe1,$ffe3,$ffe5,$ffe7,$ffea,$ffec
	dc.w	$ffee,$fff0,$fff3,$fff5,$fff7,$fff9,$fffc,$fffe

	dc.w	$0000,$0002,$0004,$0007,$0009,$000b,$000d,$0010
	dc.w	$0012,$0014,$0016,$0019,$001b,$001d,$001f,$0021
	dc.w	$0024,$0026,$0028,$002a,$002c,$002f,$0031,$0033
	dc.w	$0035,$0037,$003a,$003c,$003e,$0040,$0042,$0044
	dc.w	$0047,$0049,$004b,$004d,$004f,$0051,$0053,$0055
	dc.w	$0058,$005a,$005c,$005e,$0060,$0062,$0064,$0066
	dc.w	$0068,$006a,$006c,$006e,$0070,$0072,$0074,$0076
	dc.w	$0078,$007a,$007c,$007e,$0080,$0082,$0084,$0086
	dc.w	$0088,$008a,$008b,$008d,$008f,$0091,$0093,$0095
	dc.w	$0096,$0098,$009a,$009c,$009e,$009f,$00a1,$00a3
	dc.w	$00a5,$00a6,$00a8,$00aa,$00ab,$00ad,$00af,$00b0
	dc.w	$00b2,$00b3,$00b5,$00b7,$00b8,$00ba,$00bb,$00bd
	dc.w	$00be,$00c0,$00c1,$00c3,$00c4,$00c6,$00c7,$00c8
	dc.w	$00ca,$00cb,$00cc,$00ce,$00cf,$00d0,$00d2,$00d3
	dc.w	$00d4,$00d5,$00d7,$00d8,$00d9,$00da,$00db,$00dd
	dc.w	$00de,$00df,$00e0,$00e1,$00e2,$00e3,$00e4,$00e5
	dc.w	$00e6,$00e7,$00e8,$00e9,$00ea,$00eb,$00ec,$00ed
	dc.w	$00ed,$00ee,$00ef,$00f0,$00f1,$00f1,$00f2,$00f3
	dc.w	$00f3,$00f4,$00f5,$00f5,$00f6,$00f7,$00f7,$00f8
	dc.w	$00f8,$00f9,$00f9,$00fa,$00fa,$00fb,$00fb,$00fc
	dc.w	$00fc,$00fc,$00fd,$00fd,$00fe,$00fe,$00fe,$00fe
	dc.w	$00ff,$00ff,$00ff,$00ff,$00ff,$0100,$0100,$0100
	dc.w	$0100,$0100,$0100,$0100


*------	TOPAZ 9 FONT ----------------------------------------------------------*

topaz	dc.b	%00000000	; Space
	dc.b	%00000000
	dc.b	%00000000
	dc.b	%00000000
	dc.b	%00000000
	dc.b	%00000000
	dc.b	%00000000
	dc.b	%00000000

	dc.b	%00011000
	dc.b	%00111100
	dc.b	%00111100
	dc.b	%00011000
	dc.b	%00011000
	dc.b	%00000000
	dc.b	%00011000
	dc.b	%00000000

	dc.b	%01100110
	dc.b	%01100110
	dc.b	%00000000
	dc.b	%00000000
	dc.b	%00000000
	dc.b	%00000000
	dc.b	%00000000
	dc.b	%00000000

	dc.b	%01100110
	dc.b	%01100110
	dc.b	%11111111
	dc.b	%01100110
	dc.b	%11111111
	dc.b	%01100110
	dc.b	%01100110
	dc.b	%00000000

	dc.b	%00001100
	dc.b	%00111111
	dc.b	%01100000
	dc.b	%00111110
	dc.b	%00000011
	dc.b	%01111110
	dc.b	%00011000
	dc.b	%00000000

	dc.b	%00000000
	dc.b	%01100011
	dc.b	%01100110
	dc.b	%00001100
	dc.b	%00011000
	dc.b	%00110011
	dc.b	%01100011
	dc.b	%00000000

	dc.b	%00111100
	dc.b	%01100110
	dc.b	%01100100
	dc.b	%01111011
	dc.b	%11001110
	dc.b	%11000110
	dc.b	%01111011
	dc.b	%00000000

	dc.b	%00011000
	dc.b	%00011000
	dc.b	%00110000
	dc.b	%00000000
	dc.b	%00000000
	dc.b	%00000000
	dc.b	%00000000
	dc.b	%00000000

	dc.b	%00001110
	dc.b	%00011000
	dc.b	%00110000
	dc.b	%00110000
	dc.b	%00110000
	dc.b	%00011000
	dc.b	%00001110
	dc.b	%00000000

	dc.b	%01110000
	dc.b	%00011000
	dc.b	%00001100
	dc.b	%00001100
	dc.b	%00001100
	dc.b	%00011000
	dc.b	%01110000
	dc.b	%00000000

	dc.b	%00000000
	dc.b	%01100110
	dc.b	%00111100
	dc.b	%11111111
	dc.b	%00111100
	dc.b	%01100110
	dc.b	%00000000
	dc.b	%00000000

	dc.b	%00000000
	dc.b	%00011000
	dc.b	%00011000
	dc.b	%11111111
	dc.b	%00011000
	dc.b	%00011000
	dc.b	%00000000
	dc.b	%00000000

	dc.b	%00000000
	dc.b	%00000000
	dc.b	%00000000
	dc.b	%00000000
	dc.b	%00000000
	dc.b	%00011000
	dc.b	%00011000
	dc.b	%00110000

	dc.b	%00000000
	dc.b	%00000000
	dc.b	%00000000
	dc.b	%11111111
	dc.b	%00000000
	dc.b	%00000000
	dc.b	%00000000
	dc.b	%00000000

	dc.b	%00000000
	dc.b	%00000000
	dc.b	%00000000
	dc.b	%00000000
	dc.b	%00000000
	dc.b	%00011000
	dc.b	%00011000
	dc.b	%00000000

	dc.b	%00000011
	dc.b	%00000110
	dc.b	%00001100
	dc.b	%00011000
	dc.b	%00110000
	dc.b	%01100000
	dc.b	%11000000
	dc.b	%00000000

	dc.b	%00111110	; 0
	dc.b	%01100011
	dc.b	%01100111
	dc.b	%01111111
	dc.b	%01110011
	dc.b	%01100011
	dc.b	%00111110
	dc.b	%00000000

	dc.b	%00011000
	dc.b	%00111000
	dc.b	%00011000
	dc.b	%00011000
	dc.b	%00011000
	dc.b	%00011000
	dc.b	%01111110
	dc.b	%00000000

	dc.b	%00111110
	dc.b	%01100011
	dc.b	%00000011
	dc.b	%00001110
	dc.b	%00110000
	dc.b	%01100011
	dc.b	%01111111
	dc.b	%00000000

	dc.b	%00111110
	dc.b	%01100011
	dc.b	%00000011
	dc.b	%00011110
	dc.b	%00000011
	dc.b	%01100011
	dc.b	%00111110
	dc.b	%00000000

	dc.b	%00011110
	dc.b	%00110110
	dc.b	%01100110
	dc.b	%11000110
	dc.b	%11111111
	dc.b	%00000110
	dc.b	%00011111
	dc.b	%00000000

	dc.b	%01111111
	dc.b	%01100000
	dc.b	%01111110
	dc.b	%00000011
	dc.b	%00000011
	dc.b	%01100011
	dc.b	%00111110
	dc.b	%00000000

	dc.b	%00011110
	dc.b	%00110000
	dc.b	%01100000
	dc.b	%01111110
	dc.b	%01100011
	dc.b	%01100011
	dc.b	%00111110
	dc.b	%00000000

	dc.b	%01111111
	dc.b	%01100011
	dc.b	%00000011
	dc.b	%00000110
	dc.b	%00001100
	dc.b	%00011000
	dc.b	%00011000
	dc.b	%00000000

	dc.b	%00111110
	dc.b	%01100011
	dc.b	%01100011
	dc.b	%00111110
	dc.b	%01100011
	dc.b	%01100011
	dc.b	%00111110
	dc.b	%00000000

	dc.b	%00111110	; 9
	dc.b	%01100011
	dc.b	%01100011
	dc.b	%00111111
	dc.b	%00000011
	dc.b	%00000110
	dc.b	%00111100
	dc.b	%00000000

	dc.b	%00000000
	dc.b	%00011000
	dc.b	%00011000
	dc.b	%00000000
	dc.b	%00000000
	dc.b	%00011000
	dc.b	%00011000
	dc.b	%00000000

	dc.b	%00000000
	dc.b	%00011000
	dc.b	%00011000
	dc.b	%00000000
	dc.b	%00000000
	dc.b	%00011000
	dc.b	%00011000
	dc.b	%00110000

	dc.b	%00000110
	dc.b	%00001100
	dc.b	%00011000
	dc.b	%00110000
	dc.b	%00011000
	dc.b	%00001100
	dc.b	%00000110
	dc.b	%00000000

	dc.b	%00000000
	dc.b	%00000000
	dc.b	%11111111
	dc.b	%00000000
	dc.b	%00000000
	dc.b	%11111111
	dc.b	%00000000
	dc.b	%00000000

	dc.b	%00110000
	dc.b	%00011000
	dc.b	%00001100
	dc.b	%00000110
	dc.b	%00001100
	dc.b	%00011000
	dc.b	%00110000
	dc.b	%00000000

	dc.b	%00111110
	dc.b	%01100011
	dc.b	%00000011
	dc.b	%00001110
	dc.b	%00011000
	dc.b	%00000000
	dc.b	%00011000
	dc.b	%00000000

	dc.b	%01111110
	dc.b	%11000011
	dc.b	%11001111
	dc.b	%11011011
	dc.b	%11001111
	dc.b	%11000000
	dc.b	%01111100
	dc.b	%00000000

	dc.b	%00011000	; A
	dc.b	%00111100
	dc.b	%00111100
	dc.b	%01100110
	dc.b	%01111110
	dc.b	%11000011
	dc.b	%11000011
	dc.b	%00000000

	dc.b	%11111110
	dc.b	%01100011
	dc.b	%01100011
	dc.b	%01111110
	dc.b	%01100011
	dc.b	%01100011
	dc.b	%11111110
	dc.b	%00000000

	dc.b	%00111110
	dc.b	%01100011
	dc.b	%11000000
	dc.b	%11000000
	dc.b	%11000000
	dc.b	%01100011
	dc.b	%00111110
	dc.b	%00000000

	dc.b	%11111100
	dc.b	%01100110
	dc.b	%01100011
	dc.b	%01100011
	dc.b	%01100011
	dc.b	%01100110
	dc.b	%11111100
	dc.b	%00000000

	dc.b	%11111111
	dc.b	%01100011
	dc.b	%01100000
	dc.b	%01111100
	dc.b	%01100000
	dc.b	%01100011
	dc.b	%11111111
	dc.b	%00000000

	dc.b	%11111111
	dc.b	%01100011
	dc.b	%01100000
	dc.b	%01111100
	dc.b	%01100000
	dc.b	%01100000
	dc.b	%11111000
	dc.b	%00000000

	dc.b	%00111110
	dc.b	%01100011
	dc.b	%11000000
	dc.b	%11000111
	dc.b	%11000011
	dc.b	%01100011
	dc.b	%00111111
	dc.b	%00000000

	dc.b	%01100011
	dc.b	%01100011
	dc.b	%01100011
	dc.b	%01111111
	dc.b	%01100011
	dc.b	%01100011
	dc.b	%01100011
	dc.b	%00000000

	dc.b	%01111110
	dc.b	%00011000
	dc.b	%00011000
	dc.b	%00011000
	dc.b	%00011000
	dc.b	%00011000
	dc.b	%01111110
	dc.b	%00000000

	dc.b	%00000111
	dc.b	%00000011
	dc.b	%00000011
	dc.b	%00000011
	dc.b	%01100011
	dc.b	%01100011
	dc.b	%00111110
	dc.b	%00000000

	dc.b	%11100011
	dc.b	%01100011
	dc.b	%01100110
	dc.b	%01111100
	dc.b	%01100110
	dc.b	%01100011
	dc.b	%11100011
	dc.b	%00000000

	dc.b	%11111000
	dc.b	%01100000
	dc.b	%01100000
	dc.b	%01100000
	dc.b	%01100001
	dc.b	%01100011
	dc.b	%11111111
	dc.b	%00000000

	dc.b	%10000001
	dc.b	%11000011
	dc.b	%11100111
	dc.b	%11111111
	dc.b	%11011011
	dc.b	%11000011
	dc.b	%11000011
	dc.b	%00000000

	dc.b	%11000011
	dc.b	%11100011
	dc.b	%11110011
	dc.b	%11011011
	dc.b	%11001111
	dc.b	%11000111
	dc.b	%11000011
	dc.b	%00000000

	dc.b	%00111100
	dc.b	%01100110
	dc.b	%11000011
	dc.b	%11000011
	dc.b	%11000011
	dc.b	%01100110
	dc.b	%00111100
	dc.b	%00000000

	dc.b	%11111110
	dc.b	%01100011
	dc.b	%01100011
	dc.b	%01111110
	dc.b	%01100000
	dc.b	%01100000
	dc.b	%11111000
	dc.b	%00000000

	dc.b	%00111100
	dc.b	%01100110
	dc.b	%11000011
	dc.b	%11000011
	dc.b	%11000011
	dc.b	%01100110
	dc.b	%00111110
	dc.b	%00000011

	dc.b	%11111110
	dc.b	%01100011
	dc.b	%01100011
	dc.b	%01111110
	dc.b	%01100110
	dc.b	%01100011
	dc.b	%11100001
	dc.b	%00000000

	dc.b	%00111110
	dc.b	%01100011
	dc.b	%01110000
	dc.b	%00011100
	dc.b	%00000111
	dc.b	%01100011
	dc.b	%00111110
	dc.b	%00000000

	dc.b	%11111111
	dc.b	%10011001
	dc.b	%00011000
	dc.b	%00011000
	dc.b	%00011000
	dc.b	%00011000
	dc.b	%00111100
	dc.b	%00000000

	dc.b	%01100011
	dc.b	%01100011
	dc.b	%01100011
	dc.b	%01100011
	dc.b	%01100011
	dc.b	%01100011
	dc.b	%00111111
	dc.b	%00000000

	dc.b	%11000011
	dc.b	%11000011
	dc.b	%01100110
	dc.b	%01100110
	dc.b	%00111100
	dc.b	%00111100
	dc.b	%00011000
	dc.b	%00000000

	dc.b	%11000011
	dc.b	%11000011
	dc.b	%11000011
	dc.b	%11011011
	dc.b	%11111111
	dc.b	%11100111
	dc.b	%11000011
	dc.b	%00000000

	dc.b	%11000011
	dc.b	%01100110
	dc.b	%00111100
	dc.b	%00011000
	dc.b	%00111100
	dc.b	%01100110
	dc.b	%11000011
	dc.b	%00000000

	dc.b	%11000011
	dc.b	%11000011
	dc.b	%01100110
	dc.b	%00111100
	dc.b	%00011000
	dc.b	%00011000
	dc.b	%00111100
	dc.b	%00000000

	dc.b	%11111111	; Z
	dc.b	%11000110
	dc.b	%10001100
	dc.b	%00011000
	dc.b	%00110001
	dc.b	%01100011
	dc.b	%11111111
	dc.b	%00000000

	dc.b	%00111100
	dc.b	%00110000
	dc.b	%00110000
	dc.b	%00110000
	dc.b	%00110000
	dc.b	%00110000
	dc.b	%00111100
	dc.b	%00000000

	dc.b	%11000000
	dc.b	%01100000
	dc.b	%00110000
	dc.b	%00011000
	dc.b	%00001100
	dc.b	%00000110
	dc.b	%00000011
	dc.b	%00000000

	dc.b	%00111100
	dc.b	%00001100
	dc.b	%00001100
	dc.b	%00001100
	dc.b	%00001100
	dc.b	%00001100
	dc.b	%00111100
	dc.b	%00000000

	dc.b	%00011000
	dc.b	%00111100
	dc.b	%01100110
	dc.b	%11000011
	dc.b	%00000000
	dc.b	%00000000
	dc.b	%00000000
	dc.b	%00000000

	dc.b	%00000000
	dc.b	%00000000
	dc.b	%00000000
	dc.b	%00000000
	dc.b	%00000000
	dc.b	%00000000
	dc.b	%00000000
	dc.b	%11111111

	dc.b	%00011000
	dc.b	%00011000
	dc.b	%00001100
	dc.b	%00000000
	dc.b	%00000000
	dc.b	%00000000
	dc.b	%00000000
	dc.b	%00000000

	dc.b	%00000000	; a
	dc.b	%00000000
	dc.b	%01111100
	dc.b	%00000110
	dc.b	%00111110
	dc.b	%11000110
	dc.b	%01111011
	dc.b	%00000000

	dc.b	%11100000
	dc.b	%01100000
	dc.b	%01101110
	dc.b	%01110011
	dc.b	%01100011
	dc.b	%01100011
	dc.b	%00111110
	dc.b	%00000000

	dc.b	%00000000
	dc.b	%00000000
	dc.b	%00111110
	dc.b	%01100011
	dc.b	%01100000
	dc.b	%01100011
	dc.b	%00111110
	dc.b	%00000000

	dc.b	%00001110
	dc.b	%00000110
	dc.b	%01110110
	dc.b	%11001110
	dc.b	%11000110
	dc.b	%11000110
	dc.b	%01111011
	dc.b	%00000000

	dc.b	%00000000
	dc.b	%00000000
	dc.b	%00111110
	dc.b	%01100011
	dc.b	%01111111
	dc.b	%01100000
	dc.b	%00111110
	dc.b	%00000000

	dc.b	%00011110
	dc.b	%00110011
	dc.b	%00110000
	dc.b	%01111000
	dc.b	%00110000
	dc.b	%00110000
	dc.b	%01111000
	dc.b	%00000000

	dc.b	%00000000
	dc.b	%00000000
	dc.b	%01111011
	dc.b	%11000110
	dc.b	%11000110
	dc.b	%01111100
	dc.b	%00000110
	dc.b	%11111100

	dc.b	%11100000
	dc.b	%01100000
	dc.b	%01101110
	dc.b	%01110011
	dc.b	%01100011
	dc.b	%01100011
	dc.b	%11100011
	dc.b	%00000000

	dc.b	%00011000
	dc.b	%00000000
	dc.b	%00111000
	dc.b	%00011000
	dc.b	%00011000
	dc.b	%00011000
	dc.b	%00111100
	dc.b	%00000000

	dc.b	%00000011
	dc.b	%00000000
	dc.b	%00000111
	dc.b	%00000011
	dc.b	%00000011
	dc.b	%00000011
	dc.b	%01100011
	dc.b	%00111110

	dc.b	%11100000
	dc.b	%01100000
	dc.b	%01100011
	dc.b	%01100110
	dc.b	%01111100
	dc.b	%01100110
	dc.b	%11100011
	dc.b	%00000000

	dc.b	%00111000
	dc.b	%00011000
	dc.b	%00011000
	dc.b	%00011000
	dc.b	%00011000
	dc.b	%00011000
	dc.b	%00111100
	dc.b	%00000000

	dc.b	%00000000
	dc.b	%00000000
	dc.b	%11000110
	dc.b	%11101111
	dc.b	%11011011
	dc.b	%11000011
	dc.b	%11000011
	dc.b	%00000000

	dc.b	%00000000
	dc.b	%00000000
	dc.b	%01111110
	dc.b	%01100011
	dc.b	%01100011
	dc.b	%01100011
	dc.b	%01100011
	dc.b	%00000000

	dc.b	%00000000
	dc.b	%00000000
	dc.b	%00111110
	dc.b	%01100011
	dc.b	%01100011
	dc.b	%01100011
	dc.b	%00111110
	dc.b	%00000000

	dc.b	%00000000
	dc.b	%00000000
	dc.b	%11011110
	dc.b	%01100011
	dc.b	%01100011
	dc.b	%01111110
	dc.b	%01100000
	dc.b	%11110000

	dc.b	%00000000
	dc.b	%00000000
	dc.b	%01111101
	dc.b	%11000110
	dc.b	%11000110
	dc.b	%01111110
	dc.b	%00000110
	dc.b	%00000111

	dc.b	%00000000
	dc.b	%00000000
	dc.b	%11101110
	dc.b	%01110011
	dc.b	%01100011
	dc.b	%01100000
	dc.b	%11110000
	dc.b	%00000000

	dc.b	%00000000
	dc.b	%00000000
	dc.b	%00111111
	dc.b	%01100000
	dc.b	%00111110
	dc.b	%00000011
	dc.b	%01111110
	dc.b	%00000000

	dc.b	%00001000
	dc.b	%00011000
	dc.b	%00111110
	dc.b	%00011000
	dc.b	%00011000
	dc.b	%00011001
	dc.b	%00001110
	dc.b	%00000000

	dc.b	%00000000
	dc.b	%00000000
	dc.b	%11000110
	dc.b	%11000110
	dc.b	%11000110
	dc.b	%11000110
	dc.b	%01111011
	dc.b	%00000000

	dc.b	%00000000
	dc.b	%00000000
	dc.b	%11000011
	dc.b	%11000011
	dc.b	%01100110
	dc.b	%00111100
	dc.b	%00011000
	dc.b	%00000000

	dc.b	%00000000
	dc.b	%00000000
	dc.b	%11000011
	dc.b	%11011011
	dc.b	%11011011
	dc.b	%01100110
	dc.b	%01100110
	dc.b	%00000000

	dc.b	%00000000
	dc.b	%00000000
	dc.b	%01100011
	dc.b	%00110110
	dc.b	%00011100
	dc.b	%00110110
	dc.b	%01100011
	dc.b	%00000000

	dc.b	%00000000
	dc.b	%00000000
	dc.b	%11000011
	dc.b	%11000011
	dc.b	%01100110
	dc.b	%00111100
	dc.b	%00011000
	dc.b	%01110000

	dc.b	%00000000	; z
	dc.b	%00000000
	dc.b	%01111111
	dc.b	%01000110
	dc.b	%00011100
	dc.b	%00110001
	dc.b	%01111111
	dc.b	%00000000

	dc.b	%00001110
	dc.b	%00011000
	dc.b	%00011000
	dc.b	%01110000
	dc.b	%00011000
	dc.b	%00011000
	dc.b	%00001110
	dc.b	%00000000

	dc.b	%00011000
	dc.b	%00011000
	dc.b	%00011000
	dc.b	%00011000
	dc.b	%00011000
	dc.b	%00011000
	dc.b	%00011000
	dc.b	%00000000

	dc.b	%01110000
	dc.b	%00011000
	dc.b	%00011000
	dc.b	%00001110
	dc.b	%00011000
	dc.b	%00011000
	dc.b	%01110000
	dc.b	%00000000


;*****************************************************************
;
;	Light Speed Player v1.05 (modified)
;	Fastest Amiga MOD player ever :)
;	Written By Arnaud Carré (aka Leonard / OXYGENE)
;	https://github.com/arnaud-carre/LSPlayer
;	twitter: @leonard_coder
;
;	--------How to use--------- 
;
;	bsr LSP_MusicDriver+0 : Init LSP player code
;		In:	a0: LSP music data(any memory)
;			a1: LSP sound bank(chip memory)
;			a2: DMACON 8bits byte address (should be odd address!)
;		Out:a0: music BPM pointer (16bits)
;			d0: music len in tick count
;
;	bsr LSP_MusicDriver+4 : LSP player tick (call once per frame)
;		In:	a6: must be $dff000
;			Scratched regs: d0/d1/d2/a0/a1/a2/a3/a4/a5
;		Out:None
;
;*****************************************************************

lspplay	lea	LSPVars(pc),a1
	move.l	(a1),a0				; byte stream
.process
	moveq	#0,d0
.cloop	move.b	(a0)+,d0
	bne	.swCode
	addi.w	#$0100,d0
	bra	.cloop
.swCode	add.w	d0,d0
	move.l	m_codeTableAddr(a1),a2		; code table
	move.w	(a2,d0.w),d0			; code
	beq	.noInst
	cmp.w	m_escCodeRewind(a1),d0
	beq	.r_rewind
	cmp.w	m_escCodeSetBpm(a1),d0
	beq	.r_chgbpm

	move.w	m_noVolume(a1),d1
	add.b	d0,d0
	bcc	.noVd
	tst.w	d1
	beq	.vd
	addq.w	#1,a0				; skip
	bra	.noVd
.vd	move.b	(a0)+,$d9(a6)
.noVd	add.b	d0,d0
	bcc	.noVc
	tst.w	d1
	beq	.vc
	addq.w	#1,a0				; skip
	bra	.noVc
.vc	move.b	(a0)+,$c9(a6)
.noVc	add.b	d0,d0
	bcc	.noVb
	tst.w	d1
	beq	.vb
	addq.w	#1,a0				; skip
	bra	.noVb
.vb	move.b	(a0)+,$b9(a6)
.noVb	add.b	d0,d0
	bcc	.noVa
	tst.w	d1
	beq	.va
	addq.w	#1,a0				; skip
	bra	.noVa
.va	move.b	(a0)+,$a9(a6)
.noVa	move.l	a0,(a1)+			; store byte stream ptr
	move.l	(a1),a0				; word stream
	tst.b	d0
	beq	.noPa
	add.b	d0,d0
	bcc	.noPd
	move.w	(a0)+,$d6(a6)
.noPd	add.b	d0,d0
	bcc	.noPc
	move.w	(a0)+,$c6(a6)
.noPc	add.b	d0,d0
	bcc	.noPb
	move.w	(a0)+,$b6(a6)
.noPb	add.b	d0,d0
	bcc	.noPa
	move.w	(a0)+,$a6(a6)
.noPa	tst.w	d0
	beq	.noInst

	moveq	#0,d1
	move.l	m_lspInstruments-4(a1),a2	; instrument table
	lea	resetv+12(pc),a4
	lea	$d0(a6),a5
	moveq	#4-1,d2
.vloop	add.w	d0,d0
	bcs	.setIns
	add.w	d0,d0
	bcc	.skip
	move.l	(a4),a3
	move.l	(a3)+,(a5)
	move.w	(a3)+,4(a5)
	bra	.skip
.setIns	add.w	(a0)+,a2
	add.w	d0,d0
	bcc	.noReset
	bset	d2,d1
	move.w	d1,$96(a6)
.noReset
	move.l	(a2)+,(a5)
	move.w	(a2)+,4(a5)
	move.l	a2,(a4)
.skip	subq.w	#4,a4
	sub.w	#$10,a5
	dbf	d2,.vloop

	move.l	m_dmaconPatch-4(a1),a3		;
	move.b	d1,(a3)				; dmacom main clist		
	move.l	m_dmaconPatch2-4(a1),a3		;
	move.b	d1,(a3)				; dmacom gm clist

.noInst	move.l	a0,(a1)				; store word stream (or byte stream if coming from early out)
	rts

.r_rewind
	move.l	m_byteStreamLoop(a1),a0
	move.l	m_wordStreamLoop(a1),m_wordStream(a1)
	bra	.process

.r_chgbpm	
	move.b	(a0)+,m_currentBpm+1(a1)	; BPM
	bra	.process

lspinit	lea	base(pc),a0			; a0: music data (any mem) + 10
	add.l	#lspmusic-base,a0		;
	move.l	b_lspbank(pc),a1		; a1: sound bank data (chip mem)
	move.l	v_cl_lspdmacon(a5),a2		;

	lea	LSPVars(pc),a3
	move.w	(a0)+,m_currentBpm(a3)		; default BPM
	move.w	(a0)+,m_escCodeRewind(a3)
	move.w	(a0)+,m_escCodeSetBpm(a3)
	move.l	(a0)+,-(a7)			; who cares? replace with addq.w #4,a0?
	move.l	a2,m_dmaconPatch(a3)
	
	move.l	b_gmclist(pc),a2		; gm clist
	lea	gmlspdmacon+3-gmclist(a2),a2	;
	move.l	a2,m_dmaconPatch2(a3)		;

	move.w	(a0)+,d0			; instrument count
	lea	-12(a0),a2			; LSP data has -12 offset on instrument tab (to win 2 cycles in fast player)
	move.l	a2,m_lspInstruments(a3)		; instrument tab addr (minus 4)
	subq.w	#1,d0
	move.l	a1,d1
.relocLoop
	bset.b	#0,3(a0)			; bit0 is relocation done flag
	bne	.relocated
	add.l	d1,(a0)
	add.l	d1,6(a0)
.relocated
	lea	12(a0),a0
	dbf	d0,.relocLoop
	move.w	(a0)+,d0			; codes count (+2)
	move.l	a0,m_codeTableAddr(a3)		; code table
	add.w	d0,d0
	add.w	d0,a0
	movem.l	(a0)+,d0-d2			; word stream size, byte stream loop point, word stream loop point
	move.l	a0,m_wordStream(a3)
	lea	(a0,d0.l),a1			; byte stream
	move.l	a1,m_byteStream(a3)
	add.l	d2,a0
	add.l	d1,a1
	move.l	a0,m_wordStreamLoop(a3)
	move.l	a1,m_byteStreamLoop(a3)
	lea	m_currentBpm(a3),a0
	move.l	(a7)+,d0			; music len in frame ticks? who cares? REMOVE?
	rts

	rsreset
m_byteStream		rs.l	1	;  0 byte stream
m_wordStream		rs.l	1	;  4 word stream
m_dmaconPatch		rs.l	1	;  8 m_lfmDmaConPatch
m_codeTableAddr		rs.l	1	; 12 code table addr
m_escCodeRewind		rs.w	1	; 16 rewind special escape code
m_escCodeSetBpm		rs.w	1	; 18 set BPM escape code
m_lspInstruments	rs.l	1	; 20 LSP instruments table addr
m_relocDone		rs.w	1	; 24 reloc done flag
m_currentBpm		rs.w	1	; 26 current BPM
m_byteStreamLoop	rs.l	1	; 28 byte stream loop point
m_wordStreamLoop	rs.l	1	; 32 word stream loop point
m_dmaconPatch2		rs.l	1	; added
m_noVolume		rs.w	1	; added
sizeof_LSPVars		rs.w	0

LSPVars		ds.b	sizeof_LSPVars
	even			
resetv		dc.l	0,0,0,0


*------	MNEMOTRON LOGO --------------------------------------------------------*

logo	incbin	"logo"
logoend
	even

*------	MUSIC -----------------------------------------------------------------*

lspbank	incbin	"starbuck-titelsong.lsbank"
lspbankend
	even

lspmusic
	incbin	"starbuck-titelsong.lsmusic",10 ; skip header (10 bytes)
