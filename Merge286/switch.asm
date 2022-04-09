;SCCSIDM @(#)switch.at	3.2    LCC     ;  Modified: 15:19:59 4/10/87
;
;	Memory "network" driver for Locus Safari V software
;	David Butterfield -- Locus Computing Corporation
;	David Blanset -- AT&T Information Systems
;	October 1984
;
;	Copyright (c) 1984, 1985 Locus Computing Corporation
;	ALL RIGHTS RESERVED
;

;	conditional assembly flags
	; Recommended Settings:
	; ON: popfbug, asm8086, intnmibug, NPX
 asm8086	equ 1			; assembler doesn't have 286 inst.
 popfbug	equ 1			; 286 has popf instruction bug
 intnmibug	equ 1			; 286 has int vs. NMI bug
 NPX		equ 1			; 287 support
 ATPROBE	equ 1			; Atron AT probe support
 TIMERMOD	equ 0			; DOS timer speedup adjustment mod
 MASKINTS	equ 0			; Mask channel in service mod
 NOTICK		equ 1			; Handle internally recursive ticks
 MERGEPRINT	equ 1			; Handle printer a little different
 DOUBLEINT	equ 0			; Double interrupt handling
 SSTRUCTPTR	equ 1			; Pass only struct ptr in switchdata
 PADDING 	equ 1			; ORG switchdata struct at 100h
 KILLSYSINTS	equ 0			; Kills DOS if DOS calls from hdwr ints.
 CHKRESET	equ 0			; Watch for ctrl-alt-del reset

 CTLALTFIX	equ 1			; clear CTL and ALT on abort

 ; definitions for reason for mode switch
 COSWITCH	equ 0ffffh		; Co-routine switch
 IRET		equ 0fffeh		; Switch after processing interrupt
 ABORT		equ 0fffdh		; Fatal error switch

 SHUTDOWN	equ 0fh			; CMOS address of shutdown byte
 CMOSA		equ 70h			; CMOS address port
 CMOSD		equ 71h			; CMOS data port
 READISR	equ 0bH			; 8259 read ISR OCW
 PC8259		equ 020H		; Port address for (primary) 8259
 AT8259		equ 0a0H		; Port address for AT 2nd 8259
 SLAVEOI	equ 062H		; Specific EOI for slave in master
 CLOCKCHAN	equ 0
 CLOCKMASK	equ 1
 KBCHAN		equ 1			; Keyboard channel no
 KBMASK		equ 2			; Keyboard channel mask
 SLAVEMASK	equ 4
 SIO2MASK	equ 8
 SIO1MASK	equ 10h
 FDMASK		equ 40h
 DISABLEKB	equ 0adh
 ENABLEKB	equ 0aeh
 KB_WOP		equ 0d1h		; Write to KB controller output port
 KBSTATUS	equ 64h			; Keyboard controller status port
 KBCMD		equ 64h			; Keyboard controller command port
 KBDATA		equ 60h			; Keyboard controller data port
 ENABLE20	equ 0dfh		; KB cont ouput port value to enable
					; upper address lines
 CARRYFLAG	equ 1

include	atmerge.h

; BIOS address for BIOS block move return stack
BMSTACKADDR	EQU 67h

PAGE

; Here is the support code for switching in both directions between
; real and protected mode.

  if asm8086
; Some assemblers don't have all the 286 opcodes in them.
pusha	MACRO
	db 60h
ENDM
popa	MACRO
	db 61h
ENDM
lret	MACRO
	db 0cbh
ENDM
  endif

; Some 286's have a bug which allows an interrupt to occur during a
; popf instruction even when going from disabled state to disabled
; state.  Using this popff macro avoids this problem.
popff	MACRO
  if popfbug
	push cs
	call iretins
  else
	popf
  endif
ENDM

; Misc macros for switching between the user and the driver

switch_to_driver_stack MACRO
	; This macro must change no flags or registers other than ss:sp
	mov	ir_sssave, ss
	mov	ir_spsave, sp
	push	cs
	pop	ss
	mov	sp, ir_myspsave
ENDM

switch_to_user_stack MACRO
	; This macro must change no flags or registers other than ss:sp
	mov	ir_myspsave, sp
	mov	ss, ir_sssave
	mov	sp, ir_spsave
ENDM

push_frame MACRO
	; This macro must change no flags
	push	bp
	mov	bp, sp
	pusha
	push	ds
	push	es
ENDM

; defines for accessing pushed registers via BP
SAVEAX	EQU	-2
SAVEAL	EQU	-2
SAVEAH	EQU	-1
SAVEDX	EQU	-6
SAVEDL	EQU	-6
SAVEBX	EQU	-8

pop_frame MACRO
	; This macro must change no flags
	pop	es
	pop	ds
	popa
	pop	bp
ENDM

CSEG	SEGMENT PARA PUBLIC 'CODE'

ASSUME CS:CSEG, DS:NOTHING, ES:NOTHING
BEGIN:

	; This stack is used for returning to real mode on the AT.
	; We pretend to be returning from a BIOS block move and this
	; stack contains the appropriate number of dummy entries to
	; be popped off before an iret which takes us to from_unix.
	; Note that at_st_cs must be initialized to our cs.

;******************* WARNING: *******************
; realmode.s knows MUCH about this area of switch
; if you mess with it be sure you know the effects
; it will have on realmode.s initialization and
; any initialization in mem.asm!     -RKC
;------------------

;*************** ALL OF THIS STUFF IS KNOWN BY OFFSET FROM *****************
;*************** THE SEGMENT BEGINNING TO THE KERNEL STUFF *****************
;*************** IN REALMODE.S. MOVING THINGS AROUND COULD *****************
;*************** EASILY BREAK SOMETHING !!!!!!!!!!!!!!!!!! *****************

;*************** THIS SECTION IS INTENDED TO BE THE SAME   *****************
;*************** ACROSS ALL VERSIONS OF 286 MERGE          *****************

EVEN
signature	dw	0DABh, 0C0DEh			; offset 00
switchaddr	dw	offset switchcode, 0	; offset 04
retaddr		dw	from_unix, 0			; offset 08
switchcs	dw offset at_st_cs			; offset 0c
atstack		dw offset at_stack			; offset 0e

;*** The above data, plus the rest of the memory to SWITCH_OFFSET supplies
;*** padding in case a DOS program goes off the end of its memory space.
;*** It's okay to trash the above data because the kernel only uses it
;*** once at DOS inititialiation time.  OFFSET switchdata is hard coded
;*** as SWITCH_OFFSET in the kernel instead of passed above as it must
;*** be preserved while DOS is running.

;****************** SWITCH DATA STRUCTURE OFFSET ****************************

	if PADDING
    SWITCH_OFFSET	equ	100h
	else
	SWITCH_OFFSET	equ 10h
	endif

    ORG	SWITCH_OFFSET				; offset 10 (old) 100 (new)

switchdata	db	type sw_data dup (0)	

    MAGIC_OFFSET	equ	200h
    ORG	MAGIC_OFFSET					; offset 100 (old)  200 (new)

;*************** THIS SECTION IS SPECIFIC TO AT MERGE 286  *****************
;** (data in this section is known to the kernel relative to MAGIC OFFSET **

magic		dw	0afbh			; MAGIC OFFSET + 00

							; ireal ss:sp save of user stack
ir_spsave	dw	0			; MAGIC OFFSET + 02
ir_sssave	dw	0			; MAGIC OFFSET + 04

at_st_xtra	dw	8 dup (0)		; stack for new BIOS calls
at_stack	label	word		; MAGIC OFFSET + 22
at_st_ds	dw	0			
at_st_es	dw	0
at_st_regs	dw	8 dup (0)		; (general regs for popa)

OLDPHOENIXBIOS equ  0
if OLDPHOENIXBIOS
at_st_ax	dw	 0			; put in for BusinessLan/Wyse
endif
  
at_st_ip	dw	from_unix
at_st_cs	dw	0		
at_st_flags	dw	0	


;******************** END OF CRITICAL DATA REGION **************************
;***************************************************************************

;******************************^^^NOTICE^^^*********************************

	; We initialize our stack with the return address to a little
	; routine which switches to the user stack and lrets to him.
	; If you change the stack initialization, things will probably
	; break.
EVEN
ir_stackend:				; local stack
db 00h, 00h, 0dh, 0ah
db 'OS Merge Software by David Butterfield, David Blanset, J. David Peet,'
db 0dh, 0ah
db 'and Reid Kneeland.  Copyright (c) 1985 Locus Computing Corporation.'
db 0dh, 0ah
db 'ALL RIGHTS RESERVED.'
db 0dh, 0ah, 00h, 00h
EVEN
		dw	64 dup (0F00Dh)
ir_splow:
		dw	256 dup (0F00Dh)
ir_stackbase	dw	OFFSET CSEG:mem_lret_off
		dw	0FADEh		; mark end of stack
ir_myspsave	dw	OFFSET CSEG:ir_stackbase

	; variables
ir_intnum	dw	0		; interrupt vector/number temporary
ir_channel	dw	0		; hardware channel no for interrupt
oneshot		dw	1		; flag to do init code once only
one_change	dw	1		; one fake answer for disk change
  if NPX
fp_chip		dw	0		; 287 present? (see oneshot init code)
fp_state	dw	47 dup (0)	; 287 state info
  endif ; NPX
indos_ptr	dw	0, 0		; pointer to indos flag (if any)
inunix_ptr	dw	0, 0		; pointer to inunix flag (if any)
int_nest	dw	0		; interrupt nesting depth
tempword	dw	0		; for temp use
vecsave		dw	0, 0, 0		; for saving 30:fa - ff
jim		db	0				; Dead flag
seen_hot_key	db	0		; scan code if mode or screen hk hit
scr_or_mode	dw      0               ; hot key flag 0=sw.scr, 1=sw.mod
no_timer_ints	db	0		; for locking purposes
ticking		db	0		; for dosticks locking
mergemask	dw	0		; for masking int. in service.

s_tryboot		dw	TRYBOOT
s_wentinsane	dw	WENTINSANE



  if popfbug
iretins:
	iret
  endif

	; wait for the keyboard controller input buffer to be empty
	; (input means what the CONTROLLER thinks of as input)
kbc_ready proc	near
	in	al, KBSTATUS
	test	al, 2
	jnz	kbc_ready
	ret
kbc_ready endp

; coroutine switch to unix, expecting to return.
; On entry, ah!=0xff means switch to Unix, else means local function
; For Unix  al=sw_flag;  cx=sw_bufcount;  es:di=send_buffer;  ds:si=rec_buffer
switchcode proc	far
	; save stack and registers
	pushf
	cli
	switch_to_driver_stack
	push_frame

	push	ax
	call	update_isr
	pop	ax

if SSTRUCTPTR
	cmp	ah, 0ffh
	jnz	sw_for_unix
else
	cmp	ah, 0h
	jz	sw_for_unix
endif
if MERGEPRINT
	cmp	al,01h
	je	sw_setdevs		;FF01H - set dev_assigns
endif ;MERGEPRINT
	mov	indos_ptr, di		; set indos pointer (gag!)
	mov	indos_ptr+2, es
	mov	inunix_ptr, si		; set inunix pointer
	mov	inunix_ptr+2, ds
	jmp	sw_done

if MERGEPRINT
sw_setdevs:
	mov	ax, switchdata.sw_devassign	;Get current state
	and	ax, dx			; Assign devices to UNIX (0 bits)
	or	ax, cx			; Assign devices to DOS  (1 bits)
	mov	switchdata.sw_devassign, ax
	jmp	sw_done
endif ;MERGEPRINT

sw_for_unix:
if SSTRUCTPTR
	; set fields in switchdata from the registers
	mov     switchdata.sw_sbufaddr, ax
	mov     switchdata.sw_sbufaddr+2, dx
else
	; set fields in switchdata from the registers
	mov     switchdata.sw_flag, al
	mov     switchdata.sw_bufcount, cx
	mov     switchdata.sw_sbufaddr, di
	mov     switchdata.sw_sbufaddr+2, es
	mov     switchdata.sw_rbufaddr, si
	mov     switchdata.sw_rbufaddr+2, ds
endif

	; JJR memorial check for In Service bits.
	; He promised there wouldn't be any!
	mov	ax, READISR
	out	PC8259, al
	flush
	in	al, PC8259
	or	al, al
	jnz	sw_inservice
	mov	ax, READISR
	out	AT8259, al
	flush
	in	al, AT8259
	or	al, al
	jz	sw_noinservice
sw_inservice:
	inc	jim				; phasers on kill
	jmp	sw_strange
sw_noinservice:
	
	; get on with the switch to unix
	mov	bx, COSWITCH
	call	call_to_unix

sw_done:
	; restore stack and registers
	pop_frame
	switch_to_user_stack
 if (MASKINTS or NOTICK)
	call	chk_ticks
	popff
 else
	popff
	call	chk_ticks
 endif
	ret
switchcode endp

TIMER_LOW=6cH	; word
TIMER_HIGH=6eH	; word
TIMER_OFL=70H	; byte
; Use sw_dosticks to update the BIOS data area timer words
chk_ticks proc near
	cmp	switchdata.sw_dosticks, 0
	jnz	doticks
	jmp	noticks
doticks:
	push	es
	push	ax
	push	dx

if	TIMERMOD
	;Check and handle DOS timer sped up clock ticks...
	push	cx
	push	bx
	mov	cx, switchdata.sw_dosrate
	mov	switchdata.sw_dosrate, 0
	cmp	cx, 0f800H		;Normal rate?
	jae	no_speedup		;Yes -don't bother adjusting
	mov	ax, 0ffffH		;Normal DOS rate
	mov	dx, 0
	idiv	cx			;normal rate/current rate = 
	mov	cx, ax			; number of ticks/1 DOS tick.
	mov	bx, dx			;save fraction of a DOStick
	mov	dx, 0
	mov	ax, switchdata.sw_dosticks
	idiv	cx			;sw_dosticks/(ticks/DOStick) =
	mov	dx, switchdata.sw_dosticks
	mov	switchdata.sw_dosticks, ax	;Adjust DOSticks to update
	mov	ax,0
	cmp	bx,0
	je	no_remainder
	idiv	bx			;(sw_dosticks<<16)/sw_dosticks_rem.
no_remainder:
	mov	switchdata.sw_dosrate, ax	;fractional update
						; to sw_dosticks.
no_speedup:
	pop	bx
	pop	cx
endif ;TIMERMOD
	mov	ax, 40H
	mov	es, ax
	mov	ax, es:TIMER_LOW
	mov	dx, es:TIMER_HIGH
	add	ax, switchdata.sw_dosticks
	adc	dx, 0
	cmp	dx, 18H		; 24 hours?
	ja	is24
	jb	not24
	cmp	ax, 0b0H
	jb	not24
is24:
	sub	ax, 0b0H	; subtract 24 hours
	sbb	dx, 18H
	mov	byte ptr es:TIMER_OFL, 1
not24:
	mov	es:TIMER_LOW, ax
	mov	es:TIMER_HIGH, dx
if	TIMERMOD
	mov	ax, switchdata.sw_dosrate	;get fractional tick@dosrate
	mov	switchdata.sw_dosticks, ax	;save fractional dosticks
else
	mov	switchdata.sw_dosticks, 0
endif   ;TIMERMOD
	pop	dx
	pop	ax
	pop	es
noticks:
	ret
chk_ticks endp

switch	proc near
ife	KILLSYSINTS
; return to unix, not to come back -- no arguments
iret_to_unix:
	mov	bx, IRET

; call unix, expecting to return -- bx has interrupt number or 0ffxxh
call_to_unix:
	; set up ds
	push	cs
	pop	ds
	ASSUME DS:CSEG

else	; KILLSYSINTS 
call_to_unix:
	; set up ds
	push	cs
	pop	ds
	ASSUME DS:CSEG
	cmp	int_nest, 0
	je	goto_unix
	mov	bx, ABORT	; Someone attempted DOS call inside int.
	jmp	goto_unix

iret_to_unix:
	mov	bx, IRET
int_to_unix:
	; set up ds
	push	cs
	pop	ds
	ASSUME DS:CSEG
goto_unix:
 endif	; KILLSYSINTS 


	; safe from interrupts -- do static things every switch
	mov	ir_myspsave, sp
	mov     switchdata.sw_intvecnum, bx ; communicate int or fn number

	; enable upper address lines.  Note that we must wait for
	; controller to be ready for input after writing ENABLE20,
	; otherwise high bits may turn on too late.
; ->	; ALSO, do this as early as possible, because on some machines
	; even after the controller is ready it make take some microseconds
	; to take effect.
	call	kbc_ready
	mov	al, KB_WOP
	out	KBCMD, al
	call	kbc_ready
	mov	al, ENABLE20
	out	KBDATA, al
	call	kbc_ready

  if NPX
	; if we have a 287, save its state, then set it to protected mode
	cmp	fp_chip, 0
	jz	no287save
	mov     di, OFFSET CSEG:fp_state
	db 09bh		; wait
	db 0ddh, 035h	; fsave	[di]
	db 09bh		; wait
	db 0dbh, 0e4h	; fsetpm
no287save:
  endif ; NPX

	; put our stack pointer into BIOS block move stack save area
	; so we'll come back on reset
	mov	ax, 40h
	mov	es, ax
	mov	di, BMSTACKADDR
	mov	es:word ptr [di], OFFSET CSEG:at_stack
	mov	es:word ptr 2[di], cs
	; save last 3 words of IVT because they get clobbered on return
	mov	ax, 30h
	mov	es, ax
	mov	ax, es:[0fah]
	mov	vecsave, ax
	mov	ax, es:[0fch]
	mov	vecsave + 2, ax
	mov	ax, es:[0feh]
	mov	vecsave + 4, ax

	; setup memory management registers
	mov     di, OFFSET CSEG:switchdata.sw_gdtcopy
	
  if asm8086
	db 00fh, 001h, 015h
  else
	lgdt	[di]
  endif

	mov     di, OFFSET CSEG:switchdata.sw_idtcopy
  if asm8086
	db 00fh, 001h, 01dh
  else
	lidt	[di]
  endif

	; enter protected mode
  if asm8086
	db 00fh, 001h, 0e0h
  else
	smsw	ax
  endif

	or	ax, MSW_PE
	mov	tempword, ax	; AT Probe gets confused if this info
				; does not appear on the bus
	; make the jump to light speed
  if asm8086
	db	0fh, 01h, 36H
	dw	tempword
  else
	lmsw	tempword
  endif

	; flush instruction queue
	jmp	qflush
qflush:

	; branch to unix reentry routine
	jmp     DWORD PTR switchdata.sw_unxaddr
	ASSUME DS:NOTHING

; get here after reset
from_unix:
	; UNIX will set switchdata.sw_intvecnum to something other
	; than ABORT before doing a reset.  We always leave it set
	; to ABORT while running DOS programs to detect spurious
	; resets such as from <Ctrl><Alt><Del> or jumping to BIOS.

	cli
	cmp	switchdata.sw_intvecnum, ABORT
	je	from_strange
	jmp	really_from_unix

from_strange:
	; do not do any stack operations until new stack set up!
	cli
	xor	bx, bx			; make stack known location
	mov	ss, bx
	mov	sp, 400h
	switch_to_driver_stack
if SSTRUCTPTR
	mov		dx, cs
	mov		ax, offset s_tryboot
	mov     switchdata.sw_sbufaddr, ax
	mov     switchdata.sw_sbufaddr+2, dx
else
	mov     switchdata.sw_flag, TRYBOOT
endif
	jmp	sw_strange
insane:
if SSTRUCTPTR
	mov		dx, cs
	mov		ax, offset s_wentinsane
	mov     switchdata.sw_sbufaddr, ax
	mov     switchdata.sw_sbufaddr+2, dx
else
	mov     switchdata.sw_flag, WENTINSANE
endif
sw_strange:
	; We're dead, Jim.
if CTLALTFIX
; When CTL-ALT-DEL is pressed, make sure that when we get back to Unix,
; Unix does not think that the CTL and ALT keys are still down, because it
; may never see the break.
	and	byte ptr switchdata.sw_kbstate, 03fH
endif
	inc	jim
	; Send EOI to all DOS devices that are In Service
	mov	al, READISR
	out	AT8259, al
	flush
	in	al, AT8259
	mov	ah, al
	mov	al, READISR
	out	PC8259, al
	flush
	in	al, PC8259		; In Service
	not	bx
	and	bx, ax			; bx contains list of IS DOS devices
	je	eoidone
	mov	cx, 16			; number of bits to check
	mov	dx, 1			; bit we're working on
eoidevs:
	test	bx, dx			; should we EOI this one?
	je	notinservice
	mov	ax, 16
	sub	ax, cx			; device number
	cmp	al, 8			; which 8259?
	jge	eoi_slave
	or	ax, 60h			; specific EOI
	out	PC8259, al
	jmp	short notinservice
eoi_slave:
	and	al, 7
	or	al, 60h			; specific EOI
	out	AT8259, al
notinservice:
	shl	dx, 1			; next bit
	loop	eoidevs
eoidone:
	call	update_isr
  if NPX
	db	0dbh, 0e3h	; finit
  endif ; NPX

	; make sure the keyboard is enabled
kbelp:
	in	al, 64H
	test	al, 2
	jnz	kbelp		; wait for command to be accepted
	mov	al, 0aeH	; enable keyboard
	out	64H, al

	; see if we were already dead
	cmp	jim, 1
	ja	deadjim
	mov	bx, COSWITCH
	in	al, PC8259 + 1          ; mask off the keyboard
	or	al, KBMASK
	out	PC8259 + 1, al
	call	call_to_unix		; "shouldn't return"
	jmp	sw_strange
deadjim:
	mov	bx, ABORT
	call	call_to_unix		; "cannot return"
	jmp	sw_strange

really_from_unix:
	; safe from interrupts -- do static things every switch
					; restore stack
	mov	ax, cs
	mov	ss, ax
	mov	sp, ir_myspsave

	; the AT BIOS init code clobbers the end of the IVT (30:fa-ff)
	; so we must restore it before proceeding.
	mov	ax, 30h
	mov	ds, ax
	mov	ax, vecsave
	mov	ds:[0fah], ax
	mov	ax, vecsave + 2
	mov	ds:[0fch], ax
	mov	ax, vecsave + 4
	mov	ds:[0feh], ax

	push	cs
	pop	ds
	ASSUME DS:CSEG
	; restore 8259 interrupt masks.  All interrupts were turned off
	; before leaving protected mode because the BIOS block move code
	; enables interrupts.
	mov	ax, switchdata.sw_intmask
	out	PC8259 + 1, al
	mov	al, ah
	out	AT8259 + 1, al

	lock	nop				; foil a 386

	; do initialization code once only
	cmp	oneshot, 0
	jne	firsttime
	jmp	initdone

firsttime:
	; adjust stack if necessary
	mov	ax, word ptr ir_stackend+32
	cmp	ax, word ptr ir_stackend+124
	jne	adjust
	dec	ax
	xor	ah, al
	jz	noadjust
adjust:
	mov	ax, OFFSET CSEG:from_unix
	mov	ir_stackbase, ax
noadjust:
		; initialize the idt table
	call	intinit
  if NPX
		; Do we have a 287 ?
	db 09bh		; wait
	db 0dbh, 0e3h	; finit		; try to initialize 80287
	db 09bh		; wait
	db 0dfh, 0e0h	; fstsw ax	; get returned status
	or	al, al			; status 0 = 80287 present
	jnz	no287
	mov	fp_chip, 1
no287:
  endif ; NPX

	mov	oneshot, 0		; zap oneshot flag
	jmp	skipfirst		; leave this stuff alone first time

initdone:
	; This stuff doesn't happen the first time we switch
  if NPX
	; if we have a 287, restore its state 
	cmp	fp_chip, 0
	jz	no287restore
	mov     di, OFFSET CSEG:fp_state
	db 09bh		; wait
	db 0ddh, 025h	; frstor [di]
no287restore:
  endif  ; NPX

skipfirst:
	; This stuff happens every time we switch
	; get interrupt information
	mov     bx, switchdata.sw_intvecnum

	; Set up to detect spurious "reset"
	mov	switchdata.sw_intvecnum, ABORT
	mov	al, SHUTDOWN
	out	CMOSA, al
	flush
	mov	al, 9
	out	CMOSD, al

	; setup interrupt descriptor table register
	mov	di, OFFSET CSEG:idtinit

  if asm8086
	db 00fh, 001h, 01dh
  else
	lidt	[di]
  endif

	; see if this is an interrupt call, or a return or coreturn
	cmp	bh, 0ffh		; interrupt number 0ffh?
	jne	int_from_unix		; no, it is an interrupt

	; return or coreturn
ret_from_unix:
	; return whence call_to_unix was called
	ret

	; the very first time, ret_from_unix returns here
mem_lret_off:
	switch_to_user_stack
	lret				; ret far to snapshot program

	; interrupt from unix -- call routine and then return to unix
	; bh = channel no, bl = vector no
int_from_unix:
	inc	int_nest
	cmp	word ptr indos_ptr+2, 0
	jz	no_inc_indos
	les	di, dword ptr indos_ptr
	inc	es:byte ptr [di]	; grot!
no_inc_indos:
	cmp	word ptr inunix_ptr+2, 0
	jz	no_inc_inunix
	les	di, dword ptr inunix_ptr
	inc	es:byte ptr [di]
no_inc_inunix:

	xor	ax, ax			; look at dos user interrupt table
	mov	es, ax
	mov	cl, 2
	mov	al, bh			; interrupt channel no
	xor	bh, bh
	shl	bx, cl			; int number * 4 = vector address
	; if timer interrupt, make sure timer ints are allowed now
	cmp	al, CLOCKCHAN
	jne	chk_kchan

	; if the floppy is assigned to Unix, set the motor time out
	; word so that the BIOS will not turn the motor off.
	mov	cx, 40H
	test	cx, switchdata.sw_devassign
	jz	not_ufasn
	mov	es:440H, cx	; actual value doesn't matter much
not_ufasn:

	cmp	no_timer_ints, 0
	je	clk_bar
	inc	switchdata.sw_dosticks
	jmp	short aft_unix_int
clk_bar:
 if NOTICK
	mov	no_timer_ints, 1
 endif
chk_kchan:
	; if keyboard interrupt, check for the HotKey.
	cmp	al, KBCHAN
	jne	int_not_kb
	call	chk_hot
	jc	aft_unix_int
	mov	al, KBCHAN
int_not_kb:
	cmp	jim, 0			; are we dead jim?
	jz	intdeliver
	jmp	aft_unix_int
intdeliver:
	push	ax
	switch_to_user_stack
	pushf				; for iret in int routine
	call	es:dword ptr[bx]	; call user interrupt routine
	cli
	switch_to_driver_stack
	pop	ax
	cmp	al, KBCHAN
	jne	aft_unix_int
	call	aft_kb
aft_unix_int:
	; make sure that an EOI was done
	mov	ah, al		; channel no
	and	al, 7		; channel within an 8259
	or	al, 60h		; specific EOI for that channel
	test	ah, 8
	jnz	u_hw_slave
	out	PC8259, al	; EOI master
	jmp	short u_hw_done
u_hw_slave:
	out	AT8259, al	; EOI slave
	mov	al, SLAVEOI
	out	PC8259, al	; EOI slave channel in master
u_hw_done:
	call	update_isr

	cmp	word ptr indos_ptr+2, 0
	jz	no_dec_indos
	les	di, dword ptr indos_ptr
	cmp	es:byte ptr [di], 0
	jz	no_dec_indos
	dec	es:byte ptr [di]	; grot!
no_dec_indos:
	cmp	word ptr inunix_ptr+2, 0
	jz	no_dec_inunix
	les	di, dword ptr inunix_ptr
	cmp	es:byte ptr [di], 0
	jz	no_dec_inunix
	dec	es:byte ptr [di]
no_dec_inunix:
	; if we are leaving the outermost interrupt and we have seen
	; a hot key, let Unix know and forget that we saw it.
	dec	int_nest
	jnz	j_u_iret
 if NOTICK
	mov	no_timer_ints, 0	;Re-Allow timer ticks...
 endif
	mov	al, seen_hot_key
	or	al, al
	jz	j_u_iret
	cmp	scr_or_mode, 0   ; check if it's a switch screen or mode key
	jne     modekey
	mov	byte ptr [switchdata.sw_hotkeyhit + 1], al
	jmp	j_u_iret
modekey:
	mov	byte ptr [switchdata.sw_modkeyhit + 1], al
j_u_iret:
	mov	seen_hot_key, 0
	jmp	iret_to_unix		; return from interrupt to unix
	ASSUME DS:NOTHING

switch	endp

PAGE
; Here is the support code for handling interrupts in real mode.
; We use the IDT register in real mode to relocate the interrupt
; vector table to our own area.  We thus intercept all interrupts
; and look at them to find any hardware interrupts which should
; be handled by Unix.  Those we handle by switching to the protected
; mode interrupt handler.  DOS interrupts we handle by entering the
; DOS ISR whose address is found in the DOS-apparent interrupt vector
; table which starts at location 0.

intr	proc near

; Here are the initialization values for the IDT register.
idtinit	dw	1023			; limit
	dw	0			; low physaddr filled in by intinit
	dw	0			; high physaddr filled in by intinit

; Here is the relocated interrupt vector table.  The segments are
; filled in by the init routine below.
GENENTRY MACRO Z
	dw	i_&Z, 0
ENDM
EVEN
intreloc:
  X=0
  REPT 256
	GENENTRY %X
  X=X+1
  ENDM
PURGE GENENTRY

; Here are the actual interrupt service routines invoked through the
; relocated interrupt vector table.
GENLABEL MACRO Z
i_&Z:
ENDM
intcall:
  X=0
  REPT 256
	GENLABEL %X
	call	NEAR PTR ireal
  X=X+1
  ENDM
PURGE GENLABEL

; This routine is called at driver initialization time.
intinit:
	; initialize the cs fields in the relocated IDT
	mov	cx, 256			; 256 vectors to initialize
	mov	di, OFFSET CSEG:intreloc + 2	; point to first cs in table
intinit2:
	mov	word ptr cs:[di], cs		; point it to our own cs
	add	di, 4			; next one
	loop	intinit2		; repeat for entire table

  if ATPROBE
	; *** TESTING ***  Set up probe vectors
	mov	word ptr intreloc + 4, offset int1
	mov	word ptr intreloc + 6, cs
	mov	word ptr intreloc + 8, offset int2
	mov	word ptr intreloc + 10, cs
	mov	word ptr intreloc + 12, offset int3
	mov	word ptr intreloc + 14, cs
  endif

	; initialize the idtinit area with physaddr of relocated IDT
	mov	ax, cs			; get physaddr of table
	mov	dx, cs
	mov	cl, 12
	shr	dx, cl			; high 4 bits
	mov	cl, 4
	shl	ax, cl			; low 12 bits of segment
	add	ax, OFFSET CSEG:intreloc	; low 16 bits of physaddr
	adc	dx, 0			; propagate carry
	mov	idtinit+2, ax		; low physaddr
	mov	idtinit+4, dx		; high physaddr

	ret

  if ATPROBE
; *** TESTING ***  Special interrupt handlers for interrupts that we want
; to allow to happen via CURRENT contents of IVT at 0:0 without using our
; stack.
int1:	push	bx
	mov	bx, 1
	jmp	intx
int2:	push	bx
	mov	bx, 2
	jmp	intx
int3:	push	bx
	mov	bx,3
intx:	shl	bx, 1
	shl	bx, 1
	push	ax
	xor	ax, ax
	push	ds
	mov	ds, ax
	mov	ax, ds:[bx]
	mov	cs:intxo, ax
	mov	ax, ds:2[bx]
	mov	cs:intxs, ax
	cmp	ax, 0f000h
	jne	notbios
	int	3
notbios:
	pop	ds
	pop	ax
	pop	bx
	jmp	cs:intxp

intxp	label	dword
intxo	dw	0
intxs	dw	0
  endif

; All interrupts in real mode come here.  On the stack is the flags,
; the cs, and the ip of the interrupted code, and the ip of the
; instruction following one of the i_0, i_1, ... call instructions.
; We determine which interrupt occurred, and then route to the real
; or protected mode handler.
ireal:
	pop	ir_intnum		; get interrupt address

	;NOTICE: following needed for double interrupt checking ALSO!
if DOUBLEINT
	pop	switchdata.sw_dosip	; for profiling
	pop	switchdata.sw_doscs	; for profiling
	push	switchdata.sw_doscs	; for profiling
	push	switchdata.sw_dosip	; for profiling
endif

; Push 3 extra words on the stack to make room for the iret hack.
; Do so without affecting the current flags!
	pushf
	pushf
	pushf				; instead of pushf; sub sp, 4

	switch_to_driver_stack
	push_frame
	pushf

	; determine int number and put in cs:ir_intnum
	mov	bl, 3			; three bytes per i_?
	mov	ax, ir_intnum		; addr of i_? +3
	sub	ax, OFFSET CSEG:i_1	; subtract (base of table + 3)
	div	bl			; interrupt number in al
	xor	ah, ah			; we don't wanna know if this is not 0
	mov	ir_intnum, ax		; save it back

	; On the AT we have to determine whether this was a hardware
	; interrupt by looking for new bits in the ISR.  Since we do
	; do not use "special fully nested mode" we can assume that
	; it was not hardware if the primary 8259 ISR didn't change.
	; If the primary 8259 ISR bit for the slave just turned on and
	; no secondary ; 8259 ISR bit is on, we assume this is a level 7 phantom
	; interrupt in the slave.  Phantom interrupts in the primary
	; are detected by remembering where the primary 8259 base is
	; and assuming that we don't get software interrupts that
	; coincide with its channel 7.
	mov	al, READISR
	out	AT8259, al
	flush
	in	al, AT8259	; read slave ISR
	mov	ah, al
	mov	al, READISR
	out	PC8259, al
	flush
	in	al, PC8259	; read master ISR
	mov	bx, ax		; bx is a copy
	xor	ax, switchdata.sw_isr
	and	ax, bx		; ax is bits currently on in 8259 that
					; were not on in sw_isr
	jnz	isr_changed	; jump if new ISR bit(s) turned on
	; Could get here if an interrupt reoccurred within its
	; own interrupt handler eg if handler does the EOI with interrupts
	; enabled.  In that case we will treat the interrupt as software
	; which should not cause a problem since it is a DOS interrupt and
	; we will do the normal cleanup on return from the original one.
	; One exception to this is we could miss a switchscreen character.
	;
	; if the ISR did not change, interrupt was either software or
	; phantom.  Assume phantom if coincident with channel 7 of
	; last known base of primary 8259.  (Note that we could lose
	; genuine software interrupts here.  Should not be a problem
	; with expected values of 8259 base.)
	mov	al, switchdata.sw_lastprimary
	or	al, 7
	cmp	al, byte ptr [ir_intnum]
	je	priphan
	jmp	irdos_sw	; it was a DOS software interrupt
priphan:
	jmp	poppop		; it was probably a phantom
isr_changed:
	mov	switchdata.sw_isr, bx	; save most accurate info in sw_isr
	; next two lines handle bug under which two interrupts are allowed
	; to strike.  If there is a clock or keyboard interrupt active we
	; are handling one of them and so are a primary channel.  Otherwise
	; we are secondary if slave bit is on.
	test	al, CLOCKMASK or KBMASK
	jnz	int_corkb
	test	al, SLAVEMASK		; did slave bit change?
	jnz	secondary
int_corkb:
	mov	al, byte ptr [ir_intnum]
	mov	switchdata.sw_lastprimary, al
	and	al, 7		; channel number
	jmp	short primary
secondary:
	or	ah, ah
	jnz	not_phantom
	; we just got a phantom interrupt in the slave 8259.
	; we KNOW it was a phantom because the primary 8259 slave
	; in-service bit turned on but no slave ISR bit is on.
	mov	al, SLAVEOI
	out	PC8259, al
	flush
	call	update_isr
	jmp	poppop
not_phantom:
	mov	al, byte ptr [ir_intnum]
	and	al, 7
	or	al, 8
primary:	; it was a hardware interrupt for channel al
	inc	int_nest
 if MASKINTS
  if NOTICK
	cmp	al, CLOCKCHAN	;Don't mask out DOS timer channel
	je	skip_maskint
  endif ;NOTICK
	cmp	al, KBCHAN	;Don't mask out keyboard channel
	je	skip_maskint
	push	ax
	mov	cl, al
	mov	ax, 1
	shl	ax, cl		;Get bit position for channel being serviced
	or	mergemask, ax	;  ...mask off interrupt we're servicing
	in	al, AT8259+1	;Get Slave irq mask
	flush
	mov	ah, al
	in	al, PC8259+1	;Get Master irq mask
	flush
	or	ax, mergemask	;Mask off all irq's in mergemask also...
	out	PC8259+1, al	;Mask off Master
	flush
	mov	al, ah
	out	AT8259+1, al	;Mask off Slave
	flush
	pop	ax
  if NOTICK
skip_maskint:
  endif ;NOTICK
 endif ;MASKINTS

	xor	ah,ah
	; ax now contains the 8259 channel no 0-7 (primary) or 8-f (2nd)
	mov	ir_channel, ax
	;Check for double interrupt bug
if DOUBLEINT
	mov	bx, cs
	cmp	switchdata.sw_doscs, bx
	jne	int_no_double
	mov	cx, switchdata.sw_dosip
	cmp	cx, offset CSEG:i_0
	jb	int_no_double
	cmp	cx, offset CSEG:i_255
	ja	int_no_double
	sub	cx, offset CSEG:i_0	;We got double interrupt!
	push	ax			;Calculate interrupt vector
	mov	ax, cx
	mov	cx, 3			;offset i_1 - offset i_0
	div	cl
	mov	cx, ax
	pop	ax
	jmp	int_double		;Go handle double interrupt
endif
int_no_double:
	; check the interrupt assignment mask (0 = DOS, 1 = UNIX)
	mov	cx, ir_channel
	mov	bx, 1
	shl	bx, cl
	mov     ax, switchdata.sw_devassign        ; interrupt assignment word
	and	ax, bx			; see if the int is for us or unix
	jz	irdos_hw		; it's for us
irunix1:
	jmp	irunix			; it's for unix -- go call switch

; This hardware interrupt is for DOS.
irdos_hw:
	; bx contains the mask bit for the active channel
	; if it's a clock interrupt, check for timer lockout
	; and possible motor time out word reset.
	cmp	bx, CLOCKMASK
	jne	not_clk

	; if the floppy is assigned to Unix, set the motor time out
	; word so that the BIOS will not turn the motor off.
	mov	ax, 40H
	test	ax, switchdata.sw_devassign
	jz	not_ufasgn
	mov	es, ax
	mov	es:40H, ax	; actual value doesn't matter much
not_ufasgn:

	cmp	no_timer_ints, 0
 if NOTICK
	mov	no_timer_ints, 1
 endif
	je	not_clk
	inc	switchdata.sw_dosticks
	mov	cx, CLOCKCHAN
	jmp	short do_eoi
not_clk:
	; if it's a keyboard interrupt, check for the hot key
	cmp	bx, KBMASK		; keyboard interrupt?
	jne	dos_nkb
	call	chk_hot
	jnc	dos_nkb
 if MASKINTS
	push	bx			; Save channel for return
 endif ;MASKINTS
	jmp	iret_hard

dos_nkb:
	; deliver the hardware interrupt to DOS
	mov	bx, ir_intnum		; vector number
	xor	ax, ax			; look at dos user interrupt table
	mov	es, ax
	mov	cl, 2
	shl	bx, cl			; int number * 4 = vector address

	mov	ax, ir_channel		; we'll need channel on return
	push	ax

	switch_to_user_stack
	pushf				; for iret in int routine
	call	es:dword ptr[bx]	; call user interrupt routine
	cli
	switch_to_driver_stack
	pop	cx			; the channel for this int
 if MASKINTS
	push	cx			; Channel needed by iret_hard
 endif ;MASKINTS
	; put out a channel-specific EOI in case interrupt handler did not
 do_eoi:
	mov	ax, cx
	and	ax, 7
	or	ax, 60h			; Specific EOI command
	test	cx, 8
	jnz	d_hw_slave
	out	PC8259, al
	jmp	dos_hw_done
d_hw_slave:
	out	AT8259, al
	mov	al, SLAVEOI
	out	PC8259, al
dos_hw_done:
	call	update_isr
	cmp	cx, KBCHAN
	jne	dos_notkb
	; if the interrupt was from the keyboard, we have turned off the
	; timer interrupts.  Restore the previous state.
	call	aft_kb
dos_notkb:
dos_notclock:
	jmp	iret_hard

;*** Use following tables carefully - be sure to put address in ihdl_tbl
;    and index to address in ihdl_tbl in ids_tbl.
;	Be sure you no how these tables are used before changing them!

ids_tbl: ; *** Software interrupt - handler index translation table ***
it00_0f:	db	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
it10_1f:	db	6, 0, 0, 2, 3, 4, 0, 5, 0, 7, 0, 0, 0, 0, 0, 0
max_int 	equ	01fH 
;;; it20_2f:	db	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
;;; it30_3f:	db	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
;;; it40_4f:	db	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
;;; it50_5f:	db	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
;;; it60_6f:	db	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
;;; it70_7f:	db	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
;;; it80_8f:	db	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
;;; it90_9f:	db	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
;;; ita0_af:	db	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
;;; itb0_bf:	db	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
;;; itc0_cf:	db	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
;;; itd0_df:	db	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
;;; ite0_ef:	db	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
;;; itf0_ff:	db	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0

ihdl_tbl: ; *** Special software interrupt handler table ***
hdl_0:		dw	sw_nodev
hdl_1:		dw	ret_carry
hdl_2:		dw	sw_13
hdl_3:		dw	sw_14
hdl_4:		dw	sw_15
hdl_5:		dw	sw_17
hdl_6:		dw	sw_10
hdl_7:		dw	sw_19


irdos_sw:
; Software interrupt for DOS.  We check for BIOS requests and use them
; to allocate devices to DOS.
	push	cs
	pop	ds
	mov	ax, ir_intnum
	cmp	ax, max_int		;intnum outside translation table?
	ja	isw_nodev		;yes - just pass it thru
	mov	bx, offset ids_tbl
	xlat				;Translate int# into handler index
	mov	bx, ax
	shl	bx,1
	jmp	word ptr ihdl_tbl[bx]	;And go handle special software int's.

isw_nodev:
	xor	bx,bx
	jmp	word ptr ihdl_tbl[bx]	;Go handle int...

;------------
ret_carry:		; return from interrupt with carry set
	pop	ax
	pop_frame
	switch_to_user_stack
	add	sp, 6
	push	bp
	mov	bp, sp
	or	word ptr 6[bp], CARRYFLAG	; set carry in flags
	pop	bp
	iret		; return from the original interrupt

;------------- FLOPPY
sw_13:
	test	byte ptr SAVEDL[bp], 80h
	jnz	sw_13h		; jump if hard disk req
	mov	ax, FDMASK
	call	assign
	jl	sw_ufloppy
;
	cmp	one_change, 0	; first time asked for disk change status,
	je	gsw_nodev	; we will always say yes
	cmp	byte ptr SAVEAH[bp], 16H	; disk change ?
	jne	gsw_nodev
	dec	one_change
	mov	ax, 40H
	mov	es, ax
	mov	byte ptr es:41H, 80H	; set diskette_status to time_out
	mov	byte ptr SAVEAH[bp], 6  ; disk change line active
	jmp	ret_carry
;
gsw_nodev:
	jmp	sw_nodev
sw_13h:			; hard disk request - prohibit fixed disk use by DOS
sw_ufloppy:		; UNIX is using the floppy
	mov	byte ptr SAVEAH[bp], 80h	; status = timeout
	jmp	ret_carry

;------------- SERIAL
sw_14:
	cmp	word ptr SAVEDX[bp], 1
	ja	gsw_nodev	;Just go ahead and pass it...
	mov	ax, 10h
	jne	sw_14_0
	mov	ax, 8
sw_14_0:
	call	assign
	jl	sw_14u
	jmp	sw_nodev
sw_14u:			; DOS wants async dev; UNIX is using it
sw_14b:			; DOS asked for bad async device
	mov	word ptr SAVEAX[bp], 8000h	; async timeout
	jmp	poppop

;------------- CASSETTE/SPECIAL
sw_15:
	cmp	byte ptr SAVEAH[bp], 90h	; Device busy loop?
	je	sw_15db
	jmp	gsw_nodev
sw_15db:
;;;	mov	bx,IRET
;;;	call	call_to_unix		;return to unix.
	clc				;return with carry clear.
	jmp	poppop

;------------- PARALLEL
sw_17:
	cmp	word ptr SAVEDX[bp], 1
	ja	sw_17b
	mov	ax, 80h
	jne	sw_17_0
	mov	ax, 20h
sw_17_0:
ife MERGEPRINT
	call	assign
	jl	sw_17u
else ;MERGEPRINT
	test	ax, switchdata.sw_devassign
	jz	sw_17u
endif ;MERGEPRINT
	jmp	sw_nodev
sw_17u:			; tried to use a printer that's in use by UNIX
sw_17b:			; tried to use a printer > 1
	mov	byte ptr SAVEAH[bp], 1
	jmp	poppop

;-------------- VIDEO
sw_10:
	cld
	mov	ax, word ptr SAVEAX[bp]
	cmp	ah, 0		; check if it's funct 0
	jne	func10
; initialize egastate to 0ffh
	push	cs
	pop	es
	mov 	di, OFFSET CSEG:switchdata.sw_egastate
	mov 	cx, 18
	mov	al, 0ffh
	rep	stosb
	jmp	sw_nodev
func10:
	cmp	ah, 10h		; check if it's funct. 10
	jne	sw_nodev
	mov	bx, word ptr SAVEBX[bp]
	cmp	al, 0h
	jne	chk1
	mov	al, bh
	xor	bh, bh
	mov	byte ptr switchdata.sw_egastate[bx], al
	jmp	sw_nodev
chk1:
	cmp 	al, 1
	jne	chk2
	mov	byte ptr switchdata.sw_egastate[16], bh
	jmp	sw_nodev
chk2:
	cmp 	al, 2
	jne	chk3
	push	ds
	push	es	; place user ES in DS (assumes ES has not changed)
	pop	ds
	mov	si, word ptr SAVEDX[bp]
	push	cs
	pop	es
	mov	di, OFFSET CSEG:switchdata.sw_egastate
	mov	cx, 17
	rep	movsb
	pop	ds
	jmp	sw_nodev
chk3:	
	cmp	al, 3
	jne	sw_nodev
	mov	byte ptr switchdata.sw_egastate[17], bl
	jmp	sw_nodev
;--------------
sw_19:
	mov	ax, 0
	mov	ds, ax
	mov	bx, 400h	;BIOS data area
	mov	ax, [bx+90h]	;Floppy media state
	and	ax,0efefH	;Turn off media state determined for both
	cmp	ah,0
	je	sw_19a
	mov	ah,80h
sw_19a:
	cmp	al,0
	je	sw_19b
	mov	al,80h
sw_19b:
	mov	[bx+90h], ax
	mov	word ptr [bx+8Bh], 0000h
	mov	word ptr [bx+92h], 0000h
;	mov	word ptr [bx+94h], 0ffffh
	jmp	sw_nodev	;...and continue on to bootstrap.

;--------------
; Could add tests here for interrupts 
; 18h (ROM BASIC), 19h (bootstrap).  For now just let them work normally.

; This interrupt is for DOS, the int number is in ir_intnum.  We need to
; enter the DOS interrupt service routine with all registers and flags
; exactly as they were when we first got control.  We do this by setting
; up a stack frame with our initial flags and the DOS ISR address on it
; and then execute an iret instruction to get there.
sw_nodev:
	mov	bx, ir_intnum		; interrupt number in bx
	xor	ax, ax			; look at dos user interrupt table
	mov	es, ax
	mov	cl, 2
	shl	bx, cl			; int number * 4 = vector address
sw_getaddr:
	; get on with the call
	lds	si, dword ptr ir_spsave
	pop	ax			; put flags into stack frame
	mov	4[si], ax
	mov	ax, es:2[bx]		; cs of ISR
	mov	2[si], ax
	mov	ax, es:[bx]		; ip of ISR
	mov	[si], ax
	pop_frame
	switch_to_user_stack
	iret				; restore flags and stack and enter ISR
	; The ISR will iret to the interrupted code

if DOUBLEINT
;Control returns here after first of double interrupt bug is processed.
;This handles setup for interrupted interrupt before resuming in ireal.
dblintr:
	pop	ir_intnum		;Make everything look just like
	pop	switchdata.sw_dosip	;we came thru ireal:
	pop	switchdata.sw_doscs
	push	switchdata.sw_doscs
	push	switchdata.sw_dosip
	pushf
	pushf
	pushf

	switch_to_driver_stack
	push_frame
	pushf

	mov	al, READISR
	out	AT8259, al
	flush
	in	al, AT8259
	mov	ah, al
	mov	al, READISR
	out	PC8259, al
	flush
	in	al, PC8259
	mov	cx, ir_intnum		;Get channel # saved in int_double:
	mov	bl, switchdata.sw_lastprimary
	and	bx, 00f8h
	or	ir_intnum, bx		;Make ir_intnum valid
	mov	bx, ax			;Save isr in bx for isr_changed:
	mov	ax,1
	shl	ax,cl			;ax = bits in isr that changed.
	test	ax,0ff00h		;check if slave was interrupted
	jnz	set_slave
	jmp	isr_changed		;Let ireal handle it now...
set_slave:
	or	ax,4			;Or in slave bit also
	jmp	isr_changed		;And let ireal finish...



; Handle the double interrupt bug.
int_double:

	switch_to_user_stack

	push	bp
	mov	bp,sp
	push	bx		;save isr bit we're currently handling.
	mov	bx, cx		;get interrupted vector
	push	ds		;Save registers we'll kill
	push	es
	push	si
	push	di
	pushf
	cld
	mov	si, sp		;Set from address
	sub	sp, 2		;Pre-adjust stack for pops later
	sub	bp, 2		;Pre-adjust bp frame also
	mov	di, sp		;... and set to address
	mov	cx, ss
	mov	ds, cx		;setup for stack segment
	mov	es, cx
	mov	cx, 13		;Move entire interrupt frame
	rep	movsw		;Move 13 words of stack down
	mov	14[bp], bx	;Save vector number in "prior" int. frame
	mov	10[bp], cs	;Set segment of dblintr:
	mov	8[bp], offset CSEG:dblintr	;Set to return to dblintr:
	and	12[bp], 0fdffh		;Ensure interrupts of at dblintr:
	popff
	pop	di
	pop	si
	pop	es
	pop	ds
	pop	bx
	pop	bp

	switch_to_driver_stack

	jmp	int_no_double	;Resume processing as though nothing
				;silly happened.
endif

; This interrupt is for Unix.
; Switch to unix, passing the channel number.  When we come
; back we restore registers and continue on with the interrupted program.
irunix:
	mov	bx, ir_channel
irunix2:
 if MASKINTS
	push	bx		; Channel needed for iret_hard
 endif ;MASKINTS
 if KILLSYSINTS
	call	int_to_unix
 else ; !KILLSYSINTS
	call	call_to_unix
 endif ; !KILLSYSINTS
iret_hard:
	; we have finished handling an interrupt that struck while we were
	; in DOS.  Before returning to the DOS process, check whether this
	; is the outermost interrupt AND we have seen the hot key.  If so
	; do hot key switch to Unix.
 if MASKINTS
	pop	cx		;Get channel# just serviced
  if NOTICK
	cmp	cl, CLOCKCHAN
	je	skip_unmask	;Don't need to unmask non-masked DOS timer
  endif ;NOTICK
	cmp	cl, KBCHAN
	je	skip_unmask	;...or keyboard
	mov	bx, 1
	shl	bx, cl		;Turn on bit# for channel
	not	bx
	and	mergemask, bx	;Unmask channel from those in service
	in	al, AT8259+1	;Get current irqs being masked off
	flush
	mov	ah, al
	in	al, PC8259+1
	and	ax, bx		;Unmask channel serviced from hardware masks
	flush
	out	PC8259+1, al
	flush
	mov	al, ah
	out	AT8259+1, al
  if NOTICK
skip_unmask:
  endif ;NOTICK
 endif ;MASKINTS

	dec	int_nest
	jnz	poppop
 if NOTICK
	mov	no_timer_ints, 0
 endif
	mov	bl, seen_hot_key
	or	bl, bl
	jz	poppop
	mov	bh, 0feh	; special hot key switch indicator
	mov	seen_hot_key, 0
	call	call_to_unix
	pop	ax		; old flags
	pop_frame
	switch_to_user_stack
	add	sp, 6
 ife (MASKINTS or NOTICK)
	sti
 endif
	call	chk_ticks	; check for missed timer ticks
	iret

poppop:
	pop	ax			; flags -- we don't need them

popregs:
	pop_frame
	switch_to_user_stack
	push 	ax
	call	update_isr
	pop	ax
	add	sp, 6			; the ip of i_? plus the 4 subtracted
	iret				; return to interrupted code.
intr endp

update_isr proc     near
; read the current in service register and update sw_isr accordingly
; WARNING: this routine trashes ax

	mov	al, READISR
	out	AT8259, al
	flush
	in	al, AT8259	; read slave ISR
	mov	ah, al
	mov	al, READISR
	out	PC8259, al
	flush
	in	al, PC8259	; read master ISR
	mov	switchdata.sw_isr, ax	; Make sure sw_isr up to date
	ret
update_isr endp

chk_hot	proc	near
	; It's the keyboard -- Check for the switchscreen character.
	; There is no non-destructive read of the keyboard input.  We
	; disable the keyboard then do a destructive read.  Theory is
	; that next character cannot arrive until keyboard is enabled
	; and that in the meantime reads will give this character again.
	; We assume that the interrupt handler we jump to will disable
	; the keyboard and re-enable it when done.  This has been
	; tested successfully under DOS with the BIOS keyboard interrupt
	; routine but might not work with programs that take over the
	; keyboard.  Note that we cannot do a transition to protected
	; mode before the real interrupt routine reads the character
	; because of limitations in the keyboard controller design and
	; the fact that the gate_A20 line is on the same keyboard
	; controller port as the keyboard interface signals.  Since
	; we do not give the cpu to real mode when the screen (and
	; hence keyboard) belong to UNIX, the only time we have to worry
	; about is when a timer interrupt occurs during keyboard interrupt
	; handling.  We prevent that from happening by turning timer 
	; interrupts off while we process the keyboard interrupt.
	; Note that there is an analagous problem with returning to
	; real mode from protected mode.  We prevent it by not looking
	; for the switchscreen character until we are in real mode when
	; DOS has the screen but we are in protected mode when a keyboard
	; interrupt strikes.
	push	ax
	call	kbc_ready
	mov	al, DISABLEKB
	out	KBCMD, al
	in	al, KBDATA
	mov	cx, ax			; save for later
	mov	ah, al
	and	ah, 7fh			; forget make/break bit
	mov	dx, switchdata.sw_swscrchar ; get the switchchar info
	mov	scr_or_mode, 0          ; scr_or_mode=0 means switch screen
	cmp     ah, dl			; is it interesting?
	je	hot
chk_swmod:
	mov	dx, switchdata.sw_swmodchar ; check for switchmode char
	mov	scr_or_mode, 1          ; scr_or_mode=1 means switch mode
	cmp	ah, dl
	jne     not_hot

	; cntrl/alt/shift all correct?
hot:
	mov	dl, switchdata.sw_kbstate
	and	dl, 0f7h			; ignore alt state
	cmp	dh, dl				; is the kb state right?
        je	real_hot
	cmp	scr_or_mode, 0          ; not switch screen
	jne	not_hot               
	jmp	chk_swmod               ; go back and check for switch mode

	; yep, that's our cue to change screens.  Clean up ready for
	; next keyboard interrupt.
real_hot:
	mov	al, 61h			; keyboard-specific EOI
	out	PC8259, al
	call	update_isr
	call	kbc_ready
	mov	al, ENABLEKB
	out	KBCMD, al
	cmp	scr_or_mode, 0
	je	seen_hot
	mov	byte ptr [switchdata.sw_modkeyhit + 1], cl
	jmp	yeah
seen_hot:
	mov	byte ptr [switchdata.sw_hotkeyhit +1], cl
yeah:
	mov	seen_hot_key, cl
	stc
	pop	ax
	ret

	public	not_hot		; TESTING ONLY!!!!!
not_hot:
	; we keep track of ctrl, alt, and shift key state
	mov	ax, cx			; restore keystroke

if	CHKRESET
	and	al,07fh		;mask off make/break bit
	cmp	al,104		;DEL key?
	jne	no_chkreset
	mov	al, switchdata.sw_kbstate
	and	al, 0ch		;mask off all but ctrl&alt states
	cmp	al, 0ch		;ctrl+alt?
	jne	no_chkreset

	mov	al, 61h			; keyboard-specific EOI
	out	PC8259, al
	call	update_isr
	call	kbc_ready
	mov	al, ENABLEKB
	out	KBCMD, al

	jmp	deadjim		;abort DOS process
no_chkreset:
	mov	ax, cx
endif	;CHKRESET

	call	chk_kys
casedone:
	clc
	pop	ax
	ret
chk_hot	endp

;Table of key state bits & key scancodes
kyscan	db	38h,1dh,2ah,36h,52h,3ah,45h,46h
kystate	db	08h,04h,02h,01h,80h,40h,20h,10h

;Notice! - Trashes AX,CX,DI
chk_kys	proc	near
	push	es
	mov	cx, cs
	mov	es, cx
	mov	ah,al			;Get keyboard code
	and	al,7fh			;Save w/o make/break bit in al
	mov	di,offset kyscan
	mov	cx,8
	cld
kytest:
	repne	scasb			;kyscan = scan code?
	je	kycheck
	jmp	kydone			;No match - finish up
kycheck:
	mov	al, es:[di + ((kystate - kyscan) - 1)]	;Get keystate
	test	ah, 80h			; Break code?
	jnz	kynot			; Yes
	or	switchdata.sw_kbstate, al ;No- turn on state bit
	jmp	kydone
kynot:
	not	al
	and	switchdata.sw_kbstate, al ;Turn off state bit
kydone:
	pop	es
	ret
chk_kys	endp

aft_kb	proc near
	; re-enable keyboard in case interrupt handler doesn't
	push	ax
	call	kbc_ready
	mov	al, ENABLEKB
	out	KBCMD, al
	pop	ax
	ret
aft_kb	endp

; try to assign a device to DOS.  Return ax = 0 if already assigned, 1 if
; newly assigned (also turn on device interrupts), -1 if cannot assign.
; Set flags accordingly.
assign	proc	near
	test	ax, switchdata.sw_devassign
	jnz	not_assigned
	mov	ax, 0
ass_ret:
	or	ax, ax
	ret
not_assigned:
	test	ax, switchdata.sw_devinuse
	jnz	ass_bad
	not	ax
	and	switchdata.sw_devassign, ax
	push	ax
	in	al, AT8259 + 1
	and	al, ah
	out	AT8259 + 1, al
	pop	ax
	mov	ah, al
	in	al, PC8259 + 1
	and	al, ah
	out	PC8259 + 1, al
	mov	ax, 1
	jmp	short ass_ret
ass_bad:
	mov	ax, -1
	jmp	short ass_ret
assign	endp

CSEG	ENDS
END	BEGIN
