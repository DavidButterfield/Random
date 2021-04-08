	.file "realmode.s"
/   realmode.s SCCSID(@(#)realmode.s	1.6   LCC)  Mod Time 12:42:55 8/7/87

/	@(#)	1.16	

/
/									
/	Name:								
/		realmode.s -- mode switching functions		
/									
/ 	Description:							
/		This is the Protected Mode side of the mode 		
/		switching functions, and also the first level
/		IDT interface.
/									
/	Copyright (c) 1986 Locus Computing Corporation	
/									
/

/ NB: On the Microport system, calls to C code will result in all registers
/ being destroyed (potentially) other than SS and CS!

#ifdef ATMERGE

#include "../sys/realmode.h"
#include "../sys/mmu.h"
#include "../sys/8259.h"
#include "../sys/clock.h"

/***** Offsets where switch.sys leaves offsets to important data *******/

	.data

SHUT_DOWN =  0x8f		/ offset of shutdown byte in CMOS
CMOS_PORT =  0x70
SHUT_CMD  =  0xfe		/ shutdown command to status port
STATUS_PORT= 0x64
PC8259    =  0x20		/ primary 8259 port
AT8259	  =  0xa0		/ slave 8259 port
READ_ISR  =  0x0b		/ 8259 command to read in-service register
SLAVE_EOI =  0x62		/ 8259 command to EOI the slave controller
RESET_SP  =  0x67		/ offset in b. comm. area of SP used at reset
RESET_SS  =  0x69		/ offset in b. comm. area of SS used at reset

F_SWITCH = 0xffff		/ code from DOS indicating a co-routine switch
F_IRET   = 0xfffe		/ code from DOS indicating return from a call
F_ABORT  = 0xfffd		/ code from DOS indicating suicide
FH_RET   =   0xff		/ high byte of SWITCH, IRET, or ABORT message
FH_HOTKEY=   0xfe		/ high byte of a HOTKEY message

SWITCHCS	=	0x0c	/ points to CS location on switch stack
MAGICOFF	=	0x200	/ points to beginning of a bunch of stuff
ATSTACK		=	0x0e	/ points to beginning of at_stack

SWOFFSET  	=  	0x100	/ known offset of switchdata in switch

MERGE_ADDR	=	0xa2	/ BIOS data offset of passed stack information

/ Code below assumes that these data items live in the same segment.
usssave:	.value	0	/ place to stash ss while dossing
uspsave:	.value	0	/ place to stash sp while dossing
utsssave:	.value	0	/ place to stash tss while dossing
uldtsave:	.value	0	/ place to stash ldt while dossing
umswsave:	.value	0	/ place to stash msw while dossing
udosnest:	.value	0	/ number of nested calls_to_DOS
ufpsave	:       . = .+94	/ place to stash fp state while dossing

	.globl dossize	                / current size DOS space (must init 1)
dossize:	.value 1
	.globl dosintmask      		/ unix copy of DOS interrupt mask
dosintmask:	.value 0  
	.globl dosok            	/ Flag to say if we may go to DOS
dosok:		.value 0
	.globl	msmmask			/ IRQ bits belonging to DOS
msmmask:	.value	0

	.globl	dosnewdriver
dosnewdriver:	.value	0		/ new switch.sys needs initializing.
	.globl	dosdriver
dosdriver:	.value	0		/ address of high memory driver

/ Unix copy of switch_data stucture 
	.globl usw_data
	.even
usw_data:
usw_sbufaddr:	.value	0, 0	/ send buffer address
usw_rbufaddr:	.value	0, 0	/ receive buffer address
usw_altscreen:	.value	0, 0	/ location of alternate screen
usw_gdtcopy:	.value	0,0,0,0	/ copy of gdt alias from gdt
usw_idtcopy:	.value	0,0,0,0	/ copy of idt alias from gdt
usw_unxaddr:	.value	from_dos / off of unix switch ret code
usw_unxsel:	.value  <s>from_dos    / sel of unix switch ret code
usw_memsize:	.value	0	/ size of DOS memory (copied)
	.globl	usw_devassign
usw_devassign:	.value	0xffff	/ assign bit mask: 1=unix, 0=dos
usw_devinuse:	.value	0	/ current Unix devices in use
usw_intvecnum:	.value	0	/ int number across switch
usw_recontrol:	.byte	0xff	/ flag to recompute ctrl settings
usw_curscreen:	.byte	0	/ current screen assignment
usw_swscrchar:	.value	0	/ switchscreen character and bits
usw_swmodchar:  .value  0       / switchmode character
usw_kbstate:	.byte	0	/ keyboard state (copied)
		.byte	0	/ reserved
usw_dosip:	.value	0	/ ip from DOS for profiling
usw_doscs:	.value	0	/ cs from DOS for profiling
	.globl	usw_isr
usw_isr:	.value	0	/ current primary in-service mask
usw_intmask:	.value	0	/ 8259 mask saved during reset
	.globl	usw_dosticks
usw_dosticks:	.value	0	/ count of ticks DOS has missed
usw_hotkeyhit:	.value	0	/ DOS user hit the hotkey (scan code)
usw_modkeyhit:  .value  0       / DOS user hit the mode key (scan code)
usw_lastprimary: .byte	0x08	/ Last primary int seen
	.globl	usw_egastate
usw_egastate:	.byte   0,0,0,0,0,0,0,0,0,0	/ ega palette
		.byte	0,0,0,0,0,0,0,0 
usw_end:

usw_size = usw_end - usw_data

bxchgbuf: . = .+256

my_stbuff:	.value	0,0,0,0,0,0,0,0		/ stack buffer for new BIOS
mystack:
my_st_ds:	.value	0
my_st_es:	.value	0
my_st_regs:	.value	0,0,0,0,0,0,0,0		/ general regs for popa
my_st_ip:	.value	0
my_st_cs:	.value	0
my_st_flags:	.value	0

	.text
/ When we get here, we have taken either
/	> a hardware interrupt
/	> an exception trap (e.g. general protection)
/ We assume that there are no user INT's that make it through the IDT, i.e.
/ there are no software interrupts.
/
/ The stack looks like this (bracketed quantities may or may not exist),
/ after the "mov bp, sp" instruction below:
/
/ high address:		[SS]	ss:sp at the time of the interrupt, but
/			[SP]	    only if we came from user mode
/			FLAGS	flags,
/			 CS	  cs:ip
/			 IP	  at the time of the interrupt
/		     [ERR CODE] Error code only for certain exception traps
/		     INT NUMBER The IDT index the interrupt came through
/	bp	->	 BP	Current bp points at saved bp.

#ifdef	LCCSIASM
dolccsio0:
	mov	$<s>usw_isr, %ax
	mov	%ax, %ds
	test	$0x0010, usw_devassign	/Unix own serial port 0?
	jz	lccdecode1		/No - decode and send to DOS
	pop	%ax
	pop	%ds
	pop	%bp
	ljmp	lccsio0		/ Handle possible serial port 0 interrupt
				/ Continues at lccdecode if no serial int.
dolccsio1:
	mov	$<s>usw_isr, %ax
	mov	%ax, %ds
	test	$0x0008, usw_devassign	/Unix own serial port 1?
	jz	lccdecode1		/No - decode and send to DOS
	pop	%ax
	pop	%ds
	pop	%bp
	ljmp	lccsio1		/ Handle possible serial port 1 interrupt
				/ Continues at lccdecode if no serial int.
#endif	/* LCCSIASM */

	.globl	msm_decode
msm_decode:
	cli			/ YEAH!  Believe it or not, occasionally
				/  when a user program faults in to the
				/  kernel with a FLDCW instruction, even
				/  though we take it through an interrupt
				/  gate, interrupts may be still be ON!
				/  Let's hear it for Intel.
/ First establish addressability to the switchdata structure.
	push	%bp
	mov	%sp,  %bp
	push	%ds
	push	%ax
#ifdef	LCCSIASM
	mov	2(%bp), %ax	/ Get IDT vec #
	and	$0x7, %ax
	cmp	$0x3, %ax	/ Possible COM2 ?
	je	dolccsio1
	cmp	$0x4, %ax	/ Possible COM1 ?
	je	dolccsio0

	.globl	lccdecode
lccdecode:
#endif	/* LCCSIASM */
	mov	$<s>usw_isr, %ax
	mov	%ax, %ds
#ifdef	LCCSIASM
lccdecode1:
#endif	/* LCCSIASM */

	push	%bx
	push	%cx
/ Read both in-service registers.
	movb	$READ_ISR, %al
	outb	PC8259
	jmp	.+2
	inb	PC8259
	movb	%al, %bl
	movb	$READ_ISR, %al
	outb	AT8259
	jmp	.+2
	inb	AT8259
	movb	%al, %bh
	push	%bx
/ At this point, BX contains the actual 8259 ISR state, i.e., what is
/ actually in-service.
/ If the in-service mask is what we last thought it was, then this was not
/ a hardware interrupt.
	mov	$<s>usw_isr, %ax
	mov	%ax, %ds
	mov	usw_isr, %ax	/ AX = what we think is currently in-service
	and	$0xfffb, %bx	/ BX = actually in-service except IRQ 2 (slave)
	and	$0xfffb, %ax	/ AX = think in-service except IRQ 2 (slave)
	not	%ax		/ AX = think not-in-service, plus IRQ 2 (slave)
	and	%ax, %bx	/Any interrupts come on?
	pop	%ax
chk_phant:
	jz	wassoft
	jmp	washard
.globl	wassoft			////Testing
wassoft:
/ Certain DOS programs can trigger phantom printer interrupts (BASICA).
/ Certain other DOS programs change the 8259 base (TopView).  We do not
/ know where the 8259 base is, so we will see a phantom IRQ 7 interrupt
/ as one of (f, 17, 1f, 27, 2f, etc.).  The hack is that if we see one
/ of those software ints, we will just ignore it.  More properly, we should
/ keep track of the current 8259 base, and only ignore a phantom IRQ 7
/ relative to that base, but we will probably get away with ignoring
/ them all, on the assumption that Unix does not use software interrupts

	not	usw_isr
	and	%ax, usw_isr		/Check only slave coming on.
	mov	%ax, usw_isr		/Update usw_isr
	jnz	wasphant		/Phantom in slave!
	cmp	$7, 2(%bp) 
	jna	testx
	movb	2(%bp), %al
	andb	$7, %al
	cmpb	$7, %al
	je	ret_from_int	
/ Ok, it wasn't a hardware interrupt.  Since there are no software interrupts,
/ if the IDT index was <= 16, we will take this to be a trap, and if > 16,
/ we take it to be a phantom, and ignore it.
.globl	testx		/////Testing
testx:
	cmp	$16, 2(%bp)	/ a valid exception number?
	jle	wasexcep
/ Not a valid exception, so let's just pretend it didn't happen.
.globl	ret_from_int	/////Testing
ret_from_int:
	pop	%cx
	pop	%bx
	pop	%ax
	pop	%ds
	pop	%bp
	add	$2, %sp		/ pop off the interrupt number
	iret
/ Apparently, it was a true exception.  Clean up the stack and branch to
/ the appropriate exception handler.
.globl	wasexcep		/////Testing
wasexcep:
	pop	%cx
	pop	%bx
	pop	%ax
	pop	%ds
	cmp	$7, 2(%bp)	/ 0-7, 9, e-10 go to trp:, others to fault:
	jle	gtrp
	cmp	$9, 2(%bp)
	je	gtrp
	cmp	$14, 2(%bp)
	jge	gtrp
gflt:	
	pop	%bp
	ljmp	fault
gtrp:	
	pop	%bp
	ljmp	trp

/ At least one of the bits in the current in-service mask differs from
/ what it was last time we took an interrupt or dismissed one.  Locate the
/ highest priority IRQ level that is new, and assume that that interrupt
/ occured.
.globl washard			/////Testing
washard:
        mov	%ax, usw_isr	       /Update usw_isr
	cmp	$<s>i0, 6(%bp)         /check for double interrupt
	jne	int_no_double
	mov	4(%bp), %cx
	cmp	$i0, %cx	
	jb	int_no_double
	cmp	$i255, %cx
	ja	int_no_double
	sub	$i0, %cx		/Calculate interrupt vector
	mov	%cx, %ax
	mov	$7, %cx
	divb	%cl
	mov	%ax, %cx
	jmp	int_double		/We got double interrupt!

int_no_double:
	testb	$3, %al		/ IRQ 0 or 1?
	jnz	notslave
	testb	$4, %al   	/ did slave bit change?
	jnz	wasslave
notslave:
	mov	2(%bp), %ax	/ get IDT index
	andb	$7, %al   	/ al is channel number 0 - 7
	jmp	wasprimary
wasslave:
	orb	%ah, %ah
	jnz	notphant
/ What we got was apparently a phantom interrupt in the slave.
.globl	wasphant		//////Testing
wasphant:
	movb	$SLAVE_EOI, %al
	outb	PC8259 
	jmp	.+2
	movb	$READ_ISR, %al
	outb	PC8259
	jmp	.+2
	inb	PC8259
	movb	%al, usw_isr	/Update usw_isr
	jmp	ret_from_int	/ pretend nothing happened
/ It wasn't a phantom, compute slave channel number.
notphant:
	orb	$0x4, usw_isr   / set slave bit in usw_isr
	movb	2(%bp), %al	/ get IDT index
	andb	$7, %al   	/ al is channel number in slave
	orb	$8, %al		/ indicate channel number as 8 - f
/ Now al contains the channel number, 0-7 for primary, 8-f for slave.
wasprimary:
	xorb	%ah, %ah
	mov	%ax, %cx
	mov	$1, %bx
	shl	%cl, %bx		/ bx has bit #al on
	
/ Check to see if this device is assigned to Unix or DOS.
	test	%bx, usw_devassign 
	jz	int_for_dos
/ It's for Unix.
	jmp	int_for_unix
/ It's an interrupt for DOS, so pass it on.
.globl	int_for_dos		/////Testing
int_for_dos:
	movb	%al, %bh	/ bh has channel number 0-f
	movb	2(%bp), %bl	/ bl has interrupt number 0-ff
	lcall	<s>call_to_dos, call_to_dos / give the interrupt to DOS
/ See if DOS indicated that the user just hit the hotkey (screen) or mode key
	mov	usw_hotkeyhit, %ax
	or	%ax, %ax
	jnz     hot
	mov	usw_modkeyhit, %ax
	or	%ax, %ax
	jz	nothot
hot:
        lcall	<s>spkbint, spkbint

/ Return from the interrupt after cleaning up.
nothot:
	jmp	ret_from_int
int_double:			/We got a double interrupt bug!
	mov	%cx, %ax	/Get interrupted int. vector.
	cmpb	$0x2, %ah	/Did the vector get pushed?
	je	int_dbl2	/Yes - skip over stack space insertion
	push	%ds		/Save what we're killing
	push	%es
	push	%si
	push	%di
	pushf
	mov	%sp, %si	/Set from address
	sub	$0x2, %sp	/Pre-adjust stack for pops later
	sub	$0x2, %bp	/Adjust bp frame also!
	mov	%sp, %di	/ .. and set to address.
	mov	%ss, %cx	/Setup for stack segment
	mov	%cx, %es
	mov	%cx, %ds
	mov	$14, %cx	/Going to move entire interrupt frame.
	cld			/ a good idea
	rep
	smov			/Move 15 words on stack down
	lcall	<s>popff, popff	/Stack adjusted above by "sub $2, %sp"
	pop	%di
	pop	%si
	pop	%es
	pop	%ds
int_dbl2:
	xorb	%ah, %ah	/ Strip off remainder from vector calc.
	mov	%ax, 10(%bp)	/ Save vector number in "prior" int. frame
	mov	$<s>dblintr, 6(%bp)	/ set segment of dblintr:
	mov	$dblintr, 4(%bp)/ set to return to dblintr:
	and	$0xfdff, 8(%bp)	/ ensure interrupts off at dblintr:

        mov	$<s>usw_lastprimary, %bx / determine irq's 
	mov	%bx, %ds
	mov	usw_lastprimary, %bx
	andb	$0xf8, %bl               / bx contains current master base
	mov	2(%bp), %ax
	cmpb	%bl, %al
	jb	slave
	orb	$7, %bl	
	cmpb	%bl, %al
	ja	slave
	andb    $7, %al	         / it's a master int
	jmp	dbl_done
slave:
	andb	$7, %al          / it's a slave int
	orb	$8, %al		 / ax contains 1-st int's channel # 
dbl_done:
	jmp	wasprimary	/ Resume processing as though nothing
				/ silly happened.

dblintr:			/Handle interrupted interrupt.
	push	%bp
	mov	%sp, %bp
	push	%ds
	push	%ax
	push	%bx
	push	%cx
	movb	$READ_ISR, %al
	outb	AT8259
	jmp	.+2
	inb	AT8259
	movb	%al, %ah
	movb	$READ_ISR, %al
	outb	PC8259
	jmp	.+2
	inb	PC8259
	mov	$<s>usw_data, %bx	/Set usw_data addressability
	mov	%bx, %ds
	test	$0xff, %ax		/Begin phantom check in master
	jmp	chk_phant         	/Let msm_decode handle it now...


/ Ok, the hardware interrupt was for a Unix device.  Pass the interrupt
/ on to the normal interrupt handler, after adjusting the stack.
int_for_unix:
	mov	%ax, 2(%bp)     / replace int number with channel number
	pop	%cx
	pop	%bx
	pop	%ax
	pop	%ds

/ The saved BP is at the top of the stack, followed by the interrupt
/ IRQ (0..15).  Unix assumes that the master interrupts at MBASE, and
/ the slave at SBASE.  So add MBASE or SBASE to the IRQ value, and branch
/ to the standard interrupt handler.
	cmp	$8, 2(%bp)
	jl	madd
	add	$[SBASE-8], 2(%bp)
	jmp	popret
madd:
	add	$MBASE, 2(%bp)
popret:
	pop	%bp		/ Restore stack to what Unix wants.
	ljmp	intr


/
/									
/	Name:								
/		sw_to_dos	-- coroutine switch to dos		
/		call_to_dos	-- interrupt call to dos		
/		iret_to_dos	-- interrupt return to DOS		
/									
/ 	Description:							
/		These functions all change to real mode.  State is	
/		saved if we expect to return.				
/									
/

	.globl sw_to_dos
/ coroutine switch to DOS, expecting to return -- no arguments
sw_to_dos:
#ifdef NOTDEF
/ HACK for timing test
	mov	$100, %bx
alot:
	push	%bx
	mov	$0x0f07, %bx
	lcall	<s>call_to_dos, call_to_dos	/ fake a printer interrupt
	pop	%bx
	dec	%bx
	ja	alot
/ end HACK
#endif
	mov	$F_SWITCH, %bx		/ indicate not an interrupt

/ call DOS, expecting to return -- bx has interrupt number or 0ffxxH
call_to_dos:
	push	%bp			/ save frame
	mov	%sp, %bp
	pushf
	pusha
	push	%ds
	push	%es
	cmp	$F_SWITCH, %bx		/ is this a nested call?
	je	go_to_dos		/ not a call, just a switch
	mov	$<s>udosnest, %ax	/ a call -- increment dosnest
	mov	%ax, %ds
	inc	%ds:udosnest
	jmp	go_to_dos

/ return to DOS, not to come back -- no arguments
iret_to_dos:
	mov	$F_IRET, %bx		/ indicate not an interrupt

go_to_dos:
	/ We check for signals and for runrun whenever we are returning
	/ to DOS at nest level 0, that is, we are about to go to mainstream
	/ code and not interrupt code.
	mov	$<s>udosnest, %ax	/ check if we are at nest level 0
	mov	%ax, %ds
	cmp	$0, udosnest 
	jne	noslice			/ nested - don't allow switch or sigs

	push	%bx
	lcall	<s>to_dos_checks, to_dos_checks	/ check for signals and runrun
	pop	%bx

noslice:
/ If we have decided to abort DOS it is because of some catastrophic
/ error.  However, we might be here anyway if the error happened
/ during interrupt processing.  In that case, we don't actually
/ go there, but instead pretend that we went and that it returned
/ to us.
	mov	$<s>dosok, %ax
	mov	%ax, %ds
	cmp	$0, dosok            	/ see if we may go to DOS
	jne	forge_ahead		/ if so, don't go there but say we did
	jmp	ret_from_dos

/ We have decided to go to DOS, so we set everything up.
forge_ahead:
	cli
	cld

	mov	$<s>usssave, %ax		/ save stack pointer
	mov	%ax, %es
	mov	%ss, %es:usssave 
	mov	%sp, %es:uspsave 
	sldt    %ax      		/ save LDTR
	mov	%ax, %es:uldtsave 
	str   	%ax			/ save TSS
	mov	%ax, %es:utsssave 
	smsw   	%ax			/ save MSW
	mov	%ax, %es:umswsave 
	and	$6, %ax			/ check for 287
	cmp	$2, %ax
	jne	no287save
	clts
	fnsave	%es:ufpsave		/ save fp state
	wait
no287save:

	mov	%bx, %es:usw_intvecnum 

	/ save current interrupt mask in switchdata, and mask everything off
	inb	AT8259+1		/ get secondary isr mask
	movb	%al, %ah
	inb	PC8259+1		/ get primary isr mask
	mov	%ax, %es:usw_intmask 
	movb	$0xff, %al
	outb	AT8259+1
	outb	PC8259+1 

	/ set up to copy the Unix copy of the switchdata structure to DOS
	lcall	<s>mapdos, mapdos	/ point ds:si at switch.sys switchdata

	/ take this opportunity to init switch if necessary
	mov	$<s>dosnewdriver, %ax
	mov	%ax, %es
	cmp	$0, %es:dosnewdriver	/ new driver needs init once/load
	je	no_driver_init
	mov	$0, %es:dosnewdriver	/ ...and only once!
	mov	%es:dosdriver, %ax	/ get high driver address in K
	mov	$6, %cx
	shl 	%cl, %ax		/ turn in segment address
	mov	%ax, %cx

	mov	%cx, %ds:[0x6]		/ init switchcode segment
	mov	%cx, %ds:[0xA]		/ init retaddr segment

	mov	%ds:[SWITCHCS], %di
	mov	%cx, %ds:(%di)	 	/ at_st_cs

	mov	$[LOW_SEL_ASM(0) << 3], %ax
	mov	%ax, %es		/ es: -> 0
	mov	$MAGICOFF, %di 		/ di = MAGIC_OFFSET 

	mov	%es:[0x400+MERGE_ADDR], %ax	/ Get DOS user stack pointer
	mov	%ax, %ds:2(%di)		  	/ Save in switch:ir_spsave
	mov	%es:[0x400+MERGE_ADDR+2], %ax  	/ and stack segment
	mov	%ax, %ds:4(%di) 	 	/ and switch:ir_sssave

	mov	%ds:[ATSTACK],  %ax 		/ point bios shutdown at_stack
	mov	%ax, %es:[0x400+RESET_SP] 	/ [467] <- offset
	mov	%cx, %es:[0x400+RESET_SP+2]   	/ [469] <- segment

	push	%ds
	mov	$<s>dosdriver, %ax
	mov	%ax, %ds
	mov	%ds:dosdriver, %ax	/ get high driver address in K
	mov	%ax, %es:[0x400+0x13]	/ [413] <- dos size in K
	pop	%ds

no_driver_init:

	/ copy the switch data structure from UNIX to DOS
	push	%ds
	pop	%es
	mov	%si, %di
	mov	$usw_size, %cx
	mov	$<s>usw_data, %ax
	mov	%ax, %ds
	mov	$usw_data, %si

	clc
	rcr	$1, %cx			/Setup for word moves
	rep
	smov				/Mass move words
	jnc	umvdone			/Even # words needed moving?
	smovb				/No - Move remaining odd byte
umvdone:

	/ time to go to real mode
	movb	$SHUT_DOWN, %al		/ set the shutdown byte
	outb	CMOS_PORT           	/	in CMOS
	jmp	.+2
	movb	$9, %al			/	to 9  (block move type)
	outb	CMOS_PORT+1 

#ifdef OS2RESET
#ifdef NOTDEF
/ timing code - remove
	mov	$<s>rstcnt, %ax
	mov	%ax, %ds
	add	$1, rstcnt
	adc	$0, rstcnt+2	/ rstcnt++ (long)
	cmp	$0, rsttyp
	jne	kbdrst
/
#endif /* NOTDEF */
	lidt	%cs:nullidt		/ set up an IDT with limit = 0
	int	$3			/ cause an exception that
nullidt: .value 0,0,0			/ causes an exception, leading to
					/ a triple fault ending in a RESET
#ifdef NOTDEF
/ timing code - remove
kbdrst:
	movb	$SHUT_CMD, %al
	outb	STATUS_PORT         	/ RESET
rev_up:	hlt				/ prepare for jump to hyperspace
	jmp	rev_up
/
#endif /* NOTDEF */
#else /* ~OS2RESET */
	movb	$SHUT_CMD, %al
	outb	STATUS_PORT         	/ RESET

rev_up:	hlt				/ prepare for jump to hyperspace
	jmp	rev_up
#endif /* ~OS2RESET */

/ End of sw_to_dos


/
/									
/	Name:								
/		from_dos	-- control returns here from real mode	
/									
/ 	Description:							
/		We restore the state and decide whether this is an	
/		interrupt or a return from a switch or call.		
/									
/

/ After switching to protected mode, dos (us) jumps to this location.
	.globl from_dos
from_dos:
	cld
	mov	$<s>utsssave, %ax	/ restore tss, ldt, and stack
	mov	%ax, %ds
	mov	utsssave, %di
	mov	$[GDT_SEL << 3], %ax
	mov	%ax, %es
	andb	$0xfd, %es:5(%di)    	/ turn off tss busy bit
	ltr	utsssave		/ restore TSS register

/ Some Unix's change the access rights of the LDT slot in the GDT to
/ make it a data segment, so we cannot assume that it is safe to do an LLDT
	mov	uldtsave, %dx
	verr	%dx			/ readable?
	jnz	goodldt			/ if not, assume a valid ldt selector
	mov	%dx, %di
	and	$0xfff8, %di
	movb	%es:5(%di), %al		/ fetch ldt selector access byte
	mov	$0xe2, %es:5(%di)	/ make it a valid ldt selector
	lldt	%dx
	movb	%al, %es:5(%di)		/ put back original access byte
	jmp	didldt
goodldt:
	lldt	%dx 			/ Restore LDTR
didldt:

	mov	usssave, %ss
	mov	uspsave, %sp

	mov	umswsave, %ax
	and	$0x6, %ax		/ floating point state
	smsw	%dx
	and	$0xfff1, %dx
	or	%ax, %dx
	lmsw	%dx 			/ Restore MSW floating point state
	cmp	$2, %ax
	jne	no287restore
	frstor	ufpsave			/ save fp state
no287restore:
	mov	umswsave, %ax		/ now restore TS bit
	and	$0x8, %ax
	or	%ax, %dx
	lmsw	%dx


	.globl safeinunix
safeinunix:
	inb	AT8259+1		/ record DOS interrupt mask
	movb	%al, %ah
	inb	PC8259+1
	mov	%ax, dosintmask 
	lcall	<s>mapdos, mapdos	/ set up ds:si to point at dos copy
	mov	$<s>usw_data, %ax	/ copy usw_data struc
	mov	%ax, %es
	mov	$usw_data, %di
	mov	$usw_size, %cx

	clc
	rcr	$1, %cx			/Setup for word moves
	rep
	smov				/Mass move words
	jnc	umvdone0		/Even # words needed moving?
	smovb				/No - Move remaining odd byte
umvdone0:
	mov	%es:usw_intvecnum, %bx	/ get passed interrupt vector number

	/ What state we are in is recorded in bx:
	/	F_SWITCH:  switch -- coroutine switch
	/	F_IRET:    iret -- returning from interrupt processing
	/	F_ABORT:   abort -- we need to initiate an abort
	/	FH_HOTKEY: (bh) special key struck -- need to switch screens
	/                                             or screen mode
	/	000xH:     int x (0 - f) -- a Unix interrupt went off in DOS

	cmpb	$FH_RET, %bh		/ see if this is an int or a return
	jne	int_from_dos

ret_from_dos:				/ return or coreturn -- restore regs
	mov	$<s>udosnest, %ax	/ another return -- decrement dosnest
	mov	%ax, %ds
	cmp	$0, udosnest         	/ don't decrement below nest zero
	je	nestzero		/ we just returned from a switch
	dec	udosnest		/ we just returned from a call

	/ We think DOS is returning from an interrupt call.  Here we make
	/ sure that he has the same idea.  He might not if a user interrupt
	/ routine somehow did a system call (which is illegal).
	cmp	$F_SWITCH, %bx		/ make sure DOS didn't do system call
	jne	nestzero
	mov	$F_ABORT, %bx		/ DOS was evil -- kill him off

nestzero:
	/ If the bx function code indicates an abort, then send the DOS
	/ process the ILL signal.  (Note that the currently running
	/ process might not be the DOS process if we were just running
	/ an interrupt routine.)  We also clear dosok so that we will
	/ never go back to DOS which might otherwise happen if we were
	/ processing nested interrupts.
	cmp	$F_ABORT, %bx
	jnz	notfatalreturn
	lcall	<s>dosabort, dosabort	/ setup to abort the DOS process

notfatalreturn:
	/ Make sure ES is a valid segment, otherwise set to 0.
	pop	%ax			/ pushed es
	verr	%ax
	jz	esok
	xor	%ax, %ax
esok:
	mov	%ax, %es
	pop	%ds
	popa
	lcall	<s>popff, popff		/ do a guaranteed popf
	pop	%bp
	lret
popff:
	iret


/ DOS has forwarded an interrupt to us - pass it to the appropriate handler,
/ checking for the HOTKEY interrupt along the way.
int_from_dos:
	cmp	$0xf, %bx		/ bx a valid interrupt number?
	jbe	validint
	cmpb    $FH_HOTKEY, %bh		/ is it a special character?
	je	hot_key_int
	mov	$F_ABORT, %bx		/ if not valid, kill him off
	jmp     nestzero
/ It is a valid interrupt number, so call the interrupt handler
/ Need to establish a new idea of ISR because some of the old ones
/ might have been EOI'd by now.
validint:
/ Have to go to the Unix interrupt handler, so that it returns to us.
	pushf
	push	%cs
	push	$iret_to_dos		/ a fake interrupt frame
	cmp	$8, %bx
	jl	madd1
	add	$[SBASE-8], %bx
	jmp	popret1
madd1:
	add	$MBASE, %bx
popret1:
	push	%bx			/ the virtual interrupt number
	ljmp	intr			/ should return to iret_to_dos

hot_key_int:
	xchgb	%bh, %bl		/ put scan code in high half
	mov	%bx, %ax		/ pass arg (scan code in high half ax)
	lcall	<s>spkbint, spkbint	/ process the hot key
	jmp	iret_to_dos

/ The user has hit the hotkey, and the scan code is in ah.
spkbint:
	push	%ds
	push	%es
	pusha
	mov	%ax, %bx
	lcall	<s>spl4, spl4
	push	%ax		/ old spl level
	push	%bx		/ arg to dspecialkb
	sti
	lcall	<s>dspecialkb, dspecialkb	/ go do the switch screen
	cli
	mov	$<s>usw_hotkeyhit, %ax
	mov	%ax, %ds
	mov	$0, usw_hotkeyhit
	mov	$0, usw_modkeyhit
	add	$2, %sp
	lcall	<s>splx,splx
	add	$2, %sp
	popa
	pop	%es
	pop	%ds
	lret

/ End of from_dos

/
/									
/	Name:								
/		mapdos		-- point ds:si at dos global data area	
/									
/

mapdos:
	push	%ax
	push	%bx
	push	%cx
	push	%dx

	mov	$<s>dosdriver, %ax
	mov	%ax, %ds
	mov	dosdriver, %ax		/ get high driver address in K
	mov	$6, %cx
	shl 	%cl, %ax		/ turn in segment address
	mov	%ax, %cx		/ save realmode segment (cx)

/ now, set up the scratch selector to point at this location
	mov	$[GDT_SEL << 3], %bx		/ point at gdt
	mov	%bx, %ds
	mov	$[SCRATCH_SEL << 3], %bx    / offset of SCRATCH_SEL selector
	mov	$[0x280], (%bx)             / limit
	movb	%ah, %dl
	shrb	$4, %dl
	shl	$4, %ax
	andb	$0xf, %dl
	movb	%dl, 4(%bx)           / high byte of base address
	mov	%ax, 2(%bx)           / low word of base address
	
	mov	$[SCRATCH_SEL << 3], %ax
	mov	%ax, %ds		/ ds: -> switch.sys
	mov	$SWOFFSET, %si

	pop	%dx
	pop	%cx
	pop	%bx
	pop	%ax
	lret

/ End of mapdos


/ copybuf copies from one buffer to another
/ parameters: source buffer address
/	      destination buffer address
/	      buffer size in bytes
/
.globl	copybuf
copybuf:
	push	%bp		/ establish stack frame
	mov	%sp,%bp		/   "   "
	push	%cx
	push	%si
	push	%di
	mov	14(%bp),%cx	/number of bytes
	shr	$1,%cx		/number of words
	lds	6(%bp), %si	/first buffer
	les	10(%bp), %di	/second buffer
	cld
	rep	
	smov
	pop	%di
	pop	%si
	pop	%cx
	pop	%bp
	lret

#endif 	/* ATMERGE */

