;
;	3C501 Ethernet driver for Locus Bridge software
;	David A. Butterfield -- Locus Computing Corporation
;	March 1984
;
;	Copyright (c) 1984 Locus Computing Corporation
;

;	conditional assembly flags
	; Recommended Settings:
	; ON:  disable, checks, enableint, switchstack
	; OFF: wrongint, debug
	; verbose:  ON in house, OFF in production
	; sendnotify:  depends on application
 verbose	equ 1			; mention unusual conditions
debug		equ 1			; debugging messages
 disable	equ 1			; disable receiver during transmit
;wrongint	equ 1			; can mix up rec/trans interrupts
					;    (don't need if disable is ON)
 sendnotify	equ 1			; tell application when send complete
 switchstack	equ 1			; switch stacks at interrupt time
 checks		equ 1			; perform reasonable error checking
 enableint	equ 1			; ethsend might be called disabled
					;    (must be ON unless guaranteed not)

;	derived assembly flags
  ifdef debug
	prints	equ 1			; need print routine to debig
  else
  ifdef verbose
	prints	equ 1			; need print routine to be verbose
  endif
  endif

;	hardware interface offsets
ei_eaddr	equ 0			; ethernet address
ei_rec		equ 6			; receive csr
ei_send		equ 7			; send csr
ei_ptr		equ 8			; pointer into buffer (and rom)
ei_rptr		equ 10			; pointer into buffer for receive
ei_prom		equ 12			; window into address prom
ei_csr		equ 14			; csr
ei_buf		equ 15			; window into buffer

;	packet offsets of interest
pkt_dest	equ 0			; destination address
pkt_type	equ 12			; type field
pkt_select	equ 20			; stream select field
pkt_count	equ 32			; text count

;	select table offsets
sel_max		equ byte ptr 0		; size of table in select entries
sel_error	equ dword ptr 1		; error routine address
sel_sendint	equ dword ptr 5		; send completion notify routine
sel_zero	equ 9			; start of select zero entry
sel_routine	equ dword ptr 0		; receive routine select-entry-offset
sel_addr	equ word ptr 4		; receive packet select-entry-offset
sel_copy	equ byte ptr 8		; receive copy flag entry-offset
sel_busy	equ byte ptr 9		; receive buffer busy entry-offset

;	send queue
Q_ENTRYSIZE	equ 4			; bytes per entry
Q_MAX		equ 5			; queue is five elements long
Q_SIZE		equ Q_MAX*Q_ENTRYSIZE	; total size in bytes

;	manifest constants
maxpacket	equ 1514		; max packet size
ebase		equ 0300h		; ethernet I/O address
					; next three lines are for int level 3
intnum		equ 0bh			; interrupt number
intaddr		equ word ptr 02ch	; interrupt address
intmask		equ 08h			; bit #3 for masking interrupt

;	start of code segment
cseg	segment public para  'code'
	assume	ds:cseg, cs:cseg

  ifdef switchstack
sssave	dw 0				; stack segment save location
spsave	dw 0				; stack pointer save location
intnest dw 0				; interrupt nesting level
	dw 127 dup (0)			; new stack
ethstack dw 0
  endif


;	variables which live in cseg
q_count		db 0			; number of elements currently in q
q_head		dw 0			; offset of head (remove point) of q
q_tail		dw 0			; offset of tail (add point) of q
q_entries	dw Q_SIZE/2 dup (0)	; queue buffer

ethsent dw	0			; bytes sent
ethtable dw	2 dup (0)		; address of select table
selectentrysize db 10			; bytes in a select table row
	public	ethaddr
ethaddr db	6 dup (0)		; ethernet address
ethtype dw	8008h			; Bridge ethernet type
ethon	db	0			; recieve in progress.
ethsending db	0			; current send state
	ETHSENDIDLE	equ 0
	ETHSENDBUSY	equ 1
	ETHSENDDONE	equ 2
	ETHSENDPENDING	equ 3

ethsenderr db	0			; error result of last send
public debugon
debugon db	0			; debugging messages on/off
public printon
printon db	0			; all printing on/off

;	messages in cseg
  ifdef verbose
mscoll		db 10,13,'[collision]$'
mserr		db 10,13,'[send error]$'
mrerr		db 10,13,'[receive error]$'
mtoobig		db 10,13,'[packet too big]$'
mlostsendint	db 10,13,'[lost send interrupt]$'
  endif
  ifdef debug
msend		db '[send]$'
minit		db '[init]$'
mrecint		db '[recint]$'
mrecintend	db '[recint end]$'
msendint	db '[sendint]$'
msendintend	db '[sendint end]$'
  endif


;	initialize the ethernet interface and driver
;	es:di contains address of select table, al is ether on/off flag (0=off)
public	ethinit
ethinit proc near
  ifdef debug
	mov	dx,offset minit
	call	mlprint
  endif
	push	es
	push	ds			; save old ds and set to cs
	push	cs
	pop	ds
	mov	ethtable,di		; save the table address away
	mov	ethtable+2,es
	push	ax			; save on/off flag
	call	ethreset
	pop	bx			; on/off flag in bl
	cmp	al,0			; check for reset errors
	jne	ethinitdone
	cmp	bl,0			; if flag says "off" skip rest
	je	ethinitdone
	call	ethsetup		; set up parameters and interrupt
	push	cs			; ether addr buf addr in es:di
	pop	es
	mov	di,offset ethaddr
	call	ethgaddr		; get prom address
	mov	di,offset ethaddr
	call	ethsaddr		; set ether address
	call	ethrecenb		; enable a receive
	mov	al,0
ethinitdone:
	pop	ds
	pop	es
	ret


;	reset the ether interface
ethreset:
	mov	al,5
	out	0ah,al			; turn off DMA #1
	in	al,021h			; get interrupt mask
	or	al,intmask		; disable our int
	out	021h,al			; set interrupt mask
	mov	al,080h
	mov	dx,ebase+ei_csr
	out	dx,al			; reset the ethernet
	mov	al,0
	out	dx,al			; stop resetting the ethernet

	; indicate the new interface state
ethreset1:
	call	eth_dequeue		; flush output queue
	jnc	ethreset1
	mov	ethsending,ETHSENDIDLE

	; check for errors in the interface
  ifdef checks
	mov	bl,0fdh			; there may be errors later
	in	al,dx
	cmp	al,080h
	jne	ethresetdone		; the transmit idle didn't come on
	inc	bl			; that error didn't happen
	mov	dx,ebase+ei_send
	in	al,dx
	test	al,0fh
	jnz	ethresetdone		; the send bits didn't go away
	inc	bl
	mov	dx,ebase+ei_rec
	in	al,dx
	and	al,09fh
	cmp	al,080h
	jne	ethresetdone		; the receive bits didn't reset right
	inc	bl			; this brings us up to zero
ethresetdone:
	mov	al,bl			; error return in al
  else
	mov	al,0			; no errors checked
  endif
	ret


;	set up ether interface parameters
ethsetup:
	mov	dx,ebase+ei_send	; set up the send parameters
	mov	al,0fh			; detect underflow, coll, coll16, succ
	out	dx,al
	mov	dx,ebase+ei_rec
	mov	al,0h			; disable receive
	out	dx,al
	mov	dx,offset ethintr	; set up interrupt vector
	mov	al,intnum
	mov	ah,25h			; dos set interrupt vector function
	int	21h
	mov	dx,ebase+ei_csr		; enable int request
	mov	al,040h
	out	dx,al
public ethenable
ethenable:
	push	es			; Bagbiting XTALK munges int vector
	mov	ax,0h
	mov	es,ax
	mov	es:intaddr,offset ethintr
	mov	es:intaddr+2,cs
	pop	es
	in	al,021h			; get interrupt mask
	and	al,NOT intmask		; enable our int
	out	021h,al			; set interrupt mask
	ret


;	get the ethernet interface prom address
ethgaddr:
	mov	cx,6			; number of prom bytes
	mov	bx,0			; index of last prom byte
ethgloop:
	mov	ax,bx
	mov	dx,ebase+ei_ptr		; port address for prom pointer
	out	dx,ax			; write prom byte index
	mov	dx,ebase+ei_prom	; port address of prom window
	in	al,dx			; get prom byte
	mov	es:[di],al		; store into message block
	inc	di			; next byte
	inc	bx
	loop	ethgloop		; get all 6 of them
	ret


;	set the ethernet address
ethsaddr:
	mov	cx,6			; number of prom bytes
	mov	dx,ebase+ei_eaddr	; port address of ether address
ethsloop:
	mov	al,es:[di]		; get byte of the desired net address
	out	dx,al			; set that byte in interface
	inc	di			; next byte in source
	inc	dx			; next byte in I/O space
	loop	ethsloop		; get all 6 of them
	ret


;	enable the reciever
ethrecenb:
  ifndef disable
	mov	dx,ebase+ei_csr		; grab the interface
	mov	al,040h
	out	dx,al
  endif
  ifdef disable
	in	al,021h			; get the interrupt mask
	mov	ah,al
	mov	al,0ffh			; disable all interrupts
	out	021h,al
	mov	dx,ebase+ei_csr		; turn off ether interrupt req
	mov	al,0h
	out	dx,al
	mov	dx,ebase+ei_rec		; enable receive
	mov	al,0a0h			; accept good frames, station/broadcast
	out	dx,al

	in	al,dx			; clear spurious receive intr
	mov	dx,ebase+ei_csr		; reenable ether int req
	mov	al,040h
	out	dx,al
	mov	al,ah			; restore int mask
	out	021h,al
  endif
	jmp	ethrecgo1
ethrecgo:
	cmp	ethon,0
	je	ethrecgo1
	ret
ethrecgo1:
	mov	ethon,1

	;start the receive
	mov	dx,ebase+ei_rptr	; zero the read pointer to start of buf
	out	dx,al			; (any value zeros the hardware here)
	mov	dx,ebase+ei_csr		; start the receive
	mov	al,048h
	out	dx,al
	ret
ethinit endp


;	arrange for this packet to be sent out over the ether
;	packet address in es:di, first word is count, data follows that
public	ethsend
ethsend proc near
	push	es
	push	ds			; save old ds and set to cs
	push	cs
	pop	ds
	pushf				; save interrupt state
  ifdef enableint
	; set bl = 0 if was enabled, bl = 1 if was disabled
	pushf
	pop	dx
	mov	bl,0
	test	dx,0200h
	jne	ethsend0
	inc	bl
ethsend0:
  endif
	cli				; disable interrupts
  ifdef debug
	mov	dx,offset msend
	call	mlprint
  endif
	cmp	ethsending,ETHSENDIDLE	; is there a previous pending send?
	je	ethsend1
	call	eth_enqueue		; already sending, queue this one up
					; (result in al is passed on up)
	jmp	ethsendout		; it will be dealt with later

	; send this packet now
ethsend1:
	push	es
	push	di
	mov	ethsending,ETHSENDBUSY	; let people know we're working on it
	mov	cx,es:[di]		; byte count
	add	di,2			; point at packet data
  ifdef checks
	cmp	cx,maxpacket		; make sure count in range
	jna	ethsend2
	mov	cx,maxpacket		; place in range if not
  ifdef verbose
	mov	dx,offset mtoobig
	call	lprint
  endif
ethsend2:
  endif
	call	ethenable		; what a crock!
					; Bagbiting BASICA munges int mask
					; Bagbiting XTALK munges int vector
	call	ethxmit			; transmit the packet
	; enable interrupts so the ether send int can happen
  ifdef enableint
	; if ints were enabled before, it is safe to just enable them
	; otherwise we must enable just our interrupt, and no others
	cmp	bl,0			; were ints disabled before?
	je	ethsend3
	in	al,021h			; get interrupt mask
	push	ax			; squirrel away
	or	al,NOT intmask		; disable everyone elses int
	out	021h,al			; set it
ethsend3:
  endif
	sti				; allow the send interrupt
	mov	cx,0ffffh		; max loop time
ethswait:
	cmp	ethsending,ETHSENDDONE	; busy wait for interrupt completion
	je	ethsendcomplete
	loop	ethswait
	mov	al,0f0h			; interrupt timeout error
	mov	ethsenderr,al
	mov	es,ethtable+2
	mov	di,ethtable
  ifdef	verbose
	mov	dx,offset mlostsendint
	call	lprint
  endif
	mov	ah,1
	call	es:sel_error[di]	; call error routine
ethsendcomplete:
	cli
  ifdef enableint
	; restore the other interrupts if necessary
	cmp	bl,0			; did we mask off the other ints
	je	ethsend4
	pop	ax			; retrieve interrupt mask
	out	021h,al			; restore hardware interrupt mask
ethsend4:
  endif
	pop	di
	pop	es
  ifdef sendnotify
	; notify the application that the send completed; return any error
	mov	al,ethsenderr
	push	ds
	mov	ds,ethtable+2
	mov	si,ethtable
	call	ds:sel_sendint[si]
	pop	ds
  endif
	call	eth_dequeue		; see if anything else to do
	jnc	ethsend1		; yes, go around again
	mov	ethsending,ETHSENDIDLE	; no, we get to return
	mov	al,0			; say we have sent all the packets
ethsendout:
	popf				; restore interrupts
	pop	ds			; restore ds
	pop	es
	ret


;	actually do the transmit
;	call disabled
;	packet data in es:di, count in cx, "enable flag" in bl
ethxmit:
  ifdef enableint
	cmp	bl,0			; see if we ought to enable
	je	ethxmit1
	; enable just our interrupt so we get any pending receives
	pushf				; save old int status
	in	al,021h			; get interrupt mask
	mov	ah,al			; squirrel away
	or	al,NOT intmask		; disable everyone elses int
	out	021h,al			; set it
	sti				; enable interrupts
	nop				; just in case
ethxmit1:
  endif
	mov	ethon,0
	mov	dx,ebase+ei_csr		; take the interface from the ether
	mov	al,040h
	out	dx,al
  ifdef disable
	mov	dx,ebase+ei_rec		; disable receives
	mov	al,0h
	out	dx,al
  endif
  ifdef enableint
	cmp	bl,0
	je	ethxmit2
	popf				; reset the interrupt to disabled
	mov	al,ah			; retrieve interrupt mask
	out	021h,al			; restore hardware interrupt mask
ethxmit2:
  endif
	mov	dx,ebase+ei_ptr		; point to where out bytes should start
	mov	ax,0800h
	sub	ax,cx
	out	dx,ax
	mov	dx,ebase+ei_buf		; point at buffer window
	mov	ethsent,cx		; save count in case of collision
	cld				; let's increment
	push	ds			; save the ds register
	push	es			; set ds:si = es:di for lodsb
	pop	ds
	mov	si,di
ethfill:
	lodsb				; get next byte in al
	out	dx,al			; write it to hardware buffer
	loop	ethfill			; until done
	pop	ds			; restore ds
ethtry:
	; send the bytes onto the ether
  ifndef disable
	mov	dx,ebase+ei_rec		; in case we interrupted a read
	in	al,dx
  endif
	mov	dx,ebase+ei_rptr	; clear read pointer
	out	dx,al
	mov	dx,ebase+ei_ptr		; set pointer to start of bytes
	mov	ax,0800h
	sub	ax,ethsent
	out	dx,ax
	mov	ethsending,ETHSENDPENDING
	mov	dx,ebase+ei_csr		; start the output
	mov	al,044h
	out	dx,al
	ret
ethsend endp


;	ethernet interrupt routine
public	ethintr
ethintr proc far
  ifdef switchstack
	cmp	intnest,0
	jne	ethintrpush
	mov	cs:sssave,ss
	mov	cs:spsave,sp
	push	cs
	pop	ss
	mov	sp,offset ethstack
	push	cs:sssave
	push	cs:spsave
ethintrpush:
	inc	intnest
  endif
	push	ax			; save registers
	push	bx
	push	cx
	push	dx
	push	bp
	push	si
	push	di
	push	ds
	push	es

	push	cs			; set ds to current segment
	pop	ds

	mov	al,020h			; magic for interrupt controller
	out	020h,al

	cmp	ethsending,ETHSENDPENDING ; is there a transmit pending?
	je	ethsintr
	jmp	ethrintr


;	ethernet send interrupt routine
ethsintr:
	mov	ethsending,ETHSENDBUSY
  ifdef debug
	mov	dx,offset msendint
	call	mlprint
  endif
	mov	bl,0
	mov	dx,ebase+ei_csr		; read csr
	in	al,dx
  ifdef checks
	test	al,04h			; make sure was in transmit mode
	jnz	ethsintr1
	mov	bl,0feh
	jmp	ethserrint
ethsintr1:
	; for some reason this test fails -- net works anyway
;	test	al,080h			; make sure transmit is idle
;	jnz	ethsintr2
;	mov	bl,0fdh
;	jmp	ethserrint
ethsintr2:
  endif
	mov	dx,ebase+ei_send	; check for transmit errors
	in	al,dx
  ifdef checks
	test	al,05h			; underflow or 16 collisions
	jz	ethsintr3
	mov	bl,al
	jmp	ethserrint1
ethsintr3:
  endif
	test	al,02h			; collision
	jz	ethsintr4
	mov	dx,ebase+ei_csr		; turn off interface
	mov	al,040h
	out	dx,al
	call	ethtry			; retry
  ifdef verbose
	mov	dx,offset mscoll
	call	lprint
  endif
	jmp	ethendint		; don't report error
ethsintr4:
  ifdef checks
	mov	dx,ebase+ei_ptr		; make sure ptr high
	in	ax,dx
	cmp	ax,0800h
	je	ethsintr5
	mov	bl,0fch
	jmp	ethserrint1
ethsintr5:
	mov	dx,ebase+ei_send
	in	al,dx
	test	al,08h			; ready bit
	jnz	ethserrint2
	or	al,0f0h			; set unused bits to insure nonzero
	mov	bl,al
	jmp	ethserrint1
ethserrint:
	mov	dx,ebase+ei_send	; clear xmit interrupt
	in	al,dx
ethserrint1:
  ifdef verbose
	mov	dx,offset mserr
	call	lprint
  endif
	mov	ethsenderr,bl		; save away the error
	mov	al,bl
	mov	ah,1
	mov	es,ethtable+2
	mov	di,ethtable
	call	es:sel_error[di]	; call the application error routine
ethserrint2:
  endif
  ifdef wrongint
	mov	dx,ebase+ei_rec		; clear rec interrupt just in case
	in	al,dx
  ifndef disable
	call	ethrecenb		; reenable the hardware just in case
  endif
  endif
  ifdef disable
	call	ethrecenb		; enable for receive
  endif
	mov	ethsending,ETHSENDDONE	; flag the sender that it is done
  ifdef debug
	mov	dx,offset msendintend
	call	mlprint
  endif
	jmp	ethendint		; return from the interrupt


;	ethernet receive interrupt routine
ethrintr:
  ifdef debug
	mov	dx,offset mrecint
	call	mlprint
  endif
	; read the hardware
	mov	bl,00h			; one can hope
	mov	ethon,0
	mov	dx,ebase+ei_csr		; read csr
	in	al,dx
  ifdef checks
	test	al,01h			; make sure receive not busy
	jz	ethrintr2
	mov	bl,0feh
	jmp	ethrerrint
ethrintr2:
  endif
	mov	al,040h			; turn off interface
	out	dx,al
	mov	dx,ebase+ei_rec		; get receive status
	in	al,dx
	and	al,09fh
	cmp	al,010h			; make sure well formed, no errors
	je	ethrintr3
	or	al,060h			; set unused bits to insure nonzero
	mov	bl,al
	jmp	ethrerrint1
ethrintr3:
	mov	dx,ebase+ei_rptr	; get byte count received
	in	ax,dx
	mov	cx,ax
  ifdef checks
	mov	dx,ebase+ei_ptr		; make sure the ether dest is us
	mov	ax,pkt_dest
	out	dx,ax
	mov	dx,ebase+ei_buf
	mov	si,(offset ethaddr) - 1
	mov	ah,6
ethaddrcheck:
	inc	si
	dec	ah
	jl	ethaddrok
	in	al,dx
	cmp	al,ds:[si]
	je	ethaddrcheck
	mov	bl,0fch			; not us, or broadcast
	jmp	ethrerrint1
ethaddrok:
	mov	dx,ebase+ei_ptr		; make sure right packet type
	mov	ax,pkt_type
	out	dx,ax
	mov	dx,ebase+ei_buf
	in	al,dx
	mov	ah,al
	in	al,dx
	cmp	ax,ethtype
	je	ethtypeok
	mov	bl,0fbh			; some other type
	jmp	ethrerrint1
ethtypeok:
  endif
	push	ds
	mov	si,ethtable		; point to the entry in the select table
	mov	ds,ethtable+2
	mov	dx,ebase+ei_ptr		; look at select field in packet
	mov	ax,pkt_select
	out	dx,ax
	mov	dx,ebase+ei_buf
	in	al,dx
	cmp	al,ds:sel_max[si]	; make sure in table range
	jbe	ethselectok
	mov	bl,0fah			; out of table range
	pop	ds
	jmp	ethrerrint1
ethselectok:
	add	si,sel_zero		; point to first select entry
	mul	cs:selectentrysize	; skip preceding entries
	add	si,ax
	cmp	ds:sel_busy[si],0	; see if this channel is busy
	je	ethnotbusy
	mov	bl,0ffh			; ugh.	busy
	pop	ds
	jmp	ethrerrint1		; sorry, but I drop the packet
ethnotbusy:
	mov	ds:sel_busy[si],1	; set busy
	mov	di,ds:sel_addr[si]	; packet buffer address or argument
	mov	es,ds:sel_addr+2[si]
	cmp	ds:sel_copy[si],0	; see if we should copy the packet
	je	ethnocopy
	push	cx
	; copy cx bytes from ether into es:di
	mov	ax,0			; start at ether 0
	call	ethempty
	pop	cx
ethnocopy:
	mov	di,ds:sel_addr[si]	; restore buffer address

	mov	dx,ebase+ei_ptr		; get char count into ax
	mov	ax,pkt_count
	out	dx,ax
	mov	dx,ebase+ei_buf
	in	al,dx
	mov	ah,al
	in	al,dx
	xchg	ah,al
	call	ds:sel_routine[si]	; call the application receive routine
	pop	ds
	jmp	ethrerrint2
ethrerrint:
	mov	dx,ebase+ei_csr
	mov	al,040h			; turn off interface
	out	dx,al
	mov	dx,ebase+ei_rec		; clear receive interrupt
	in	al,dx
ethrerrint1:
  ifdef verbose
	mov	dx,offset mrerr
	call	lprint
  endif
	mov	al,bl
	mov	ah,0
	mov	di,ethtable
	mov	es,ethtable+2
	call	es:sel_error[di]	; call the application error routine
ethrerrint2:
  ifdef wrongint
	mov	dx,ebase+ei_send
	in	al,dx
  endif
	; start another read
	call	ethrecgo
  ifdef debug
	mov	dx,offset mrecintend
	call	mlprint
  endif


ethendint:
	pop	es
	pop	ds
	pop	di
	pop	si
	pop	bp
	pop	dx
	pop	cx
	pop	bx
	pop	ax
  ifdef switchstack
	dec	intnest
	jne	ethendint1
	pop	cs:spsave
	pop	cs:sssave
	mov	ss,cs:sssave
	mov	sp,cs:spsave
ethendint1:
  endif
	iret
ethintr endp


;	copy the bytes out of the hardware into the buffer
;	ax = ether offset;  cx = count;	 es:di = destination
public	ethempty
ethempty proc near
	cld				; incremental direction
	mov	dx,ebase+ei_ptr		; set the hardware buffer pointer
	out	dx,ax
	mov	dx,ebase+ei_buf		; point at hardware buffer window
ethempty1:
	in	al,dx			; get next byte
	stosb				; put it into memory
	loop	ethempty1		; repeat cx times
	ret
ethempty endp


;	call disabled;	returns al = 0ffh if queued, al = 0feh if not
eth_enqueue proc near
	cmp	q_count,Q_MAX		; is the q full already?
	jl	eth_enqueue1
	mov	al,0feh			; sorry, full
	ret
eth_enqueue1:
	inc	q_count			; adjust the count up
	mov	bx,q_tail		; look at the free spot
	mov	q_entries[bx],di	; queue the di and the es
	mov	q_entries+2[bx],es
	add	bx,Q_ENTRYSIZE		; point at the next slot
	cmp	bx,Q_SIZE		; wrap at end
	jle	eth_enqueue2
	mov	bx,0
eth_enqueue2:
	mov	q_tail,bx		; save the new tail away
	mov	al,0ffh			; indicate success
	ret
eth_enqueue endp


;	call disabled;	returns es and di from q with clc, or stc
eth_dequeue proc near
	cmp	q_count,0		; are we already empty?
	jne	eth_dequeue1
	stc				; nothing there
	ret
eth_dequeue1:
	dec	q_count			; take something off
	mov	bx,q_head		; look at next element
	mov	di,q_entries[bx]	; get the elements
	mov	es,q_entries+2[bx]
	add	bx,Q_ENTRYSIZE		; point at next element
	cmp	bx,Q_SIZE		; wrap at end
	jle	eth_dequeue2
	mov	bx,0
eth_dequeue2:
	mov	q_head,bx
	clc
	ret
eth_dequeue endp


  ifdef prints
public mlprint
mlprint proc near
	cmp	cs:debugon,0
	je	mlprintout
	call	lprint
mlprintout:
	ret
mlprint endp


public mouthex
mouthex proc near
	cmp	cs:debugon,0
	je	mouthexout
	call	outhex
mouthexout:
	ret
mouthex endp


hextable db '0$1$2$3$4$5$6$7$8$9$A$B$C$D$E$F$ $'
public	outhex
outhex	proc near
	push	ds
	push	cs
	pop	ds
	push	bx
	push	cx
	push	dx
	mov	ch,4
	mov	cl,12
hexloop:
	mov	bx,ax
	shr	bx,cl
	and	bx,0fh
	shl	bx,1
	mov	dx,offset hextable
	add	dx,bx
	call	lprint
	sub	cl,4
	dec	ch
	jg	hexloop
	mov	dx,(offset hextable) + 32
	call	lprint
	pop	dx
	pop	cx
	pop	bx
	pop	ds
	ret
outhex	endp
	

rowcol	dw 0

public	lprint
lprint	proc near
	cmp printon,0
	je lprintout
	push ax
	push bx
	push cx
	push dx
	push si
	push di
	push bp
	push ds
	push es
	push ss
	pushf
	cli

	mov si, dx
	push cs
	pop ds

	mov	dx,0b000h		; screen segment
	mov	es, dx
	mov	dx,rowcol		; row and column

out_loop:
	lodsb				; get character
	cmp al, '$'
	je end_of_string
	cmp al, 10
	je out_loop
;	je line_feed
	cmp al, 13
	je carraige

	mov bl,al			; save char in bl
	mov al,80			; get screen buffer address
	mul dh				; row
	push dx
	mov dh, 0
	add ax,dx			; col
	pop dx
	shl ax, 1
	mov bh, 7
	mov di,ax
	mov es:[di],bx			; write out the character

	inc dl				; next char position
	cmp dl, 79
	jbe mov_cur
carraige:
	mov dl, 0
line_feed:
	inc dh
	cmp dh, 23
	jbe mov_cur
	mov dh, 0
mov_cur:
	mov rowcol,dx
	jmp out_loop

end_of_string:
	popf
	pop ss
	pop es
	pop ds
	pop bp
	pop di
	pop si
	pop dx
	pop cx
	pop bx
	pop ax

lprintout:
	ret
lprint	endp
  endif

cseg	ends
end
