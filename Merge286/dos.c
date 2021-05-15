#ifdef SCCS
static char Lccsid[] = "@(#)dos.c	1.6   LCC";  /* Mod Time 12:59:27 8/7/87 */
#endif /* SCCS */
/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*	@(#)	1.18	*/

#include "sys/param.h"
#include "sys/types.h"
#include "sys/sysmacros.h"
#include "sys/systm.h"
#include "sys/dir.h"
#include "sys/signal.h"
#include "sys/user.h"
#include "sys/errno.h"
#include "sys/proc.h"
#include "sys/conf.h"
#include "sys/map.h"
#include "sys/buf.h"
#include "sys/tty.h"
#include "../sys/realmode.h"
#include "sys/mmu.h"
#include "sys/seg.h"
#include "sys/sxt.h"
#include "sys/8259.h"
#include "sys/kd_video.h"
#include "sys/kd_color.h"

#ifdef ATMERGE 

#define ktoc(x) ((x) << 1)		/* K to clicks */
#define ctok(x) (((x) + 1) >> 1)	/* clicks to K */


#define	CURINV	0x0800			/* invisible cursor */
#define	CURNORM	0x0707			/* visible cursor */

extern int msm_ok;			/* can we run merge on this system? */


struct proc *dosprocp = 0;		/* pointer to the DOS process */
int swscr_default, swmod_default;        
unsigned int screenbytes = 32768;	/* number of bytes in the screen */
int lockunixscr = 0;			/* disallow switching to DOS screen */ 
unsigned int altstart;			/* click address of alt screen */
int altseparate;			/* 1 if alt screen is disjoint
						from DOS partition */

#define NIMAGES 2
struct image {
	unsigned int im_addr;		/* pointer in swap space of image */
	unsigned int im_size;		/* size in cliks */
} dosimage[NIMAGES];

extern unsigned int dossize;		/* size of dos space in clicks */
extern char dosintmask;			/* dos interrupt mask word */
extern char dosok;			/* flag to say if we may switchtodos */
extern struct kda_state kda;		/* keyboard state */
char dosscrlock = 0;			/* if 1, don't switch to DOS screen */
char dosissafe = 0; 			/* if 1, can run DOS off UNIX screen */
extern struct sw_data usw_data;		/* switchdata structure */
extern int (*devreset[])();
int unixdev = 0xfffe;			/* mask of Unix device drivers */
unsigned char doskb = 0;                /* saves dos LED states        */
char *egabuffer;			/* buffer for ega screen memory */

extern unsigned	dosnewdriver;
extern unsigned	dosdriver;

/*
 * dos system call
 */
dos()
{
	register struct a {
		int req;		/* request type */
		int val;		/* argument to request */
		int val2;		/* argument 2 to request */
		int val3;		/* argument 3 to request */
	} *uap = (struct a *)u.u_ap;
	register int x;

	switch(uap->req) {
	case 0:				/* Make me a dos server */
		if (!suser())
			u.u_error = EPERM;
		else if (! msm_ok)
			u.u_error = ENOMEM;
		else if (dosprocp!=0)
			u.u_error = EBUSY;	/* exclusive */
		else if (!isconsole())
			u.u_error = EACCES;	/* not on console */
		else {
			dosscrlock = dosissafe = usw_data.sw_dosticks = 0;
			dosprocp = u.u_procp;	/* I'm the DOS guy */
			dosok = 0;		/* no DOS ints yet */
			usw_data.sw_recontrol = 0xff;
			protectdos(0);	   /* unprotect dos segments */
					/* preassign nonunix devs */
			devassign(0, ~unixdev);
		}
		break;

	case 1:				/* Switch to DOS */
		if (u.u_procp==dosprocp)
			sw_to_dos();
		else
			u.u_error = EPERM;
		u.u_rval1 = usw_data.sw_sbufaddr[0];
		u.u_rval2 = usw_data.sw_sbufaddr[1];
		break;

	case 2:				/* Get interrupt assignment mask */
		if (u.u_procp==dosprocp || suser())
			u.u_rval1 = usw_data.sw_devassign;
		break;

	case 3:				/* Set interrupt assignment mask */
		if (u.u_procp==dosprocp || (suser() && dosprocp!=0))
		{
					/* don't try to assign keyboard */
			devassign(uap->val, uap->val2 & 0xfffd);
			u.u_rval1 = usw_data.sw_devassign;
		}
		else
			u.u_error = EPERM;
		break;

	case 4:				/* select screen (DOS or UNIX) */
		if (u.u_procp==dosprocp || suser())
			if (uap->val != usw_data.sw_curscreen)
				swtchscr();
		break;

	case 5:				/* Initialize screen */
		if (u.u_procp==dosprocp || suser())
			initscreen(uap->val);
		break;

	case 6:				/* Set DOS memory space size */
		if (u.u_procp==dosprocp)
			u.u_rval1 = getdosspace(uap->val);
		else
			u.u_error = EPERM;
		break;

	case 7:				/* Inquire about DOS process */
		x = spl7();
		if (dosprocp==0)
			u.u_rval1 = 0;
		else
			u.u_rval1 = dosprocp->p_pid;
		splx(x);
		break;

	case 8:				/* Set the switch character */
		/* If you are the DOS proc then we assume you know what  */
		/* you are doing.  Otherwise there cannot be a DOS	 */
		/* process (because of keyboard state) and you must be   */
		/* either the Superuser or on the console.		 */
		if (uap->val)
		{
			if (u.u_procp==dosprocp
			    || (dosprocp==0
				&& (isconsole() || suser())))
				    usw_data.sw_swscrchar = uap->val & 0x067f;
			else
			{
				u.u_error = EPERM;
				break;
			}
		}
		/* return old values */
		u.u_rval1 = usw_data.sw_swscrchar;
		u.u_rval2 = swscr_default;
		break;

	case 9:				/* Reboot the system */
		if (suser())
			reboot();
		break;

	case 10:			/* Set Image */
	{
		struct image *im;
		unsigned int coreaddr = ktoc(uap->val2);
		unsigned int size = ktoc(uap->val3);
		if (u.u_procp!=dosprocp)
		{
			/* Must be the DOS process to do this */
			u.u_error = EPERM;
			break;
		}
		if (uap->val >= NIMAGES) 
		{
			/* bad argument */
			u.u_error = EINVAL;
			break;
		}
		im = &dosimage[uap->val];
		if (coreaddr + size > dossize)
		{
			/* out of bounds */
			u.u_error = EACCES;
			break;
		}
		/* free the old space if any */
		if (im->im_size) 
		{
			mfree(swapmap, ctod(im->im_size), im->im_addr);
			im->im_size = 0;
		}
		/* if the new size is zero, it's time to get off */
		if (size==0)
			break;
		/* allocate space in swap for the image */
		if ((im->im_addr=swapalloc(size,0))==0) 
		{
			/* No space in swap */
			u.u_error = ENOSPC;
			break;
		}
		im->im_size = size;
		/* write out the image */
		swap(u.u_procp,im->im_addr,coreaddr,size,B_WRITE);
		break;
	}

	case 11:			/* Load Image */
	{
		struct image *im;
		unsigned int coreaddr = ktoc(uap->val2);
		if (u.u_procp!=dosprocp) 
		{
			/* Must be the DOS process to do this */
			u.u_error = EPERM;
			break;
		}
		if (uap->val >= NIMAGES) 
		{
			/* bad argument */
			u.u_error = EINVAL;
			break;
		}
		im = &dosimage[uap->val];
		if (coreaddr + im->im_size > dossize) 
		{
			/* out of bounds */
			u.u_error = EACCES;
			break;
		}
		if (im->im_size!=0) 
		{
			/* read in the image */
			swap(u.u_procp,im->im_addr,coreaddr,im->im_size,B_READ);
		}
		u.u_rval1 = ctok(im->im_size);
		break;
	}

	case 12:			/* Check Image */
		if (u.u_procp!=dosprocp && !suser()) 
		{
			/* Must be the DOS process or superuser to do this */
			u.u_error = EPERM;
			break;
		}
		if (uap->val >= NIMAGES) 
		{
			/* bad argument */
			u.u_error = EINVAL;
			break;
		}
		/* return the size of this image in clicks */
		u.u_rval1 = ctok(dosimage[uap->val].im_size);
		break;

	case 13:			/* Set new DOS mem driver addr */
		if (u.u_procp!=dosprocp) 
		{
			/* Must be the DOS process to do this */
			u.u_error = EPERM;
			break;
		}
		if (uap->val >= dossize) 
		{
			/* out of bounds */
			u.u_error = EACCES;
			break;
		}
		dosdriver = uap->val;
		dosnewdriver = 1;
		dosok = 1;
		dclkinit();
		msmenable (1);		/* enable the DOS timer int      */
		mode_setup();           /* set up screen & cursor modes  */
		break;

	case 14:
		/* tell caller if is running from the console */
		u.u_rval1 = isconsole(); /* 1=on console, 0=not */
		break;

	case 15:			/* manage dosscrlock */
		/* dosscrlock == 1 means do not switch to DOS screen */
		if (uap->val == 2)
		    u.u_rval1 = dosscrlock;
		else
		    u.u_rval1 = dosscrlock = (uap->val == 0);
		break;	

	case 16:			/* manage dosissafe */
		/* dosissafe == 1 means dos can run in the background */
		if (uap->val == 2)
		    u.u_rval1 = dosissafe;
		else
		{
		    u.u_rval1 = dosissafe = (uap->val != 0);
		    if (dosissafe) {
			devassign(0,1);		/* give timer to DOS */
		    }
		}
		break;

	case 17:                    
		break;

	case 18:		/* Set the mode switch character */
		/* If you are the DOS proc then we assume you know what  */
		/* you are doing.  Otherwise there cannot be a DOS	 */
		/* process (because of keyboard state) and you must be   */
		/* either the Superuser or on the console.		 */
		if (uap->val)
		{
			if (u.u_procp==dosprocp
			    || (dosprocp==0
				&& (isconsole() || suser())))
				    usw_data.sw_swmodchar = uap->val & 0x067f;
			else
			{
				u.u_error = EPERM;
				break;
			}
		}
		/* return old values */
		u.u_rval1 = usw_data.sw_swmodchar;
		u.u_rval2 = swmod_default;
		break;

	case 19:			/* get merge version */
		u.u_rval1 = 0;
		break;

	default:			/* Bad function value */
		u.u_error = EINVAL;
		break;
	}
}

/*
 * This is called once at boot time
 */
dosinit()
{
	int i, j;
	extern struct seg_desc gdtalias, idt;
	extern char kb_id;


	/* Set defaults for switch characters */
	swscr_default = 0x0454;	/* CTL-SYSREQ */
	swmod_default = 0x0654; /* CTL-LSFT-SYSREQ */

	/* setup various fields in the switchdata structure */
	*(struct seg_desc *)usw_data.sw_gdtcopy = gdtalias;
	*(struct seg_desc *)usw_data.sw_idtcopy = idt;


	/* Now initialize the screen state */
	usw_data.sw_curscreen = UNIXMODE; /* We start off on the Unix screen */

	/* set switch screen character */
	usw_data.sw_swscrchar = swscr_default;
	/* set switch mode character */
	usw_data.sw_swmodchar = swmod_default;

	/* initialize devices */
	usw_data.sw_devinuse |= DEV_NOASSIGN; /* Unix has unassignable devs */
#ifdef NOTDEF
	/* for the moment, this does not seem to be necessary, and especially
	   we do not want to call flopreset(), as that will trigger an
	   unexpected floppy interrupt.
	 */
	for (i=0, j=1; i<NDEVS; i++, j<<=1)
		if ((usw_data.sw_devinuse & j) == 0)
			(*devreset[i])(i);    /* set devices to known state */
#endif

	/* Let's be paranoid and make sure that the clock and KB are right */
	unixdev |= 0x100;	/* Unix clock driver */
	unixdev |=  2;		/* Unix has the keyboard */
	msmdisable (1);		/* disable DOS timer for now */

	printf("\nMerge 286 Version 1.30\n");
	printf("Copyright (c) 1985, 1987 Locus Computing Corporation\n");
	printf("All Rights Reserved\n\n");
}

/*
 * This is called once for each process exit
 */
dosexit()
{
	int x;
	if (u.u_procp!=dosprocp)
		return;

	/* DOS process is exiting, clean up after it */
	x = spl7();
	dosprocp = 0;			/* No more DOS to kick around */
	dosok = 0;			/* don't run anymore DOS code */
	msmdisable (1);			/* turn off DOS timer         */

	/* Check if primary int. base is what it was when initialized */
	if ((usw_data.sw_lastprimary & 0xf8) != MICW2)
	{
		initpic();		/* Re-setup interrupt bases */
		outb(0x70, 0xc);	/* Select register C in cmos */
		inb(0x70);		/* Special RTC EOI */
		usw_data.sw_lastprimary = MICW2;
	}

	if (usw_data.sw_curscreen != UNIXMODE)
		swtchscr();		/* switch to Unix screen */
	devassign (DEV_ALLUNIX,0xfffe);	/* give all devices except timer */
					/*	to Unix */
	splx (x);

	dosfree();			/* Free all DOS memory */
	doskb = 0;			/* Clear DOS keyboard state */
	outb(0x61, inb(0x61)&0x7c);	/* Turn off speaker and retain */

}

/*
 * Called at interrupt time.
 *
 * This is called when a catastophic error occurs.  It is called "cli"
 * and must spl7() before doing anything else.  It is OK for it to 
 * sti and splx() if it wants to, after it is done zapping devices.
 *
 * This is as good a place as any to explain the flag dosok.  Dosok
 * is what allows go_to_dos to actually transfer control to realmode.
 * If it is clear, go_to_dos short circuits that process and jumps to
 * the return_from_dos code so that it appears to higher level code as
 * though we went and returned.
 *
 * Dosok is set when the user specifies the address of the
 * switch.sys memory driver.  Therefore it is important that the
 * user process always copy a valid image to DOS space before doing
 * this.  Since the user process is required to be superuser to
 * even have the opportunity to do this, we trust it implicitly.
 *
 * Dosok is cleared when the user process exits, or when we have
 * decided to abort DOS.  At this point we have assigned all the devices
 * back to Unix anyway so it doesn't matter, with the exception of the
 * keyboard.  Since the user can assign the keyboard to DOS without there
 * existing a DOS process, the dosok flag can save us from an attempt to
 * have a nonexistent DOS handle that interrupt.
 */
dosabort()
{	int x = spl7();
	dosok = 0;			/* signal that we are going away */
	devassign(DEV_ALLUNIX,0xfffe);	/* and give all devices except timer */
					/*	to Unix */
	if (dosprocp!=NULL)
		psignal(dosprocp, SIGILL);	/* make this guy go away */
	splx(x);
}

/*
 * This is the table of device routines for DOS assignment.  The
 * devreset routine is called when the device is retired from use
 * by either DOS or Unix, and it should put the device into a
 * standard DOS state.  We want to leave the device in the state
 * that DOS would like to find it, because an auto-assignment
 * occurs on the DOS side without notification to the Unix side.
 * The clock is an exception since it needs to be turned on when
 * it is assigned to DOS; there is no problem, however, because
 * the clock is always assigned to DOS a priori and is never
 * auto-assigned.
 */
int nuldevreset(), nulldev();
extern int flopreset();

int (*devreset[NDEVS])() = {
	nuldevreset, nuldevreset, nuldevreset, nuldevreset,
	nuldevreset, nuldevreset, flopreset,   nuldevreset,
	nuldevreset, nuldevreset, nuldevreset, nuldevreset,
	nuldevreset, nuldevreset, nuldevreset, nuldevreset
};

static msmenable(mask)
{
    int x = spl7 ();
    extern unsigned msmmask;

    msmmask &= ~ mask;
    splx (x);
}

msmdisable(mask)
{
    int x = spl7 ();
    extern unsigned msmmask;

    msmmask |= mask;
    splx (x);
}

/*
 * This tries to make the interrupt assignments under the mask "new"
 */
devassign(new,mask)
{
	register int num, bit;
	int changes;
	int x = spl7();

	changes = usw_data.sw_devassign ^ new; /* differences from old to new */
	changes &= mask;		/* under the mask */
	changes &= ~usw_data.sw_devinuse; /* don't change if in use by unix */
	changes &= ~DEV_NOASSIGN;	/* Some devices are not assignable */

	if (changes) {
	    for (num=0, bit=1; num<NDEVS; num++, bit<<=1) {
		if ((changes&bit)==0)
			continue;
		if (new&bit) {		/* assign to Unix */
			(*devreset[num])(num);	/* Zap the device */
			aeoi(num);
			usw_data.sw_devassign |= bit;	/* assign it */
		} else {		/* assign to DOS */
			usw_data.sw_devassign &= ~bit;	/* assign it */
			dosintmask &= ~bit;	/* remove from DOS mask list */
		}
	    }
	    usw_data.sw_recontrol = 0xff;
	    if (!dosok || (usw_data.sw_curscreen == UNIXMODE && !dosissafe)) {
		msmdisable(~usw_data.sw_devassign);
	    	msmenable(usw_data.sw_devassign);
	    } else {
		msmenable(0xffff);
	    }
	}
	splx(x);
}

/* sets the bit in sw_devinuse which corresponds to the argument */
/* if udevassign allows */
devclaim(devnum)
int devnum;
{
	int mybit = 1<<devnum;
	int rc = -1;
	int x = spl7();
	if (usw_data.sw_devassign&mybit) {
		usw_data.sw_devinuse |= mybit;
		usw_data.sw_recontrol = 0xff; /* tell DOS to recomp control */
		rc = 0;
	}
	splx(x);
	return(rc);
}

/* clear the bit in sw_devinuse that corresponds to the argument */
devrelse(devnum)
int devnum;
{
	int x;
	int mybit = 1<<devnum;
	x = spl7();
	(*devreset[devnum])(devnum);	/* Zap the device for DOS */
	usw_data.sw_devinuse &= ~mybit;
	usw_data.sw_recontrol = 0xff;	/* tell DOS to recompute control */
	splx(x);
}

/*
 * interrupt handler for DOS clock when when it is not assigned to DOS.
 * This happens once when the system starts up, due to the BIOS having 
 * done us the favor of turning it on.  It can also happen when a DOS
 * process is exiting, and the clock interrupts before we get a chance
 * to turn it off.
 */
dosintr()
{
}

/*
 * null device reset routine
 */
nuldevreset(devnum)
{
}

/*
 * Called at interrupt time.
 *
 * toggle between DOS/UNIX screens
 * update display flag, set map, exchange buffers
 * change keyboard assignment
 *
 * If switching to the UNIX screen, disable all interrupts assigned to
 * DOS.  If switching to the DOS screen, enable all interrupts assigned
 * to DOS.
 */

static int dos_cursor;

swtchscr()
{
    int x = spl6 ();  

    if (usw_data.sw_curscreen == UNIXMODE){
	swtodos ();
    } else { 
	swtounix ();
    }
    splx(x);  
}

#define CRT_MODE     0x49
#define CRT_MODE_SET 0x65
#define CRT_START    0x4e
#define CRT_PALLETTE 0x66
#define KB_FLAG      0x17


static swtounix()
{
    char *bcomm = gstokv(LOW_SEL (0)) + 0x400;

    /* turn the speaker off */
    outb(0x61, inb(0x61) & ~2);

    usw_data.sw_curscreen = UNIXMODE;
    dos_cursor = readcursor();

    devassign(3,3);		/* give kbd and DOS timer to Unix */

    unix_mode(bcomm[CRT_MODE]);
    setvid(-1, -1, -1);	/* set start addr, cursor addr, colors */

    doskb = bcomm[KB_FLAG] & 0x70;
    set_unix_kb();
}

static swtodos()
{
    char *bcomm = gstokv(LOW_SEL (0)) + 0x400;

    if (dosscrlock)
	return;

    set_dos_kb();
    usw_data.sw_curscreen = DOSMODE;
    dos_mode (bcomm[CRT_MODE]); /* set BIOS screen mode */
    wakeup(&usw_data.sw_curscreen);
    setvid(*((int *)&bcomm[CRT_START]), dos_cursor, bcomm[CRT_PALLETTE]);
    devassign (0,3);			/* give kbd and DOS timer to DOS */

}


/*
 * initialize screen 
 */
initscreen(flag)
{
	/* flag = 0 we copy the Unix screen to the DOS one */
	/* flag = 1 we blank the DOS screen */
	register short *sscreen;
	register short *dscreen;
	register int i, j, k;
	unsigned int offset = 0;
	unsigned int scrbytes = scrsize ();

	/* turn off video */

	video_off();

	/* Move the bytes - one screen is 25x80 char/attr words.
	   We copy 23 lines, and blank the last two. */
	sscreen = (short *) gstokv(VIDEOSEL);
	dscreen = (short *) gstokv(ALTSCREEN_SEL);
	for (k = 0; k < 23; k++)
	{
	    /* Check for wrap around - assumes all screens wrap same way. */
	    offset += 160;
	    if (offset > scrbytes)
	    {
	    	offset -= scrbytes;
		sscreen = (short *) gstokv(VIDEOSEL)  +  offset;
	    }

	    /* Copy 1 line of 80 char/attr words */
	    for (i = 0; i < 80; i++)
		*dscreen++ = flag ? 0x720 : *sscreen++;
	}

	/* blank out two more lines */
	for (i = 0; i < 160; i++)
	    *dscreen++ = 0x720;

	/* turn on video */
	video_on();
}

/*
 *	This is stuff to do on the way to DOS when we are at a
 *	non-nested level, that is, we are not processing an
 *	interrupt.  This occurs when doing a sw_to_dos or when
 *	doing an iret_to_dos when when udosnest is zero (which
 *	occurs at the end of an interrupt).  The fact that this
 *	simple looking code works is really rather subtle in the
 *	following ways:
 *
 *	(0) The test for runrun and the qswitch are easy, since
 *	when we are rescheduled we are brought right back here
 *	and we then get to go on as if nothing happened.  (Qswitch
 *	itself may be fun to understand, but that's another story.)
 *
 *	(1) This code may longjump us back to syscall and
 *	from there to the user.  Thus any global state variables
 *	must be reset to their proper values if this happens.
 *	Currently there are none that have to be set.  (Udosnest
 *	must be zero, but this routine is only called when it is
 *	zero, so it requires no attention.)
 *
 *	(2) Handling signals in this way works provided that the
 *	user recognizes EINTR error returns from his sw_to_dos
 *	system call and reissues the system call when his signal
 *	catching routine returns.  This is because the coroutine
 *	nature of the DOS/Unix interaction allows coroutine "returns"
 *	to DOS to look the same as simple interrupt returns.  The
 *	subsequent user re-issue of the sw_to_dos call will effect
 *	the return from the interrupt which originally caused the
 *	signal.
 */

to_dos_checks()		/* stuff to do on the way to dos */
{
	while(1){
	    if (runrun)
		qswtch();
	    if (u.u_procp->p_sig && issig())
		longjmp(u.u_qsav);
 	    if(!dosissafe && usw_data.sw_curscreen == UNIXMODE)
		sleep(&usw_data.sw_curscreen, PUSER);
	    else
		break;
	}
}


protectdos(flag)
int flag;
{
int protect, i;

	protect = flag? ACC_KDATA : ACC_UDATA;
	for (i=LOW_SEL(0); i<LOW_SEL(16); i ++)
		gdt[i].sd_access = protect;
}

/* Dosexec is requesting a DOS partition of size K bytes.  Allocate as
   much as possible, and allocate the alternate screen buffer as well.
 */
getdosspace(sizek)
int sizek;
{
    unsigned int desclicks, screenclicks, sizeclicks, egasave;
    long start;

    egasave = kda.ega ? 28672 : 0;
    sizeclicks = ktoc(sizek);
    altseparate = 0;

    /* Determine the size of the console screen */
    screenbytes = scrsize() + egasave; 
    screenclicks = btoc(screenbytes);

    /* Try to dosmalloc() the user's requested size + screen size.  We
       must end up with an even number of clicks (K multiple). */
    desclicks = sizeclicks + screenclicks;
    dosmalloc(desclicks);	/* sets dossize */
    if (dossize <= 1)
	return(0);

    if (dossize & 1)
    {
    	/* Since desclicks must be an even number, if we got an odd number,
    	   give back the last click. */
	   mfree(coremap, 1, --dossize);
    }

    if (dossize == desclicks)
    {
    	/* Fabulous - give the last screenclicks clicks to the alt screen */
    chop:
    	if (dossize < screenclicks)
    	{
	    mfree(coremap, dossize - 1, 1);
	    return(0);
	}
    	dossize -= screenclicks;
    	altstart = dossize;
    }
    else
    {
    	/* Didn't get all we wanted.  Try to malloc() the alt screen */
    	altstart = malloc(coremap, screenclicks);
    	if (altstart > 0)
    	{
	    /* We got what we needed from the malloc().  If there is any
	       excess in what dosmalloc() got, free it. */
	    if (dossize > sizeclicks) 
	    {
		mfree(coremap, dossize - sizeclicks, sizeclicks);
		dossize = sizeclicks;
	    }
	    altseparate = 1;
	}
    	else
    	{
	    /* Failed to malloc() the alternate screen buffer.  Steal it
	       from the partition. */
	    goto chop;
	}
    }

    start = ctob((long)altstart);
    gdt[ALTSCREEN_SEL].sd_hibase = lobyte(hiword(start));
    gdt[ALTSCREEN_SEL].sd_lowbase = loword(start);
    gdt[ALTSCREEN_SEL].sd_limit = screenbytes - 1;
    gdt[ALTSCREEN_SEL].sd_access = ACC_KDATA;

    if (kda.ega)
        egabuffer = gstokv(ALTSCREEN_SEL) + scrsize();
    return(ctok(dossize));
}

reboot()
{
}

#endif /* ATMERGE */
