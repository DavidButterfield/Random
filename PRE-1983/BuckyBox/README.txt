Bucky Box -- 1981					Notes by David Butterfield, 2022

The "Bucky Box" was a serial port front-end processor for a Digital Equipment Corporation (DEC)
LSI-11/23 computer at the UCLA Math Department in the early 1980s.  It was designed and built by
James W. Lindelien in 1981.  It is based on the S-100 bus and a Zilog Z-80 CPU, and can support
up to 16 serial terminals.

The native DEC serial interfaces produced an interrupt for each character outgoing to an ASCII
terminal, which limited the number of users we could concurrently support at 9600 BAUD.

The purpose of the Bucky Box was to offload serial interrupt processing from the LSI-11 CPU,
using bulk transfers of character bytes and only producing an interrupt when more output was
needed.  This significantly reduced the per-character CPU processing, allowing us to increase
the number of concurrent users we could support under Unix v7 from three to seven.

The Bucky Box is in its own (very sturdy) standalone rack with power supply, fans, and an S-100
bus with five cards plugged into it:

    Z-80 CPU card
    RAM card
    2 OCTOPLUS 8-port serial cards
    Custom Wirewrap interface to ribbon cable

At the other end of the ribbon cable is an MDB bus foundation module card for the Q-bus, used by
the LSI-11 family of processors.

The design did not use DMA.  Bytes were written from the LSI-11 kernel to the Z-80 using a fast
loop with interrupts disabled on both sides.  To avoid character loss, the Z-80 loop (input and
store) had to be faster than the LSI-11 loop (fetch and output).

IIRC the Z-80 fast loop was a single instruction that input from a port and stored the result
into a memory location pointed to by a pair of registers, incremented the pointer register pair,
decremented a counter register, and repeated until counter reached zero.

The LSI-11/23 fast loop used a few instructions including a conditional jump.  It was necessary
to add one NOP instruction to the minimal LSI-11 loop to ensure the Z-80 reached its next input
before the LSI-11 reached its next output.  The Z-80 side would insert wait-states on input from
the port until the byte written by the LSI-11 was available.

While a grad student I ran this computer for the Math Department, and by coincidence I was
finishing up my M.S. and leaving for a startup company at the same time the LSI-11/23 was being
retired in favor of a new VAX.  I got the department to "loan" me the obsolete machine for a few
years, and when I finally turned it back in, I kept the Bucky Box.  I even have a full manual
somewhere.

In this directory are photos taken of the Bucky Box and its components when I unboxed it in
2022.  It had been in my garage since 1999, but the box was well sealed.  It was clean inside
when I opened it after dusting it off.  So far nothing has disintegrated in my hands.  The
ribbon cable flexes normally.  It seems in good shape.  But I'm afraid to plug it in.

Of course, there's no LSI-11 to connect it to, but IIRC it has some kind of monitor mode where
you can give it commands through one of the serial ports.  I need to find that manual.
