Bucky Box -- 1981						    Notes by David Butterfield

April 10, 2022

The "Bucky Box" was a serial port front-end processor for a Digital Equipment Corporation (DEC)
LSI-11/23 computer at the UCLA Math Department in the early 1980s.  It was designed and built by
James W. Lindelien in 1981.  It was based on the S-100 bus and Zilog Z-80 CPU, supporting up to
16 serial terminals.

The native DEC serial interfaces produced an interrupt for each character outgoing to an ASCII
terminal, which limited the number of users we could concurrently support at 9600 BAUD.

The purpose of the Bucky Box was to offload serial interrupt processing from the LSI-11 CPU,
using bulk transfers of character bytes and only producing an interrupt when more output was
needed.  This significantly reduced the per-character CPU processing, allowing us to increase
the number of concurrent users we could support under Unix v7 from three to seven.

I recently rediscovered this device, which had been sealed in a box since around 1989.  It had
been in my garage since 1999, but the box was well sealed, and clean inside when I opened it
after dusting off the outside.  I took these photographs after unboxing the machine in 2022.

The Bucky Box is in its own very sturdy standalone rack with power supply, dual fans, and a
ten-slot S-100 bus with five cards plugged into it:

    Z-80 CPU card
    RAM card
    2 OCTOPLUS 8-port serial cards
    Custom wirewrap interface to ribbon cable

At the other end of the ribbon cable is an MDB bus foundation module card for the Q-bus, used by
DEC's LSI-11 family of processors.

The design did not use DMA.  Bytes were written from the LSI-11 kernel to the Z-80 using a fast
loop with interrupts disabled on both sides.  To avoid character loss, the Z-80 loop (input and
store) had to be faster than the LSI-11 loop (fetch and output), so that the Z-80 was always
ready to input when the LSI-11 did the output.  The Z-80 side would insert wait-states on input
from the port until the byte written by the LSI-11 was available.

The Z-80 fast loop was a single instruction that input from a port and stored the result into a
memory location pointed to by a pair of registers, incremented the pointer register pair,
decremented a counter register, and repeated until the counter reached zero.

The LSI-11/23 fast loop used a few instructions including a conditional jump.  It was necessary
to add one NOP instruction to the minimal LSI-11 loop to ensure the Z-80 reached its next input
before the LSI-11 reached its next output.

While a grad student I ran this computer for the Math Department, and by coincidence I was
finishing up my M.S. and leaving for a startup company at the same time the LSI-11/23 was being
retired in favor of a new VAX.  I got the department to "loan" me the obsolete machine for a few
years, and when I finally turned it back in, I kept the Bucky Box.  I even have a full manual
somewhere.

So far nothing has disintegrated in my hands.  The ribbon cable flexes normally.  It seems to be
in good shape.  But I'm afraid to plug it in.

Of course, there's no LSI-11 to connect it to, but IIRC it has some kind of monitor mode where
you can give it commands through one of the serial ports.  I need to find that manual.

April 12, 2022

I unboxed my H19 RS-232 ASCII terminal and it seemed to work offline.

April 13, 2022

I found the manual for the Bucky Box.  From the schematics I was able to see how to wire a
serial connector.  I have a few of the old "DB-25 to phone jack" dongles from Locus, so I just
took a phone jack module and screwed its terminal ends down to the Bucky Box's terminal block in
a compatible way.

I put the CPU and memory cards back into the Bucky Box along with one of the serial cards.  For
simplicity I left out the second serial card and the bus interface card.  I plugged in the Bucky
Box and switched it on.  What a noisy pair of fans!

Garbage streamed onto the CRT screen in that distinctive manner characteristic of a BAUD rate
mismatch.  I googled up a manual for the H19 and switched it to 9600 BAUD, then reset the Bucky
Box again.

The Bucky Box came up and printed its help menu like it is supposed to!  But then it began
continuous printing of a warning message "Warning: Invalid PDP function request issued."
Apparently the firmware doesn't handle a missing bus interface card very gracefully.

I tried with the card in, but without the ribbon cable to the LSI-11 card; but that wouldn't
even boot up with the help menu.  Adding the ribbon cable with the LSI-11 card on the other end
shows the same behavior as omitting the S-100 card -- the continuous warning message.

