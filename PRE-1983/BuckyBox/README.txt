Bucky Box -- 1981			Notes by David Butterfield, 2022

The "Bucky Box" was a serial port front-end processor for a Digital Equipment Corporation (DEC)
LSI-11/23 computer run by the UCLA Math Department in the early 1980s.

The Bucky Box was designed and built by James W. Lindelien in 1981.  It is based on the S100 bus
and a Zilog Z-80 CPU, and can support up to 16 serial terminals.

The native DEC serial interfaces produced an interrupt for each character outgoing to an ASCII
terminal, which we found limited the number of users that could be concurrently supported under
Unix version 7 (with adequate performance) to three.

The purpose of the Bucky Box was to offload serial interrupt processing from the DEC CPU,
allowing bulk transfers of character bytes and only producing an interrupt when more output
characters were needed.

This significantly reduced the per-character CPU processing, allowing us to increase the number
of concurrent users on the system from three to seven.

The design did not use DMA.  Characters were written from the LSI-11 kernel to the Z-80 using a
fast loop with interrupts disabled on both sides.  To avoid character loss, the Z-80 loop
(reading) had to be faster than the LSI-11 loop (writing).  It was necessary to add one NOP
instruction to the LSI-11 loop to ensure this.  The Z-80 side would insert wait-states on input
from the port until the character written by the LSI-11 was available.
