.\" SCCSID @(#)tmac.Xlong	1.1                    Modified: 15:24:53   9/10/87
.\"
.\"			NROFF/TROFF  MACROS   by Bruce Walker
.\"
.\"
.\"
.\"  These macros are compatible with version 7 troff/nroff and eqn/neqn.
.\"  The macros have been designed to meet the needs of as many users as
.\"  possible by having most macros controlled by user settable variables.
.\"  The following scenerio outlines how to take advantage of this:
.\"	.so /usr/lib/tmac/tmac.X	\" bring in the macro file
.\"     .nr tf 1.5i	\" make changes to variables
.\"     .ds Te 'EVEN PAGE'TITLE'LINE'
.\"	.ds To 'ODD PAGE'TITLE'LINE'
.\"	.en	 	\" set up the environment so paging, etc. happens.
.\"	continue with the text of the paper etc.
.\"
.\"  In particular, NO ONE should make a copy of either this file or
.\"  /usr/lib/tmac/tmac.X.  They should perhaps keep a copy of the variable
.\"  changes they like to run with but should not copy this file and
.\"  then make changes to it.  For one thing, this is a gross waste of
.\"  disk space and for another thing, this macro file is quite
.\"  complicated and any changes made to it could cause it to stop
.\"  running.
.\"
.\"  This file is not the tmac.X file that was used in the .so command
.\"  in the scenerio above.  The file in /usr/lib/tmac/tmac.X is the same as
.\"  this one except that all comments have been deleted to allow it to
.\"  run faster.
.\"
.\"
.\"
.\"			USER MACROS
.\"
.\" .en  - set up the environment (ie. traps for page start and end).
.\"	 - this should be called near the start of an nroff.
.\"	  - "th" is the total space of header (in units);
.\"	  - "bh" is space before and including header (in units)
.\"	  - "tf" is the total space of footer (in units);
.\"	  - "bf" is space before and including footer (in units);
.\" .pp	 - start a paragraph.
.\"	 - spaces "sh" vertical; needs "sg" vertical; indents "sf" units;
.\" .sc	 - start a new chapter.
.\"	 - begin a new page; can have 0 or 2 parameters; with 0 parameters,
.\"	   the next two input lines are centered, after "sl" unit spaces;
.\"	   With two parameters, the first can be either +,- or chapter #;
.\"	   A + indicates that a one line title consisting of the second
.\"	   parameter should be centered, after the appropriate spaces.
.\"	   It should also appear as is in the Table of Contents.
.\"	   A - indicates that the second parameter is to appear in the
.\"	   Table of Contents but that no spacing or centering or printing
.\"	   of a title is to be done.
.\"	   With a chapter # as the first parameter, a
.\"	   two line chapter title is centered after "sl" unit blank lines.
.\"	   The first line is CHAPTER #;  The second is as given in the
.\"	   second parameter.  The Table of Contents entry is the concat of
.\"	   the two parameters.
.\" .sa  - start a new appendix.
.\"	 - begin a new page and space down "sl" unit spaces.  There can be
.\"	   0 or 2 parameters.  With 0 parameters the next two input lines
.\"	   are centered.  With two parameters, the first is the appendix
.\"	   name/number.  The second is the title.  A two line appendix title
.\"	   is centered after "sl" unit blank lines.  The first line is
.\"	   APPENDIX \\$1.  The second is \\$2, where \\$1 and \\$2 are the
.\"	   two parameters to .sa.  The table of contents entry is
.\"	   Appendix \\$1 \\$2 .............  page#
.\" .su	 - start a new subsection.
.\"	 - "sa" unit spaces are left; "sm" units are needed; There can be
.\"	   0, 1 or 2 parameters.  With zero parameters, the next input line
.\"	   is underlined with .ul.  With one parameter, it is underlined and
.\"	   put as is in the table of contents.  With two parameters, the
.\"	   second parameter is a level number which is used to control the
.\"	   indenting of the entry in the table of contents.
.\" .es	 - example start;
.\"	 - space "si" vertical; save mode (fill/nofill); switch to nofill
.\"      - indent "sj" units;
.\" .ee  - example end;
.\"	 - space "sk" vertical; indent -"sj" units; restore fill mode if needed
.\" .l1	 - start a list;
.\"	 - set the first tab to be the current indent + "sn" and 2 other
.\"	 - tabs at 5m intervals; indent +"sn" units;
.\"	 - There is an optional parameter.  If a parameter is given, the
.\"	   list is automatically numbered in "format" which is given
.\"	   as the parameter (1 is straight numbering; i is roman numerals;
.\"	   a is for letters (see .af built in command in ref manual)).
.\" .e1	 - end list;
.\"	 - break; indent -"sn" units; restore tabs to default;
.\" .le  - list entry; done before each list entry.
.\"	  - space down "sp" units; temporarily indents -"so" units.
.\"	  - If the list is being numbered automatically (see .l1), then the
.\"	    number, followed by a dot, followed by a tab and set up so
.\"	    next user's input will go on the same line (ie. a \c) is done.
.\" .fn  - start footnote;
.\"	 - If the user wishes to number footnotes on a page by page basis,
.\"	   use either \*N (for superscripting) or \*O (for [#]) at the
.\"	   reference point and at the start of the reference.
.\" .ef  - end footnote text.
.\" .fs  - figure start.  Text up to .fe is printed in environment 2.
.\"	 - If there is room on the current page, the figure will be printed
.\"	   there.  Otherwise the page will be filled with subsequent text
.\"	   and the figure will appear at the top of the next page.
.\" .fe  - figure end.  There is an optional parameter which is the entry
.\"	   that will be put in the List of Figures.
.\" .fg  - full page figure.  There is an optional parameter which is the
.\"	   entry that will be put in the List of Figures.  The page on
.\"        which the full page figure would be put is omitted (to save
.\"        troffing) but the page numbers are incremented accordingly.
.\"        NOTE: Use of this does not work with multi-columning (see MC).
.\" .rb  - reference begin.
.\"	 - set tabs to "se", "se"+5, "se"+10; save current indent;
.\"	   go to spacing set in "sq"; indent to "se";
.\" .rf  - reference entry.
.\"	 - space "sd" units; temporarily indent -"se" units; need "sc" units;
.\" .re  - refernce end.
.\"   - reset indentation to that value saved at reference begin.
.\"	   restore line spacing to it's previous value;
.\"	   Restore tabs to default.
.\" .LL  - change line length and length of titles to the parameter given.
.\"	 - This macro changes these lengths in all the environments.
.\" .MC  - Request to do multiple columns
.\"	   This macro expects 4 parameters
.\"	     - number of columns
.\"	      - width of each column (units must be given).
.\"	      - width of space between columns (units must be given).
.\"	      - page offset of the whole thing (units must be given).
.\"	 - NOTE: use of ".fg" does not work with multi-columning.
.\" .PS  - change the character point size to the parameter given.
.\"	 - This macro changes the point size in all the environments.
.\" .RT  - reset tabs to standard nroff or troff.
.\" .EQ  - start an equation; save mode (fill/nofill); switch to nofill.
.\"	 - 2 parameters: first is L,C, or I - for left justified,
.\"	   centered or indented equation.  Second is the equation number.
.\"      - if only 1 parameter, it is treated as an equation number unless
.\"        it is L, C, or I.
.\" .EN  - end and equation; restore fill mode if necessary.
.\" .HS  - a macro which is called at the beginning of the header macro.
.\"	   The user can redefine this. It is initialized to null.
.\" .HE  - a macro which is called at the end of the header macro.
.\"	   The user can redefine this. It is initialized to null.
.\" .FS  - a macro which is called at the beginning of the footer macro.
.\"	   The user can redefine this. It is initialized to null.
.\" .FE  - a macro which is called at the end of the footer macro.
.\"	   The user can redefine this. It is initialized to null.
.\"
.\"
.\"			HIDDEN MACROS
.\"
.\" .hd	 - header macro, executed at the top of each page (after .en done).
.\" .fo  - footer macro, executed at the bottom of each page or when
.\"	   footnotes must start to print.
.\" .mf  - multiple column footer processing.
.\" .fx  - end of page trap which is hit if footnotes are printing.  It
.\"	   saves excess footnotes in "fy".
.\" .xf	 - print footnotes macro; called by .fo if there are any to print.
.\"	   This can trap to .fx.
.\" .fz  - save left over footnote for printing on a subsequent page.
.\"	   This is called by .hd if there were left over footnote text.
.\" .Ff	 - print figure macro; called by .fo if there are any to print.
.\" .Fa,Fb,Fc,Fd,Fe,F0 - used in figure collection and printing.
.\" .T1,T2 - used in Table of Contents and List of Figure entry generation.
.\" .SC  - used by .sc to ensure that a new page is started.  It is a recursive
.\"	   .bp.  A single .bp in .sc would be insufficient because it might
.\"	   be used to get to a new page to print a figure.  Then the chapter
.\"	   would not be on a new page.
.\" .ZZ  - Cleanup at end of processing. This is automatically called at
.\"	   the end of processing.
.\"	   This will dump any figures which have been diverted and are
.\"	   waiting for the next page.  Also it will print the Table of
.\"	   Contents and List of Figures if they are requested.
.\" .AB  - Abort and print error message.
.\"
.\" 		REGISTERS USED OTHER THAN THOSE WHICH ARE USER SETTABLE
.\"
.\" t  : internal temporary used for page numbers.
.\" x  : counter- counts number of footnotes to be genned at bottom of page.
.\" y  : footer top - has negative of # u's from bottom of page at which
.\"                   footer starts, includes space for footnotes
.\" z  : temporary variable used in footnote processing.
.\" w  : footnote counter usable by user to number his footnotes on each page
.\" c  : internal  -- column count for multiple columning
.\" z1 : internal  -- size of leftover footnote
.\" he : internal  -- flag to footer to tell that it is in a header.
.\" fa : internal  -- flag from footer to header to tell it to do new page.
.\" fg : internal  -- flag indicating that a figure is being printed.
.\" fn : internal  -- flag indicating that a footnote is being processed
.\" ex : internal  -- flag indicating that processing is in an example
.\" fs : internal  -- flag indicating that a figure is being processed.
.\" tt : internal  -- flag to indicate that a figure has caused a trap
.\" mx : internal  -- line number saved to go back to for multiple columning.
.\" cw,cs,co,cp: internal  -- used for multiple columnning.
.\" f0-f7 : internal  - used for figures.
.\" h1-h2 : internal  - used for headers and footers.
.\" l0-l9 : internal - used for lists
.\" t1 : internal  - used in reference macros to save status.
.\" FI : internal  - saves fill/nofill mode status in equations and examples
.\" dn : troff counter - counts # of lines of text in last diversion
.\" nl : troff counter - counts # of lines of text on current page
.\" .p : troff variable - contains current page length
.\" .t : troff variable - length to next trap
.\" .i : troff variable - current indent.
.\" .$ : troff variable - number of parameters to current macro.
.\" .z : troff variable - current diversion name
.\" %  : troff variable - page number
.\"
.\"
.
.\"			** ENVIRONMENT **
.\"			*****************
.\"
.\" Set up the environment be setting the traps and initializing some
.\" variables.  The .br causes the header macro .hd to be trapped
.\" to if .en is done before any text processing.
.
.de en
.wh 0 hd
.wh -\\n(tfu fo
.\" change the footer trap so the end of footnote trap can be put under it.
.ch fo 32000
.wh -\\n(tfu fx
.ch fo -\\n(tfu
.nr x 0 1
.nr y 0-\\n(tfu
.\" set ZZ to be the macro called at the end of input text.
.em ZZ
.nr w 1
.nr he 0
.nr fa 0
..
.\"
.\"			** PARAGRAPH **
.\"			***************
.de pp
.br
.ne \\n(sgv+\\n(shv
.sp \\n(shv
.ti +\\n(sfu
..
.\"
.\"			** CHAPTER **
.\"			*************
.de sc
.br
.\" do .rs and .SC in order to flush out all figures at the end of the
.\"  last chapter.
.rs
.SC
.sv \\n(slu
.\" There can be 0 or 2 arguments to .sc.  The 1st is either a +, a -, or
.\"  a chapter number.
.ie \\n(.$ \{\
.    if !\\n(.$-1 .AB"1 and not 2 parameters were sent to .sc"
.\" A first parameter of + will center one line (the second parameter) and
.\" put it in the table of contents.  This is useful for Bibliography, etc.
.ie "\\$1"+" \{ .if \\n(ft .T1 "\\$2" "\\n%" "0" "1"
.    ce 1
\\$2\}
.el \{\
.\" A first parameter of - will put the second parameter
.\" in the Table of Contents but will not put out a title.
.    ie "\\$1"-" \{.if \\n(ft .T1 "\\$2" "\\n%" "0" "1"\}
.    el \{\
.\" If the first parameter is neither a + nor a -, it is assumed to be a
.\" chapter number and a two line title is output; also an entry in the
.\" Table of Contents.
.        if \\n(ft .T1 "\\$1 \\$2" "\\n%" "0" "1"
.        ce 2
\\*(CH\\$1
\\$2\}\}\}
.el .ce 2
..
.\"
.\"			** APPENDIX **
.\"			**************
.\"
.\" List .sc except the title and Table of Contents Entries are for
.\" Appendix instead of Chapter.
.de sa
.br
.rs
.SC
.sv \\n(slu
.ie \\n(.$ \{\
.    if !\\n(.$-1 .AB"1 and not 2 parameters were sent to .sa"
.    if \\n(ft \{.T1 "\\*(AP\\$1 \\$2" "\\n%" "0" "1"\}
.    ce 2
\\*(AX\\$1
\\$2\}
.el .ce 2
..
.\"
.\"			** SUBTITLE **
.\"			**************
.\"
.de su
.\" If there is any parameters, the first is the subsection title and the
.\" optional second is the indentation number for the Table of Contents.
.br
.ne \\n(smu+\\n(sau
.sp \\n(sau
.ie \\n(.$ \{\
.    ie \\n(.$-1 \{\
.        if \\n(ft .T1 "\\$1" "\\n%" "\\$2" "0"  \}
.    el .if \\n(ft .T1 "\\$1" "\\n%" "0" "0"
.    ft \\n(fv
\\$1\fP\}
.el .ul 1
..
.\"
.\"			** EXAMPLES **
.\"			**************
.\"
.de es
.\" Start an example but do not allow nesting.
.ie \\n(ex .AB"Nesting examples is a no no"
.el .nr ex 1
.sp \\n(siv
.nr FI \\n(.u
.nf
.in +\\n(sju
..
.de ee
.\" Example end.  Ensure that an example was started.
.ie !\\n(ex .AB"Ending an example (.ee) without starting an example"
.el .nr ex 0
.sp \\n(skv
.in -\\n(sju
.if \\n(FI .fi
..
.\"
.\"			** LISTS **
.\"			***********
.\"
.\" Start of list
.de l1
.if \\n+(l0=4 .AB"Nesting of lists too deep"
.ta \\n(sou +5m +5m
.in +\\n(snu
.af l1 1
.nr l3 \\n(l2
.nr l2 \\n(l1
.nr l9 \\n(l8
.nr l8 \\n(l7
.rn l5 l6
.rn l4 l5
.nr l1 0 1
.nr l7 0
.ds l4 "1
.if \\n(.$ \{\
.    ds l4 "\\$1
.    nr l7 1\}
.af l1 \\*(l4
..
.\" End of list
.de e1
.br
.in -\\n(snu
.if \\n-(l0<0 .AB"Ending more lists than started"
.ie \\n(l0=0 \{\
.\" Reset tabs to default.
.    RT\}
.el \{\
.    nr l1 \\n(l2 1
.    nr l2 \\n(l3 1
.    nr l7 \\n(l8
.    nr l8 \\n(l9
.    rn l5 l4
.    rn l6 l5
.    af l1 \\*(l4\}
..
.\" Use this before each member of a list
.de le
.sp \\n(spu
.ti -\\n(sou
.if \\n(l7=1 \{\
\\n+(l1.\t\c\}
..
.\"
.\"
.\"			** FOOTNOTES **
.\"			***************
.\"
.\" Use this before each footnote.
.\"
.de fn
.\" Exercise a need so a certain amount of the footnote will be on the
.\" same page as the reference to it.
.\" The need for footnote after the first is less because there is no need
.\" for the seperator.
.ie \\nx .ne \\n(rcv-2v
.el .ne \\n(rcv
.\" Do not allow nesting of footnotes.
.if \\n(fn .AB"Nesting of footnotes is a no no"
.nr fn 1
.da FN
.ev 1
.\" increment footnote counter
.ie \\n+x=1\{\
.\" for first footnote of page, put in seperator.
\l'1i'
.    br \}
.el .sp \\n(srv
.fi
.ad
..
.\"
.\" END FOOTNOTE - use at end of footnote text, i.e., sequence is
.\" .fn, footnote text, .ef
.\"
.de ef
.\" Ensure that a footnote was started.
.if !\\n(fn .AB"Ending a footnote (.ef) when one was not started"
.nr fn 0
.br
.nr z \\n(.v
.ev
.di
.\" set net footer position
.nr y -\\n(dn
.\" check if there is room for the whole footnote and if not, set trap to
.\" next line.
.\" y is the position where the footnote is to begin printing, which is
.\" one "v" after the trap.
.if (\\n(nlu+1v)>(\\n(.pu+\\nyu-1v) \{\
.    nr y (2v+\\n(nlu-\\n(.pu)u\}
.ch fo (\\nyu-1v)
.\" increment page-by-page footnote counter
.nr w +1
..
.\"
.\"
.\"			** FIGURES **
.\"			*************
.\" Figure start
.de fs
.\" do not allow nexting of figures.
.if \\n(fs .AB"Nesting of figures is a no no"
.\" If there are already full page diverted figures after 2 pages
.\" of partial page figures, quit.
.if \\n(f7>0 .AB"Too many figures diverted near page \\n%"
.\" Increment diverted figure counter and set figure start flag
.nr f0 +1
.nr fs 1
.da F2
.ev 2
.\" set trap in the diversion to be a full page.  Trap goes to .Fc.
.\" "tt" is used to indicate if the figure trapped to .Fc.
.dt \\n(.pu-\\n(thu-\\n(tfu Fc
.nr tt 0
..
.\"
.\" Figure end
.de fe
.\" Ensure that  figure was started.  If not, abort.
.ie !\\n(fs .AB"Ending a figure (.fe) which was not started"
.el .nr fs 0
.\" Make sure that the complete figure is diverted.  May cause trap.
.br
.\" Test to see if the figure caused a trap to .Fc (ie. "tt" >0).
.ie \\n(tt \{\
.    ev
.    di
.\" If a figure trapped but did so at the last line (ie dn =0) then there
.\" is no problem.  Otherwise the figure was truncated and the user is told so.
.    if \\n(dn>0 \{\
.\" If the figure truncated was being put on a page after another figure,
.\" this is a figure overflow and processing stops
.        ie \\n(f0=3 .AB"Too many figures diverted near page \\n%"
.        el \{\
.         tm NROFF/TROFF Warning: A figure after page \\n% was truncated.\}\}\}
.el \{\
.\" The figure did not cause a trap.
.    ev
.    da
.    nr f2 +\\n(dnu\}
.\" if .fs called with a parameter, call .Fb, else call .Fd or .Fe
.ie \\n(.$ .Fb "\\$1"
.el \{\
.\" Test if this is the first figure diverted.  If so call Fe, else Fd.
.\" The second parameter indicates whether a title was given for the
.\" List of Figures.
.    ie \\n(f0+\\n(f5>1 .Fd " " "0"
.    el .Fe " " "0" \}
..
.\"
.\"  Full page figure.  This will leave a skip a page at the appropriate
.\"  time (no printing of the blank page will be done but the page number
.\"  will be incremented. The table of contents will be updated if requested
.\"  and if a parameter is given to the macro
.de fg
.nr t 2
.ie \\n(f7>0 .nr f7 +1 1
.el \{\
.    ie \\n(f0>1 .nr f7 1 1
.    el \{\
.       nr t 1
.       ie \\n(f6>0 .nr f6 +1 1
.       el \{\
.          ie \\n(f0=1 .nr f6 1 1
.          el \{\
.             nr f5 +1 1
.             nr t 0\}\}\}\}
.nr t +\\n%+\\n(f5+\\n(f6+\\n(f7
.if \\n(ff+\\n(.$-1 .F0 "\\$1" "\\nt"
..
.\"
.\"			** REFERENCE **
.\"			***************
.\"
.\" Reference Macros .rb .rf .re
.\" .rb - begin references
.\"	- set tabs, set spacing to value in "sq", and indent;
.\" .rf - do before each reference; temp indent, space and need.
.\" .re - refernce end.
.\"	- reset tabs; restore indent; restore line spacing.
.\"
.de rb
.ta \\n(sem +5m +5m
.nr t1 \\n(.i
.ls \\n(sq
.in \\n(sem
..
.de rf
.br
.ne \\n(scu+\\n(sdu
.sp \\n(sdu
.ti -\\n(sem
..
.de re
.br
.ls
.in \\n(t1u
.\" Reset tabs to default
.RT
..
.\"
.\"			** SET LINE LENGTH **
.\"			*********************
.\"
.\" Macro to set the line and title length in all environments
.de LL
.if !\\n(.$ .AB"Called LL with no parameter"
.ev2   \"should be ev3
.lt \\$1
.ll \\$1
.ev
.ev2
.lt \\$1
.ll \\$1
.ev
.ev1
.lt \\$1
.ll \\$1
.ev
.ev0
.lt \\$1
.ll \\$1
.ev
..
.\"
.\"
.\"			**  MUTIPLE COLUMN MACRO  **
.\"			****************************
.\"
.\" This macro expects 4 parameters
.\"   - number of columns
.\"   - width of each column (units must be given).
.\"   - width of space between columns (units must be given).
.\"   - page offset of the whole thing (units must  be given).
.\"
.de MC
.if \\n(.$<4 .AB"Called MC with less than 4 parameters"
.nr cn \\$1
.nr cw \\$2
.nr cs \\$3
.nr co \\$4
.po \\n(cou
.LL \\$2
.nr c 1 1
.mk mx
..
.\"
.\"			** POINT SIZE CHANGE **
.\"			***********************
.\"
.\" macro to change the point size in all environments
.de PS
.ev2  \" should be ev 3
.ps\\$1
.ev
.ev2
.ps\\$1
.ev
.ev1
.ps\\$1
.ev
.ev0
.ps\\$1
.ev
..
.\"
.\"			** LINE WIDTH (ie. VS) SIZE CHANGE **
.\"			***********************
.\"
.\" macro to change the line width (ie. vs) size in all environments
.de VS
.ev2  \" should be ev 3
.vs\\$1
.ev
.ev2
.vs\\$1
.ev
.ev1
.vs\\$1
.ev
.ev0
.vs\\$1
.ev
..
.\"
.\"			** RESET TABS **
.\"			****************
.de RT
.ie n .ta .8i +.8i +.8i +.8i +.8i +.8i +.8i +.8i +.8i +.8i
.el .ta .5i +.5i +.5i +.5i +.5i +.5i +.5i +.5i +.5i +.5i
..
.\"
.\"
.\"			** DISPLAYED EQUATIONS **
.\"			*************************
.\"  These equation macros were originally copied from BELL, but have been
.\"  modified by Mart Molle to be compatible with tmac.X.
.\"  The definition of symbols and the delim statement are found later
.\"  so that dollar signs will not be misinterpreted if this file is
.\"  run through neqn.
.\"
.\"  ".EQ ... .EN" will place the equation on a separate line with blank space
.\"  above and below (1 line in nroff, 0.5 lines in troff). Each equations is
.\"  a single very-long character string, which can cause a "word overflow"
.\"  in fill mode. Hence ".EQ" saves state of fill/nofill mode and switches
.\"  into nofill mode, ".EN" restores fill mode if it was previously set.
.\"
.\"  "EQ" accepts two parameters:  the first should be C (the DEFAULT:
.\"  center the equation on the output line), L (left justify the
.\"  equation) or I (indent the equation a distance of 10 n's ).
.\"  The second will be used as an "equation number" at the right margin
.\"  IMPORTANT NOTE: "mark ... lineup" in equations is done with respect
.\"  to the START OF THE EQUATION, hence it will not work with the
.\"  DEFAULT of CENTERING equations.  Use only left justification or
.\"  indenting (same one for ALL equations meant to lineup).
.\"
.de EQ
.br
.ne 2.5
.if t .sp .5
.if n .sp 1P
.nr FI \\n(.u
.nf
.ce
.ds EL \\$1  \" save first parameter, if any
.if \\n(.$-1 .ds EL \\$2    \" save second parmeter
.nr LE \\n(.$
.ie "\\$1"L" \{\
.   ce0
.   nr LE -1\}
.el \{\
.   ie "\\$1"C" .nr LE -1
.   el \{\
.      if "\\$1"I" \{\
.        ce0
.        ti +10n
.        nr LE -1 \}\}\}
.ls 0
..
.\"
.\"	Equation end: restore fill mode (if it had been set), space down,
.\"	add eqn number and center, left adjust or indent equation as needed
.\"
.de EN
.ce 0
.lt \\n(.lu
.if \\n(LE \{\
.   ie n \{\
.      tl '''\\*(EL'\}
.   el \{\
.      sp -1-\\n(.au
.      tl '''\\*(EL'  
.      sp
.\" the last two lines were in reverse order ??? 
.      sp -1+\\n(.au\}\}
.if \\n(FI .fi
.if n .sp 1P
.if t .sp 0.5
.ls
.sp
..
.\"
.de HS
..
.de HE
..
.de FS
..
.de FE
..
.\"
.\"
.\"				HEADER MACRO
.\" invoked at the top of each page, after .en done
.\" also invoked by .fo directly in case of multi-columns
.de hd
.HS	\" call dummy macro which user can define
.\" set "he" to tell footer (if you enter it) that you are in header
.nr he +1
.\" Test if multi-column or not.  If so, then test if new column or
.\" or new page.  A new column only needs to set po and do
.\" some minus spacing to
.\" the last marked line.  A new page needs the header printed and to
.\" reset the column counter "c" and to mark the line.
.ie \\n(cn>1 \{\
.    ie \\nc>\\n(cn \{\
.\" need new page
'        sp \\n(bhu-1v
.        lt (\\n(cn*\\n(cwu)u+(\\n(csu*(\\n(cn-1))u
.\" print the even or odd titles, depending on the page.
.        ie e .tl \\*(Te
.        el .tl \\*(To
'        sp |\\n(thu
.        nr c 1 1
.        mk mx \}
.    el \{\
.\" just need a new column
.        po +\\n(cwu+\\n(csu
'        sp |\\n(mxu\}\}
.el \{\
.\" no multiple column stuff so print title
.\" Before printing title, which may have page number, increment the page
.\" number according to how many full page figures are stored (via "f5").
.    if \\n(f5 \{\
.	nr % +\\n(f5
.       nr f5 0 1\}
'    sp \\n(bhu-1v
.\" print the even or odd titles, depending on the page.
.    ie e .tl \\*(Te
.    el .tl \\*(To
'    sp |\\n(thu\}
.ch fx -\\n(tfu
.ch fo -\\n(tfu
.\" save leftover footnotes if there are any (from xf).
.if \\n(z1 .fz
.\" reset footer so if there are figures, footnote leftovers won't be in way.
.ch fo -\\n(tfu
.\" test if there are figures to print.  If not, go into no-space mode.
.if !\\n(f0 .ns
.if \\n(f0 \{\
.\" Print the first figure diverted.
.\" Shift the counters of full page figures.
.    nr f5 \\n(f6
.    nr f6 \\n(f7
.    nr f7 0
.    Ff
.\" if figure printed caused a footer trap, "fa" will be set.  Reset "fa"
.\" and set h2 which will cause .fo to be called again to do rest
.\" of the footer stuff.
.    ie \\n(fa \{\
.        nr fa 0
.        nr h2 1\}
.    el \{\
.\" figure printed did not cause footer trap. Decide whether to go to new
.\" page or continue processing here.
.\" Note that this may cause h1 to be set, which will cause
.\" a 'bp to be done, trapping to the footer.
.        ie \\nx \{ .if (\\n(nlu+5v)>(\\n(.p+\\nyu) .nr h1 1 \}
.        el \{ .if \\n(.tu<6v  .nr h1 1 \} \}\}
.\" Test "h1" and "h2" outside the nested if-elses.
.if \\n(h1=1 \{\
.    nr h1 0
'    bp\}
.if \\n(h2=1 \{\
.    nr h2 0
.    fo\}
.\" After any and all figures done, reset footer trap to catch any
.\" leftover footnotes.
.\" footer trap is to be one line before the start of footnotes.
.ch fo (\\nyu-1v)
.nr he -1
.HE	\" call dummy macro which user can define
..
.\"
.\"
.\" FOOTER macro - invoked at bottom of each page if there are no footnotes,
.\" or at footnote text margin if there are footnotes.
.\" Also called on occassion from .hd and from .Fe.
.de fo
.FS	\" call dummy macro which user can define
.\" if the trap is done inside the header (figure printing), don't do
.\" footnotes.  Otherwise do them.  Print footer and start a new page
.\" unless you trapped inside a figure (ie. "fg" is set).  Set
.\" "fa" to tell the caller that you went thru the footer and diverted
.\" the rest of the figure to GA (garbage).
.if !\\n(he  \{.if \\nx .xf\}
.nr fa 0
.ie \\n(fg \{\
.    nr fa 1
.    di GA\}
.el \{\
.\" not inside a figure.  Call .mf if doing multi-columning.
.    ie \\n(cn<2 \{\
.\" doing single column
.\" remove the footer traps so that spacing to the footer position does
.\" not cause them to be exercised.
'	 ch fo 32000
'	 ch fx 32000
'	 sp (\\n(.pu-\\n(nlu-\\n(tfu+\\n(bfu-1v)u
.\" print even or odd footer, depending on whether the page is even or odd.
.        ie e .tl \\*(Be
.        el .tl \\*(Bo
'        bp\}
.    el .mf\}
.FE	\" call dummy macro which user can define
..
.\"
.\"  Multiple Column footer,  called from .fo
.de mf
.\" if not last column, just go to header.
.\" if last column, reset page offset, do footer and 'bp to header.
.ie \\n+c<=\\n(cn .hd
.el \{\
.    po \\n(cou
.    lt (\\n(cn*\\n(cwu)u+(\\n(csu*(\\n(cn-1))u
'    ch fo 32000
'    ch fx 32000
'    sp (\\n(.pu-\\n(nlu-\\n(tfu+\\n(bfu-1v)u
.\" print even or odd footer, depending on whether the page is even or odd.
.    ie e .tl \\*(Be
.    el .tl \\*(Bo
'    bp\}
..
.\"
.\" FOOTNOTE REMAINDER - invoked when generating footnotes, but bottom
.\" of page is run into. It saves remaining text in the "fy" string.
.\"
.de fx
.if \\nx .di fy
..
.\"
.\"
.\" FOOTNOTE REMAINDER- saves footnote text that didn't fit on previous page
.\" so that it will be generated on subsequent page
.\" Called by header macro hd.
.de fz
.nr x 0 1
.nr y 0-\\n(tfu
.nr z1 0
.fn
.nf
.fy
.fi
.ef
.nr w 1 1
..
.\"
.\"
.\" FOOTNOTE GENERATOR - invoked if there are footnotes to generate on this page.
.\" called by fo macro
.\"
.de xf
.ev 1
.nf
.\" Print footnotes stored in FN.  This may cause a trap to fx which will
.\" divert the rest of the footnote into "fy".
.\" first space to footnote top
.sp(\\n(.pu+\\nyu-\\n(nlu)u
.FN
.rm FN
.\" Test if any text was diverted into "fy"
.ie "\\n(.z"fy" \{\
.    di
.\" set z1 to the length of the text diverted into "fy".
.    nr z1 \\n(dn \}
.el .nr z1 0
.nr x 0 1
.nr y 0-\\n(tfu
.ev
.nr w 1 1
..
.\"
.\" FIGURE PRINT MACRO
.\" called from hd if there are figures to print at the top of the page.
.\" called from .fe thru .Fe if ther currently input figure should be
.\" printed right abway.
.de Ff
.nr f1 0
.ev 2
.nf
.\" Print the figure(s) diverted into F1.  This may cause a footer trap
.nr fg 1
.F1
.nr fg 0
.\" End diversion that may have been started if a footer trap was caused.
.\" If "fa" is set, then the footer was trapped into.
.if \\n(fa .di
.br
.ev
.rm F1
.\" if there are more figures (in F2), move them to F1.
.if \\n(f0-1 .Fa
.\" decrement count of number of diverted figures.
.nr f0 -1
..
.\"
.\" FIGURE DIVERSION MOVE
.de Fa
.\" Add the diversion stored in F2 to the one in F1.
.\" This is used to copy F2 to F1 (in .Ff and .Fe) and to append the
.\" figure in F2 to the one in F1 (in .Fd).
.da F1
.ev 2
.nf
.F2
.br
.ev
.da
.nr f1 +\\n(f2
.nr f2 0
.rm F2
..
.de Fb
.\" Called from .fe if .fe was called with a parameter.
.\" This parameter is the title of the figure, to be put in the List of Figs.
.ie \\n(f0+\\n(f5>1 \{\
.    Fd "\\$1" "1"\}
.el \{\
.    Fe "\\$1" "1"\}
..
.de Fc
.\" trapped to from inside the figure diversion being done in .fs
.\" if the figure goes to the
.\" bottom of the page.  Some of the figure may or may not get lost.
.ev
.di
.nr f2 \\n(dnu
.nr tt 1
.di GA
.ev 2
..
.de Fd
.\" called from .fe or .Fb if there are figures already diverted.
.\" $1 is the title of the figure.  $2=1 indicates the is a title in $1.
.\" First check if figure was appended onto the second page of diverted figs.
.ie \\n(f0=3 \{\
.    nr f0 2
.    nr t \\n%+\\n(f5+\\n(f6+2\}
.\" if put on second page, see if it can be put on first page
.el \{\
.    nr f0 2
.    nr t \\n%+\\n(f5+\\n(f6+2
.    if \\n(f6=0 \{\
.        if (\\n(.pu-\\n(f1u-\\n(f2u-\\n(thu-\\n(tfu)>=0 \{\
.\"  ready to put both figures on same page
.            nr f0 -1
.            nr t \\n%+\\n(f5+1
.            Fa\}\}\}
.if '\\n(ff\\$2'11' .F0 "\\$1" "\\nt"
..
.de Fe
.\" called from .fe or .Fb if there are no figures already diverted.
.\" $1 is the title of the figure.  $2=1 indicates there is a title in $1.
.rm F1
.nr f1 0
.Fa
.nr t \\n%+\\n(f5+1 1
.ie \\n(f1+2v>\\n(.tu \{.if '\\$2\\n(ff'11' .F0 "\\$1" "\\nt"\}
.el \{\
.    if '\\$2\\n(ff'11' \{\
.         F0 "\\$1" "\\n-t"\}
.    br
.    Ff
.    if \\n(fa \{\
.        nr fa 0
.        fo\}\}
..
.de F0
.\" Add an entry to the List of Figures.  Called from .Fd and .Fe.
.\" $1 is the title, $2 is the page #.
.da FP
.ev 2
.br
\t
.T2 "\\$1" "\\$2" "0"
.br
.\" reset tabs
.RT
.ev
.da
..
.\" Add an entry to the Table of Contents.
.\" Called from .sc .sa and .su.
.de T1
.da TP
.ev 2
.br
.if '\\$4'1' .sp 1v
.T2 "\\$1" "\\$2" "\\$3"
.br
.\" reset tabs
.RT
.ev
.da
..
.\" PADDER and GENERATOR of TABLE of CONTENTS and LIST of FIGURES
.\" Called from .F0 and .T1.
.\" If the length of the title (parameter 1) exceeds the available line length,
.\" adjust margins and print title in fill mode on as many lines as necessary.
.\" There is no way to find current position on output line, so the last
.\" (partially filled) line of the title is forced out (.nf causes a break),
.\" so that its length can be determined from the .n register, taking care
.\" to do a reverse line feed to return to the last line.  The string PD
.\" is then used to print the leader (dots) and page number (parameter 2).
.\" If the whole title fits on one line, PD also contains the title,
.\" otherwise enough blank space is included to skip the (previously printed)
.\" last line of the title.
.de T2
.nr FI \\n(.u
.nr AD \\n(.j
.if \\$3>0 .in +(4n*\\$3u)u
.ie \\w'\\$1'u-\\n(.lu+\\n(.iu+3m \{\
.	if \\$3>0 .in +4n
.	ta (\\n(.lu-\\n(.iu-3m)u +3mR
.	if \\$3>0 .ti -4n
.	fi
.	na
.	ll -3m
\\$1
.	nf
.	sp-1
.	ll +3m
.	ds PD \\h'\\n(.nu'\}
.el \{\
.	ta (\\n(.lu-\\n(.iu-3m)u +3mR
.	ds PD \\$1\}
.ie \\n(.lu-\\n(.iu-\\w'\\*(PD'u-3n-3m .as PD "  \a\t\\$2
.el .as PD \\h'\\n(.lu-\\n(.iu-\\w'\\*(PD'u-3m+1u'\t\\$2
\\*(PD
.if \\$3>0 .in -(4n*\\$3u)u
.if \\$3>0 .in -4n
.if \\n(FI .fi
.ad \\n(.j
..
.\" Recursive macro to begin pages until there are no figures left
.\" so a new chapter can be started.  Called from .sc.
.de SC
.if \\n(nlu>\\n(thu \{ .bp
.    SC\}
..
.\"
.\"
.\" End processing cleanup routine.  This is automatically (.en does an .em)
.\" called at the end of text processing.  It cleans out left over figures
.\" optionally prints the Table of Contents and List of Figures.
.de ZZ
.br
\\c
.if \\n(f0 'bp
.rs
.ie !"\\*(TL\\*(Te"''''''''" \{\
.    ds Te ''''
.    ds To ''''
.    if \\n(ff+\\n(ft>0 'bp\}
.el \{\
.    if \\n(nlu>\\n(thu \{\
.        if \\n(ff+\\n(ft>0 'bp\}\}
.\" if requested, print out Table of Contents.
.if \\n(ft \{ .af % i
.    nr % \\n(su
.    ev 2
.    ce
\\*(TC
.    sp \\n(rau
.    ta \\n(.luR
\t\\*(PG
.    sp \\n(rbu
.    TP
.    ev\}
.\" if requested, print out List of Figures
.if \\n(ff \{ .af % i
'    bp +1
.    ev 2
.    ce
\\*(LF
.    sp \\n(rau
.    ta \\n(.luR
\t\\*(PG
.    sp \\n(rbu
.    FP\}
.if \\n(cn>1 \{\
.nr c \\n(cn
.bp\}
..
.\"
.de AB	\" ABort and exit
.ab NROFF/TROFF aborted near input line \\n(.c: \\$1.
..
.\"			**  EQUATION DELIMITER STUFF **
.\"			*******************************
.\"
.\"
.\"  First define "$" as the default delimiter for imbedded equations
.\"  and some specially constructed symbols (see manual "New Graphic
.\"  Symbols for EQN and NEQN", Carmela Scrocca, Bell Labs, for more).
.\"  The statements below between (and including) the .ig and .. must be
.\"  seen by neqn or eqn.  This can be done in one of two ways:
.\"  Either the file /usr/lib/tmac/tmac.X is included explicitly as an input
.\"  file (explicitly means without the use of the .so command), or these
.\"  statements are copied into a file that is included explicitly.
.\"
.ig   \" This stuff is meant for eqn/neqn's eyes only! This will discard
.     \" it from the input stream even if eqn/neqn preprocessor not used
.EQ
delim $$
ndefine =del ` "\z=\z^\v'-.25v'-\v'.25v'" `
tdefine =del ` "\v'.3m'\z=\v'-.6m'\h'.3m'\s-1\(*D\s+1\v'.3m'" `
ndefine cdot ` "\u.\d" `
ndefine oppA ` "V-" `
.EN
..    \" End of discarded input meant for eqn/neqn preprocessor
.\"
.\"
.\"			**  SET UP ENVIRONMENTS  **
.\"			***************************
.\"              
.\" ENVIRONMENT 2  -Used for figures, table of contents and list of figures
.\"
.ev 2
'nf
.lt 6.5i
.ll 6.5i
.ls 1
.ps 10
.nh
.ev
.
.\" ENVIRONMENT 1
.ev 1
.lt 6.5i
.ll 6.5i
.ls 1
.ps 10
'nh
.ev
.
.\" ENVIRONMENT 0
.ev 0
.ls 2
.ll 6.5i
.lt 6.5i
.ps 10
.ev
.\"
.\"
.\"			**  VARIABLE INITIALIZATION  **
.\"			*******************************
.\"
.\"
.
.pl 11i
.\"WARNING: The .tr command below is incompatible with NEQN/EQN definitions
.      \"         of approx & approx =. It is safer to override
.      \"         this translation and use unpaddable space ("\ ").
.tr ~
.nr th 1i  \" # of inches in top of page (used as u).
.nr bh 7i/12u \" # of inches down to and including header line (used as u).
.nr tf 19i/16u   \" total # of inches at bottom of page (used as u).
.nr bf 10i/16u  \" # of inches after text but including footer  (used as u).
.nr ra 0v  \" # of spaces between tbl of content and page line (used a u).
.nr rb 2v  \" # of spaces between page line and start of entries (used as u).
.nr rc 5u  \" # of sp. needed for a footnote to go on current page (used as v).
.nr sq 1u  \" line spacing for references.
.nr sc 0.5i  \" # of inches of need for references (used as u).
.nr sd 1i/6u  \" # of inches to space between references (used as u).
.nr sf 0.5i  \" space to indent for paragraph (used as u).
.nr sg 3u  \" # of spaces needed before trap to start paragraph(used as v).
.nr sh 1u  \" # of spaces done to start a new paragraph (used as v).
.nr si 1u \" # of spaces used to start an example (used as v).
.nr sj 0.5i  \" space to indent for an example (used as u);
.nr sk 2u  \" # of spaces left at the end of an example (used as v);
.nr sl 1.0i  \" # of inches at top of page, before chaper (used as u).
.nr sn 0.5i  \" amount of space to indent for list (used as u).
.nr so 0.5i  \" amount of space to temp indent for each list entry (used as u).
.nr sp 1i/6u  \" spacing for each element of list (or sublist) (used as u).
.nr sr 1u \" space between footnotes. (used as v).
.nr su 3 \" page number of table of contents.
.ds Te \\*(TL\" even paged header title
.ds To \\*(TL\" odd paged header title
.ds Be \\*(BL\" even paged footer title
.ds Bo \\*(BL\" odd paged footer title
.ds BL ''''\" Footer title (used in default of Be and Bo
.ds TL ''''\"Header title (used in default of Te and To
.ds CH "CHAPTER 
.ds AP "Appendix 
.ds AX "APPENDIX 
.ds TC "Table of Contents
.ds LF "List of Figures
.ds PG "page
.ds N \\s-3\\u\\nw\\d\\x'-0.5m'\\s+3
.ds O [\\nw]
.nr x 0 1  \" footnote counter
.nr ff 0  \" flag indicating if List of Figures is Desired
.nr ft 0  \" flag indicating if Table of Contents is Desired
.nr cn 1  \" number of columns of output. Is reset by a call to MC.
.nr f0 0  \" count of the number of diverted figures
.nr f1 0  \" size of F1 figure diversion
.nr f2 0   \" size of F2 figure diversion
.nr f5 0 \" number of full page figures before a page of diverted figs.
.nr f6 0 \" number of full page figures between pages of diverted figures
.nr f7 0 \" number of full page figures after two pages of diverted figs.
.nr l0 0 1 \" list level number (uses auto incre/dcrement)
.ie n \{\
.\" initialization for nroff
.nr sa 2v  \" # of spaces to do before subtitles (used as u).
.nr se 10  \" # of spaces to indent for reference (used as m).
.nr sm 3i/2u  \" # of spaces of need to do a subtitle (used as u).
.nr fv 2\}   \"    set font variable for subtitles to Italics (underlining).
.el  \{\
.\" set variables for troff
.nr sa 1v  \" # of spaces to do before subtitles (used as u).
.nr se 8u  \" # of spaces to indent for a reference (used as ems).
.nr sm 1i  \" # of spaces of need to do a subtitle (used as u).
.nr fv 3\}      \"    set font variable for subtitles to Bold
