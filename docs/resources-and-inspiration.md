
# Resources and Inspiration

### [Project Oberon](http://www.projectoberon.com/)

Project Oberon is a very impressive 12kloc single threaded, single user operating
system, filesystem, GUI, compiler (3kloc), drawing tool, and various other little
tools, all running on each an [emulator](https://github.com/pdewacht/oberon-risc-emu)
for or an FPGA implementation (1kloc of verilog) of a 32 bit RISC processor.

The Project Oberon book (PDFs available from the site above) is a wonderful overview
of all the parts of the system, including all kinds of clever little design tidbits
like:

> Sector pointers are represented by sector numbers of type INTEGER. Actually,
> we use the numbers multiplied by 29. This implies that any single-bit error
> leads to a number which is not a multiple of 29, and hence can easily be
> detected. Thereby the crucial sector addresses are software parity checked
> and are safe (against single-bit errors) even on computers without hardware
> parity check. The check is performed by procedures Kernel.GetSector and
> Kernel.PutSector.

I had been slowly getting my compiler project underway when I stumbled over
Project Oberon again and it reminded me that you really don't need *all that much*
code to get some impressively useful results.

The Oberon language itself is quite nice, though it does feel a bit antiquated
and shouty, probably largely due to the all-caps BASIC/PASCAL style syntax
compared to more popular modern braces-centric languages. You can see some
significant influences to Go though without looking all that hard.

> "Oberon is an understudied gem. It had a huge influence on Go via Robert 
> Griesemer. Some notes from one of his talks: [link to slide 14](https://talks.golang.org/2015/gophercon-goevolution.slide#14")
> - David Crawshaw


### Some Oberon & Project Oberon Resources

The Project Oberon Book (PDF):\
[Part One](http://www.inf.ethz.ch/personal/wirth/ProjectOberon/PO.System.pdf),
[Part Two](http://www.inf.ethz.ch/personal/wirth/ProjectOberon/PO.Applications.pdf),
and [Part Three](http://www.inf.ethz.ch/personal/wirth/ProjectOberon/PO.Computer.pdf)

[Project Oberon RISC5 Reference](project-oberon-risc5-architecture.txt)\
Project Oberon's various descriptions of its RISC5 Architecture are not entirely
in agreement (likely due to evolution over time, the odd typo, source/docs out
of sync). I threw together this reference doc, which I believe is correct vs the
latest version.

[The Programming Language Oberon](https://inf.ethz.ch/personal/wirth/Oberon/Oberon07.Report.pdf)\
(The Oberon-07 Report, 2016, PDF)\
A 17 page description of the language.

[Programming in Oberon](https://inf.ethz.ch/personal/wirth/Oberon/PIO.pdf)\
(A Tutorial, 2016, PDF)


### Prof. Wirth's [Compiler Construction](https://inf.ethz.ch/personal/wirth/CompilerConstruction/) Book

Prof. Wirth also has a wonderful and concise compiler book, Compiler Construction
(PDFs + Source Code), which walks one through the creation of a subset-of-Oberon
compiler.

I've taken the liberty of combining the two PDFs into one and adding an functional
index/outline for easier navigation through this delightful read:
http://frotz.net/misc/Compiler.Construction.Wirth.2017.pdf

I had the lexer working and the parser already underway, but the approach to
straight line code generation I've started working with is very much influenced
by this book and the Project Oberon compiler.


### [The Selfie Project](http://selfie.cs.uni-salzburg.at)

A related project of interest is "selfie", a 12kloc 64bit RISCV (formerly MIPS)
subset emulator, hypervisor, C subset compiler, etc. They don't go up the stack
to a full OS+GUI but rather look down the stack, toward emulation, virtual
machines, etc. 

