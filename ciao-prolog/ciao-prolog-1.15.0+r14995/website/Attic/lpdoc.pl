:- use_package(assertions).

:- comment(title,"The lpdoc Documentation Generator").

:- comment(author,"The CLIP Group").

:- comment(module,"
[Full documentation in the
 @uref{on-line HTML reference manual}{lpdoc-2.0.38.html/lpdoc.html}]

@tt{lpdoc} is an @em{automatic program documentation
generator} for (C)LP systems.

@tt{lpdoc} generates a reference manual automatically from one or more
source files for a logic program (including ISO-Prolog, Ciao, many CLP
systems, ...). It is particularly useful for documenting library
modules, for which it automatically generates a description of the
module interface. However, @tt{lpdoc} can also be used quite
successfully to document full applications and to generate nicely
formatted plain ascii ``readme'' files. A fundamental advantage of
using @tt{lpdoc} to document programs is that it is much easier to
maintain a true correspondence between the program and its
documentation, and to identify precisely to what version of the
program a given printed manual corresponds.

The quality of the documentation generated can be greatly enhanced by
including within the program text:

@begin{itemize}
@item @em{assertions} (types, modes, etc. ...) for the predicates in the program, and 

@item @em{machine-readable comments} (in the ``literate programming'' style).
@end{itemize}

The assertions and comments included in the source file need to be
written using the Ciao system @em{assertion language}. A simple
compatibility library is available to make traditional (constraint)
logic programming systems ignore these assertions and comments
allowing normal treatment of programs documented in this way.

The documentation is currently generated first in <tt>texinfo</tt>
format. From the @tt{texinfo} output, printed and on-line manuals in
several formats (dvi, ps, info, html, etc.) can be easily generated
automatically, using publicly available tools. @tt{lpdoc} can also
generate 'man' pages (Unix man page format) as well as brief
descriptions in html or emacs info formats suitable for inclusion in
an on-line index of applications. In particular, @tt{lpdoc} can create
and maintain fully automatically WWW and info sites containing on-line
versions of the documents it produces.

The @tt{lpdoc} manual (and the Ciao system manuals) are generated by
@tt{lpdoc}.

@tt{lpdoc} is distributed under the GNU general public license.

@bf{Note:} @tt{lpdoc} is currently fully supported only on Linux and
other Un*x-like systems, due to the use of @tt{Makefile}s and other
Un*x-related utilities. It is possible to run @tt{lpdoc} under Win32
using @tt{Cygwin}. A version which is written entirely in Prolog and
will thus run standalone also on Win32 is currently under beta
testing.").

% <UL><LI><A HREF=lpdoc_html/lpdoc.html><B>lpdoc</B> Reference Manual - in html hypertext format </A></UL><UL><LI><A HREF=lpdoc.pdf.gz><B>lpdoc</B> Reference Manual - in Adobe pdf (acrobat) format (compressed)</A></UL><UL><LI><A HREF=lpdoc.ps.gz><B>lpdoc</B> Reference Manual - in postscript format (compressed)</A></UL>

main.