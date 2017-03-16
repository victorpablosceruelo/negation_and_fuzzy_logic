:- use_package([assertions,pure]).
:- doc(nodoc,assertions).

:- doc(filetype, documentation).

:- doc(title,"Crossed-references of a program").
:- doc(subtitle_extra,"@bf{The CIAO System Documentation Series}").
:- doc(subtitle_extra,"@em{Draft printed on:} @today{}").
:- doc(author,"Francisco Bueno").

:- doc(module,"The @tt{xrefs} crossed-references Ciao library includes
	several modules which allow displaying crossed-references
	of the code in a program. Crossed-references identify modules which
	import code from other modules, or files (be them modules or not)
	which use code in other files.
        Crossed-references can be obtained as a term representing a graph,
	displayed graphically (using daVinci, a graph displayer developed by
	U. of Bremen, Germany), or printed as a list.

	The libraries involved are as follows:
        @begin{itemize}
        @item @lib{etc(xmrefs)} displays a graph of crossed-references between
              modules using daVinci,
        @item @lib{etc(xfrefs)} displays a graph of crossed-references between
              files using daVinci,
        @item @lib{library(xrefs)} obtains a graph of crossed-references
              between files,
        @item @lib{library(xrefs(mrefs))} obtains a graph of 
              crossed-references between modules,
        @item @lib{library(xrefs(pxrefs))} prints a list of 
              crossed-references between files.
        @end{itemize}
        The first two are intended to be used by loading in @apl{ciaosh}.
        The other three are intended to be used as modules within an
        application.

        The following is an example graph of the library modules involved
        in the crossed-references application. It has been obtained
        with:
        @begin{verbatim}
[ciao/etc]> ciaosh
Ciao-Prolog 1.5 #24: Tue Dec 28 14:12:11 CET 1999
?- use_module(xmrefs).

yes
?- set_flag(X).

X = 3 ? 

yes
?- set_files([xfrefs, xmrefs,
              library(xrefs),
              library(xrefs(mrefs)),
              library(xrefs(pxrefs)),
              library(xrefs(xrefs2graph)),
              library(xrefs(xrefsbuild)),
              library(xrefs(xrefsread))
	     ]).

yes
?- xmrefs.

        @end{verbatim}
        @noindent
        so that it is displayed by daVinci as:

        @image{xmgraph}

        The following is an example graph of the same module files, where
        crossed-references have been obtained with @tt{xfrefs:xfrefs(whodefs)}
        instead of @tt{xmrefs:xmrefs}:

        @image{xfgraph}

        For more information refer to the xrefs documentation 
        (@tt{xrefs_doc.dvi}) in the source library of the Ciao distribution.").


