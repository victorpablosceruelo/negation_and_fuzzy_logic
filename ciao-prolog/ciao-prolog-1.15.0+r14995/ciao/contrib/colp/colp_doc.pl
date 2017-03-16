:- use_package(assertions).
:- doc(nodoc,assertions). 

:- doc( title, "Co-logic Programming").
:- doc( author, "R@'{e}my Haemmerl@'{e}").
:- doc( version(0*1+0,2008/09/17), "first implementation").

:- doc( module, "This package provides two implementations of
Co-logic Programming. One developed a CLIP (referred as \"east
implementation\") and which is the portage of the one developed for
YAP by Ajay Bansal and Gopal Gupta (referred as \"west
implementation\").

The user can chose the implementation he want thanks to the directive
@pred{colp/1}. The default implementation is not specified.").

:- doc(bug, "The behaviour of two implementations do not seems correct").


:- true decl colp(Option) : atom # "Specifies a compilation option :
@begin{itemize}
@item Option @pred{impl(I)} specifies which implementation will be
 used. @pred{colp(east)} forces the use of the east implementation and
 @pred{colp(west)} forces the use of the west implementation.
@item Option @pred{trace} activates a very simple tracing (for debugging).
@end{itemize}
".

:- true decl inductive(Spec) : callable # " Specifies that the predicate should be interpreted inductively.".


:- true decl coinductive(Spec) : callable # " Specifies that the predicate should be interpreted coinductively.".

