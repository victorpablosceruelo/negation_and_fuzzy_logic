:- use_package(assertions).

:- doc(filetype, application). % TODO: or 'documentation'?

:- doc(title,"The Ciao Compiler Internals"). % TODO: Also the engine? They are linked
%:- doc(subtitle, "A New Generation, Multi-Paradigm Programming Language and Environment").
%:- doc(subtitle, "(Including a State-of-the-Art ISO-Prolog)").

:- doc(logo, 'ciao-shadow-64h').

:- doc(subtitle_extra,"REFERENCE MANUAL").
:- doc(subtitle_extra,"@bf{The Ciao Documentation Series}").
:- doc(subtitle_extra,"@href{http://www.ciaohome.org/}").
:- doc(subtitle_extra,"@em{Generated/Printed on:} @today{}").
%:- doc(subtitle_extra,"Technical Report CLIP 3/97-@version{}").

% TODO: Replace 'credits' by 'editor'? (JFMC)
%:- doc(credits, "@bf{Edited by:}").
%:- doc(credits, "Francisco Bueno").
%:- doc(credits, "Daniel Cabeza").
%:- doc(credits, "Manuel Carro").
%:- doc(credits, "Manuel Hermenegildo").
%:- doc(credits, "Pedro L@'{o}pez").
%:- doc(credits, "Germ@'{a}n Puebla").

:- include(library('ClipAddress')).

:- include(library('Copyright')).

%% :- doc(bug,"Although the documentation is acceptable at this
%%    point, we are still really in beta mode in this regard.").

:- doc(bug,"This is still just a first shot...").

:- doc(summary,"

   @include{Warning.lpdoc}

   @includefact{this_manual/1}

   ").

:- doc(module,"

   @include{Warning.lpdoc}

   @includefact{this_manual/1}
").

this_manual("This is the manual for the CIAO Prolog compiler.  It
  documents the @apl{ciaoc} application and the libraries which provide
  the functionalities for compiling/processing code.").


