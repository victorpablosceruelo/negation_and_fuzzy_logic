:- module(_, _, [assertions]).

:- include(library(andprolog_nd(andprolog_nd_ops))).
%%**********************************************************************
%% PROPERTIES
%%**********************************************************************


:- prop andcallable/1.

:- comment(andcallable(Expression), "@var{Expression} is of the form
   @tt{goal&goal}.").

andcallable(A&B) :-
	callable(A),
	callable(B).


:- prop handler/1.

:- comment(handler(Handler), "@var{Handler} is a handler.").

handler(Handler) :- int(Handler).

