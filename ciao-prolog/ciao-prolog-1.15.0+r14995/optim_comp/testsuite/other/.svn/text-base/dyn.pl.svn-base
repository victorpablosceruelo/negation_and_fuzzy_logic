:- module(_, _, []).

:- use_module(library(dynamic), [asserta/1]).
:- dynamic(foo/0).
main :-
	F = foo,
	asserta((foo :- display(hola), nl)),
	F.
