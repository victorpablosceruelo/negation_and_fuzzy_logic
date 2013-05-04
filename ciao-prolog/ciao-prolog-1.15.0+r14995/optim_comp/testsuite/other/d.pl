:- module(_, _, []).

:- internal_decl(foo/0, ptoc).

foo :-
	X is 10 + 23,
	( Y = 1 ; Y = 2 ; Y = 3 ),
	display(hola(X, Y)), nl.
