:- module(neg,[neg/1],[]).

neg(X):-
	\+ p(X).

p(a).
