:- module(call,[c/1],[]).

c(X):-
	call(p(X)).

p(a).
