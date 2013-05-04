:- module(findall,[f/1],[]).

:- use_module(library(aggregates)).

f(X):-
	findall(Y, p(Y), X).

p(a).
p(b).

