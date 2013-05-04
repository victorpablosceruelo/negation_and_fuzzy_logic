:- module(minimal_dynamic, [main/1],[assertions]).

:- use_module(library(dynamic)).

:- trust success pp(A) => ground(A).
:- dynamic pp/1.
pp(_).

main(X):-
%	assertz((pp(X) :- q(X))),
	assertz(pp(a)),
	pp(X).


q(X):- X = a.
