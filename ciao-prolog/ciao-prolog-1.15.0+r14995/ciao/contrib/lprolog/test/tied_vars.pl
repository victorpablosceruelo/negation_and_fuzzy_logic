:- module(tied_vars,[main/0],[]).
:- use_package(lprolog).
:- use_module(library(write)).

q(c).

main :-
	sigma x \ ((p :- q(x)) => p, display_ref(x)), nl,
	(p:- q(X)) => p, display_ref(X), nl.
