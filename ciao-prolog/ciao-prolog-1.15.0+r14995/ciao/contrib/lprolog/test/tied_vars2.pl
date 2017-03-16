:- module(tied_vars2,[main/0],[]).
:- use_package(lprolog).
:- use_module(library(write)).

q(c).
q(b).
r(b).

main :-
	(p:- q(X)) => p,
	display_ref(X), nl,
	r(X),
	display_ref(X), nl,
	(p ->
		display('should have failed'), nl
	;
		display('failed correctly'), nl
	).
