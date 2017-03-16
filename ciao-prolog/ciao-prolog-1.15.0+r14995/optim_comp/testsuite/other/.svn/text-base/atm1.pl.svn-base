% Program to test max atom table size...
:- module(_, _, []).
:- use_module(library(prolog_sys)).

c(X) :-
	display('Creating '), display(X), display(' atoms...\n'),
	t(0, X).

t(I, I) :- !,
	display(ok), nl.
t(I0, I) :-
	( I0 mod 10000 =:= 0 ->
	    display(I0), nl
	; true
	),
	I1 is I0 + 1,
%	number_codes(I1, C),
%	atom_codes(_, C),
	new_atom(_),
	t(I1, I).
