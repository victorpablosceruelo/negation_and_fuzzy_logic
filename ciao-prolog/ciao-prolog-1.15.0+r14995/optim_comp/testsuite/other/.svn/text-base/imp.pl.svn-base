:- module(_, _, []).

:- use_module(library(lists), [append/3]).
:- use_module(library(prolog_sys)).

% Test runtime expansions

p :-
	X = append([A], [], Z),
	X,
	Y = new_atom(A),
	Y,
	display(Z), nl.
