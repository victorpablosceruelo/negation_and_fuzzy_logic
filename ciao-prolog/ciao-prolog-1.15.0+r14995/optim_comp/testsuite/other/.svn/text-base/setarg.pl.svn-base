:- module(_, _, []).

main :-
	'$global_vars_set'(3, _),
	set(1, 1),
	l(1, 1000),
	get(100, X),
	display(X), nl.

l(I, N) :- I >= N, !.
l(I, N) :-
	get(I, A0),
	A is A0 + 1,
	I1 is I + 1,
	set(I1, A),
	l(I1, N).

:- use_module(library(lists)).
:- use_module(library(sort)).
:- use_module(library(dict)).
:- use_module(engine(internals)).

set(K, V) :-
	'$global_vars_get'(3, Dic),
	dic_replace(Dic, K, V, Dic2),
	'$global_vars_set'(3, Dic2).

get(K, V) :-
	'$global_vars_get'(3, Dic),
	dic_lookup(Dic, K, V).
