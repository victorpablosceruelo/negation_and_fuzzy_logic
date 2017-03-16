:- module(bn, [main/0]).

:- use_module(library(prolog_sys)).
:- use_module(library(format)).

main :-
        Exp = 2048,
        exponential_naive(Exp, R),
	display(R), nl.

exponential_naive(0, 1).
exponential_naive(Exp, Res):-
        Exp > 0,
        NewExp is Exp - 1,
        exponential_naive(NewExp, PartRes),
        Res is PartRes * 8.
