:- module(_, _, []).

:- '$preddef'(p/2, ptoc).
:- '$ptoc_prop'('ptoc_b:p'/2, [imp = det, register = true, indexed = false]).
p(X, Y) :- X = 0, !.
p(X, Y) :- X = 1, !.
p(X, Y).
