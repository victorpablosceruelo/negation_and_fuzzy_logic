:- module(_, [evalpol/3], [assertions, regtypes,
		ciaopp(examples(resources(exectimell)))]).

:- entry evalpol(C, X, R) : list(int) * int * var.

evalpol([],    _X, 0).
evalpol([C|L], X,  R) :-
	evalpol(L, X, R0),
	R1 is R0 * X,
	R is R1 + C.
