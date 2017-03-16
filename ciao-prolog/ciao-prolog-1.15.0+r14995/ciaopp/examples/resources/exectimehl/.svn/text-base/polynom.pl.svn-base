:- module(polynom, [evalpol/3], [assertions, regtypes, nativeprops,
		ciaopp(examples(resources(exectimehl)))]).

:- entry evalpol(C, X, R) : int_list * int * var.

:- export(int_list/1).
:- regtype int_list/1.

int_list([]).
int_list([X|L]) :-
	int(X),
	int_list(L).

evalpol([],    _X, 0).
evalpol([C|L], X,  R) :-
	evalpol(L, X, R0),
	R is R0 * X + C.
