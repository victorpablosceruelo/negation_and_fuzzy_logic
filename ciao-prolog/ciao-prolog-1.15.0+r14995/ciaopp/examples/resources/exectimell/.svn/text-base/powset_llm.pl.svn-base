:- module(_, [powset/2], [assertions, regtypes,
		ciaopp(examples(resources(exectimell)))]).


:- entry powset(A, B) : list(int) * var.

powset([],    [[]]).
powset([X|L], P) :-
	powset(L, P0),
	appendelem(P0, X, P, P0).

appendelem([],     _X, T,          T).
appendelem([L|Ls], X,  [[X|L]|Rs], T) :-
	appendelem(Ls, X, Rs, T).
