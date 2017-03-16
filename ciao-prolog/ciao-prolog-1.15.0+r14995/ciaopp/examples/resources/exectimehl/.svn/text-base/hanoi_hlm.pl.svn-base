:- module(hanoi_hlm, [hanoi/5], [assertions, regtypes, nativeprops,
		ciaopp(examples(resources(exectimehl)))]).

:- entry hanoi(A, B, C, D, E) : num * elem * elem * elem * var.

:- trust comp hanoi(N, A, B, C, M)
	+ ( size_metric(A, void),
	    size_metric(B, void),
	    size_metric(C, void) ).

hanoi(0, _A, _, _C, []) :- !.
hanoi(N, A,  B, C,  M) :-
	N1 is N - 1,
	hanoi(N1, A, C, B, M1),
	hanoi(N1, B, A, C, M2),
	append(M1, [mv(A, C)], T),
	append(T,  M2,         M).

append([],     L,  L).
append([X|L1], L2, [X|L3]) :-
	append(L1, L2, L3).

:- export(elem/1).
:- regtype elem/1.

elem(a).
elem(b).
elem(c).
