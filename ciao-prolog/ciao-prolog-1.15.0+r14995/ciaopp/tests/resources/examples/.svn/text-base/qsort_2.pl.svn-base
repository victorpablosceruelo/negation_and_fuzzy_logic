:- module(qsort_2, [qsort/2], [assertions,
		ciaopp(tests(resources)),
		predefres(res_steps)]).

:- trust pred qsort(A, B)
	: (list(A, num), var(B))
	=> ( list(A, num), list(B, num),
	    size(A, length(A)),
	    size(B, exp(2, length(A)) -1.0) )
	+ cost(ub, steps, 10 * length(A) * length(A) + 20).

:- entry qsort(A, B) : list(num) * var.

qsort([X|L], R) :-
	partition(L, X, L1, L2),
	qsort(L2, R2),
	qsort(L1, R1),
	append(R1, [X|R2], R).
qsort([], []).

partition([],    _, [],        []).
partition([E|R], C, [E|Left1], Right) :-
	E < C, !,
	partition(R, C, Left1, Right).
partition([E|R], C, Left, [E|Right1]) :-
	E >= C,
	partition(R, C, Left, Right1).

append([],    X, X).
append([H|X], Y, [H|Z]) :- append(X, Y, Z).
