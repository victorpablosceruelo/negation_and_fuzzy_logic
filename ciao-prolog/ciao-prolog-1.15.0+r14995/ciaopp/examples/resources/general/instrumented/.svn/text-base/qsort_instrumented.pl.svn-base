:- module(_,
	    [qsort/2],
	    [assertions, regtypes, nativeprops, library(resdefs(resources_decl))]).

:- entry qsort(A, B) : list(num) * var.

qsort([X|L], R) :-
	partition(L, X, L1, L2),
	qsort(L2, R2),
	qsort(L1, R1),
	append(R1, [X|R2], R).
qsort([], []).

partition([],    _, [],        []) :- display('+2').
partition([E|R], C, [E|Left1], Right) :- display('+2'),
	E < C, !,
	partition(R, C, Left1, Right).
partition([E|R], C, Left, [E|Right1]) :- display('+2'),
	E >= C,
	partition(R, C, Left, Right1).


append([],    X, X).
append([H|X], Y, [H|Z]) :- append(X, Y, Z).

:- load_resource_module(qsort_res).
:- resource lists_parallelized.

:- head_cost(ub, lists_parallelized, delta_lists_parallelized).
:- literal_cost(ub, lists_parallelized, 0).
