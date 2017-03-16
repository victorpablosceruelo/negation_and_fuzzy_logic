:- module(qsort_4, [qsort/2], [assertions, nativeprops,
		ciaopp(examples(resources(rtchecks)))]).

:- entry qsort(A, B) : list(num) * var.

% :- regtype int_list/1.

% int_list([]).
% int_list([X|L]) :-
% 	int(X),
% 	int_list(L).

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

/*
:- use_module(library(random)).
:- export(random_/3).
random_(A, B, C) :-
	random(B, C, A).
*/