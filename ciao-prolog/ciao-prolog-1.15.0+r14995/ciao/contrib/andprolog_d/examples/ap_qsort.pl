:- module(ap_qsort, [qsort/2, main/1, main_seq/1], [fsyntax, andprolog_d]).

:- use_module(library(prolog_sys), [statistics/2]).
:- use_module(library(lists), [append/3]).
:- use_module(library(random), [random/3]).
:- use_module(library(format), [format/2]).

:- fun_eval arith(true).

qsort([X|L], R) :-
	partition(L, X, L1, L2),
	qsort(L2, R2) '&!' qsort(L1, R1),
	append(R1, [X|R2], R),
	!.
qsort([], []).

qsort_seq([X|L], R) :-
	partition(L, X, L1, L2),
	qsort_seq(L2, R2),
	qsort_seq(L1, R1), 
	append(R1, [X|R2], R),
	!.
qsort_seq([], []).

partition([], _B, [], []).
partition([E|R], C, [E|Left1], Right) :- 
	E < C,
	partition(R, C, Left1, Right).
partition([E|R], C, Left, [E|Right1]) :-
	E >= C,
	partition(R, C, Left, Right1).

:- fun_eval gen_list/1.
gen_list(0) := [].
gen_list(X) := [~random(1,1000000)|~gen_list(X-1)]
	    :- X > 0.

main([N]) :-
	atom_codes(N, N1),
	number_codes(Number, N1),
	goal_thread,
	statistics(walltime, _),
	qsort(~gen_list(Number), _Solution),
	statistics(walltime, [_,Time]),
% 	format("Solution for ~w random values: ~w~n", [Number, Solution]),
	format("Time taken: ~w mscs.~n", [Time]).

main_seq(N) :-
	goal_thread,
	statistics(walltime, _),
	qsort_seq(~gen_list(N), _Solution),
	statistics(walltime, [_,Time]),
% 	format("Solution for ~w random values: ~w~n", [N, Solution]),
	format("Time taken: ~w mscs.~n", [Time]).


