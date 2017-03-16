:- module(_, _, []).

% 11-queens program (obtain all the solutions)

:- use_module(library(prolog_sys)).
benchmark_system(newciao).
'$cputime'(X) :- statistics(runtime, [X|_]).
benchmark_start :-
	'$cputime'(T1),
	benchmark_data(Name, Count, Data),
	benchmark_loop(Count, Data),
	'$cputime'(T2),
	Time is T2-T1,
	benchmark_system(System),
	display(t(System,Name,Count,Time)), display('.'), nl.

benchmark_loop(Count, Data) :-
	repeat(Count),
	benchmark(Data, _Result),
	fail.
benchmark_loop(_, _).

repeat(_N).
repeat(N) :-
	N > 1,
	N1 is N - 1,
	repeat(N1).

benchmark_data(queens11, 1, _).

benchmark(_Data, _Out) :-
	do_queens.

do_queens:-
        queens(11, Qs),
%	display(Qs), nl,
	fail.
do_queens.

queens(N, Qs):-
        queens_list(N, Ns),
        queens_2(Ns, [], Qs).


queens_2([], Qs, Qs).
queens_2(Unplaced, Placed, Qs):-
        sel(Q, Unplaced, NewUnplaced),
        no_attack(Q, Placed),
        queens_2(NewUnplaced, [Q|Placed], Qs).

no_attack(Q, Safe):- no_attack_2(Safe, Q, 1).

no_attack_2([], _Queen, _Nb).
no_attack_2([Y|Ys], Queen, Nb) :-
        A is Y + Nb,
	Queen =\= A,
        B is Y - Nb,
	Queen =\= B,
        Nb1 is Nb + 1,
        no_attack_2(Ys, Queen, Nb1).

sel(X, [X|Ys], Ys).
sel(X, [Y|Ys], [Y|Zs]):-
        sel(X, Ys, Zs).

queens_list(0, []).
queens_list(N, [N|Ns]) :-
        N > 0,
        N1 is N - 1,
        queens_list(N1, Ns).
