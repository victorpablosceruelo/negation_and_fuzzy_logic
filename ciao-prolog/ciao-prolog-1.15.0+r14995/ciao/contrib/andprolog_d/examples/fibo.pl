:- module(fibo, [fibo_par/3, main/1, main_seq/1], [andprolog_d]).

:- use_module(library(prolog_sys), [statistics/2]).
:- use_module(library(format), [format/2]).

fibo_par(0, 0, _).
fibo_par(1, 1, _).
fibo_par(N, F, Level) :-
        N > 1,
        (
            N =< Level ->
            fibo_seq(N, F)
        ;
            N1 is N - 1,
            N2 is N - 2,
            fibo_par(N1, F1, Level) '&!' fibo_par(N2, F2, Level),
            F is F1 + F2
        ),
	!.

fibo_seq(0, 0).
fibo_seq(1, 1).
fibo_seq(N, F) :-
        N > 1,
        N1 is N - 1,
        N2 is N - 2,
        fibo_seq(N1, F1),
        fibo_seq(N2, F2),
        F is F1 + F2.

main([N]) :-
	atom_codes(N, N1),
	number_codes(Number, N1),
	goal_thread,
	statistics(walltime, _),
	fibo_par(Number, Solution, 20),
	statistics(walltime, [_,Time]),
	format("Fibonacci of ~w: ~w~n", [Number, Solution]),
	format("Time taken: ~w mscs.~n", [Time]).

main_seq(N) :-
	goal_thread,
	statistics(walltime, _),
	fibo_seq(N, Solution),
	statistics(walltime, [_,Time]),
	format("Fibonacci of ~w: ~w~n", [N, Solution]),
	format("Time taken: ~w mscs.~n", [Time]).

