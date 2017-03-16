:- module(_,_,[]).

:- use_module(library(prolog_sys)).

main :- benchmark_start.
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

benchmark_data(tak, 1000, triple(18, 12, 6)).

benchmark(triple(X, Y, Z), Out) :-
	tak(X, Y, Z, Out).

tak(X,Y,Z,A) :-
	X =< Y, !,
	Z = A.
tak(X,Y,Z,A) :-
	% X > Y,
	X1 is X - 1,
	tak(X1,Y,Z,A1),
	Y1 is Y - 1,
	tak(Y1,Z,X,A2),
	Z1 is Z - 1,
	tak(Z1,X,Y,A3),
	tak(A1,A2,A3,A).
