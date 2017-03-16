:- module(nreverse, [], [benchmark]).

name("Naive reverse").
repeat_count(1).
solve(Out) :-
	list(500, Xs),
	nreverse(Xs, Out).

list(0, []) :- !.
list(N, [N|Xs]) :-
	N1 is N - 1,
	list(N1, Xs).

nreverse([], []).
nreverse([X|Xs], Ys):-
	nreverse(Xs, Ys0),
	append(Ys0, [X], Ys).

append([], Ys, Ys).
append([X|Xs], Ys, [X|Zs]) :-
	append(Xs, Ys, Zs).

