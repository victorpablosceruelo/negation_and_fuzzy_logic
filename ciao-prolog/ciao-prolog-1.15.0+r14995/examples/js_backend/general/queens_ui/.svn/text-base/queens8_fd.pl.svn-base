:- module(queens8_fd, [], [benchmark]).

:- use_package(clpfd).

:- use_module(library(lists)).

name := "8-Queens Puzzle (Finite Domains)".
repeat_count := 1.
solve(X) :- queens(8, X).

queens(N, L) :-
	queens_(N, L, [ff]).

queens_(N, L, Lab) :-
	length(L, N),
	domain(L, 1, N),
	safe(L),
	labeling(Lab, L).

safe([]).
safe([X|L]) :-
	noattack(L, X, 1),
	safe(L).

noattack([], _, _).
noattack([Y|L], X, I) :-
	diff(X, Y, I),
	I1 is I + 1,
	noattack(L, X, I1).

diff(X,Y,I):-
	X#\=Y,
	X#\=Y+I,
	X+I#\=Y.


