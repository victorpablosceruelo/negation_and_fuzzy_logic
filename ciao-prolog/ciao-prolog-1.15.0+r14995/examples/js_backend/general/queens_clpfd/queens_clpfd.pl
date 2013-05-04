:- module(queens_clpfd, [], [benchmark]).

:- use_package(clpfd).
:- use_module(library(lists)).

name("50-Queens using CLP(fd) - first solution").
repeat_count(1).
solve(R) :-
	queens(50, R, [ff]),
	!. % just one one solution

% ---------------------------------------------------------------------------

queens(N, L, Lab) :-
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
        % abs(X - Y) #\= I,
	X#\=Y+I,
	X+I#\=Y.


