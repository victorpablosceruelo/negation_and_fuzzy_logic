/*-------------------------------------------------------------------------*/
/* Benchmark (Finite Domain)                                               */
/*                                                                         */
/* Name           : queens.pl                                              */
/* Title          : N-queens problem                                       */
/* Original Source: P. Van Hentenryck's book                               */
/*                                                                         */
/* Put N queens on an NxN chessboard so that there is no couple of queens  */
/* threatening each other.                                                 */
/*                                                                         */
/* Solution:                                                               */
/* N=4  [2,4,1,3]                                                          */
/* N=8  [1,5,8,6,3,7,2,4]                                                  */
/* N=16 [1,3,5,2,13,9,14,12,15,6,16,7,4,11,8,10]                           */
/*-------------------------------------------------------------------------*/

% Adapted to SWI Prolog.
:- use_module(library(clpfd)).

% :- use_module(library(write),           [write/1]).
% :- use_module(library(prolog_sys),      [statistics/2]).
% :- use_module(library(lists),           [length/2]).

main(N, Lab) :-
	statistics(runtime, _),
	queens(N, L, Lab),
	statistics(runtime, [_, Y]),
	write(L),
	nl,
	write('time : '),
	write(Y),
	nl.

domain([], Min, Max).
domain([X|Xs], Min, Max) :-
	X in Min..Max,
	domain(Xs, Min, Max).

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


diff(X, Y, I) :-
 	X #\= Y,
 	X #\= Y+I,
 	X+I #\= Y.
