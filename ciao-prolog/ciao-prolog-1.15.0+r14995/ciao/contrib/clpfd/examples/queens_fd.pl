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

:- module(queens_fd, _).

:- use_package(hiord).
:- use_module(library(write), [write/1]).
:- use_module(library(prolog_sys), [statistics/2]).
:- use_module(library(lists), [length/2]).

:- use_module(library(clpfd(fd_term))).
:- use_module(library(clpfd(fd_constraints))).
:- use_module(library(clpfd(fd_labeling))).

% domain(L, A, B) :- L ins A..B.

domain([], _A, _B) .
domain([X|T], A, B):-
	fd_term:new(X),
	fd_term:in(X, '..'(A, B)), 
	domain(T, A, B).

main :- main_(4, []).

main_(N, Lab) :-
	statistics(runtime, _),
	queens(N, L, Lab),
	statistics(runtime, [_, Y]),
	my_write(L),	nl,
	write('time : '),
	write(Y),
	nl.

queens(N, L, Lab) :-
	length(L, N),
	domain(L, 1, N),
	safe(L),
	fd_labeling:labeling(Lab, L).

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
	fd_constraints:'a<>b'(X,Y),
	fd_constraints:'a<>b+c'(X,Y,I),
	fd_constraints:'a<>b+c'(Y,X,I).

:- use_module(library(hiordlib), [map/3]).
my_write(L):-
	map(L, (''(X, Y):-fd_term:const(X, Y)), NL), 
	write(NL).

