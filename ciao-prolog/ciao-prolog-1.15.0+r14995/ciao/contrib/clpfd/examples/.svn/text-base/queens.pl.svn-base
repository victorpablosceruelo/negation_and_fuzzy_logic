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

:- module(queens, _, [clpfd, expander, fsyntax]).

:- use_module(library(write),           [write/1]).
:- use_module(library(prolog_sys),      [statistics/2]).
:- use_module(library(lists),           [length/2]).
:- use_module(library(clpfd(fd_range)), [fd_range_type/1]).
:- use_module(library(clpfd(fd_constraints))).

%:- module(queens, [main_/2]).
%:- use_module(library(clpfd)).

main(N, Lab, Const) :-
	statistics(runtime, _),
	queens(N, L, Lab, Const),
	statistics(runtime, [_, Y]),
	write(L),
	nl,
	write('time : '),
	write(Y),
	write('\t('), write(~diff_type), write(', '), write(~fd_range_type), 
	write(')'),
	nl.

queens(N, L, Lab, Const) :-
	length(L, N),
	domain(L, 1, N),
	safe(L, Const),
	labeling(Lab, L).

safe([], _Const).
safe([X|L], Const) :-
	noattack(L, X, 1, Const),
	safe(L, Const).

noattack([], _, _, _Const).
noattack([Y|L], X, I, Const) :-
	diff(Const, X, Y, I),
	I1 is I + 1,
	noattack(L, X, I1, Const).


:- discontiguous diff/4.

diff(clpfd, X, Y, I) :-
 	X #\= Y,
 	X #\= Y+I,
 	X+I #\= Y.

:- use_module(library(clpfd(fd_constraints))).

diff_type(fd).

diff(fd, X,Y,I):-
	fd_diff(~wrapper(X), ~wrapper(Y), I).

fd_diff(X, Y, I):-
	fd_constraints:'a<>b'(X,Y),
	fd_constraints:'a<>b+t'(X,Y,I),
	fd_constraints:'a<>b+t'(Y,X,I).

:- use_package('clpfd/indexicals').

diff(idx, X,Y,I):-
	idx_diff(~wrapper(X), ~wrapper(Y), I).

idx_diff(X, Y, I) +:
 	X in -{val(Y), val(Y)+c(I), val(Y)-c(I)},
 	Y in -{val(X), val(X)-c(I), val(X)+c(I)}.

:- use_module(library(clpfd(fd_term))).

diff(kernel, X,Y,I):-
	kernel_diff(~wrapper(X), ~wrapper(Y), I).

kernel_diff(X, Y, I) :-
  	fd_term:add_propag(Y, val, 'queens:cstr'(X, Y, I)),
  	fd_term:add_propag(X, val, 'queens:cstr'(Y, X, I)).

% Y is always singleton.
cstr(X, Y, I):-
  	fd_term:integerize(Y, Y0),
  	fd_term:prune(X, Y0),
  	Y1 is Y0 + I,
  	fd_term:prune(X, Y1),
  	Y2 is Y0 - I,
  	fd_term:prune(X, Y2).
