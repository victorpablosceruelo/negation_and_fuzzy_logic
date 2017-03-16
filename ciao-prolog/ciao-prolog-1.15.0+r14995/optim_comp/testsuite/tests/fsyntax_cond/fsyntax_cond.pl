% Test for conditional expressions in fsyntax 
% Author: Jose F. Morales
% Date: Sun Aug 15 10:50:06 CEST 2010

:- module(_, [main/0], [fsyntax]).

% This function should be compiled to only one predicate, indexing on A.
cond(A, B) := A = 1 ? got1(A, B) |
	      A = 2 ? got2(A, B) |
	      gotE(A, B).

% This function should be compiled to a main predicate and an
% auxiliary one indexing on A.
condw(A, B) := A = 1 ? got1(A, C) |
	       A = 2 ? got2(A, C) |
	       gotE(A, C) :- C = f(B).

test1 :-
	X = [~cond(1,a), ~cond(2,b), ~cond(3,c)],
%	display(X), nl,
	X = [got1(1,a), got2(2,b), gotE(3,c)].
test2 :-
	X = [~condw(1,a), ~condw(2,b), ~condw(3,c)],
%	display(X), nl,
	X = [got1(1,f(a)), got2(2,f(b)), gotE(3,f(c))].
test3 :-
	A = 1, B = 0,
	X = ( T is B + 1, A = T ? yes | no ),
%	display(X), nl,
	X = yes.

test :- test1, test2, test3.

main :-
	( test ->
	    true
	; display('test fsyntax_cond failed'), nl
	).