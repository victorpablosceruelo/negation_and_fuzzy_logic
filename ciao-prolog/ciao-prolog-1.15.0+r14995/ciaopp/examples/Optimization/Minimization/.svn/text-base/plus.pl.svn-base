:- module( plus, [main/0], [assertions] ).

:- use_module(library(write)).

main:- 	go(3,5).

go(A,B) :- 
	p(A,B,_).
%	p(A,_,B).

p(X,Y,Z):-
	plus(X,Y,Z),
	write(Z), write(' is '),
	write(X), write(' + '), write(Y),nl.

plus(X,Y,Z):-
	integer(X),
	integer(Y),!,
	Z is X+Y.

plus(X,Y,Z):-
	integer(Y),
	integer(Z),!,
	X is Z-Y.


plus(X,Y,Z):-
	integer(X),
	integer(Z),!,
	Y is Z-X.
