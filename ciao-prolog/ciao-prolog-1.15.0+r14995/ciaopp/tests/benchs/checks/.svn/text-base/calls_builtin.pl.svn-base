:- module(calls_builtin,[p/0],[assertions]).


p:-
	q(_X,4),
	q(_X,_Y).

:- calls q(X,Y) : ground(Y).

q(X,Y):-
	X is Y + 1.
 
