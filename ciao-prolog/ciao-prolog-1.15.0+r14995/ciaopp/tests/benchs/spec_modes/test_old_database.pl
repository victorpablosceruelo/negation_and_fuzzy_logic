:- module(_,[p/0],[]).

:- use_module(library(old_database)).


p:-
	q(X,Y),
	recorda(X,Y,Z).

q(X,X).
q(X,X).
