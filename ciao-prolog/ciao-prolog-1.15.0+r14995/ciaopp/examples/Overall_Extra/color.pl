:- module(_,[p/1],[assertions,regtypes,fsyntax]).

%% :- entry p/1 : { ground, color }.

:- regtype color/1. 

color := red | green | blue.

p(X) :- 
	q(X).

q(red).
q(green).
q(red).
q(blue).
q(green).
