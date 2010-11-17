:- module(_,_,[assertions]).

:- entry impar(X): (ground(X), num(X)).

impar(X):-
	Y is X rem 2,
	Y\==0.



