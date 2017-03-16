:- module(minimal,[r/1],[assertions]).

r(Y):-
	r2(_X,Y).

% this assertion should be falsified

:- check success r2(A,B) : var(A) => var(B).

r2(a,a).
	


