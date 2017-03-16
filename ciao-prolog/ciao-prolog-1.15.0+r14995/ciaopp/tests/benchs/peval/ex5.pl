:- module(ex5,[main/1],[assertions]).

:- entry main(X):ground(X).

main(X):-
	ground(X),
	p(X),
	q(X).

p(a).

q(_).
