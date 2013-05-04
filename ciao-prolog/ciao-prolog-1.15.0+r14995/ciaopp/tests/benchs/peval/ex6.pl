:- module(ex6,[main/2],[assertions]).

main(X,Y):-
	aux(X,Y),
	s(Y).

aux(X,Y):-
	p(X),
	q(X,Y).

p(a).

q(a,B):-
	r(B).

q(b,_).

r(b).


s(_).
s(X):-
	s(X).
