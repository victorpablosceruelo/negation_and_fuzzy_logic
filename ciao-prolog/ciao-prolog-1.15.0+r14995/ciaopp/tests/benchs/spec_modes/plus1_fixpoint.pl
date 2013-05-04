:- module(_,[p/1]).

p(Res):- even(Tmp), plus1(Tmp,Res).

even(0).
even(E):-
	even(E1),
	E is E1 + 2.

plus1(X,Y):- ground(X), Y is X + 1.
plus1(X,Y):- var(X),    X is Y - 1.
