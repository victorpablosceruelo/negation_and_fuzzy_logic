:- module(_,[p/2],[]).

:- use_package(assertions).

:- entry p(2,X) : var(X).

p(Value,Res):- Tmp is Value * 3, plus1(Tmp,Res).

plus1(X,Y):- ground(X), Y is X + 1.
plus1(X,Y):- var(X), X is Y - 1.
