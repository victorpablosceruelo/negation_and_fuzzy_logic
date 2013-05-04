:- module(_,[p/2]).

p(Value,Res):- plus1_1(Tmp, Value), Tmp2 is Tmp + 3, plus1_2(Tmp2,Res).


plus1_1(X,Y):- ground(X), Y is X + 1.
plus1_1(X,Y):- var(X),    X is Y - 1.

plus1_2(X,Y):- ground(X), Y is X + 1.
plus1_2(X,Y):- var(X),    X is Y - 1.
