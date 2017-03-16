:- module(_,[p/2],[]).

p(Value,Res):- 
	plus1_1(Tmp, Value), plus1_2(Tmp,Tmp1), plus1_3(Tmp1,Tmp2),
	plus1_4(Tmp2,Tmp3), plus1_5(Tmp3,Res).


plus1_1(X,Y):- ground(X), Y is X + 1.
plus1_1(X,Y):- var(X), X is Y - 1.

plus1_2(X,Y):- ground(X), Y is X + 1.
plus1_2(X,Y):- var(X), X is Y - 1.

plus1_3(X,Y):- ground(X), Y is X + 1.
plus1_3(X,Y):- var(X), X is Y - 1.

plus1_4(X,Y):- ground(X), Y is X + 1.
plus1_4(X,Y):- var(X), X is Y - 1.

plus1_5(X,Y):- ground(X), Y is X + 1.
plus1_5(X,Y):- var(X), X is Y - 1.
