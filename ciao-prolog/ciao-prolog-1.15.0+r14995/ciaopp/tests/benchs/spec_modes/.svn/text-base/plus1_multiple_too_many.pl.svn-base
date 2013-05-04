:- module(_,[p/2]).

p(Value,Res):- 
	plus1(Tmp, Value), plus1(Tmp,Tmp1), plus1(Tmp1,Tmp2),
	plus1(Tmp2,Tmp3), plus1(Tmp3,Res).


plus1(X,Y):- ground(X), Y is X + 1.
plus1(X,Y):- var(X), X is Y - 1.
