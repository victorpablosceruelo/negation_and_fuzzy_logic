:- module(_mayor,_,[assertions]).

equilatero(X,X,X).

isosceles(X,X,Y):- neg(equilatero(X,X,Y)).
isosceles(X,Y,X):- neg(equilatero(X,Y,X)).
isosceles(Y,X,X):- neg(equilatero(Y,X,X)).

escaleno(X,Y,Z) :- neg(equilatero(X,Y,Z)), neg(isosceles(X,Y,Z)). 



