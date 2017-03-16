:-module(ex,[ex/3],[assertions,regtypes]).

plus(X,Y,Z):- 
	ground(X),
	ground(Y),!,
	Z is X + Y. 

plus(X,Y,Z):- 
	ground(Y),
	ground(Z),!,
	X is Z - Y.

plus(X,Y,Z):- 
	ground(X),
	ground(Z),!,
	Y is Z - X.

add(X,Y,Z,Res):- 
	plus(X,Y,Tmp),
	plus(Z,Tmp,Res).

ex(X,Z,Res):-
	X=1,
	add(X,Z,Res).

:- entry ex(X,Z,Res):int(Z).


