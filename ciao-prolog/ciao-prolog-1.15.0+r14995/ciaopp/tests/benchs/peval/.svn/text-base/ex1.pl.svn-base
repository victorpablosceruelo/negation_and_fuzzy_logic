:-module(ex1,[main/3],[assertions,regtypes,nativeprops]).

:- entry main(X,Z,Res):(ground(Z),var(Res),indep([[X,Res]])).

main(X,Z,Res):-
	X=1,
	Y=Z,
	add3(X,Y,Z,Res).

add3(X,Y,Z,Res):- 
	plus(X,Y,Tmp),
	plus(Z,Tmp,Res).

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

