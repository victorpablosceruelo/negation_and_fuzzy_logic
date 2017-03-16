:- module(_,[fact/2],[assertions,regtypes]).

:- entry fact/2 : num * var.

fact(0,1).
fact(N,R) :- 
	N>0, 
	N1 is N-1, 
	fact(N1,R2), 
	R is R1*N.
