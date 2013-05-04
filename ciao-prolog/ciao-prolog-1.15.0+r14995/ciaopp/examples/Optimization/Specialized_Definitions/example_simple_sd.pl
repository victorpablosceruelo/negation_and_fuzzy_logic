:- module(_,[main/2],[]).
main(A,B):-
 	equal(Tmp,A),
	one(A),
	ground(Tmp),
	increment(Tmp,B).
main(A,B):-
	equal(Tmp,A),
	one(A),
	var(Tmp),
	zero(Tmp,B).

equal(A,A).

one(s(0)).
one(A):-
	one(A).

increment(A,s(A)).

zero(_A,0).
