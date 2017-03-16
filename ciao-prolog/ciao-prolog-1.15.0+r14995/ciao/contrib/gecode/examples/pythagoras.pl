:- module(pythagoras, [do/0]).

:- use_package(gecode).

do :-
	L = [A,B,C],
	LL = [AA,BB,CC],
	L in 1..1000,
	LL in 1..1000000,
 	AA .=. A*A,
 	BB .=. B*B,
 	CC .=. C*C,
 	AA + BB .=. CC,
 	A .=<. B,
	B .=<. C,
	labeling(L).
