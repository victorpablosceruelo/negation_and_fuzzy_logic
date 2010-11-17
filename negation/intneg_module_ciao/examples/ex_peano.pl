:- module(ex_peano,_,[intneg]).

peano(0,0).
peano(N,s(P1)):-
	N > 0,
	N1 is N-1,
	peano(N1,P1).


