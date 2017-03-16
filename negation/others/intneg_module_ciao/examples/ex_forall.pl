:- module(ex_forall,_,[intneg]).

peano_number(0).
peano_number(s(X)):-
	peano_number(X).

odd(s(X)) :- peano_number(X), even(X).
even(0).
even(s(X)) :- peano_number(X), odd(X).

busca(X) :- peano_number(X), even(Y).
busca(X) :- even(Y).