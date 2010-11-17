:- module(ex_numbers,_,[intneg]).

peano_number(0).
peano_number(s(X)):-
	peano_number(X).

odd(s(X)) :- peano_number(X), even(X).
even(0).
even(s(X)) :- peano_number(X), odd(X).
