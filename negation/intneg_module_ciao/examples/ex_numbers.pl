:- module(ex_numbers,_,[intneg]).

even(0).
even(s(s(X))):-
	even(X).

odd(s(X)) :- even(X).

number(X) :- even(X).
number(X) :- odd(X).