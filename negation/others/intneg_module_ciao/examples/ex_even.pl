:- module(ex_even,_,[intneg]).

even(0).
even(s(s(X))):- even(X).

odd(X) :- intneg(even(X)).


peano(X) :- odd(X).
peano(X) :- even(X).