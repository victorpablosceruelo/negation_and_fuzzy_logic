:- module(ex_overlapping,_,[intneg]).

q(0).

p(X, 0) :- q(X).
p(0, X) :- q(X).
p(0,0).
