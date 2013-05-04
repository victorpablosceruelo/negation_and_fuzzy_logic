:- module(rec3,[p/0]).

p:- r(b).

r(X):- q(X).

q(a).
q(X):- q(X).
