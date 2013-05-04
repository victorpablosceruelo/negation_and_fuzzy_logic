:- module(_,[p/1],[]).

p(X) :- q(X),r(X).
q(X):- X=a.
q(X):- q(X).
r(X):- X=a.
r(X):- X=b.
