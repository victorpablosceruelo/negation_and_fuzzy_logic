:- module(rec,[p/1]).

p(a).
p(X):- p(X).
