:- module(example_ground,[p/1],[]).

p(Y):- X= f(a,g(b)),q(X,Y).
% p(Y):- X= f(a,_Z),q(X,Y).

q(X,Y) :- ground(X),!,Y=3.
q(_X,Y) :- Y=4.
