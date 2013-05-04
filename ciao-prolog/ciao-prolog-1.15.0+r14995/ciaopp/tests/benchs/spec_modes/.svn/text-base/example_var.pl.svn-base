:- module(example_var,[p/1]).

p(Y):- X= f(a,_Z),q(X,Y).
% p(Y):- X= _Z,q(X,Y).

q(X,Y) :- var(X),!,Y=3.
q(_X,Y) :- Y=4.
