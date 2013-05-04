:- module(_,[r/2]).

p(a).
p(b).
p(s(X)) :- p(X).

q(X) :- p(X).

r(X,Y) :- q(X),q(Y).

