:- module(fairness, _, [.(cneg)]).
:- use_module(.(dist),[dist/2]). 

p(X) :- p(X).
q(b) :- p(b).
q(b) :- p(a).

r(X) :- t(X).
t(1).