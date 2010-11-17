:- module(example_forall,_,[.(forall), clpq]).

p(0).
p(s(X)) :- p(X).

even(0).
even(s(s(X))) :- even(X).

odd(s(X)).
odd(s(s(X))) :- odd(X).

q(X) :- X .=. 0.