:- module(m2, [q/1]).

q(X) :-
        r(X),
        X @< 10.

r(4).
