:- module( _example7_1, [r/1], [assertions,nativeprops,regtypes,rtchecks] ).


r(X) :-
        q(X),
        p(X).

q(a).
q(f(X)) :-
        q(X).

p(a).
p(f(X)) :-
        p(X).
p(g(X)) :-
        p(X).



