:- module(_, _, []).

% testing multifile (un)loading...

:- multifile a/1.
:- dynamic a/1.
a(3).
a(5).

%q(X) :- display(ca), nl, a(X).
q(X) :- a(X).
r(X) :- retract_fact(a(X)).
