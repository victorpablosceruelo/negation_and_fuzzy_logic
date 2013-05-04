:- module(_, _, [assertions]).

t :-
	q(p(3), 3).

p(X) :-
	display(ok(X)), nl.

:- meta_predicate q(goal, ?).
q(P, X) :-
	P,
	P.
