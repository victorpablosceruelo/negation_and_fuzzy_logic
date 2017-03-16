:- module(dependencies, [p/0], [hiord, assertions]).

:- doc(author, "Edison Mera").

:- doc(bug, "Bug or behavior: The dependencies verification is
	unable to detect that q/1 is being used.  Work around: to add
	a meta_predicate declaration for metapred1.").

:- use_module(library(aggregates)).

p :-
	findall(A, q(A,_,_), _B),
	metapred1(q).

% :- meta_predicate metapred1(pred(1)).

metapred1(P) :-
% 	P(A),
	call(P, A, B, C),
	display(t(A, B, C)),
	nl.

q(a, b, c).
