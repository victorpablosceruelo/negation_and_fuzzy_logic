:- module(_, _, [assertions]).

t :-
	q(true).

:- meta_predicate q(goal).
q(P) :-
	P.

t2 :-
	'$meta_exp'(primitive(spec), (=)/2, N/2),
	X =.. [N,1,3],
	'$meta_call'(X).

:- use_module(engine(hiord_rt), ['$meta_call'/1]).
