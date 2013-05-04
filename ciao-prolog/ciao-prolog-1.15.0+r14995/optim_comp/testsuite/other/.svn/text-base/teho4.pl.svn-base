:- module(_, _, [assertions, compiler(compiler_object)]).

:- all_instantiable.

t :-
	% bug1: this thing does not work... why?
	% q(p(3), 3).
	'$this'(This),
	'$meta_exp'(goal, This/teho4:p(3), X),
	display(X), nl.

p(X) :-
	display(ok(X)), nl.

:- static(q/2).
:- meta_predicate q(goal, ?).
q(P, _) :-
	P,
	P.
