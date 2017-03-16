% this is not a bug! it is hard to tell when we can apply the second expansion
% TODO: implement metatype analysis...?
:- module(_,_,_).

:- meta_predicate foo(goal).

a :-
	X = p,
	%
	'$meta_exp'(goal,foo(X),Y),
	display(a(Y)), nl,
	%
	'$meta_exp'(spec,p/0,X3),
	display(d(X3)), nl,
	'$meta_exp'(goal,X,X2),
	display(b(X2)), nl,
	'$meta_exp'(goal,foo(X2),Y2),
	display(c(Y2)), nl.

p.
foo(_).
