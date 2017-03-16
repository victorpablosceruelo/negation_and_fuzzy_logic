:- module(subst_exp_hlm, [substitute/3], [assertions, regtypes,
		ciaopp(examples(resources(exectimehl)))]).

:- entry substitute(X, Y, Z) : arithexpression * list(replacement) * var.

:- export(replacement/1).
:- regtype replacement/1.

replacement('='(A, B)) :- arithexpression(A), arithexpression(B).


substitute(A + B, Subs, NewA + NewB) :-
	!,
	substitute(A, Subs, NewA),
	substitute(B, Subs, NewB).
substitute(A * B, Subs, NewA * NewB) :-
	!,
	substitute(A, Subs, NewA),
	substitute(B, Subs, NewB).
substitute(A - B, Subs, NewA - NewB) :-
	!,
	substitute(A, Subs, NewA),
	substitute(B, Subs, NewB).
substitute(A = B, Subs, NewA = NewB) :-
	!,
	substitute(A, Subs, NewA),
	substitute(B, Subs, NewB).
substitute(exp(A, B), Subs, exp(NewA, B)) :-
	integer(B),
	!,
	substitute(A, Subs, NewA).
substitute(A, Subs, B) :-
	find_replacement(A, Subs, B),
	!.
substitute(A, _, A).

find_replacement(A, [A = B|_], B).
find_replacement(A, [_|Ys],    B) :- find_replacement(A, Ys, B).
