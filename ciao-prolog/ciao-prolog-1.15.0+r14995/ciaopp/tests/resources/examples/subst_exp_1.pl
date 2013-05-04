:- module(subst_exp_1, [substitute/3], [assertions, regtypes, nativeprops,
		ciaopp(tests(resources)),
		predefres(res_all)]).

:- doc(author, "Edison Mera").

:- resource res_steps.

:- head_cost(ub, res_steps, 1).
:- literal_cost(ub, res_steps, 0).

:- head_cost(lb, res_steps, 1).
:- literal_cost(lb, res_steps, 0).

:- trust_default + cost(ub, res_steps, 0).
:- trust_default + cost(lb, res_steps, 0).

:- entry substitute(X, Y, Z) : arithexpression * list(replacement) * var.

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
substitute((A ** B), Subs, (NewA ** B)) :-
	integer(B),
	!,
	substitute(A, Subs, NewA).
substitute(A, Subs, B) :-
	find_replacement(A, Subs, B),
	!.
substitute(A, _, A).

find_replacement(A, [A = B|_], B).
find_replacement(A, [_|Ys],    B) :- find_replacement(A, Ys, B).
