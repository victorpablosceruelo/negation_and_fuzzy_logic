:- module(_, [substitute/3], [assertions, nativeprops, regtypes,
		library(resdefs(resources_decl))]).

%:- use_module(engine(arithmetic),[arithexpression/1]).

:- load_resource_module(subst_exp_res).
:- resource replacements.
:- head_cost(ub, replacements, delta_replacements).
:- literal_cost(ub, replacements, 0).

:- doc(author, "Edison Mera").

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
substitute((A ** B), Subs, (NewA ** B)) :-
	integer(B),
	!,
	substitute(A, Subs, NewA).
substitute(A, Subs, B) :-
	find_replacement(A, Subs, B),
	!.
substitute(A, _, A).

find_replacement(A, [A = B|_], B) :- display('+1').
find_replacement(A, [_|Ys],    B) :- display('+1'),
	find_replacement(A, Ys, B).
