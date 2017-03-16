:- module(subst_exp_2, [substitute/3], [assertions, regtypes, nativeprops,
		ciaopp(examples(resources(rtchecks)))]).

:- doc(author, "Edison Mera").

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
