:- module(_, [flatten/2], [assertions, regtypes,
		ciaopp(tests(resources)),
		predefres(res_steps)]).

:- entry flatten/2 : gnd * var.

:- resource res_steps.

:- head_cost(ub, res_steps, 1).
:- literal_cost(ub, res_steps, 0).
:- trust_default + cost(ub, res_steps, 0).


append([],    L,  L).
append([H|L], L1, [H|R]) :-
	append(L, L1, R).

flatten(X, [X]) :-
	atomic(X),
	X \== [], !.
flatten([],     []).
flatten([X|Xs], Ys) :-
	flatten(X,  Ys1),
	flatten(Xs, Ys2),
	append(Ys1, Ys2, Ys).
