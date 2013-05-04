:- module(list_intersect_1, [intersect/3], [assertions, nativeprops,
		ciaopp(tests(resources)), predefres(res_all)]).

:- doc(author, "Edison Mera").

:- resource res_steps.

:- head_cost(ub, res_steps, 1).
:- head_cost(lb, res_steps, 1).

:- literal_cost(ub, res_steps, 0).
:- literal_cost(lb, res_steps, 0).

:- trust_default + cost(ub, res_steps, 0).
:- trust_default + cost(lb, res_steps, 0).

:- entry intersect/3 : list(gnd) * list(gnd) * var.

intersect([],     _L, []).
intersect([H|L1], L2, [H|L3]) :-
	memberchk(H, L2),
	!,
	intersect(L1, L2, L3).
intersect([_H|L1], L2, L3) :-
	intersect(L1, L2, L3).

memberchk(X, [X|_]) :- !.
memberchk(X, [_|L]) :- memberchk(X, L).
