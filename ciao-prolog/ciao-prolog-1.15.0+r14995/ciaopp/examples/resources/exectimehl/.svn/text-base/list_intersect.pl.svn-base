:- module(list_intersect, [intersect/3],
	    [assertions, ciaopp(examples(resources(exectimehl)))]).

:- entry intersect/3 : list(num) * list(num) * var.

intersect([],     _L, []).
intersect([H|L1], L2, [H|L3]) :-
	memberchk(H, L2),
	!,
	intersect(L1, L2, L3).
intersect([_H|L1], L2, L3) :-
	intersect(L1, L2, L3).

memberchk(X, [X|_]) :- !.
memberchk(X, [_|L]) :- memberchk(X, L).
