:- module(_, [list_diff/3], [assertions, regtypes,
		ciaopp(examples(resources(exectimell)))]).

%:- entry list_diff/3 : list(gnd) * list(gnd) * var.
:- entry list_diff/3 : list(num) * list(num) * var.

list_diff([],     _L, []).
list_diff([H|L1], L2, L3) :-
	memberchk(H, L2),
	!,
	list_diff(L1, L2, L3).
list_diff([H|L1], L2, [H|L3]) :-
	list_diff(L1, L2, L3).

memberchk(X, [X|_]) :- !.
memberchk(X, [_|L]) :- memberchk(X, L).
