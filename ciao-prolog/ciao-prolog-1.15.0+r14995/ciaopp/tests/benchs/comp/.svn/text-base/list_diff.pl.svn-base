:- module(list_diff, [diff/3], [assertions]).

:- entry diff/3 : list(gnd) * list(gnd) * var.

diff([], _L, []).
diff([H|L1], L2, L3) :-
     memberchk(H, L2),
     !,
     diff(L1, L2, L3).
diff([H|L1], L2, [H|L3]) :-
     diff(L1, L2, L3).

memberchk(X, [X|_]) :- !.
memberchk(X, [_|L]) :- memberchk(X, L).
