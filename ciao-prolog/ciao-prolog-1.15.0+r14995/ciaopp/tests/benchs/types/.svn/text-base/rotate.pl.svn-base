:- module(_, [rotate/2], [assertions]).

:- entry rotate(A,B) : list(num) * var.

rotate([], []).
rotate([H|T], RList):-
        append([H|P], Suffix, [H|T]),
        append(Suffix, [H|P], RList).

append([], L, L).
append([X|Xs], L, [X|Zs]) :- append(Xs, L, Zs).


