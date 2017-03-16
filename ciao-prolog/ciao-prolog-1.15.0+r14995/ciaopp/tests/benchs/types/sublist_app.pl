:- module(_, [sublist/2], [assertions]).

:- entry sublist(A,B) : list(num) * var. 

sublist(Sub, List) :-
        append(_, Sub, L),
        append(L, _, List).

append([], L, L).
append([X|Xs], L, [X|Zs]) :-
        append(Xs, L, Zs).


