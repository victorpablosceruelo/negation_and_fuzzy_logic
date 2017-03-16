:- module(map, [map/3], []).

:- meta_predicate(map(pred(2),?,?)).

map(_, [], []).
map(P, [X|Xs], [Y|Ys]) :-
        P(X,Y),
        map(P, Xs, Ys).
