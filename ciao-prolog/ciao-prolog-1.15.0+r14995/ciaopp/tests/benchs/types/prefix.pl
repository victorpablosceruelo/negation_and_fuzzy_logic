:- module(_, [ensure_prefix/3], [assertions]).

:- entry ensure_prefix(A,B,C) : list(num) * list(num) * var.

ensure_prefix(Prefix, List, List):-
        append(Prefix, _, List),
        !.
ensure_prefix(Prefix, List, NewList):-
        append(Prefix, List, NewList).

append([],X,X).
append([H|X],Y,[H|Z]):- append(X,Y,Z).


