:- module(_, _, []).

:- '$preddef'(ap/3, ptoc).
:- '$ptoc_prop'('ptoc_ap:ap'/3, [imp=nondet, indexed = false]).
:- '$ptoc_prop'('ptoc_ap:ap'/3, [should_trim_frame=no]).
:- '$ptoc_prop'('ptoc_ap:ap'/3, [register = true]).

ap([], X, X).
ap([X|Xs], Ys, [X|Zs]) :- ap(Xs, Ys, Zs),
    display(ok), nl.

:- '$preddef'(b/0, ptoc).
:- '$ptoc_prop'('ptoc_ap:b'/0, [imp=nondet, indexed = false]).
:- '$ptoc_prop'('ptoc_ap:b'/0, [should_trim_frame=no]).
:- '$ptoc_prop'('ptoc_ap:b'/0, [register = true]).

b :-
    c,
    nl.

:- '$preddef'(c/0, ptoc).
:- '$ptoc_prop'('ptoc_ap:c'/0, [imp=nondet, indexed = false]).
:- '$ptoc_prop'('ptoc_ap:c'/0, [should_trim_frame=no]).
:- '$ptoc_prop'('ptoc_ap:c'/0, [register = true]).

c :-
    nl,
    nl.
