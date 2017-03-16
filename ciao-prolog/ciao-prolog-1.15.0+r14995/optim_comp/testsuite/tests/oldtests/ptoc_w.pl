:- module(_, _, []).

:- '$preddef'(tak/0, ptoc).
:- '$ptoc_prop'('ptoc_w:tak'/0, [imp=nondet]).
:- '$ptoc_prop'('ptoc_w:tak'/0, [should_trim_frame=no]).
:- '$ptoc_prop'('ptoc_w:tak'/0, [register = true]).
:- '$ptoc_prop'('ptoc_w:tak'/0, [indexed = false]).

tak :-
        nl,
	no, !.
tak.

no :- fail.
