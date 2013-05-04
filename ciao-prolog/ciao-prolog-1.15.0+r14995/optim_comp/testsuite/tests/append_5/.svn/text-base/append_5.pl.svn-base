:- module(_, [], [fsyntax]).

:- '$pragma'(analyze_all).

:- export(extract_paths/0).
:- '$props'(extract_paths/0, [impnat = ptoc]).
extract_paths :-
        extract_path([1], _Cs).

:- '$props'(extract_path/2, [impnat = ptoc]).
extract_path(_, a).
extract_path(_, B) :-
	extract_path([], B).

