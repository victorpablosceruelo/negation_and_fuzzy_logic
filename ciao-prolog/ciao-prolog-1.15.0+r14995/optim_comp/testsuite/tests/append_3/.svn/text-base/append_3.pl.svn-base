:- module(_, [], [fsyntax]).

:- '$pragma'(analyze_all).

%:- export(q/1).
% TODO: define $export_entry
% TODO: the entry is moved to top, why?
:- '$trust_entry'(q/1, sht, [(var;fnc(f/1,[float]))]).
:- '$props'(q/1, [impnat = ptoc]).
q(X) :-
	'$trust_type'(X, (var;fnc(f/1,[float]))),
	X = f(a).
