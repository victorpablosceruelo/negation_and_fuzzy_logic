:- module(_, [], []).

:- export(f/1).
:- '$context'(f/1, instance).
f(X) :-
	'$this'(This),
	X = This.

:- export(g/1).
:- '$context'(f/1, none).
g(X) :-
	'$this'(This),
	X = This.
