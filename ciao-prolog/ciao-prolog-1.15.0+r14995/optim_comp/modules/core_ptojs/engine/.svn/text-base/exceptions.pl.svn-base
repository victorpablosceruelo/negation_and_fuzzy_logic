:- module(exceptions, [], [oo_syntax, hiord, assertions, fsyntax, mutables, string_type]).

% TODO: not implemented (catch and intercept are synonyms of 'call' here)

:- export(catch/3).
catch(Goal, _Error, _Handler) :-
	call(Goal).

:- export(intercept/3).
intercept(Goal, _Error, _Handler) :-
	call(Goal).

:- export(throw/1).
throw(_) :- '$nodef'.

:- export(halt/0).
halt :- '$nodef'.

:- export(halt/1).
halt(_) :- '$nodef'.

:- export(abort/0).
abort(_) :- '$nodef'.

