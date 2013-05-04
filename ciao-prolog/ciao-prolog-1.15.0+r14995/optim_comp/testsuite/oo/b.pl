:- module(_, [], []).

:- reexport(.(a)).

%:- export(bf/2).
%
%bf(X,Y) :-
%	X/b:f(Y).
:- use_module(.(b)).

%:- export(bg/1).

%bg(X) :-
%	b:g(X).

:- export(bg/1).
bg(X) :-
	b:g(X).

:- export(rbg/1).
rbg(X) :-
	M = b,
	M:g(X).
