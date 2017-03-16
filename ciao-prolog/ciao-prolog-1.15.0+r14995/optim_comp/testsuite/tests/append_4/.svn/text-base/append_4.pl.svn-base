:- module(_, [], [fsyntax]).

:- '$pragma'(analyze_all).

:- export(t/1).
:- '$props'(t/1, [impnat = ptoc]).
t(Ys) :- % on exit Ys is a reclist
	li(Xs),
	Xs = [_|_],
	Ys = Xs.

:- export(p/1).
:- '$props'(p/1, [impnat = ptoc]).
p(Zs) :-
	li(As),
	As = [],
	Zs = As.

:- '$props'(li/1, [impnat = ptoc]).
li([]).
li([_|Xs]) :- li(Xs).
	
