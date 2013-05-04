:- module(foo2,[p/0],[]).

:- use_package(assertions).

p:-
%	_X=f(a),
	q(_X).

:- check calls q(X): ground(X).

q(_).
