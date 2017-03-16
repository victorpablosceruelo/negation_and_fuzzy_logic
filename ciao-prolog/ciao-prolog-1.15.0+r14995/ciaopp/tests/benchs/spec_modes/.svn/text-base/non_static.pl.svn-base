:- module(non_static,[p/1,q/1,r/1],[]).

p(X):-
	q(X),
	s(X).
%	r(X).

:- dynamic q/1.
:- data s/1.

:- impl_defined(r/1).

