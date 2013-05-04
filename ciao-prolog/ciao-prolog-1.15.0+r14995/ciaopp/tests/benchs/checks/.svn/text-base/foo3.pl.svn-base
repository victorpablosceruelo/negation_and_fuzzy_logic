:- module(foo3,[p/0],[]).

:- use_package(assertions).

p:-
	q(_X,_Y).

% this assertion should be checked

:- check success q(X,Y): ground(X) => ground(Y).

q(_,a).
