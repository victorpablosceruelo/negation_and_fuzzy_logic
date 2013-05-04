:- module(foo,[p/0],[assertions,regtypes]).

p:-
%	X=f(a),
	q(X).

:- regtype mylist/1.

mylist([]).
mylist([_|T]):-
	mylist(T).

:- check calls q(X): mylist(X).

q(_).
