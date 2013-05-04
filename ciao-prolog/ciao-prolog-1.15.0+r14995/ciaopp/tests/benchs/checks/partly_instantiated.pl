:- module(_,[p/0],[assertions,regtypes]).

p:-
	X = [],
	q([_|X]).

:- regtype mylist/1.

mylist([]).
mylist([_|T]):-
	mylist(T).

:- check calls q(X): mylist(X).

q(_).
