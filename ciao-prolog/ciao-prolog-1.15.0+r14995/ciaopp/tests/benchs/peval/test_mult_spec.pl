:- module(_,[main/0],[assertions,regtypes]).

:- use_module(test_modular_spec).

main:-
	L = 3,
	q(L),
	p([2]),
	p([L]),
	q(_W).

q(X):-
	r(X), %mylist(X),
	p([X]).
%% q(X):-
%% 	mylist(X).
	
	
r(_X).
r(X):- r(X).

:- regtype mylist/1.

%mylist := [] | [_|~mylist].

mylist([]).
mylist([_|T]):-
	mylist(T).
