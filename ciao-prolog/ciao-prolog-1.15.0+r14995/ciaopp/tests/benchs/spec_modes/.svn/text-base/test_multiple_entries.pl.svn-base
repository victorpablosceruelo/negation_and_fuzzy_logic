:- module(_,[p/1,h/1],[assertions]).

:- entry p(X) : ground(X).

:- entry p(X) : var(X).
 
:- entry p(X).

p(X):-
	ground(X),
	q(X).
p(X):-
	var(X),
	r(X).

q(a).
q(a).

r(b).
r(b).

:- entry h(X) : ground(X).

:- entry h(X) : var(X).
 
:- entry h(X).

h(X):-
	ground(X),
	q(X).
h(X):-
	var(X),
	r(X).

