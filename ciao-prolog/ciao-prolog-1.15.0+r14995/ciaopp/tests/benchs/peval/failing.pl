:- module(_,[p/0],[]).

:- use_module(library(dynamic)).

:- dynamic fact/1.

p:- 
	produce1(X),
	to_filter(X),
	produce2(Y),
%	assert(fact(Y)),
	var(0),
	to_filter(Y).

produce1(a).
produce1(f(X)):-
	produce1(X).
%% produce1(g(X)):-
%% 	produce1(X).

produce2(c).
produce2(h(X)):-
	produce2(X).

	
to_filter(a).
to_filter(b).
to_filter(f(X)):-
	to_filter(X).
to_filter(g(X)):-
	to_filter(X).
