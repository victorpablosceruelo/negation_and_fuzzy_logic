:- module(_,[p/2],[]).

p(X,Y):- 
	produce1(X),
	to_filter(X),
	produce2(Y),
	to_filter(Y).

produce1(a).
produce1(f(X)):-
	produce1(X).
%% produce1(g(X)):-
%% 	produce1(X).

produce2(b).
produce2(g(X)):-
	produce2(X).
%% produce2(f(X)):-
%% 	produce2(X).
	
to_filter(a).
to_filter(b).
to_filter(c).
to_filter(X):- X=d.
to_filter(f(X)):-
	to_filter(X).
to_filter(g(X)):-
	to_filter(X).
