:- module(_,[p/0],[]).

:- use_module(library(streams)).


p:-
%	open_null_stream(X),
	q(X,Y),
	open_null_stream(X).

q(X,X).
