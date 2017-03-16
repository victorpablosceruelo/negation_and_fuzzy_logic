:-module(plus,[plus/3]).

plus(X,Y,Z):-
	ground(X), ground(Y), !, Z is X + Y.
plus(X,Y,Z):-
	ground(Y), ground(Z), !, X is Z - Y.
plus(X,Y,Z):-
	ground(X), ground(Z), !, Y is Z - X.

