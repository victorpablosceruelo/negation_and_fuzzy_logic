:- module(_, [append/3], [assertions]).

:- entry append(A,B,C) : list(gnd) * list(gnd) * var.

append([],X,X).
append([H|X],Y,[H|Z]):- append(X,Y,Z).
