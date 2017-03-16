% 1998/11/05, Pedro Lopez Garcia
% created to test the treatement of is/2.

:- module(istest, [p/3], [assertions]).

:- entry p/3 : num * num * var.

p(X, Y, Z):-
   X is Y + 2,
   Z is X + 1.
p(X, Y, Z):-
   Z is X + Y.
