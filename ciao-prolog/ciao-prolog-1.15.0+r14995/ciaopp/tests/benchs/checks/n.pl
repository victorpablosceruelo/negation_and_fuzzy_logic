
:- module(n,[nn/1,nnn/1],[assertions]).

:- calls nn(X) : num(X).
:- trust success nn(X) => true.

nn(_).

:- calls nnn(X) : ground(X).
:- trust success nnn(X) => ( var(X), num(X) ).

nnn(_).

