:- module(_,[ll1/1],[]).

:- use_module(.(ll2)).

:- initialization(ll2(b)).

ll1(X) :- ll2(X).
