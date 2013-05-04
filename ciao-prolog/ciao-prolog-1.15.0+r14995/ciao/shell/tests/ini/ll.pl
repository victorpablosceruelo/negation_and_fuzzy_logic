:- module(_,[main/1],[]).

:- use_module(.(ll1)).

:- initialization(ll1(a)).

main(X) :- display(X).
