:- module(_,_,[]).

:- use_module(mi1).
:- use_module(mi2).

:- initialization((this_module(M), display(M), nl)).

main(X) :- display(X).
