:- module(_,_,[]).

:- use_module(library(write)).

pf(F,V) :- call(current_prolog_flag, F, V).
c(P, X) :- P(X).
