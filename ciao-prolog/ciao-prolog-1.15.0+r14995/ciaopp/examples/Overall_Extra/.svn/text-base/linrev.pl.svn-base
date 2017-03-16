:- module(linrev, [reverse/2], [assertions]).

:- use_module(library(assertions(native_props))).

:- entry reverse(A,B) : {ground, list} * var.

:- check comp reverse(A,B) + steps_ub(1+length(A)) .


reverse(L,K) :-
        rev(L,[],K).

:- entry rev/3 : {ground, list} * ground * var.

rev([],L,L).
rev([H|T],L,K) :-
        rev(T,[H|L],K).
