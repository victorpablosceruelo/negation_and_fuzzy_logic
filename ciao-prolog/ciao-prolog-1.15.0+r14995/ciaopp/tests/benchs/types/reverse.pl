:- module(reverse, [reverse/2], [assertions]).
%:- use_module(engine(basic_props),[list/2,gnd/1]).
%:- use_module(library(assertions(native_props)),[var/1]).

:- entry reverse(A,B) : list(gnd) * var.

reverse(L,K) :-
        rev(L,[],K).

rev([],L,L).
rev([H|T],L,K) :-
        rev(T,[H|L],K).
