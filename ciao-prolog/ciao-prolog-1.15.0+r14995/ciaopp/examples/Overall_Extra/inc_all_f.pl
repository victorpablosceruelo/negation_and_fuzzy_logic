:- module(inc_all_f,_,[fsyntax,assertions]).

:- entry inc_all(A,B) : (list(A,num), ground(A), var(B)).

inc_all([])    := [].
inc_all([H|T]) := [ aux(H) | ~inc_all(T) ].

aux(H) := H+1.
