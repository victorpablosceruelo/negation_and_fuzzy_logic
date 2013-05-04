
:- module(inc_all,_,[assertions]).

:- entry inc_all(A,B) : (list(A,num), ground(A), var(B)).

inc_all([],[]).
inc_all([H|T],[NH|NT]) :- NH is H+1, inc_all(T,NT).
