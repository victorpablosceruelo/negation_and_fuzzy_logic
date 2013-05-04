:- module(labeling, 
	[
	    do/0
	]).

:- use_package(gecode).
:- use_module(library(lists), [length/2]).

do :-
	N = 7,
        length(Vars, N),
	Vars in 1 .. N,
	labeling(Vars).