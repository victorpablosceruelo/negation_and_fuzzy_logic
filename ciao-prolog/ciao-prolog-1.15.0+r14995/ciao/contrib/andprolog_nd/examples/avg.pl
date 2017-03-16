:- module(avg,
	[
	    avg/2
	], []).

:- use_module(library(lists)).

add_elements([N],N) :- !.
add_elements([H|R],N1) :- 
	add_elements(R,N2),
	N1 is N2 + H.

avg(L,Av) :-
	length(L,N),
	N > 0,
	add_elements(L,S),
	Av is S / N.