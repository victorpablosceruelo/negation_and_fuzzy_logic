:- module( _, [main/4], [assertions] ).

:- use_module(library(write), [write/1]).
:- use_module(library(lists), [length/2]).

main(L1,L2,R1,R2):-
	write(hello),
	all_lengths([[a,b]|L1],R1),
	all_lengths([[b,a]|L2],R2).


all_lengths([],[]).
all_lengths([L|Ls],[R|Rs]):-
	length(L,R),
	all_lengths(Ls,Rs).
