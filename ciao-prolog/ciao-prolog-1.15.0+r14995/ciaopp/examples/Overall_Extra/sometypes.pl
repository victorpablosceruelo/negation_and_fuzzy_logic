:- module(_,_,[assertions,regtypes]).

:- regtype color/1. 

color(red).
color(green).
color(blue).

:- regtype natlist/1.

natlist([]).
natlist([H|T]) :-
	nat(H),
	natlist(T).

:- regtype nat/1.

nat(0).
nat(s(N)) :- nat(N).
