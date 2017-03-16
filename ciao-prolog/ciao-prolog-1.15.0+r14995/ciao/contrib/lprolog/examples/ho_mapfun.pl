:- module(ho_mapfun,[main/0],[]).
:- use_package(lprolog).
:- use_module(library(write)).


mapfun(F, nil, nil).
mapfun(F, (X::L1), (F @ X::L2)) :- 
	mapfun(F, L1, L2).

main :-
	pi a \ pi b \mapfun(F, a::b::nil, h(c,a)::h(c,b)::nil),
	%pi a \ pi b \ mapfun(F, a::b::nil, h(a)::h(b)::nil),
	%pi a \ (F(a) = g(a,a)),
	display_ref(F), nl.
