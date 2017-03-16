:- module(ho_map2,[main/0],[]).
:- use_package(lprolog).
:- use_module(library(write)).

mappred(_P, nil, nil).
mappred(P, (X::L1), (Y::L2)) :- 
	P(X, Y),
	mappred(P, L1, L2).

age(john, 20).
age(suzy, 22).

main :-
	mappred(sigma x \ sigma y \ age(y,x), (20::22::nil), F),
	%pi x \ ho_append((x::nil), (a::nil), F@x),
	display_ref(F), nl,
	hnorm(F, FNorm),
	display_ref(FNorm), nl.
