:- module(ho_map3,[main/0],[]).
:- use_package(lprolog).
:- use_module(library(write)).

mappred(_P, nil, nil).
mappred(P, (X::L1), (Y::L2)) :- 
	P(X, Y),
	mappred(P, L1, L2).

parent(alpha, beta).
parent(beta, gamma).
parent(able, baker).
parent(baker, charlie).

main :-
	mappred(sigma x \ sigma y \ sigma z \ (parent(x,z) & parent(z,y)), (alpha::able::nil), F),
	display_ref(F), nl,
	hnorm(F, FNorm),
	display_ref(FNorm), nl.
