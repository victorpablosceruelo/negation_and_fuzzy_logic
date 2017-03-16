:- module(ho_reverse2,[main/0],[]).
:- use_package(lprolog).
:- use_module(library(write)).

reverse(LL1, LL2) :-
pi rev_aux \ (rev_aux(nil, L, L)  & (rev_aux(X::L1, L2, L3) :-  rev_aux(L1, L2, X::L3))) => rev_aux(LL1, LL2, nil).

main :-
	reverse((a::b::nil), F),
	display_ref(F), nl,
	hnorm(F, FNorm),
	display_ref(FNorm), nl.
