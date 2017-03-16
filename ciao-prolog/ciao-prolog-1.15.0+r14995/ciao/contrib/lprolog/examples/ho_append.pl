:- module(ho_append,[main/0],[]).
:- use_package(lprolog).
:- use_module(library(write)).

%append nil K K.
ho_append(nil, K, K).
%append (X::L) K (X::M) :- append L K M.
ho_append((X::L), K, (X::M)) :- ho_append(L, K, M).
%ho_append((X::L), K, (X::M)).

main :-
	ho_append((x::nil), (a::nil), F),
	%pi x \ ho_append((x::nil), (a::nil), F@x),
	display_ref(F), nl,
	hnorm(F, FNorm),
	display_ref(FNorm), nl.
