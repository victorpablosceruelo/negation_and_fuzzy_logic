:- use_module(library(aggregates),[setof/3]).

disy_to_conj(Ld,Lc):-
	setof(D,obtain_disy(Ld,D),Lc).

obtain_disy([],[]).
obtain_disy([Conj|L],[X|Disy]):-
	member(X,Conj),
	obtain_disy(L,Disy).
