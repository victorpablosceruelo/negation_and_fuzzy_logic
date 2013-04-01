:- use_module(library(aggregates),[setof/3]).

disy_to_conj(Ld,Lc):-
	setof(D,obtain_disy(Ld,D),Lc).

obtain_disy([],[]).
obtain_disy([Conj|L],[X|Disy]):-
	member(X,Conj),
	obtain_disy(L,Disy).

%Ciao-Prolog 1.6 #3: Sun Nov 12 11:31:04 CET 2000
%?- ensure_loaded('/home/susana/tesis/micodigo/ciao/cneg/version8/disy_to_conj.pl').

%yes
%?- disy_to_conj([[a,e,i,o,u],[1,2]],L).

%L = [[a,1],[a,2],[e,1],[e,2],[i,1],[i,2],[o,1],[o,2],[u,1],[u,2]] ? ;

%no
%?- disy_to_conj([[a,e,i,o,u],[1,2],[A,B,C]],L).

%L = [[a,1,A],[a,1,B],[a,1,C],[a,2,A],[a,2,B],[a,2,C],[e,1,A],[e,1,B],[e,1,C],[e,2,A],[e,2,B],[e,2,C],[i,1,A],[i,1,B],[i,1,C],[i,2,A],[i,2,B],[i,2,C],[o,1,A],[o,1,B],[o,1,C],[o,2,A],[o,2,B],[o,2,C],[u,1,A],[u,1,B],[u,1,C],[u,2,A],[u,2,B],[u,2,C]] ? ;

no
?- 