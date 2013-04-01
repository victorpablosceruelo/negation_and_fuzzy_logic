%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Module for being loaded by cneg to add store_clauses/2 facts
% to flatten the structure of facts and clauses of a program
% that contains cneg/1 calls to be able to aply the try/2 
% technique for the constructive negation of the goals.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(cneg_tr,[flat_dpred/3,flat_cpred/3,conj_to_list/2],[]).

:- use_module(library(aggregates),[findall/4]).

% dynamic predicates to store clauses that are going to be
% expanded. It is used to expand them in a continous way
:- data pre_stored/1.
:- data pre_clause/1.

% flat_dpred(Sentence,SentList,Module) sustitutes Sentence in 
% the program Module that is being compilated by the list of 
% sentences SentList. The result clauses are discontinous
flat_dpred(end_of_file,[end_of_file],_):- 
	!.
flat_dpred((Head :- Body), [(Head :- Body),(stored_clause(Head,ListBody))],_):-
	!,
	conj_to_list(Body,ListBody).
flat_dpred(Fact, [(stored_clause(Fact,[]))],_).
	
% conj_to_list(Conj,List) provides in List the elements of
% the conjunction Conj
conj_to_list((A,B),[A|ListB]):-
	!,
	conj_to_list(B,ListB).
conj_to_list(A,[A]).	

% flat_cpred(Sentence,SentList,Module) sustitutes Sentence in 
% the program Module that is being compilated by the list of 
% sentences SentList. The result clauses are continous
flat_cpred(end_of_file,Cls,_):- 
	!,
	findall(CL,(retract_fact(pre_clause(CL))),Cls,Clauses),
	findall(CL,(retract_fact(pre_stored(CL))),Clauses,[end_of_file]).
flat_cpred((Head :- Body), [], Module):-
	!,
	assertz_fact(pre_clause((Head :- Body))),
	conj_to_list(Body,ListBody),
	assertz_fact(pre_stored(stored_clause('Module': Head,ListBody))).
flat_cpred(Fact, [], _):-
	assertz_fact(pre_clause(Fact)),
	assertz_fact(pre_stored(stored_clause(Module':' Fact,[]))).
