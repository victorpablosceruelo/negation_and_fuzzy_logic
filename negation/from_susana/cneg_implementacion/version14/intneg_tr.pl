%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Module for being loaded by intneg to add the complemented
% predicates to the predicates of the module that is being
% compiled. It uses intensional negation to obtain them.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(intneg_tr,[comp_pred/3],[]).

:- use_module(library(aggregates),[findall/4]).
%:- use_module(library(term_basic),[functor/3]).

% dynamic predicates to store clauses that are going to be
% expanded. It is used to expand them in a continous way
:- data pre_intneg/1.
:- data pre_pred/1.

% flat_dpred(Sentence,SentList,Module) sustitutes Sentence in 
% the program Module that is being compilated by the list of 
% sentences SentList. The result clauses are discontinous.
% It is not expanded the predicate stored_pred/2 to obtain
% the non meta_term of the name of a predicate.
flat_dpred(end_of_file,[end_of_file],_):- 
	!.
flat_dpred((Head :- Body), [(Head :- Body),(stored_clause(Head,ListBody))],_):-
	!,
	conj_to_list(Body,ListBody).
flat_dpred(Fact, [(stored_clause(Fact,[]))],_).

% free_list(N,L) returns a list L of N free variables
free_list(0,[]).
free_list(N,[_|L]):-
	N1 is N-1,
	free_list(N1,L).

% conj_to_list(Conj,List) provides in List the elements of
% the conjunction Conj
conj_to_list((A,B),[A|ListB]):-
	!,
	conj_to_list(B,ListB).
conj_to_list(A,[A]).	

% comp_pred(Sentence,SentList,Module) sustitutes Sentence in 
% the program Module that is being compilated by the list of 
% sentences SentList. The result clauses are continous
comp_pred(end_of_file,Cls,_):- 
	!,
	findall(CL,(retract_fact(pre_intneg(CL))),Cls,Clauses),
	findall(CL,(retract_fact(pre_pred(CL))),Clauses,[end_of_file]).
comp_pred((Head :- Body), [],_):-
	!,
	assert_pred(Head),
	assertz_fact(pre_intneg((intneg(Head) :- Body))).
comp_pred(Fact, [], _):-
	assert_pred(Fact),
	assertz_fact(pre_intneg(intneg(Fact))).

% assert_pred(Head) asserts the fact pre_pred(Pred,Pred) if it is not
% asserted yet where Pred is Head with free vars in the arguments
assert_pred(Head):-
	functor(Head,Name,Arity),
	free_list(Arity,Args),
	Pred=..[Name|Args],
	\+ pre_pred(Pred),!,
	assertz_fact(pre_pred(stored_pred(Pred,Pred))).
assert_pred(_Head).

% !!!!!!!!!!!!!!!!!!!!
% Con esto del assert_pred/1 intentaba que no se insertara mas que una
% vez por predicado pero no lo consigo, lo inserta tantas veces como
% clausulas y hechos haya y es porque no se puede consultar normalmente
% el predicado












