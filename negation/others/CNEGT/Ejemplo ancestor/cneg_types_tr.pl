%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Module for being loaded by cneg_types to add store_clauses/2 %
% facts to flatten the structure of facts and clauses of a     %
% program that contains cneg/1 calls to be able to aply the    %
% try/2 technique for the constructive negation of the goals.  %
% It also adds new predicates with a name composed by the      %
% name of the existing original predicates and the sufix       %
% '_check_types'                                               %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(cneg_types_tr,[flat_cpred/3],[default]).
% 'default', in the third argument of the module declaration,
% let use default predicates like append/3

:- use_module(library(aggregates),[findall/3, findall/4]).

% dynamic predicates to store clauses that are going to be
% expanded. It is used to expand them in a continous way
:- data pre_clause/2.
:- data pre_pred/1.
:- data pre_stored/1.


% free_list(Num,VarList). VarList is a list with a number of
% 'Num' free variables
free_list(0,[]):-
        !.
free_list(N,[_|L]):-
	N1 is N-1,
	free_list(N1,L).


% change_name(OriginalName,TransfName). TransfName is
% the same as OriginalName but concatenated to the 
% sufix '_check_types'
change_name(NamePred,Name_inst) :-
	atom_concat(NamePred,'_check_types',Name_inst).


% conj_to_list(Conj,List) provides in List the elements of
% the conjunction Conj
conj_to_list((A,B),[A|ListB]):-
	!,
	conj_to_list(B,ListB).
conj_to_list(A,[A]).


% flat_cpred(Sentence,SentList,Module) sustitutes Sentence in 
% the program Module that is being compilated by the list of 
% sentences SentList.
flat_cpred(end_of_file,[:-(use_module(dist))|RES],_):-
	!,
	findall(CL,(retract_fact(pre_stored(CL))),ST_CL),
	findall(PR,(retract_fact(pre_pred(PR))),Preds),
	walk_list(Preds, Cls),
	append(ST_CL,Cls,RES).
flat_cpred((:- success(Pred)), [],_):- 
	arg(1,Pred,Arg),
	% this Arg is really another predicate
	arg(2,Pred,Types),
	functor(Arg,Name,_Arity),
	Arg=..[Name|ListaArgs],
	change_name(Name,Name_inst),
	Predinst=..[Name_inst|ListaArgs],
	retractall_fact(pre_pred(Predinst)),
	retractall_fact(pre_clause(Predinst,_)),
	assertz_fact(pre_pred(temp)),
	assertz_fact(pre_clause(temp,(:- success(Pred)))),
	assertz_fact(pre_pred(Predinst)),
	assertz_fact(pre_clause(Predinst,(Predinst:-Types))).
flat_cpred((:- (Body)), [],_):-
	assertz_fact(pre_pred(temp)),
	assertz_fact(pre_clause(temp,(:- (Body)))).
flat_cpred((Head :- Body), [],_):-
	!,
	functor(Head,Name,Arity),
	free_list(Arity,ListArgs),
	Pred=..[Name|ListArgs],
	change_name(Name,Name_inst),
	Predinst=..[Name_inst|ListArgs],
	assertz_fact(pre_pred(Pred)),
	assertz_fact(pre_clause(Pred,(Head :- Body))),
	conj_to_list(Body,ListBody),
	assertz_fact(pre_stored(stored_clause(Head,ListBody))),
	assertz_fact(pre_pred(Predinst)),
	assertz_fact(pre_clause(Predinst,Predinst)),
	assertz_fact(pre_stored(stored_pred(Pred,Pred))).
flat_cpred(Fact, [], _):-
	Fact\==0, % for avoiding to add store_clause(0,[])
	functor(Fact,Name,Arity),
	free_list(Arity,ListArgs),
	Pred=..[Name|ListArgs],
	change_name(Name,Name_inst),
	Predinst=..[Name_inst|ListArgs],
	assertz_fact(pre_pred(Pred)),
	assertz_fact(pre_clause(Pred,Fact)),
	assertz_fact(pre_stored(stored_clause(Fact,[]))),
	assertz_fact(pre_pred(Predinst)),
	assertz_fact(pre_clause(Predinst,Predinst)),
	assertz_fact(pre_stored(stored_pred(Pred,Pred))).
flat_cpred(0, [],[]).

% walk_list(List,Result) returns in Result the list of 
% continuos clauses that is going to include in the 
% expanded program for the aparition of each predicate
% in the list 'List'. Moreover, it includes the stored_
% pred for 'dist'
walk_list([],[stored_pred('dist:dist'(X,Y),dist(X,Y)),end_of_file]).
walk_list([Pred|T],Cls):-
	findall(CL,(retract_fact(pre_clause(Pred,CL))),Cls,RT),
        walk_list(T,RT).
