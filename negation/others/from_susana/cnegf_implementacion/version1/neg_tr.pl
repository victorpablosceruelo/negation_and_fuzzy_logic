%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Module for being loaded by deneg to substitute negation calls
% for calls to naf with the delay of the variables involved 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%:- module(neg_tr,_,[]).

:- module(neg_tr,[out_delay/3,
		  in_entry/3,
		  in_cnegf/3],[]).

:- use_module(library(terms_vars),[varset/2]).
:- use_module(library(aggregates),[findall/3]).
:- use_module(library(lists),[append/3]).
:- use_module(library(idlists),[memberchk/2]).
%:- use_module(pplib(database),[db_get/1,trust_complexity/9]).

:- use_module(pplib(database), [db_get/1,trust_complexity/9]).


%%%%%%%%%%%%%%% OUT_DELAY %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% out_delay(Term,TermList,Module) sustitutes Term in the program
% Module if it is a call to \+/1 with a delay for a call to neg/1
out_delay(end_of_file, end_of_file, _):- 
	!.
out_delay(when(_Cond,\+ call(Pred)), neg(Pred), _):-
	!.

%%%%%%%%%%%%%%% IN_ENTRY %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% in_entry(Clause,SentList,Module) substitute all neg/1 calls
% for when/2 calls that delay the \+/1 call till variables
% of the head of the predicate where is the neg/1 goal are ground
in_entry(end_of_file,[end_of_file],_Module):-
	!.
in_entry((H :- B), [(H:-B1)|SentList],_Module):-
	get_entries(B,B1,H,SentList).

% get_entries(Goal,Goal1,H,List) returns in List the list of 
% sentences that is going to include in the expanded program
% for the aparition of the subgoal Goal in a clause of head H
% and substitute Goal for Goal1 in the clause.	
get_entries((A,B),(A1,B1),H,List):-
	!,
	get_entries(A,A1,H,List1),
	get_entries(B,B1,H,List2),
	append(List1,List2,List).

get_entries((A,B),(A1;B1),H,List):-
	!,
	get_entries(A,A1,H,List1),
	get_entries(B,B1,H,List2),
	append(List1,List2,List).
get_entries(neg(Pred),Goal,H,List):-
	!,
	varset(Pred,VarsPred),
	varset(H,VarsHead),
	intersectionchk(VarsPred,VarsHead,Vars),
	(Vars=[] ->
	    Goal= (\+ call(Pred)),
	    List=[]
	;
	    get_condition(Vars,Condition),
	    Goal= when(Condition,\+ call(Pred)),
	    assert_variables(Vars,H,List)
	).
get_entries(B,B,_H,[]).

% assert_variables(LV,Pred,LS) returns in LS a list of entry
% facts, one for each variable of LV for the predicate Pred 
assert_variables([],_Pred,[]).
assert_variables([Var|List],Pred,[Entry|List1]):-
	Entry = :-(entry(:(Pred,var(Var)))),
	assert_variables(List,Pred,List1).

% get_condition(Vars,Condition) returns in Condition the conjuction of
% the terms ground(X) for all elements of the list of variables Vars
get_condition([X],ground(X)).
get_condition([X,Y|Vars],(ground(X),Condition)):-
	get_condition([Y|Vars],Condition).

% intersectionchk(L1,L2,L3) returns in L3 the variables from L1 that
% appears in L2. L1,L2, and L3 are lists of variables
intersectionchk([],_Vars2,[]).
intersectionchk([Var|Vars1],Vars2,[Var|Vars]):-
	memberchk(Var,Vars2),
	!,
	intersectionchk(Vars1,Vars2,Vars).
intersectionchk([_Var|Vars1],Vars2,Vars):-
	intersectionchk(Vars1,Vars2,Vars).


%%%%%%%%%%%%%%% IN_CNEGF %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% in_cnegf(Clause,SentList,Module) substitute all neg(G) calls
% calls for cnegf(G) calls when the analyses determine that G
% is going to have a finite number of solutions
in_cnegf(end_of_file,[end_of_file],_Module):-
	!.
in_cnegf((H :- B), [(H:-B1)],_Module):-
	finsol(B,B1).

% finsol(Goal,Goal1,H) returns in Goal1 the same term that in Goal
% except if Goal is "neg(Pred)" and Pred is a predicate with a finite
% number of solutions.	
finsol((A,B),(A1,B1)):-
	!,
	finsol(A,A1),
	finsol(B,B1).
finsol((A,B),(A1;B1)):-
	!,
	finsol(A,A1),
	finsol(B,B1).
finsol(neg(Pred),Goal):-
	functor(Pred,F,A),
	finite_solutions(F/A),
	!, 
	Goal= (cnegf(Pred)).
finsol(B,B).


% finsol(Goal,Goal1,H) returns in List the list of 
% sentences that is going to include in the expanded program
% for the aparition of the subgoal Goal in a clause of head H
% and substitute Goal for Goal1 in the clause.	
% finsol((A,B),(A1,B1),H,List):-
% 	!,
% 	finsol(A,A1,H,List1),
% 	finsol(B,B1,H,List2),
% 	append(List1,List2,List).

% finsol((A,B),(A1;B1),H,List):-
% 	!,
% 	finsol(A,A1,H,List1),
% 	finsol(B,B1,H,List2),
% 	append(List1,List2,List).

% finsol(neg(Pred),Goal,_H,List):-
% %	analysis_finsol(Pred),
% 	functor(Pred,F,A),
% 	finite_solutions(F/A),
% 	!, 
% 	Goal= (cnegf(Pred)),
% 	List=[:-(use_module(neg))].
% finsol(B,B,_H,[]).


% analysis_finsol(Pred) successes if Pred has a finite number
% of solutions. It gets this information from two analysis:
% non_failure and finite cost. If Pred doesn't fail and its
% cost is not infinite then it has a finite number of solutions
analysis_finsol(Pred):-
        functor(Pred,F,A),
	db_get(trust_nonfail(Pred,_IntTypes,_OutTypes,
	                          FailInfo,_CoverInfo)),
	FailInfo = not_fail,			  
        trust_complexity(F/A,_Mode,Measure,_Mutex,Solution_Det,  
                              _Size,_Relation,Time,_Domain),
	Inf is 100,
	(Measure < Inf; Time < Inf; Solution_Det < Inf).

% Mientras esté esto asi todos salen que tienen finitas soluciones
db_get(trust_nonfail(_Pred,_IntTypes,_OutTypes,not_fail,_CoverInfo)).
trust_complexity(_F/_A,_Mode,3,_Mutex,2,_Size,_Relation,5,_Domain).




finite_solutions(F/A):-
	functor(Head0,F,A),
	db_get(trust_nonfail(Head0, _InTypes, _OuTypes, FailInfo, _CoverInfo)),
        trust_complexity(F/A, _Mode, _Measure, _Mutex, 
                        _Solution_Det, _Size, _Relation, Time, _Domain),
        FailInfo == not_fails,
        Time \== inf.










%%%%%%%%%%%%%%%%%%%%%%%%% END %%%%%%%%%%%%%%%%%%%%%%%%%%%%%






