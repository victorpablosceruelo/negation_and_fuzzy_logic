%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Module for being loaded by cneg1 to add store_clauses/2 facts
% to flatten the structure of facts and clauses of a program
% that contains cneg/1 calls to be able to aply the try/2 
% technique for the constructive negation of the goals.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(cneg_tr,[trans_sent/3, trans_goal/3]).

%:- use_module(library(engine(data_facts)),[retract_fact/1]).
:- use_module(cneg_diseq,[cneg_diseq/3, cneg_eq/2]).
:- use_module(cneg_lib).
:- use_module(cneg_aux).

% dynamic predicate(s) 
:- dynamic cneg_processed_predicates/1.
:- dynamic cneg_dynamic_cls/1.
:- dynamic cneg_static_cls/1.

trans_clause(Whatever, Whatever, _) :-
	debug('trans_cl', trans_cl(Whatever)).

trans_goal(Whatever, Whatever, _).

% trans_sent(Sentence,SentList,Module) sustitutes Sentence in 
% the program Module that is being compilated by the list of 
% sentences SentList. The result clauses are continous
trans_sent(Input, Output, SourceFileName) :-
	debug('trans_sent :: Input', (Input)), 
	debug('trans_sent :: SourceFileName', (SourceFileName)), 
	trans_sent_aux(Input, Output, SourceFileName),
	debug('trans_sent :: Output', (Output)), 
	!.

trans_sent(Input, _Output, _SourceFileName) :-
	msg('ERROR :: Impossible to translate', (Input)), !.

trans_sent_aux(X, [], _SourceFileName):- 
	var(X), !, fail.

% (:- include(dist, _)), (:- include(cneg_lib)), 
trans_sent_aux(end_of_file,ClsFinal, SourceFileName):- !,
	trans_sent_eof(ClsFinal, SourceFileName).

trans_sent_aux(0, [], _SourceFileName) :- !.

% Do not modify module imports, declarations and so on.
trans_sent_aux((:- Whatever),[(:- Whatever)],_):- !.
%	msg('Warning: cneg does not work for imported predicates unless cneg\'s package is imported from each one. Problematic declaration:', Whatever).

% Aqui es donde da el warning porque no conoce a dist:dist aqui.
trans_sent_aux(Clause, Result, SourceFileName) :-
	functor_local(Clause, ':-', 2, _Arguments),
%	debug('trans_sent_aux', functor_local(Clause, Name, 2, _Arguments)),
	!,
	arg(1, Clause, Head),
	arg(2, Clause, Body),
%	debug('trans_sent_aux', trans_clause_with_body(Head, Body, Result, SourceFileName)),
	trans_clause_with_body(Head, Body, Result, SourceFileName).

trans_sent_aux(Clause, Result, SourceFileName) :-
	functor_local(Clause, Name, Arity, _Arguments),
	(
	 (
	  Name\==':-'
	 )
	;
	 (
	  Arity\==2
	 )
	), !,
	trans_clause_without_body(Clause, Result, SourceFileName).

trans_sent_aux(Clause, [], _SourceFileName) :-
	functor_local(Clause, Name, Arity, Arguments),
	debug('trans_sent_aux', functor_local(Clause, Name, Arity, Arguments)),
	!.

trans_clause_with_body(Head, Body, [Result], SourceFileName) :-
	trans_body(Body, NewBody, ListBodies),
%	debug('trans_clause_with_body', trans_body(Body, NewBody, ListBodies)),
	trans_head(Head, NewHeadName, NewHeadArity, NewHead),
%	debug('trans_clause_with_body', trans_head(Head, NewHeadName, NewHeadArity, NewHead)),
	process_and_save_info(NewHeadName, NewHeadArity, SourceFileName, NewHead, ListBodies),
	functor_local(Result, ':-', 2, _Arguments),
	arg(1, Result, NewHead),
	arg(2, Result, NewBody).

trans_clause_without_body(Fact, [(Fact)], SourceFileName):-
	Fact\==0, %% for avoiding to add store_clause(0,[])
	!, % No backtracking.
	trans_head(Fact, NewHeadName, NewHeadArity, NewFact),
	process_and_save_info(NewHeadName, NewHeadArity, SourceFileName, NewFact, [[]]).

process_and_save_info(Name, Arity, SourceFileName, Head, ListBodies) :-
	cneg_processed_pred_info(Name, Arity, SourceFileName, _Index, PP_Info),
	retrieve_list_of_processed_predicates(List_In),
	retrieve_processed_pred_info(PP_Info, List_In, List_Out),
	process_and_save_info_aux(ListBodies, Head, PP_Info, New_PP_Info),
	save_list_of_processed_predicates([New_PP_Info|List_Out]).

process_and_save_info_aux([], _Head, PP_Info, PP_Info) :- !.
process_and_save_info_aux([ListBody|ListBodies], Head, PP_Info, New_PP_Info) :- !,
	assert_information_clause_aux(ListBody, Head, PP_Info, Tmp_PP_Info),
	cneg_static_negation(Tmp_PP_Info, Head, ListBody),
	process_and_save_info_aux(ListBodies, Head, Tmp_PP_Info, New_PP_Info).


assert_information_clause_aux(ListBody, Head, PP_Info, New_PP_Info) :-
	cneg_processed_pred_info(Name, Arity, SourceFileName, Index, PP_Info),
	New_Index is Index +1,
	cneg_processed_pred_info(Name, Arity, SourceFileName, New_Index, New_PP_Info),
	generate_frontier_body(ListBody, FrontierBody),
	save_clause_info(New_PP_Info, Head, ListBody, FrontierBody),
	!.

save_clause_info(PP_Info, Head, ListBody, FrontierBody) :-
	cneg_processed_pred_info(Name, Arity, SourceFileName, _Index, PP_Info),
	assertz(cneg_dynamic_cls(cneg_dynamic_cl(Name, Arity, SourceFileName, Head, ListBody, FrontierBody))).

generate_frontier_body([], []) :- !.
generate_frontier_body([Predicate | ListBody], [Predicate | FrontierBody]) :-
	(
	    goal_is_equality(Predicate, _T1_1, _T2_1), ! 
	;
	    goal_is_disequality(Predicate, _T1_2, _T2_2, _FreeVars), !
	),
	generate_frontier_body(ListBody, FrontierBody).

generate_frontier_body([_Predicate | ListBody], FrontierBody) :-
	generate_frontier_body(ListBody, FrontierBody).

save_list_of_processed_predicates(List) :-
	assertz(cneg_processed_predicates(List)).

retrieve_list_of_processed_predicates(List) :-
	retract(cneg_processed_predicates(List)), !.
retrieve_list_of_processed_predicates([]).

retrieve_processed_pred_info(PP_Info, [], []) :- !,
	cneg_processed_pred_info(_Name, _Arity, _SourceFileName, 0, PP_Info), !.
retrieve_processed_pred_info(PP_Info, [Current|List_In], List_In) :-
	cneg_processed_pred_info(Name, Arity, SourceFileName, Index, PP_Info),
	cneg_processed_pred_info(Name, Arity, SourceFileName, Index, Current),
	!.
retrieve_processed_pred_info(PP_Info, [Current|List_In], [Current|List_Tmp]) :-
	retrieve_processed_pred_info(PP_Info, List_In, List_Tmp), !.

	
cneg_processed_pred_info(Name, Arity, SourceFileName, Index,
	cneg_processed_pred(Name, Arity, SourceFileName, Index)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

trans_head(Head, NewHeadName, HeadArity, NewHead) :-
	functor_local(Head, HeadName, HeadArity, HeadArgs),
	name(HeadName, HeadName_String),
	remove_qualification(HeadName_String, NewHeadName_String), !,
	name(NewHeadName, NewHeadName_String),
	functor_local(NewHead, NewHeadName, HeadArity, HeadArgs).

trans_head(Head, _NewHeadName, _HeadArity, Head) :-
	debug('trans_head :: FAILED conversion for', Head), !, fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

trans_sent_eof(ClsOut, _SourceFileName) :-
	retrieve_list_of_processed_predicates(List_Of_Predicates),
	cneg_impl(Cneg_Impl),
	append(List_Of_Predicates, Cneg_Impl, ClsTmp_1),
	findall(Cl,(retract(cneg_dynamic_cls(Cl))), ClsTmp_2, ClsTmp_1),
	findall(Cl,(retract(cneg_static_cls(Cl))), ClsOut, ClsTmp_2),
	!. %Backtracking forbiden.
%	nl, nl,
%	debug_list('ClsOut', ClsOut),
%	nl, nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

trans_body(Body_In, NewBody, ListBodies):-
%	debug('trans_body_aux :: IN', (Body_In)),
	trans_body_aux(Body_In, NewBody, [[]], ListBodies).
%	debug('trans_body_aux :: OUT', (Body_In, NewBody, [], ListBodies)).

trans_body_aux(Body_In, (Body_Out_A, Body_Out_B), Before, Result):-
	goal_is_conjunction(Body_In, Body_In_A, Body_In_B), !,
	trans_body_aux(Body_In_A, Body_Out_A, Before, Tmp), 
	trans_body_aux(Body_In_B, Body_Out_B, Tmp, Result).
trans_body_aux(Body_In, (Body_Out_A; Body_Out_B), Before, Result):-
	goal_is_disjunction(Body_In, Body_In_A, Body_In_B), !,
	trans_body_aux(Body_In_A, Body_Out_A, Before, Result_A), 
	trans_body_aux(Body_In_B, Body_Out_B, Before, Result_B),
	append(Result_A, Result_B, Result).
trans_body_aux(Body_In, Body_Out, Before, Result) :- !,
	change_conflictive_predicates(Body_In, Body_Out),
	join_previous_sols_with_curent_one(Body_Out, Before, Result). 

join_previous_sols_with_curent_one(_NewA, [], []) :- !.
join_previous_sols_with_curent_one(NewA, [[]], [[NewA]]) :- !.
join_previous_sols_with_curent_one(NewA, [Before], [[NewA|Before]]) :- !. 
join_previous_sols_with_curent_one(NewA, [Before|MoreBefore], [[NewA|Before]|MoreResult]) :- !,
	join_previous_sols_with_curent_one(NewA, MoreBefore, MoreResult).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

change_conflictive_predicates(A, A) :-
	var(A), !. % Variables remain unchanged.
change_conflictive_predicates(A, NewA) :-
	functor_local(A, Name, Arity, Args_In),
	change_conflictive_predicate(Name, NewName, Arity, NewArity, Args_In, Args_Aux), 
	change_conflictive_arguments(NewArity, Args_Aux, Args_Out),
	functor_local(NewA, NewName, NewArity, Args_Out).

change_conflictive_arguments(0, [], []) :- !.
change_conflictive_arguments(1, [Arg], [NewArg]) :- !,
	change_conflictive_predicates(Arg, NewArg).
change_conflictive_arguments(Arity, [Arg|Args], [NewArg|NewArgs]) :- !,
        NewArity is Arity-1,
	change_conflictive_predicates(Arg, NewArg),
	change_conflictive_arguments(NewArity, Args, NewArgs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%change_conflictive_predicate(',', ',', 2) :- 
%	msg('Error', 'Trying to change the name for functor --,--'), !, fail.
%change_conflictive_predicate(';', ';', 2) :- 
%	msg('Error', 'Trying to change the name for functor --;--'), !, fail.
change_conflictive_predicate('dist', 'cneg_diseq', 2, 3, Args_In, Args_Out) :- !, 
	add_empty_list_argument(Args_In, Args_Out).
change_conflictive_predicate('cneg_diseq', 'cneg_diseq', 2, 3, Args_In, Args_Out) :- !,
	add_empty_list_argument(Args_In, Args_Out).
change_conflictive_predicate('eq', 'cneg_eq', 2, 2, Args, Args) :- !.
change_conflictive_predicate(P, P, Arity, Arity, Args, Args) :- !.

add_empty_list_argument([], [[]]) :- !.
add_empty_list_argument([Arg|Args_In], [Arg|Args_Out]) :- !,
	add_empty_list_argument(Args_In, Args_Out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cneg_impl([
	(cneg(Predicate) :- cneg_aux(Predicate, [])),
	 (cneg_aux(Predicate, Universal_Vars) :- cneg_lib_aux(Predicate, Universal_Vars, Result), 
	  debug_nl, 
	  debug('cneg :: INPUT :: ', Predicate),
	  debug('cneg :: Universal Vars IN :: ', Universal_Vars),
	  debug('cneg :: OUTPUT :: ', call(Result)), 
	  debug_nl, 
	  !, call(Result)),
	  (cneg_static_predicate_call(_Goal, _SourceFileName, 0)),
	   (cneg_static_predicate_call(Goal, SourceFileName, Occurences) :-
	   Occurences \== 0,
%	debug('cneg_static_predicate_call :: IN', cneg_static_pred(Goal, SourceFileName, Occurences)),
	   cneg_static_cl(Goal, SourceFileName, Occurences), 
	   NewOccurences is Occurences -1,
	   cneg_static_predicate_call(Goal, SourceFileName, NewOccurences)),
%	   (main :- getenvstr('CIAO_CALL', CIAO_CALL_STRING), nl, write(CIAO_CALL_STRING),
%	    atom_codes(CIAO_CALL, CIAO_CALL_STRING), call(CIAO_CALL), 
%	    nl, write(CIAO_CALL)),
	   end_of_file
	  ]).
%cneg_impl([
%		 (:- import subsumes_chk/2 from subsumes),
%	         (:- use_module(library(write))),
%		 (:- use_module(intneg_dist,[intneg_dist/2, intneg_forall_aux/4])),
%	         ( :- meta_predicate(cneg_static) ),
%		 % Intneg
%		 (cneg_static((P1, P2)) :- cneg_static(P1), cneg_static(P2))
%		 ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% cneg_static_negation(_PP_Info, _Head, _ListBody) :- !.
cneg_static_negation(PP_Info, Head, ListBody) :- 
	cneg_processed_pred_info(Name, Arity, SourceFileName, Index, PP_Info),
	functor_local(Goal, Name, Arity, Args),
	varsbag_local(Args, [], [], GoalVars),
%	debug('cneg_static_negation IN', negate_subfrontier((Head, ListBody), Goal, GoalVars)),
	negate_subfrontier((Head, ListBody), Goal, GoalVars, Sol),
%	debug('cneg_static_negation OUT', Sol),
	functor_local(Cneg_Cl, ':-', 2, _Arguments_Cneg_Cl),
	arg(1, Cneg_Cl, cneg_static_cl(Goal, SourceFileName, Index)),
	arg(2, Cneg_Cl, Sol),
	assertz(cneg_static_cls(Cneg_Cl)).

cneg_static_negation(PP_Info, Head, ListBody) :- !,
	msg('ERROR', cneg_static_negation(PP_Info, Head, ListBody)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
