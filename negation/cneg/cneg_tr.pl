%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Module for being loaded by cneg1 to add store_clauses/2 facts
% to flatten the structure of facts and clauses of a program
% that contains cneg/1 calls to be able to aply the try/2 
% technique for the constructive negation of the goals.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(cneg_tr,[trans_sent/3, trans_clause/3],[assertions]).

:- use_module(library(engine(data_facts)),[retract_fact/1]).
:- use_module(library(aggregates),[findall/3]).
:- use_module(library('cneg/cneg_aux')).
%:- use_module(library(terms), _).

:- comment(title, "Contructive Negation Transformation").

:- comment(author, "V@'{i}ctor Pablos Ceruelo").

:- comment(summary, "This module does de transformation needed to evaluate 
	the constructive negation of any predicate in the original file.").

% dynamic predicate(s) 
:- data cneg_tr_pred_head_and_body/1.
:- data cneg_tr_pred_info/1.
:- multifile file_debug_is_activated/1.

file_debug_is_activated(true). % Change to disable debugging.

name_for_assert_cneg_tr_pred_head_and_body('cneg_tr_pred_head_and_body').
name_for_assert_cneg_tr_pred_info('cneg_tr_pred_info').

trans_clause(Whatever, Whatever, _) :-
	echo_msg(1, '', 'cneg_tr', 'trans_clause', (Whatever)).

% trans_sent(Sentence,SentList,Module) sustitutes Sentence in 
% the program Module that is being compilated by the list of 
% sentences SentList. The result clauses are continous
trans_sent(Input, Output, SourceFileName) :-
	trans_sent_aux(Input, Output, SourceFileName), !,
	echo_msg(2, '', 'cneg_tr', 'trans_sent(Input, Output, SourceFileName)', trans_sent(Input, Output, SourceFileName)), !.

trans_sent(Input, [Input, cneg_not_translated(Input)], _SourceFileName) :-
	echo_msg(1, '', 'cneg_tr', 'ERROR :: Impossible to translate', (Input)), !.

trans_sent_aux(X, [], _SourceFileName):- 
	var(X), !, fail.

% (:- include(dist, _)), (:- include(cneg_lib)), 
trans_sent_aux(end_of_file, ClsFinal, SourceFileName):- !,
	echo_msg(2, 'nl', 'cneg_tr', '', ''), 
	echo_msg(2, '', 'cneg_tr', 'INFO: #################################################', ''), 
	echo_msg(1, '', 'cneg_tr', 'INFO: #############  Now computing negation  ##############', ''), 
	echo_msg(2, '', 'cneg_tr', 'INFO: #################################################', ''), 
	echo_msg(2, 'nl', 'cneg_tr', '', ''), 
	trans_sent_eof(ClsFinal, SourceFileName).

trans_sent_aux(0, [], _SourceFileName) :- !.
%	echo_msg(2, '', 'cneg_tr', 'INFO :: Use cneg_not_translated/1 to see errors in translation of ', SourceFileName).

% Do not modify module imports, declarations and so on.
trans_sent_aux((:- Whatever),[(:- Whatever)],_):- !.
%	msg('Warning: cneg does not work for imported predicates unless cneg\'s package is imported from each one. Problematic declaration:', Whatever).

trans_sent_aux(Clause, [Clause], _SourceFileName) :-
%	echo_msg(2, '', 'cneg_tr', 'save_sent_info(Clause) ', save_sent_info(Clause)),
	save_sent_info(Clause).

save_sent_info(Clause) :-
	functor_local(Clause, Name, 2, _Arguments),
	Name==':-', !,
	arg(1, Clause, Head),
	arg(2, Clause, Body),
	unifications_in_head_to_equality(Head, New_Head, Equality),
	split_goal_with_disjunctions_into_goals((Equality, Body), 'cneg_tr', Bodies),
	echo_msg(2, '', 'cneg_tr', 'Bodies', Bodies),
	split_bodies_into_E_IE_NIE(Bodies, Split_Bodies),
	store_head_and_bodies_info(New_Head, Head, Split_Bodies).

save_sent_info(Head) :-
	functor_local(Head, Name, Arity, _Arguments),
	(
	    Name\==':-' 
	; 
	    Arity\==2
	), !,
	unifications_in_head_to_equality(Head, New_Head, Equality),
	echo_msg(2, '', 'cneg_tr', 'Bodies', [Equality]),
	split_bodies_into_E_IE_NIE([Equality], Split_Bodies),
	store_head_and_bodies_info(New_Head, Head, Split_Bodies).

unifications_in_head_to_equality(Head, New_Head, Equality) :-
	% Take the unifications in the head and move them to the body.
	functor_local(Head, Name, Arity, Args), 
	functor_local(New_Head, Name, Arity, New_Args), 
	functor_local(Equality, '=', 2, [New_Args | [Args]]).

split_bodies_into_E_IE_NIE([], []) :- !.
split_bodies_into_E_IE_NIE([Body | Bodies], [(E, IE, NIE) | Split_Bodies]) :- !,
	split_bodies_into_E_IE_NIE_aux(Body, true, true, true, E, IE, NIE),
	split_bodies_into_E_IE_NIE(Bodies, Split_Bodies).

split_bodies_into_E_IE_NIE_aux(Body, E_In, IE_In, NIE_In, E_Out, IE_Out, NIE_Out) :- !,
	goal_is_conjunction(Body, G1, G2),
	split_bodies_into_E_IE_NIE_aux(G1, E_In, IE_In, NIE_In, E_Aux, IE_Aux, NIE_Aux),
	split_bodies_into_E_IE_NIE_aux(G2, E_Aux, IE_Aux, NIE_Aux, E_Out, IE_Out, NIE_Out).
split_bodies_into_E_IE_NIE_aux(Body, E_In, IE_In, NIE_In, E_Out, IE_In, NIE_In) :-
	goal_is_equality(Body, _Arg_1, _Arg_2, _GV, _EQV, _UQV), !,
	goals_join_by_conjunction(E_In, Body, E_Out).
split_bodies_into_E_IE_NIE_aux(Body, E_In, IE_In, NIE_In, E_In, IE_Out, NIE_In) :-
	goal_is_disequality(Body, _Arg_1, _Arg_2, _GV, _EQV, _UQV), !,
	goals_join_by_conjunction(IE_In, Body, IE_Out).
split_bodies_into_E_IE_NIE_aux(Body, E_In, IE_In, NIE_In, E_In, IE_In, NIE_Out) :- !,
	goals_join_by_conjunction(NIE_In, Body, NIE_Out).

store_head_and_bodies_info(Head, Test, Bodies) :-
%	echo_msg(2, '', 'cneg_tr', 'store_head_and_bodies_info(Head, Test, Bodies) ', store_head_and_bodies_info(Head, Test, Bodies)),
	store_head_and_bodies_info_aux(Head, Test, Bodies).

store_head_and_bodies_info_aux(_Head, _Test, []) :-
	!. % Backtracking forbidden.
store_head_and_bodies_info_aux(Head, Test, [Body | Bodies]) :-
	store_head_info(Head, Counter),
	store_body_info(Head, Body, Test, Counter),
	store_head_and_bodies_info(Head, Test, Bodies).

store_body_info(Head, Body, Test, Counter) :-
	name_for_assert_cneg_tr_pred_head_and_body(Store_Name),
	assertz_fact_local(Store_Name, (Head, Body, Test, Counter)).


store_head_info(Head, NewCounter) :-
	name_for_assert_cneg_tr_pred_info(Store_Name),
	functor_local(Head, Name, Arity, _Arguments),
	retract_fact_local(Store_Name, (Name, Arity, Counter), (Name, Arity, 0)),
	NewCounter is Counter + 1,
	assertz_fact_local(Store_Name, (Name, Arity, NewCounter)).

% Saves a list with name List_Name and argument List.
assertz_fact_local(Store_Name, Whatever) :-
	functor_local(To_Store, Store_Name, 1, [Whatever]),
	assertz_fact(To_Store), !.
%	echo_msg(2, '', 'cneg_tr', 'assertz_fact(To_Store)', assertz_fact(To_Store)).

% Retrieves a list with name List_Name and argument List.
retract_fact_local(Store_Name, Whatever, _Whatever_On_Error) :-
	functor_local(To_Retrieve, Store_Name, 1, [Whatever]),
	retract_fact(To_Retrieve), !.
%	echo_msg(2, '', 'cneg_tr', 'retract_fact_local', To_Retrieve).
retract_fact_local(_Store_Name, Whatever, Whatever) :- !.
%	echo_msg(2, '', 'cneg_tr', 'retract_fact_local', Whatever).

retract_all_fact_local(Store_Name, Result) :-
	functor_local(To_Retrieve, Store_Name, 1, [Whatever]),
	findall(Whatever, retract_fact(To_Retrieve), Result).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

generate_auxiliary_code([ Multifile |[ end_of_file]]) :-
	Multifile = (:- multifile cneg_pre_frontier/6).
%	Multifile = (:- multifile cneg_pre_frontier/6, call_to/3),
%	Call_To_This_File_Pred = (call_to(Predicate) :- call(Predicate)).

trans_sent_eof(Cls_Out, SourceFileName) :-
	generate_auxiliary_code(Aux_Code),

	name_for_assert_cneg_tr_pred_info(Store_Name_1),
	retract_all_fact_local(Store_Name_1, List_Of_Preds),
	echo_msg(2, 'list', 'cneg_tr', 'List_Of_Preds', List_Of_Preds),

	name_for_assert_cneg_tr_pred_head_and_body(Store_Name_2),
	retract_all_fact_local(Store_Name_2, List_Of_H_and_B),
	echo_msg(2, 'list', 'cneg_tr', 'List_Of_H_and_B', List_Of_H_and_B),

	remove_predicates_to_ignore(List_Of_Preds, List_Of_H_and_B, List_Of_Preds_Aux, List_Of_H_and_B_Aux),
	echo_msg(2, 'list', 'cneg_tr', 'List_Of_Preds_Aux', List_Of_Preds_Aux),
	echo_msg(2, 'list', 'cneg_tr', 'List_Of_H_and_B_Aux', List_Of_H_and_B_Aux),

	generate_pre_frontiers(List_Of_H_and_B_Aux, SourceFileName, Aux_Code, Cls_Out),
	echo_msg(2, 'nl', 'cneg_tr', '', ''), 
	echo_msg(2, 'nl', 'cneg_tr', '', ''), 
	echo_msg(2, 'list', 'cneg_tr', 'Cls_Out', Cls_Out),
	echo_msg(2, 'nl', 'cneg_tr', '', ''), 
	echo_msg(2, 'nl', 'cneg_tr', '', ''), 
	!. %Backtracking forbiden.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

generate_pre_frontiers([], _SourceFileName, Cls, Cls) :- !.
generate_pre_frontiers([(Head, Body, Test, _Counter) | List_Of_Preds], SourceFileName, Cls_In, Cls_Out) :-
	functor(Head, Head_Name, Head_Arity),
%	take_body_first_unification(Body, Head_Test),
%	% Noooooooooo. Esto esta mal. El test debe ser la propia cabecera vieja.
%	CL = (cneg_pre_frontier(Head_Name, Head_Arity, SourceFileName, Head, Body, Head_Test)),
	CL = (cneg_pre_frontier(Head_Name, Head_Arity, SourceFileName, Head, Body, Test)),
	generate_pre_frontiers(List_Of_Preds, SourceFileName, [CL | Cls_In], Cls_Out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

remove_predicates_to_ignore(List_Of_Preds, List_Of_H_and_B, List_Of_Preds_Aux, List_Of_H_and_B_Aux) :-
	get_list_of_preds_to_ignore(List_Of_H_and_B, List), 
	List \== [], !, % If empty this procedure is not useful.
	echo_msg(2, 'nl', 'cneg_tr', '', ''),
	echo_msg(2, '', 'cneg_tr', 'get_list_of_preds_to_ignore :: List', List),  
	echo_msg(2, 'list', 'cneg_tr', 'get_list_of_preds_to_ignore :: List', List),  
	echo_msg(2, 'nl', 'cneg_tr', '', ''), 
	remove_each_pred_in_list_1(List, List_Of_Preds, List_Of_Preds_Aux),
%	echo_msg(2, 'list', 'cneg_tr', 'List_Of_Preds_Aux', List_Of_Preds_Aux),
	remove_each_pred_in_list_2(List, List_Of_H_and_B, List_Of_H_and_B_Aux).
%	echo_msg(2, 'list', 'cneg_tr', 'List_Of_H_and_B_Aux', List_Of_H_and_B_Aux).

remove_predicates_to_ignore(List_Of_Preds, List_Of_H_and_B, List_Of_Preds, List_Of_H_and_B) :- !,
	echo_msg(2, 'nl', 'cneg_tr', '', ''),
	echo_msg(2, '', 'cneg_tr', 'get_list_of_preds_to_ignore', 'No List'),  
	echo_msg(2, 'nl', 'cneg_tr', '', '').

get_list_of_preds_to_ignore([], []) :- !.
get_list_of_preds_to_ignore([(_Head, _Body, Test, _Counter) | _List_Of_H_and_B], List) :-
%	echo_msg(2, '', 'cneg_tr', 'get_list_of_preds_to_ignore :: Test', Test),  
	functor(Test, 'cneg_ignores_preds', 1), !,
	arg(1, Test, List_Tmp),
	List = ['cneg_ignores_preds'/1 | [ 'file_debug_is_activated'/1 | List_Tmp ]].
%	List = ['cneg_ignores_preds'/1 | [ 'cneg_choosen_negation'/1 | List_Tmp ]].
get_list_of_preds_to_ignore([(_Head, _Body, _Test, _Counter) | List_Of_H_and_B], List) :-
	get_list_of_preds_to_ignore(List_Of_H_and_B, List).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

remove_each_pred_in_list_1(_List, [], []) :- !. % Optimization.
remove_each_pred_in_list_1(List, [(Name, Arity, _C) | List_Of_Preds], List_Of_Preds_Aux) :-
%	echo_msg(2, '', 'cneg_tr', 'remove_each_pred_in_list_1 :: Testing (Name, Arity)', (Name, Arity)),
	memberchk(Name/Arity, List), !,
%	echo_msg(2, '', 'cneg_tr', 'remove_each_pred_in_list_1 :: Removing (Name, Arity)', (Name, Arity)),
	remove_each_pred_in_list_1(List, List_Of_Preds, List_Of_Preds_Aux).
remove_each_pred_in_list_1(List, [(Name, Arity, C) | List_Of_Preds], [(Name, Arity, C) | List_Of_Preds_Aux]) :-
	remove_each_pred_in_list_1(List, List_Of_Preds, List_Of_Preds_Aux).

remove_each_pred_in_list_2(_List, [], []) :- !. % Optimization.
remove_each_pred_in_list_2(List, [(Head, _Body, _T, _C) | List_Of_H_and_B], List_Of_H_and_B_Aux) :- 
%	echo_msg(2, '', 'cneg_tr', 'remove_each_pred_in_list_2 :: Testing (Head)', (Head)),
	functor(Head, Name, Arity), 
%	echo_msg(2, '', 'cneg_tr', 'remove_each_pred_in_list_2 :: Testing (Name, Arity)', (Name, Arity)),
	memberchk(Name/Arity, List), !,
%	echo_msg(2, '', 'cneg_tr', 'remove_each_pred_in_list_2 :: Removing (Name, Arity)', (Name, Arity)),  
	remove_each_pred_in_list_2(List, List_Of_H_and_B, List_Of_H_and_B_Aux).
remove_each_pred_in_list_2(List, [(Head, Body, T, C) | List_Of_H_and_B], [(Head, Body, T, C) | List_Of_H_and_B_Aux]) :- 
	remove_each_pred_in_list_2(List, List_Of_H_and_B, List_Of_H_and_B_Aux).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
