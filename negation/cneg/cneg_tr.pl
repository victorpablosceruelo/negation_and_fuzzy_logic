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
	unifications_in_head_to_equality(Head, Clean_Head, Head_Name, Head_Arity, Equality),
	split_goal_with_disjunctions_into_goals((Equality, Body), Bodies),
	echo_msg(2, '', 'cneg_tr', 'Bodies', Bodies),
	split_bodies_into_E_IE_NIE(Bodies, Split_Bodies),
	store_head_and_bodies_info(Head, Clean_Head, Head_Name, Head_Arity, Split_Bodies).

save_sent_info(Head) :-
	functor_local(Head, Name, Arity, _Arguments),
	(
	    Name\==':-' 
	; 
	    Arity\==2
	), !,
	unifications_in_head_to_equality(Head, Clean_Head, Head_Name, Head_Arity, Equality),
	echo_msg(2, '', 'cneg_tr', 'Bodies', [Equality]),
	split_bodies_into_E_IE_NIE([Equality], Split_Bodies),
	store_head_and_bodies_info(Head, Clean_Head, Head_Name, Head_Arity, Split_Bodies).

unifications_in_head_to_equality(Head, Clean_Head, Head_Name, Head_Arity, Equality) :-
	% Take the unifications in the head and move them to the body.
	functor_local(Head, Head_Name, Head_Arity, Args), 
	functor_local(Clean_Head, Head_Name, Head_Arity, New_Args), 
	functor_local(Equality, '=', 2, [New_Args | [Args]]).

store_head_and_bodies_info(Head, Clean_Head, Head_Name, Head_Arity, Bodies) :-
	echo_msg(2, '', 'cneg_tr', 'store_head_and_bodies_info(Head, Test, Bodies) ', store_head_and_bodies_info(Head, Clean_Head, Head_Name, Head_Arity, Bodies)),
	store_head_and_bodies_info_aux(Head, Clean_Head, Head_Name, Head_Arity, Bodies).

store_head_and_bodies_info_aux(_Head, _Clean_Head, _Head_Name, _Head_Arity, []) :-
	!. % Backtracking forbidden.
store_head_and_bodies_info_aux(Head, Clean_Head, Head_Name, Head_Arity, [([E], [IE], [NIE], [Body]) | Bodies]) :-
	% Store head info
	name_for_assert_cneg_tr_pred_info(Store_Name_Heads),
	retract_fact_local(Store_Name_Heads, (Head_Name, Head_Arity, Old_Counter), (Head_Name, Head_Arity, 0)),
	Counter is Old_Counter + 1,
	assertz_fact_local(Store_Name_Heads, (Head_Name, Head_Arity, Counter)),

	% Store body info
	name_for_assert_cneg_tr_pred_head_and_body(Store_Name_Bodies),
	assertz_fact_local(Store_Name_Bodies, (Clean_Head, E, IE, NIE, Head, Body, Head_Name, Head_Arity, Counter)),

	% More
	store_head_and_bodies_info(Head, Clean_Head, Head_Name, Head_Arity, Bodies).

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
	echo_msg(2, 'nl', 'cneg_tr', '', ''), !,

	name_for_assert_cneg_tr_pred_head_and_body(Store_Name_2),
	retract_all_fact_local(Store_Name_2, List_Of_H_and_B),
	echo_msg(2, 'list', 'cneg_tr', 'List_Of_H_and_B', List_Of_H_and_B), 
	echo_msg(2, 'nl', 'cneg_tr', '', ''), !,

	get_list_of_preds_to_ignore(List_Of_H_and_B, To_Ignore_List),
	echo_msg(2, 'list', 'cneg_tr', 'To_Ignore_List', To_Ignore_List), 
	echo_msg(2, 'nl', 'cneg_tr', '', ''), !,

	generate_pre_frontiers(List_Of_H_and_B, SourceFileName, To_Ignore_List, Aux_Code, Cls_Out),
	echo_msg(2, 'nl', 'cneg_tr', '', ''), 
	echo_msg(2, 'list', 'cneg_tr', 'Cls_Out', Cls_Out),
	echo_msg(2, 'nl', 'cneg_tr', '', ''), 
	!. %Backtracking forbiden.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

generate_pre_frontiers([], _SourceFileName, _To_Ignore_List, Cls, Cls) :- !.
generate_pre_frontiers([H_and_B | List_Of_H_and_B], SourceFileName, To_Ignore_List, Cls_In, Cls_Out) :-
	H_and_B = (_Clean_Head, _E, _IE, _NIE, _Head, _Body, Head_Name, Head_Arity, _Counter),
	memberchk(Head_Name/Head_Arity, To_Ignore_List), !,
	generate_pre_frontiers(List_Of_H_and_B, SourceFileName, To_Ignore_List, Cls_In, Cls_Out).

generate_pre_frontiers([H_and_B | List_Of_H_and_B], SourceFileName, To_Ignore_List, Cls_In, Cls_Out) :-
	H_and_B = (Clean_Head, E, IE, NIE, Head, Body, Head_Name, Head_Arity, _Counter),
	CL = (cneg_pre_frontier(Head_Name, Head_Arity, SourceFileName, Clean_Head, E, IE, NIE, Head, Body)),
	generate_pre_frontiers(List_Of_H_and_B, SourceFileName, To_Ignore_List, [CL | Cls_In], Cls_Out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_list_of_preds_to_ignore([], []) :- !.
get_list_of_preds_to_ignore([ H_and_B | _List_Of_H_and_B], List) :-
	H_and_B = (_Clean_Head, _E, _IE, _NIE, Head, _Body, Head_Name, Head_Arity, _Counter),
%	echo_msg(2, '', 'cneg_tr', 'get_list_of_preds_to_ignore :: Test', Test),  
	Head_Name = 'cneg_ignores_preds', 
	Head_Arity = 1, !,
	arg(1, Head, List_Tmp),
	List = ['cneg_ignores_preds'/1 | [ 'file_debug_is_activated'/1 | List_Tmp ]].
%	List = ['cneg_ignores_preds'/1 | [ 'cneg_choosen_negation'/1 | List_Tmp ]].
get_list_of_preds_to_ignore([ _H_and_B | List_Of_H_and_B], List) :-
	get_list_of_preds_to_ignore(List_Of_H_and_B, List).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
