%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Module for being loaded by cneg1 to add store_clauses/2 facts
% to flatten the structure of facts and clauses of a program
% that contains cneg/1 calls to be able to aply the try/2 
% technique for the constructive negation of the goals.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(cneg_tr,[trans_sent/3, trans_clause/3],[assertions]).

:- use_module(library(engine(data_facts)),[retract_fact/1]).
:- use_module(cneg_aux, _).
:- use_module(library(terms), _).
:- use_module(cneg_tr_vpc, [generate_cneg_tr_vpc/4, cneg_main_and_aux_cl_names/3, take_body_first_unification/2]).
:- reexport(cneg_tr_vpc, [cneg_main_and_aux_cl_names/3]).

:- comment(title, "Contructive Negation Transformation").

:- comment(author, "V@'{i}ctor Pablos Ceruelo").

:- comment(summary, "This module does de transformation needed to evaluate 
	the constructive negation of any predicate in the original file.").

% dynamic predicate(s) 
:- data cneg_list_of_heads_and_bodies/1.
:- data cneg_list_of_predicates/1.

list_name_for_cneg_heads_and_bodies('cneg_list_of_heads_and_bodies').
list_name_for_cneg_predicates('cneg_list_of_predicates').

trans_clause(Whatever, Whatever, _) :-
	echo_msg(1, '', 'cneg_tr', 'trans_clause', (Whatever)).

% trans_sent(Sentence,SentList,Module) sustitutes Sentence in 
% the program Module that is being compilated by the list of 
% sentences SentList. The result clauses are continous
trans_sent(Input, Output, SourceFileName) :-
	% echo_msg(trans_sent(Input)), 
	% echo_msg('trans_sent', 'trans_sent', source_file_name(Info)), 
	trans_sent_aux(Input, Output, SourceFileName), !.

trans_sent(Input, [Input, cneg_not_translated(Input)], _SourceFileName) :-
	echo_msg(2, '', 'cneg_tr', 'ERROR :: Impossible to translate', (Input)), !.

trans_sent_aux(X, [], _SourceFileName):- 
	var(X), !, fail.

% (:- include(dist, _)), (:- include(cneg_lib)), 
trans_sent_aux(end_of_file, ClsFinal, SourceFileName):- !,
%	echo_msg(1, '', 'cneg_tr', 'INFO: #################################################', ''), 
	echo_msg(1, '', 'cneg_tr', 'INFO: #############  Now computing negation  ##############', ''), 
%	echo_msg(1, '', 'cneg_tr', 'INFO: #################################################', ''), 
	trans_sent_eof(ClsFinal, SourceFileName).

trans_sent_aux(0, [], _SourceFileName) :- 	!.
%	echo_msg(2, '', 'cneg_tr', 'INFO :: Use cneg_not_translated/1 to see errors in translation of ', SourceFileName).

% Do not modify module imports, declarations and so on.
trans_sent_aux((:- Whatever),[(:- Whatever)],_):- !.
%	msg('Warning: cneg does not work for imported predicates unless cneg\'s package is imported from each one. Problematic declaration:', Whatever).

% Aqui es donde da el warning porque no conoce a dist:dist aqui.
trans_sent_aux(Clause, [Clause], _SourceFileName) :-
	echo_msg(0, '', 'cneg_tr', 'INFO :: save_sent_info(Clause) ', save_sent_info(Clause)),
	save_sent_info(Clause).

save_sent_info(Clause) :-
	functor_local(Clause, Name, 2, _Arguments),
	Name==':-', !,
	arg(1, Clause, Head),
	arg(2, Clause, Body),
	unifications_in_head_to_equality(Head, New_Head, Equality),
	split_goal_with_disjunctions_into_goals((Equality, Body), 'cneg_tr', Bodies),
	store_head_and_bodies_info(New_Head, Head, Bodies).

save_sent_info(Head) :-
	functor_local(Head, Name, Arity, _Arguments),
	(
	    Name\==':-' 
	; 
	    Arity\==2
	), !,
	unifications_in_head_to_equality(Head, New_Head, Equality),
	store_head_and_bodies_info(New_Head, Head, [Equality]).

unifications_in_head_to_equality(Head, New_Head, Equality) :-
	% Take the unifications in the head and move them to the body.
	functor_local(Head, Name, Arity, Args), 
	functor_local(New_Head, Name, Arity, New_Args), 
	functor_local(Equality, '=', 2, [New_Args | [Args]]).


store_head_and_bodies_info(Head, Test, Bodies) :-
	echo_msg(0, '', 'cneg_tr', 'store_head_and_bodies_info(Head, Test, Bodies) ', store_head_and_bodies_info(Head, Test, Bodies)),
	store_head_and_bodies_info_aux(Head, Test, Bodies).

store_head_and_bodies_info_aux(_Head, _Test, []) :-
	!. % Backtracking forbidden.
store_head_and_bodies_info_aux(Head, Test, [Body | Bodies]) :-
	store_head_info(Head, Counter),
	store_body_info(Head, Body, Test, Counter),
	store_head_and_bodies_info(Head, Test, Bodies).

store_body_info(Head, Body, Test, Counter) :-
	list_name_for_cneg_heads_and_bodies(List_Name),
	retrieve_list_of(List_Name, List),
	save_list_of(List_Name, [(Head, Body, Test, Counter) | List]).

store_head_info(Head, NewCounter) :-
	functor_local(Head, Name, Arity, _Arguments),
	list_name_for_cneg_predicates(List_Name),
	retrieve_list_of(List_Name, List),
	remove_from_list_with_counter(List, (Name, Arity, Counter), NewList),
	NewCounter is Counter + 1,
	save_list_of(List_Name, [(Name, Arity, NewCounter) | NewList]).

remove_from_list_with_counter([], (_Name, _Arity, 0), []) :- !.
remove_from_list_with_counter([(Name, Arity, Counter) | List], (Name, Arity, Counter), List) :- !.
remove_from_list_with_counter([ Elto | List], (Name, Arity, Counter), [Elto | NewList]) :- !,
	remove_from_list_with_counter(List, (Name, Arity, Counter), NewList).

% Saves a list with name List_Name and argument List.
save_list_of(List_Name, List) :-
	functor_local(Functor, List_Name, 1, [List]),
	assertz_fact(Functor), !,
	echo_msg(0, '', 'cneg_tr', 'assertz_fact ', assertz_fact(Functor)).

% Retrieves a list with name List_Name and argument List.
retrieve_list_of(List_Name, List) :-
	functor_local(Functor, List_Name, 1, [List]),
	retract_fact(Functor), !,
	echo_msg(0, '', 'cneg_tr', 'retract_fact ', retract_fact(Functor)).
retrieve_list_of(_List_Name, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

generate_auxiliary_code([ Multifile |[ end_of_file]]) :-
	Multifile = (:- multifile cneg_pre_frontier/6).
%	Multifile = (:- multifile cneg_pre_frontier/6, call_to/3),
%	Call_To_This_File_Pred = (call_to(Predicate) :- call(Predicate)).

trans_sent_eof(Cls_Out, SourceFileName) :-
	generate_auxiliary_code(Aux_Code),
	list_name_for_cneg_predicates(List_Name_1),
	retrieve_list_of(List_Name_1, List_Of_Preds),
	echo_msg(0, '', 'cneg_tr', 'List_Of_Preds', List_Of_Preds),
	list_name_for_cneg_heads_and_bodies(List_Name_2),
	retrieve_list_of(List_Name_2, List_Of_H_and_B),
	echo_msg(0, '', 'cneg_tr', 'List_Of_H_and_B', List_Of_H_and_B),

	generate_cneg_tr_vpc(List_Of_Preds, List_Of_H_and_B, Aux_Code, Cls_4),
	generate_pre_frontiers(List_Of_H_and_B, SourceFileName, Cls_4, Cls_Out),
	echo_msg(0, 'nl', 'cneg_tr', '', ''), 
	echo_msg(0, 'nl', 'cneg_tr', '', ''), 
	echo_msg(0, '', 'cneg_tr', 'Cls_Out', Cls_Out),
	echo_msg(0, 'nl', 'cneg_tr', '', ''), 
	echo_msg(0, 'nl', 'cneg_tr', '', ''), 
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

