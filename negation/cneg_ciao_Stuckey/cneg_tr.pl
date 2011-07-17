%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Module for being loaded by cneg1 to add store_clauses/2 facts
% to flatten the structure of facts and clauses of a program
% that contains cneg/1 calls to be able to aply the try/2 
% technique for the constructive negation of the goals.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(cneg_tr,[trans_sent/3],[assertions]).

:- use_module(library(engine(data_facts)),[retract_fact/1]).
:- use_module(cneg_diseq,[cneg_diseq/3, cneg_eq/2]).
:- use_module(cneg_lib, _).
:- use_module(cneg_aux, _).

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
	debug_msg(0, 'trans_cl', trans_cl(Whatever)).

% trans_sent(Sentence,SentList,Module) sustitutes Sentence in 
% the program Module that is being compilated by the list of 
% sentences SentList. The result clauses are continous
trans_sent(Input, Output, SourceFileName) :-
	% debug_msg('trans_sent', trans_sent(Input)), 
	% debug_msg('trans_sent', source_file_name(Info)), 
	trans_sent_aux(Input, Output, SourceFileName), !.

trans_sent(Input, [Input, cneg_not_translated(Input)], _SourceFileName) :-
	debug_msg(2, 'ERROR :: Impossible to translate', (Input)), !.

trans_sent_aux(X, [], _SourceFileName):- 
	var(X), !, fail.

% (:- include(dist, _)), (:- include(cneg_lib)), 
trans_sent_aux(end_of_file,ClsFinal, SourceFileName):- !,
	trans_sent_eof(ClsFinal, SourceFileName).

trans_sent_aux(0, [], SourceFileName) :- 	!,
	debug_msg(2, 'INFO :: Use cneg_not_translated/1 to see errors in translation of ', SourceFileName).

% Do not modify module imports, declarations and so on.
trans_sent_aux((:- Whatever),[(:- Whatever)],_):- !.
%	msg('Warning: cneg does not work for imported predicates unless cneg\'s package is imported from each one. Problematic declaration:', Whatever).

% Aqui es donde da el warning porque no conoce a dist:dist aqui.
trans_sent_aux(Clause, [Clause], _SourceFileName) :-
	debug_msg(1, 'INFO :: save_sent_info(Clause) ', save_sent_info(Clause)),
	save_sent_info(Clause).

save_sent_info(Clause) :-
	functor_local(Clause, Name, 2, _Arguments),
	Name==':-', !,
	arg(1, Clause, Head),
	arg(2, Clause, Body),
	split_disjunctions_in_bodies(Body, Bodies),
	store_head_and_bodies_info(Head, Bodies).

save_sent_info(Clause) :-
	functor_local(Clause, Name, Arity, _Arguments),
	(
	    Name\==':-' 
	; 
	    Arity\==2
	), !,
	store_head_and_bodies_info(Clause, [['true']]).

%split_disjunctions_in_bodies(Body, Bodies)
split_disjunctions_in_bodies(Body, _Bodies) :- 
	debug_msg(1, 'INFO :: split_disjunctions_in_bodies :: Body ', Body), fail.	

split_disjunctions_in_bodies(Body, Bodies) :- 
	goal_is_conjunction(Body, Body_Conj_1, Body_Conj_2), !,
	split_disjunctions_in_bodies(Body_Conj_1, Bodies_Conj_1),
	split_disjunctions_in_bodies(Body_Conj_2, Bodies_Conj_2),
	cartesian_product_of_lists(Bodies_Conj_1, Bodies_Conj_2, Bodies).

split_disjunctions_in_bodies(Body, [Body_Result_1, Body_Result_2]) :- 
	goal_is_disjunction(Body, Body_Disj_1, Body_Disj_2), !,
	split_disjunctions_in_bodies(Body_Disj_1, Body_Result_1),
	split_disjunctions_in_bodies(Body_Disj_2, Body_Result_2).

split_disjunctions_in_bodies(Body, [[Body]]). % Goal is something else.

cartesian_product_of_lists([], _List_2, []) :- !.
cartesian_product_of_lists([Elto | List_1], List_2, Result) :-
	cartesian_product_of_lists_aux(Elto, List_2, Result_1),
	cartesian_product_of_lists(List_1, List_2, Result_2),
	append(Result_1, Result_2, Result).

cartesian_product_of_lists_aux(_Elto_1, [], []) :- !.
cartesian_product_of_lists_aux(Elto_1, [Elto_2 | List], [Result | More_Results]) :-
	append(Elto_1, Elto_2, Result),
	cartesian_product_of_lists_aux(Elto_1, List, More_Results).

store_head_and_bodies_info(Head, Bodies) :-
	debug_msg(1, 'store_head_and_bodies_info(Head, Bodies) ', store_head_and_bodies_info(Head, Bodies)),
	store_head_and_bodies_info_aux(Head, Bodies).

store_head_and_bodies_info_aux(_Head, []) :-
	!. % Backtracking forbidden.
store_head_and_bodies_info_aux(Head, [Body | Bodies]) :-
	store_head_info(Head, Counter),
	store_body_info(Head, Body, Counter),
	store_head_and_bodies_info(Head, Bodies).

store_body_info(Head, Body, Counter) :-
	list_name_for_cneg_heads_and_bodies(List_Name),
	retrieve_list_of(List_Name, List),
	save_list_of(List_Name, [(Head, Body, Counter) | List]).

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
	debug_msg(1, 'assertz_fact ', assertz_fact(Functor)).

% Retrieves a list with name List_Name and argument List.
retrieve_list_of(List_Name, List) :-
	functor_local(Functor, List_Name, 1, [List]),
	retract_fact(Functor), !,
	debug_msg(1, 'retract_fact ', retract_fact(Functor)).
retrieve_list_of(_List_Name, []).

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
	debug_msg(0, 'trans_head :: FAILED conversion for', Head), !, fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

trans_sent_eof(ClsOut, _SourceFileName) :-
	list_name_for_cneg_predicates(List_Name_1),
	retrieve_list_of(List_Name_1, List_Of_Preds),
	generate_main_cls(List_Of_Preds, Cls_1),
	list_name_for_cneg_heads_and_bodies(List_Name_2),
	retrieve_list_of(List_Name_2, List_Of_H_and_B),
	negate_head_and_bodies(List_Of_H_and_B, Cls_2),
	append(Cls_1, Cls_2, ClsOut),
	!, %Backtracking forbiden.
	nl, nl,
	debug_msg_list(1, 'ClsOut', ClsOut),
	nl, nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%generate_main_cls(List_Of_Preds, Cls_1).
generate_main_cls([], []) :- !.
generate_main_cls([(Name, Arity, Counter) | List_Of_Preds], [Main_Pred | [Main_SubPred | Cls]]) :-
	generate_main_pred(Name, Arity, Main_Pred),
	generate_main_subpred(Name, Arity, Counter, Main_SubPred), 
	generate_main_cls(List_Of_Preds, Cls).

generate_main_pred(Main_Pred_Name, Arity, Main_Pred) :-
	functor_local(Main_Pred, ':-', 2, _Args_Main_Pred), 
	arg(1, Main_Pred, Head),
	arg(2, Main_Pred, (Initializer, (SubCall, Tester))),
	functor_local(Head, Main_Pred_Name, Arity, _Args_Head),
	NewArity is Arity + 4,
	main_subpred_name(Main_Pred_Name, Main_SubPred_Name),
	functor_local(SubCall, Main_SubPred_Name, NewArity, _Args_SubCall),
	copy_args(Arity, Head, SubCall),
	adjust_last_four_args(NewArity, SubCall, F_In, F_Out, Cont_In, Cont_Out),
	% F_In, F_Out : F -> Forall
	functor_local(Initializer, 'cneg_initialize', 2, [F_In | [Cont_In]]),
	functor_local(Tester, 'cneg_test', 2, [F_Out | [Cont_Out]]).

adjust_last_four_args(Arity, Functor, F_In, F_Out, Cont_In, Cont_Out) :-
	arg(Arity, Functor, Cont_Out), 
	Arity_2 is Arity -1, 
	arg(Arity_2, Functor, Cont_In), 
	Arity_3 is Arity_2 -1,
 	arg(Arity_3, Functor, F_Out), 
	Arity_4 is Arity_3 -1,
 	arg(Arity_4, Functor, F_In).

generate_main_subpred(Main_Pred_Name, Arity, Counter, Main_SubPred) :-
	functor_local(Main_SubPred, ':-', 2, _Args_Main_Pred), 
	arg(1, Main_SubPred, Head),
	arg(2, Main_SubPred, Body),
	NewArity is Arity + 4,
	main_subpred_name(Main_Pred_Name, Main_SubPred_Name),
	functor_local(Head, Main_SubPred_Name, NewArity, _Args_SubCall),
	adjust_last_four_args(NewArity, Head, F_In, F_Out, Cont_In, Cont_Out),
	generate_all_the_subcalls(Counter, Main_SubPred_Name, NewArity, Body, F_In, F_Out, Cont_In, Cont_Out).
	
main_subpred_name(Main_Pred_Name, Main_SubPred_Name) :-
	name(Main_Pred_Name, String),
	append("cneg_", String, NewString),
	name(Main_SubPred_Name, NewString).

generate_all_the_subcalls(0, _Main_SubPred_Name, _Arity, 'true', F_In, F_In, Cont_In, Cont_In) :- !.
generate_all_the_subcalls(Counter, Main_SubPred_Name, Arity, (Body, Current), F_In, F_Out, Cont_In, Cont_Out) :-
	generate_name_from_counter(Counter, Main_SubPred_Name, Current_Name),
	functor_local(Current, Current_Name, Arity, _Args), 
	adjust_last_four_args(Arity, Current, New_F_Out, F_Out, New_Cont_Out, Cont_Out),
	NewCounter is Counter - 1, 
	generate_all_the_subcalls(NewCounter, Main_SubPred_Name, Arity, Body, F_In, New_F_Out, Cont_In, New_Cont_Out).

generate_name_from_counter(Counter, Name, New_Name) :-
	name(Name, String_1),
	name(Counter, String_2), 
	append("_", String_2, String_3),
	append(String_1, String_3, String), 
	name(New_Name, String).


% Here we convert the unifications in heads in equalities in the bodies.
% Besides we adequate the head so we do not have to modify it again.
% Head indexes and variables for FreeVars and Continuation are added in this step.

%negate_head_and_bodies(List_Of_H_and_B, Cls_2).
negate_head_and_bodies(_List_Of_H_and_B, []).


trans_body(Body_In, NewBody, ListBodies):-
%	debug_msg('trans_body_aux :: IN', (Body_In)),
	trans_body_aux(Body_In, NewBody, [[]], ListBodies).
%	debug_msg('trans_body_aux :: OUT', (Body_In, NewBody, [], ListBodies)).

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

