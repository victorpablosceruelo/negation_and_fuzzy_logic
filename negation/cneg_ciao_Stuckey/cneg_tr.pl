%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Module for being loaded by cneg1 to add store_clauses/2 facts
% to flatten the structure of facts and clauses of a program
% that contains cneg/1 calls to be able to aply the try/2 
% technique for the constructive negation of the goals.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(cneg_tr,[trans_sent/3, trans_clause/3, cneg_main_and_aux_cl_names/3],[assertions]).

:- use_module(library(engine(data_facts)),[retract_fact/1]).
:- use_module(cneg_aux, _).
:- use_module(library(terms), _).

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
	debug_msg(1, 'trans_clause', (Whatever)).

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
trans_sent_aux(end_of_file, ClsFinal, SourceFileName):- !,
%	debug_msg(1, 'INFO: #################################################', ''), 
	debug_msg(1, 'INFO: #############  Now computing negation  ##############', ''), 
%	debug_msg(1, 'INFO: #################################################', ''), 
	trans_sent_eof(ClsFinal, SourceFileName).

trans_sent_aux(0, [], _SourceFileName) :- 	!.
%	debug_msg(2, 'INFO :: Use cneg_not_translated/1 to see errors in translation of ', SourceFileName).

% Do not modify module imports, declarations and so on.
trans_sent_aux((:- Whatever),[(:- Whatever)],_):- !.
%	msg('Warning: cneg does not work for imported predicates unless cneg\'s package is imported from each one. Problematic declaration:', Whatever).

% Aqui es donde da el warning porque no conoce a dist:dist aqui.
trans_sent_aux(Clause, [Clause], _SourceFileName) :-
	debug_msg(0, 'INFO :: save_sent_info(Clause) ', save_sent_info(Clause)),
	save_sent_info(Clause).

save_sent_info(Clause) :-
	functor_local(Clause, Name, 2, _Arguments),
	Name==':-', !,
	arg(1, Clause, Head),
	arg(2, Clause, Body),
	unifications_in_head_to_equality(Head, New_Head, Equality),
	split_disjunctions_in_bodies((Equality, Body), Bodies),
	store_head_and_bodies_info(New_Head, Bodies).

save_sent_info(Head) :-
	functor_local(Head, Name, Arity, _Arguments),
	(
	    Name\==':-' 
	; 
	    Arity\==2
	), !,
	unifications_in_head_to_equality(Head, New_Head, Equality),
	store_head_and_bodies_info(New_Head, [[Equality]]).

unifications_in_head_to_equality(Head, New_Head, Equality) :-
	% Take the unifications in the head and move them to the body.
	functor_local(Head, Name, Arity, Args), 
	functor_local(New_Head, Name, Arity, New_Args), 
	functor_local(Equality, '=', 2, [New_Args | [Args]]).

%split_disjunctions_in_bodies(Body, Bodies)
split_disjunctions_in_bodies(Body, Bodies) :- 
	debug_msg(0, 'INFO :: split_disjunctions_in_bodies :: Body ', Body), 
	split_disjunctions_in_bodies_aux(Body, Bodies),
	debug_msg(0, 'INFO :: split_disjunctions_in_bodies :: Bodies ', Bodies), 
	!.

split_disjunctions_in_bodies_aux(Body, Bodies) :- 
	goal_is_conjunction(Body, Body_Conj_1, Body_Conj_2), !,
	split_disjunctions_in_bodies_aux(Body_Conj_1, Bodies_Conj_1),
	split_disjunctions_in_bodies_aux(Body_Conj_2, Bodies_Conj_2),
	cartesian_product_of_lists(Bodies_Conj_1, Bodies_Conj_2, Bodies).

split_disjunctions_in_bodies_aux(Body, [Body_Result_1, Body_Result_2]) :- 
	goal_is_disjunction(Body, Body_Disj_1, Body_Disj_2), !,
	split_disjunctions_in_bodies_aux(Body_Disj_1, Body_Result_1),
	split_disjunctions_in_bodies_aux(Body_Disj_2, Body_Result_2).

split_disjunctions_in_bodies_aux(Body, [[Body]]). % Goal is something else.

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
	debug_msg(0, 'store_head_and_bodies_info(Head, Bodies) ', store_head_and_bodies_info(Head, Bodies)),
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
	debug_msg(0, 'assertz_fact ', assertz_fact(Functor)).

% Retrieves a list with name List_Name and argument List.
retrieve_list_of(List_Name, List) :-
	functor_local(Functor, List_Name, 1, [List]),
	retract_fact(Functor), !,
	debug_msg(0, 'retract_fact ', retract_fact(Functor)).
retrieve_list_of(_List_Name, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

trans_sent_eof(Cls_Out, _SourceFileName) :-
	list_name_for_cneg_predicates(List_Name_1),
	retrieve_list_of(List_Name_1, List_Of_Preds),
	debug_msg_list(0, 'List_Of_Preds', List_Of_Preds),
	debug_msg(0, 'trans_sent_eof', generate_cneg_main_cls(List_Of_Preds, [end_of_file], Cls_1)),
	generate_cneg_main_cls(List_Of_Preds, [end_of_file], Cls_1),
	debug_msg_list(0, 'Cls_1', Cls_1),
	!, %Backtracking forbiden.
	list_name_for_cneg_heads_and_bodies(List_Name_2),
	retrieve_list_of(List_Name_2, List_Of_H_and_B),
	debug_msg_list(0, 'List_Of_H_and_B', List_Of_H_and_B),
	generate_cneg_disjs(List_Of_H_and_B, Cls_1, Cls_2),
	debug_msg_list(0, 'Cls_2', Cls_2),
	!, %Backtracking forbiden.
	generate_double_negation_main_cls(List_Of_Preds, Cls_2, Cls_3),
	debug_msg_list(0, 'Cls_3', Cls_3),
	!, %Backtracking forbiden.
	generate_double_negation_clauses(List_Of_H_and_B, Cls_3, Cls_Out),
	debug_msg_nl(0), debug_msg_nl(0),
	debug_msg_list(0, 'Cls_Out', Cls_Out),
	debug_msg_nl(0), debug_msg_nl(0), 
	!. %Backtracking forbiden.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%generate_main_cls(List_Of_Preds, Cls_1).
generate_cneg_main_cls([], Cls, Cls) :- !.
generate_cneg_main_cls([(Name, Arity, Counter) | List_Of_Preds], Cls_In, Cls_Out) :- !,
	debug_msg(0, 'generate_cneg_main_cls :: (Name, Arity, Counter)', (Name, Arity, Counter)),
	generate_cneg_main_cl(Name, Arity, Counter, Main_Cl, Aux_Cl),
	debug_msg(0, 'generate_cneg_main_cls :: (Main_Cl, Aux_Cl)', (Main_Cl, Aux_Cl)),
	!, %Backtracking forbiden.
	generate_cneg_main_cls(List_Of_Preds, [Main_Cl |[Aux_Cl | Cls_In]], Cls_Out).

generate_cneg_main_cl(Name, Arity, Counter, Main_Cl, Aux_Cl) :-
	cneg_main_and_aux_cl_names(Name, Main_Cl_Name, Aux_Cl_Name),	
	New_Arity is Arity + 4,

	% Generate the main clause.
	functor_local(Main_Cl, ':-', 2, [Head_Main_Cl |[Aux_Cl_Call]]), 

	functor_local(Head_Main_Cl, Main_Cl_Name, Arity, _Args_Head),
	functor_local(Aux_Cl_Call, Aux_Cl_Name, New_Arity, _Args_Aux_Cl_Call), 
	copy_args(Arity, Head_Main_Cl, Aux_Cl_Call),
	adjust_args_for_status(New_Arity, Aux_Cl_Call, Status),
	status_operation(Status, [], _UQV_Aux, 'fail', 'true'), 

	% Generate the auxiliary clause.
	functor_local(Aux_Cl, ':-', 2, [Head_Aux_Cl |[ Body_Aux_Cl ]]), 

	functor_local(Head_Aux_Cl, Aux_Cl_Name, New_Arity, _Args_SubCall),
	adjust_args_for_status(New_Arity, Head_Aux_Cl, Status_Head_Aux_Cl),

	% We need to copy the args from the aux functor to the aux_i functors.
	head_aux_cl_info(Info_Aux_Cl, Counter, Head_Aux_Cl, Aux_Cl_Name, New_Arity, Arity),
	generate_cneg_conj(1, Info_Aux_Cl, Body_Aux_Cl, Status_Head_Aux_Cl).
%	generate_subcalls_2(1, Info_Aux_Cl, Body_Aux_Cl_2, Status_Head_Aux_Cl),
%	Body_Aux_Cl = ((Body_Aux_Cl_1) ; (Body_Aux_Cl_2)). 

head_aux_cl_info(head_aux_cl_info_aux(Counter, Head_Aux_Cl, Aux_Cl_Name, New_Arity, Arity), 
	Counter, Head_Aux_Cl, Aux_Cl_Name, New_Arity, Arity).

adjust_args_for_status(Arity, Functor, Status) :-
	status_operation(Status, UQV_In, UQV_Out, Allowed_To_Fail, Result),
	arg(Arity, Functor, Result), 
	Arity_2 is Arity -1,
	arg(Arity_2, Functor, Allowed_To_Fail),
	Arity_3 is Arity_2 -1,
 	arg(Arity_3, Functor, UQV_Out), 
	Arity_4 is Arity_3 -1,
 	arg(Arity_4, Functor, UQV_In).

cneg_main_and_aux_cl_names(Name, Main_Cl_Name, Aux_Cl_Name) :-
	name(Name, Name_String),
	append("cneg_", Name_String, Main_Cl_String),
	name(Main_Cl_Name, Main_Cl_String),
	
	append(Main_Cl_String, "_aux", Aux_Cl_String),
	name(Aux_Cl_Name, Aux_Cl_String).

generate_cneg_conj(Index, Info_Aux_Cl, 'fail', Status_In) :- 
	head_aux_cl_info(Info_Aux_Cl, Counter, _Head_Aux_Cl, _Aux_Cl_Name, _New_Arity, _Arity),
	Index > Counter, !,
	status_operation(Status_In, UQV_In, UQV_In, _Allowed_To_Fail, 'fail').

generate_cneg_conj(Index, Info_Aux_Cl, Body, Status_In) :-
	head_aux_cl_info(Info_Aux_Cl, Counter, Head_Aux_Cl, Aux_Cl_Name, New_Arity, Arity),
	status_operation(Status_In, UQV_In, UQV_Out, Allowed_To_Fail, Result),
	generate_name_from_counter(Index, Aux_Cl_Name, SubCall_Name),

	functor_local(SubCall, SubCall_Name, New_Arity, _Args),
	copy_args(Arity, Head_Aux_Cl, SubCall),
	adjust_args_for_status(New_Arity, SubCall, Status_Aux),
	status_operation(Status_Aux, UQV_In, UQV_Aux, Allowed_To_Fail, Result_Aux),

	(
	    (   % Optimizations for the last one.
		Index == Counter,
		UQV_Aux = UQV_Out,
		Result = Result_Aux,
		Body = (SubCall)
	    )
	;
	    (
		Index < Counter,
		test_for_true(Test_For_True, Result_Aux),
		test_for_fail(Test_For_Fail, Result_Aux),
		generate_equality(Op_1, UQV_Out, UQV_Aux),
		generate_equality(Op_2, Result, Result_Aux),
		Ops_When_Fail=(Test_For_Fail, (Op_1, Op_2)),
		Body = (SubCall, (Ops_When_Fail ; (Test_For_True, MoreBody))),
		status_operation(Status_Out, UQV_Aux, UQV_Out, Allowed_To_Fail, Result),
		NewIndex is Index + 1,
		generate_cneg_conj(NewIndex, Info_Aux_Cl, MoreBody, Status_Out)
	    )
	).

generate_name_from_counter(Counter, Aux_Cl_Name, New_Name) :-
	name(Aux_Cl_Name, String_1),
	name(Counter, String_2), 
	append("_", String_2, String_3),
	append(String_1, String_3, String), 
	name(New_Name, String).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 1- We translate the names of the predicates by using the respective counters, 
%     and we add 5 variables for UQV_In, UQV_Out, Allowed_To_Fail, Results_In, Results_Out.
% 2- We convert:
%      2.1- the unifications in heads in equalities in the bodies.
%      2.2- the subcalls to other predicates are translated into prodicates we've translated.
%      2.3- vars for freevars and continuation vars are inserted at this point.

%generate_cneg_disjs(List_Of_H_and_B, Cls_2).
generate_cneg_disjs([], Cls_In, Cls_In).
generate_cneg_disjs([(Head, Body, Counter) | List_Of_H_and_B], Cls_In, Cls_Out) :-
	debug_msg(0, 'generate_cneg_disjs_aux :: (Head, Body, Counter)', (Head, Body, Counter)),
	generate_cneg_disj(Head, Body, Counter, New_Cl), !,
	debug_msg(0, 'generate_cneg_disjs_aux :: New_Cl', New_Cl),
	% Recursive create the other clauses.
	generate_cneg_disjs(List_Of_H_and_B, [New_Cl | Cls_In], Cls_Out).

generate_cneg_disj(Head, Body, Counter, New_Cl) :-
	% We suppose the head has no unifications (we've removed them before).
	functor_local(Head, Name, Arity, _Args_Head), 
	% New head (new name, new arity, new args).
	New_Arity is Arity + 4,
	cneg_main_and_aux_cl_names(Name, _Main_Cl_Name, Aux_Cl_Name),
	generate_name_from_counter(Counter, Aux_Cl_Name, New_Name),
	functor_local(New_Head, New_Name, New_Arity, _Args_New_Head),
	copy_args(Arity, Head, New_Head),
	status_operation(Status_New_Head, UQV_In, UQV_Out, Allowed_To_Fail, Result),
	adjust_args_for_status(New_Arity, New_Head, Status_New_Head),

	% Determine which variables are in the body but are not in the head.
	cneg_aux:varsbag(New_Head, [], [], Vars_New_Head), 
	cneg_aux:varsbag(Body, Vars_New_Head, [], UQV_Body), 
	functor_local(Vars_Append, 'append', 3, [UQV_In |[UQV_Body |[ UQV_Tmp]]]),

	% negate_body_conjunction
	status_operation(Status, UQV_Tmp, UQV_Out, Allowed_To_Fail, Result),
	negate_body_conj(Body, Neg_Body, Status),
	% Build new clause.
	functor_local(New_Cl, ':-', 2, [New_Head | [(Vars_Append, Neg_Body)]]).

negate_body_conj([], (Op_1, Op_2), Status) :- !,
	status_operation(Status, UQV_In, UQV_Out, _Allowed_To_Fail, Result),
	generate_equality(Op_1, UQV_In, UQV_Out),
	generate_equality(Op_2, Result, 'fail').

negate_body_conj([Atom | Body], Negated_Body, Status_In) :-
	status_operation(Status_In, UQV_In, UQV_Out, Allowed_To_Fail, Result_In),
	status_operation(Status_Aux, UQV_In, UQV_Aux, Allowed_To_Fail, Result_Aux),

	debug_msg(0, 'negate_atom :: Atom ', Atom),
	negate_atom(Atom, Negated_Atom, Status_Aux),  
	debug_msg(0, 'negate_atom :: Negated_Atom ', Negated_Atom),

	test_for_true(Test_For_True, Result_Aux),
	test_for_fail(Test_For_Fail, Result_Aux),
	generate_equality(Op_1, UQV_Out, UQV_Aux),
	generate_equality(Op_2, Result_In, Result_Aux),

	Ops_When_True = (Test_For_True, (Op_1, Op_2)),
	Ops_When_Fail = (Test_For_Fail, New_Negated_Body),

	Negated_Body = (Negated_Atom, (Ops_When_True ; Ops_When_Fail)),

	status_operation(Status_Out, UQV_Aux, UQV_Out, Allowed_To_Fail, Result_In),
	negate_body_conj(Body, New_Negated_Body, Status_Out).

% negate_atom(Atom, Negated_Atom, UQV_In, UQV_Out, Results_In, Results_Out).
negate_atom(Atom, Neg_Atom, Status) :-
	goal_is_equality(Atom, A_Left, A_Right), !,
	functor_local(Neg_Atom, 'cneg_diseq', 6, [A_Left |[A_Right | Status]]).

negate_atom(Atom, Neg_Atom, Status) :-
	goal_is_disequality(Atom, A_Left, A_Right, _FreeVars), !,
	functor_local(Neg_Atom, 'cneg_eq', 6, [A_Left |[A_Right | Status]]).

negate_atom(Atom, Neg_Atom, Status_In) :-
	functor_local(Atom, 'cneg', 2, [UQV |[ Arg ]]), !,
	status_operation(Status_In, UQV_In, UQV_Out, Allowed_To_Fail, Result_In),
	functor_local(Difference, 'varsbag_difference', 3, [UQV_In |[ UQV |[UQV_Aux]]]),
	Neg_Atom = (Difference, Neg_Atom_Aux),
	status_operation(Status_Aux, UQV_Aux, UQV_Out, Allowed_To_Fail, Result_In),
	double_negation_atom(Arg, Neg_Atom_Aux, Status_Aux).

negate_atom(Atom, Neg_Atom, Status) :-
	functor_local(Atom, Name, Arity, Args), !,
	cneg_main_and_aux_cl_names(Name, _Main_Cl_Name, Aux_Cl_Name),
	New_Arity is Arity + 4, 
	append(Args, Status, New_Args),
	functor_local(Neg_Atom, Aux_Cl_Name, New_Arity, New_Args).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

double_negation_atom(Atom, DN_Atom, Status_In) :-
	goal_is_conjunction(Atom, Conj_1, Conj_2), !,
	status_operation(Status_In, UQV_In, UQV_Out, Allowed_To_Fail, Result_In),

	test_for_true(Test_For_True, Result_Aux),
	test_for_fail(Test_For_Fail, Result_Aux),
	generate_equality(Op_1, UQV_Out, UQV_Aux),
	generate_equality(Op_2, Result_In, Result_Aux),

	Ops_When_True = (Test_For_True, DN_Conj_2),
	Ops_When_Fail = (Test_For_Fail, (Op_1, Op_2)),
	DN_Atom = (DN_Conj_1, (Ops_When_Fail ; Ops_When_True)),

	status_operation(Status_C1, UQV_In, UQV_Aux, Allowed_To_Fail, Result_Aux),
	double_negation_atom(Conj_1, DN_Conj_1, Status_C1),
	status_operation(Status_C2, UQV_Aux, UQV_Out, Allowed_To_Fail, Result_In),
	double_negation_atom(Conj_2, DN_Conj_2, Status_C2).

double_negation_atom(Atom, DN_Atom, Status_In) :-
	goal_is_disjunction(Atom, Disj_1, Disj_2), !,
	status_operation(Status_In, UQV_In, UQV_Out, Allowed_To_Fail, Result_In),

	test_for_true(Test_For_True, Result_Aux),
	test_for_fail(Test_For_Fail, Result_Aux),
	generate_equality(Op_1, UQV_Out, UQV_Aux),
	generate_equality(Op_2, Result_In, Result_Aux),

	Ops_When_True = (Test_For_True, (Op_1, Op_2)),
	Ops_When_Fail = (Test_For_Fail, DN_Disj_2),
	DN_Atom = (DN_Disj_1, (Ops_When_True ; Ops_When_Fail)),

	status_operation(Status_D1, UQV_In, UQV_Aux, Allowed_To_Fail, Result_Aux),
	double_negation_atom(Disj_1, DN_Disj_1, Status_D1),
 	status_operation(Status_D2, UQV_Aux, UQV_Out, Allowed_To_Fail, Result_In),
	double_negation_atom(Disj_2, DN_Disj_2, Status_D2). 

double_negation_atom(Atom, DN_Atom, Status_In) :-
	functor_local(Atom, 'cneg', 2, [UQV |[ Arg ]]), !,
	status_operation(Status_In, UQV_In, UQV_Out, Allowed_To_Fail, Result_In),

	functor_local(Addition, 'varsbag_addition', 3, [UQV |[ UQV_In |[UQV_Aux]]]),
	DN_Atom = (Addition, Neg_Atom),
	status_operation(Status_Aux, UQV_Aux, UQV_Out, Allowed_To_Fail, Result_In),
	negate_atom(Arg, Neg_Atom, Status_Aux). % Problematic

double_negation_atom(Atom, (Addition, DN_Atom), Status_In) :-
	goal_is_disequality(Atom, A_Left, A_Right, UQV), !,
	status_operation(Status_In, UQV_In, UQV_Out, Allowed_To_Fail, Result_In),
	functor_local(Addition, 'varsbag_addition', 3, [UQV |[ UQV_In |[UQV_Aux]]]),
	status_operation(Status_Aux, UQV_Aux, UQV_Out, Allowed_To_Fail, Result_In),
	functor_local(DN_Atom, 'cneg_diseq', 6, [A_Left |[A_Right | Status_Aux]]).

double_negation_atom(Atom, DN_Atom, Status_In) :-
	goal_is_equality(Atom, A_Left, A_Right),
	functor_local(DN_Atom, 'cneg_eq', 6, [A_Left |[A_Right | Status_In]]).	

double_negation_atom(Atom, Neg_Atom, Status_In) :-
	functor_local(Atom, Name, Arity, Args_In), !,
	New_Arity is Arity + 4,
	append(Args_In, Status_In, Args_Out),
	generate_double_negation_name(Name, New_Name),
	functor_local(Neg_Atom, New_Name, New_Arity, Args_Out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

generate_double_negation_name(Name, New_Name) :-
	name(Name, String_Name),
	append("double_negation_", String_Name, String_New_Name),
	name(New_Name, String_New_Name).

generate_name_with_counter(Name, Counter, New_Name) :-
	name(Name, String_Name),
	name(Counter, String_Counter),
	append(String_Name, "_", String_Name_Tmp),
	append(String_Name_Tmp, String_Counter, String_New_Name),
	name(New_Name, String_New_Name).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

generate_double_negation_clauses(List_Of_H_and_B, Cls_In, Cls_Out) :-
	debug_msg(0, 'generate_double_negation_clauses :: List_Of_H_and_B, Cls_In', (List_Of_H_and_B, Cls_In)),
	generate_dn_cls(List_Of_H_and_B, Cls_In, Cls_Out).

% generate_dnb(List_Of_H_and_B, Cls_In, Cls_Out) :-
generate_dn_cls([], Cls_In, Cls_In).
generate_dn_cls([(Head, Body, Counter) | List_Of_H_and_B], Cls_In, Cls_Out) :-
	debug_msg(0, 'generate_dn_cls :: (Head, Body, Counter)', (Head, Body, Counter)),
	generate_dn_cl(Head, Body, Counter, New_Cl), !,
	debug_msg(0, 'generate_dn_cls :: New_Cl', New_Cl),
	% Recursive create the other clauses.
	generate_dn_cls(List_Of_H_and_B, [New_Cl | Cls_In], Cls_Out).

generate_dn_cl(Head, Body, Counter, New_Cl) :-
	functor_local(Head, Name, Arity, _Args),
	generate_double_negation_name(Name, Aux_Name),
	generate_name_with_counter(Aux_Name, Counter, New_Name),
	New_Arity is Arity + 4,
	functor_local(New_Head, New_Name, New_Arity, _New_Args),
	copy_args(Arity, Head, New_Head),
	adjust_args_for_status(New_Arity, New_Head, Status),
	generate_dn_body(Body, Head, Counter, Status, New_Body),
	functor_local(New_Cl, ':-', 2, [New_Head |[New_Body]]).

generate_dn_body([], _Head, _Counter, Status_In, (Op_1, Op_2)) :-
	status_operation(Status_In, UQV_In, UQV_Out, _Allowed_To_Fail, Result_In),
	generate_equality(Op_1, UQV_In, UQV_Out),
	generate_equality(Op_2, Result_In, 'true').

generate_dn_body([Conj_1 | Body], Head, Counter, Status_In, DN_Conjunction) :-
	status_operation(Status_In, UQV_In, UQV_Out, Allowed_To_Fail, Result_In),

	test_for_true(Test_For_True, Result_Aux),
	test_for_fail(Test_For_Fail, Result_Aux),
	generate_equality(Op_1, UQV_Out, UQV_Aux),
	generate_equality(Op_2, Result_In, Result_Aux),

	Ops_When_True = (Test_For_True, DN_Body),
	Ops_When_Fail = (Test_For_Fail, (Op_1, Op_2)),
	DN_Conjunction = (DN_Conj_1, (Ops_When_Fail ; Ops_When_True)),

	status_operation(Status_C1, UQV_In, UQV_Aux, Allowed_To_Fail, Result_Aux),
	double_negation_atom(Conj_1, DN_Conj_1, Status_C1),
	status_operation(Status_C2, UQV_Aux, UQV_Out, Allowed_To_Fail, Result_In),
%	double_negation_atom(Conj_2, DN_Conj_2, Status_C2).
	generate_dn_body(Body, Head, Counter, Status_C2, DN_Body).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

generate_double_negation_main_cls([], Cls, Cls) :- !.
generate_double_negation_main_cls([(Name, Arity, Counter) | List_Of_Preds], Cls_In, Cls_Out) :-
	debug_msg(0, 'generate_double_negation_main_cls :: (Name, Arity, Counter)', (Name, Arity, Counter)),
	generate_double_negation_main_cl(Name, Arity, Counter, DN_Main_Cl),
	debug_msg(0, 'generate_double_negation_main_cls :: Main_Cl', DN_Main_Cl),
	!, %Backtracking forbiden.
	generate_double_negation_main_cls(List_Of_Preds, [DN_Main_Cl | Cls_In], Cls_Out).

generate_double_negation_main_cl(Head_Name, Arity, Counter, Main_Cl) :-
	generate_double_negation_name(Head_Name, New_Head_Name),
	functor_local(Main_Cl, ':-', 2, [Head |[ SubCalls ]]),
	New_Arity is Arity + 4,
	functor_local(Head, New_Head_Name, New_Arity, _Args_Head),
%	status_operation(Status, UQV_In, UQV_Out, Results_In, Results_Out),
	adjust_args_for_status(New_Arity, Head, Status),

	generate_double_negation_subcalls(Head, Arity, Status, 1, Counter, SubCalls).

generate_double_negation_subcalls(_Head, _Arity, _Status, Index, Counter, 'fail') :-
	Counter < Index, !. % Security check.

generate_double_negation_subcalls(Head, Arity, Status_In, Index, Counter, Ops) :-
	status_operation(Status_In, UQV_In, UQV_Out, Allowed_To_Fail, Result_In),
	Ops = (SubCall, (Ops_When_True ; Ops_When_Fail)), 

	Ops_When_True = (Test_For_True, (Op_1, Op_2)),
	Ops_When_Fail = (Test_For_Fail, More_Ops),

	test_for_true(Test_For_True, Result_Aux),
	test_for_fail(Test_For_Fail, Result_Aux),
	generate_equality(Op_1, UQV_Aux, UQV_Out),
	generate_equality(Op_2, Result_Aux, Result_In),

	% Obtain New_Arity and Head_Name of new Head.
	functor_local(Head, Head_Name, New_Arity, _Head_Args),

	% Generate SubCall.
	generate_name_with_counter(Head_Name, Index, New_Name),
	functor_local(SubCall, New_Name, New_Arity, _SubCall_Args),
	copy_args(Arity, Head, SubCall),
	status_operation(Status_Aux, UQV_In, UQV_Aux, Allowed_To_Fail, Result_Aux),
	adjust_args_for_status(New_Arity, SubCall, Status_Aux),

	status_operation(Status_Out, UQV_Aux, UQV_Out, Allowed_To_Fail, Result_In),
	New_Index is Index + 1,
	generate_double_negation_subcalls(Head, Arity, Status_Out, New_Index, Counter, More_Ops).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test_for_true(Test_For_True, Var) :-
	generate_equality(Test_For_True, Var, 'true').

test_for_fail(Test_For_True, Var) :-
	generate_equality(Test_For_True, Var, 'fail').

generate_equality(Equality, Term_1, Term_2) :-
	functor_local(Equality, '=', 2, [Term_1 |[Term_2]]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

status_operation([UQV_In |[UQV_Out |[Allowed_To_Fail |[Results]]]], 
	UQV_In, UQV_Out, Allowed_To_Fail, Results).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
