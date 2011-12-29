
:- module(cneg_tr_vpc,[generate_cneg_tr_vpc/4, cneg_main_and_aux_cl_names/3, 
	take_body_first_unification/2],[assertions]).
:- use_module(cneg_aux, _).
:- use_module(library(terms), _).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

generate_cneg_tr_vpc(List_Of_Preds, List_Of_H_and_B, Cls_In, Cls_Out) :-
	echo_msg(0, 'cneg_tr_vpc', '', 'trans_sent_eof', generate_cneg_main_cls(List_Of_Preds, Cls_In, Cls_1)),
	cneg_tr_generate_main_cls(List_Of_Preds, [end_of_file], Cls_1),
	echo_msg(0, 'cneg_tr_vpc', '', 'Cls_1', Cls_1),
	!, %Backtracking forbiden.
	cneg_tr_generate_cls_bodies(List_Of_H_and_B, Cls_1, Cls_2),
	echo_msg(0, 'cneg_tr_vpc', '', 'Cls_2', Cls_2),
	!, %Backtracking forbiden.
	cneg_tr_generate_double_neg_main_cls(List_Of_Preds, Cls_2, Cls_3),
	echo_msg(0, 'cneg_tr_vpc', '', 'Cls_3', Cls_3),
	!, %Backtracking forbiden.
	cneg_tr_generate_double_neg_bodies(List_Of_H_and_B, Cls_3, Cls_Out),
	echo_msg(0, 'cneg_tr_vpc', '', 'Cls_4', Cls_Out),
	!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%generate_main_cls(List_Of_Preds, Cls_1).
cneg_tr_generate_main_cls([], Cls, Cls) :- !.
cneg_tr_generate_main_cls([(Name, Arity, Counter) | List_Of_Preds], Cls_In, Cls_Out) :- !,
	echo_msg(0, 'cneg_tr_vpc', '', 'generate_cneg_main_cls :: (Name, Arity, Counter)', (Name, Arity, Counter)),
	cneg_tr_generate_main_cl(Name, Arity, Counter, Main_Cl, Aux_Cl),
	echo_msg(0, 'cneg_tr_vpc', '', 'generate_cneg_main_cls :: (Main_Cl, Aux_Cl)', (Main_Cl, Aux_Cl)),
	!, %Backtracking forbiden.
	cneg_tr_generate_main_cls(List_Of_Preds, [Main_Cl |[Aux_Cl | Cls_In]], Cls_Out).

cneg_tr_generate_main_cl(Name, Arity, Counter, Main_Cl, Aux_Cl) :-
	cneg_main_and_aux_cl_names(Name, Main_Cl_Name, Aux_Cl_Name),	
	generate_double_negation_name(Name, Main_DN_Name),

	New_Arity is Arity + 2, % Arity + GoalVars + Result

	% Generate the main clause.
	functor_local(Main_Cl, ':-', 2, [Main_Cl_Head |[Main_Cl_Body]]), 

	Main_Cl_Body = (goalvars(Main_Cl_Head, GoalVars), Aux_Cl_Call),
	functor(Main_Cl_Head, Main_Cl_Name, Arity),
	functor(Aux_Cl_Call, Aux_Cl_Name, New_Arity), 
	copy_args(Arity, Main_Cl_Head, Aux_Cl_Call),
	args_for_cneg_tr(New_Arity, Aux_Cl_Call, GoalVars, 'true'),

	% Generate the auxiliary clause.
	functor_local(Aux_Cl, ':-', 2, [Aux_Cl_Head |[ Aux_Cl_Body ]]), 
	functor(Aux_Cl_Head, Aux_Cl_Name, New_Arity),
	args_for_cneg_tr(New_Arity, Aux_Cl_Head, GoalVars, Result),
	Aux_Cl_Body = (Aux_Cl_Body_1 ; Aux_Cl_Body_2),

	% We need to copy the args from the aux functor to the aux_i functors.
	auxiliary_info(Info_1, Counter, Aux_Cl_Head, Aux_Cl_Name, New_Arity, Arity),
	generate_auxiliary_conj(1, Info_1, GoalVars, Result, Aux_Cl_Body_1),

	auxiliary_info(Info_2, Counter, Aux_Cl_Head, Main_DN_Name, New_Arity, Arity),
	generate_auxiliary_disj(1, Info_2, GoalVars, Result, Aux_Cl_Body_2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

auxiliary_info(head_aux_cl_info_aux(Counter, Head_Aux_Cl, Aux_Cl_Name, New_Arity, Arity), 
	Counter, Head_Aux_Cl, Aux_Cl_Name, New_Arity, Arity).

args_for_cneg_tr(Arity, Functor, GoalVars, Result) :-
	arg(Arity, Functor, Result), 
	Arity_2 is Arity -1,
	arg(Arity_2, Functor, GoalVars).

cneg_main_and_aux_cl_names(Name, Main_Cl_Name, Aux_Cl_Name) :-
	name(Name, Name_String),
	append("cneg_", Name_String, Main_Cl_String),
	name(Main_Cl_Name, Main_Cl_String),
	
	append(Main_Cl_String, "_aux", Aux_Cl_String),
	name(Aux_Cl_Name, Aux_Cl_String).

generate_name_from_counter(Counter, Aux_Cl_Name, New_Name) :-
	name(Aux_Cl_Name, String_1),
	name(Counter, String_2), 
	append("_", String_2, String_3),
	append(String_1, String_3, String), 
	name(New_Name, String).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

generate_auxiliary_conj(Index, Aux_Info, _GoalVars, 'fail', 'fail') :- 
	auxiliary_info(Aux_Info, Counter, _Head_Aux_Cl, _Aux_Cl_Name, _New_Arity, _Arity),
	Index > Counter, !.

generate_auxiliary_conj(Index, Aux_Info, GoalVars, Result, Body) :-
	auxiliary_info(Aux_Info, Counter, Aux_Cl_Head, Aux_Cl_Name, New_Arity, Arity),
	generate_name_from_counter(Index, Aux_Cl_Name, SubCall_Name),

	functor(SubCall, SubCall_Name, New_Arity),
	copy_args(Arity, Aux_Cl_Head, SubCall),
	args_for_cneg_tr(New_Arity, SubCall, GoalVars, Result_Aux),
	test_for_true(Test_For_True, Result_Aux),

	(
	    (   % Optimizations for the last one.
		Index == Counter,
		test_for_true(Result_Is_True, Result),
		Body = ((SubCall, Test_For_True), Result_Is_True)
	    )
	;
	    (
		Index < Counter,
		Body = (SubCall, (Test_For_True, MoreBody)),
		NewIndex is Index + 1,
		generate_auxiliary_conj(NewIndex, Aux_Info, GoalVars, Result, MoreBody)
	    )
	).

generate_auxiliary_disj(Index, Aux_Info, _GoalVars, 'fail', 'fail') :- 
	auxiliary_info(Aux_Info, Counter, _Head_Aux_Cl, _Aux_Cl_Name, _New_Arity, _Arity),
	Index > Counter, !.

generate_auxiliary_disj(Index, Aux_Info, GoalVars, Result, Body) :-
	auxiliary_info(Aux_Info, Counter, Aux_Cl_Head, Aux_Cl_Name, New_Arity, Arity),
	generate_name_from_counter(Index, Aux_Cl_Name, SubCall_Name),

	functor(SubCall, SubCall_Name, New_Arity),
	copy_args(Arity, Aux_Cl_Head, SubCall),
	args_for_cneg_tr(New_Arity, SubCall, GoalVars, Result_Aux),

	% Prepare tests.
	test_for_true(Test_For_True, Result_Aux),

	(
	    (   % Optimizations for the last one.
		Index == Counter,
		test_for_fail(Result_Is_Fail, Result),
		Body = ((SubCall, Test_For_True), Result_Is_Fail)
	    )
	;
	    (
		Index < Counter,
		Body = ((SubCall, Test_For_True) ; MoreBody),
		NewIndex is Index + 1,
		generate_auxiliary_disj(NewIndex, Aux_Info, GoalVars, Result, MoreBody)
	    )
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 1- We translate the names of the predicates by using the respective counters, 
%     and we add 5 variables for UQV_In, UQV_Out, Allowed_To_Fail, Results_In, Results_Out.
% 2- We convert:
%      2.1- the unifications in heads in equalities in the bodies.
%      2.2- the subcalls to other predicates are translated into prodicates we've translated.
%      2.3- vars for freevars and continuation vars are inserted at this point.

%cneg_tr_generate_cls_bodies(List_Of_H_and_B, Cls_2).
cneg_tr_generate_cls_bodies([], Cls_In, Cls_In).
cneg_tr_generate_cls_bodies([(Head, Body, Test, Counter) | List_Of_H_and_B], Cls_In, Cls_Out) :-
	echo_msg(0, 'cneg_tr_vpc', '', 'cneg_tr_generate_cls_bodies :: (Head, Body, Test, Counter)', (Head, Body, Test, Counter)),
	cneg_tr_generate_cl_body(Head, Body, Counter, New_Cl), !,
	echo_msg(0, 'cneg_tr_vpc', '', 'cneg_tr_generate_cls_bodies :: New_Cl', New_Cl),
	% Recursive create the other clauses.
	cneg_tr_generate_cls_bodies(List_Of_H_and_B, [New_Cl | Cls_In], Cls_Out).

cneg_tr_generate_cl_body(Head, Body, Counter, New_Cl) :-
	% We suppose the head has no unifications (we've removed them before).
	functor(Head, Name, Arity), 
	% New head (new name, new arity, new args).
	New_Arity is Arity + 2, % GoalVars + Result
	cneg_main_and_aux_cl_names(Name, _Main_Cl_Name, Aux_Cl_Name),
	generate_name_from_counter(Counter, Aux_Cl_Name, New_Name),
	functor(New_Head, New_Name, New_Arity),
	copy_args(Arity, Head, New_Head),
	args_for_cneg_tr(New_Arity, New_Head, GoalVars, Result),

	% Build new clause.
	functor(New_Cl, ':-', 2),
	arg(1, New_Cl, New_Head),
	arg(2, New_Cl, New_Body),

	% Aqui es donde se sabe si hara falta la negacion de Chan o no.
	% El problema es q las variables de la cabecera no la necesitan, 
	% por lo que hay que distinguir entre cabecera y cuerpo. 

	functor(Test_Cneg_RT, 'test_if_cneg_rt_needed', 4),
	arg(1, Test_Cneg_RT, GoalVars), 
	arg(2, Test_Cneg_RT, Body_First_Unification), 
	arg(3, Test_Cneg_RT, Body),
	arg(4, Test_Cneg_RT, Result_Aux),

	take_body_first_unification(Body, Body_First_Unification),
	test_for_true(Test_For_True, Result_Aux),
	test_for_fail(Test_For_Fail, Result_Aux),
	New_Body = (Test_Cneg_RT, (Test_For_True ; (Test_For_Fail, Neg_Body))),

	% negate_body_conjunction
	echo_msg(0, 'cneg_tr_vpc', '', 'cneg_tr_generate_cl_body :: negate_atom :: (Body, GoalVars, Result)', (Body, GoalVars, Result)),
	negate_atom(Body, GoalVars, Result, Neg_Body).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

take_body_first_unification(Body, Body) :-
	goal_is_equality(Body, _T_1, _T_2, _EQV, _UQV), !.
take_body_first_unification(Body, Body_First_Unification) :-
	goal_is_conjunction(Body, Conj_1, _Conj_2), !,
	take_body_first_unification(Conj_1, Body_First_Unification).
take_body_first_unification(Body, 'fail') :-
	echo_msg(1, 'cneg_tr_vpc', '', 'take_body_first_unification :: Impossible to determine for Body', (Body)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% negate_atom(Atom, Negated_Atom, UQV_In, UQV_Out, Results_In, Results_Out).
negate_atom(Atom, GoalVars, Result, Neg_Atom) :-
	goal_is_conjunction(Atom, Conj_1, Conj_2), !,

	test_for_true(Test_For_True, Result_Aux),
	test_for_fail(Test_For_Fail, Result_Aux),
	generate_equality(Op, Result, Result_Aux),

	Ops_When_True = (Test_For_True, Op),
	Ops_When_Fail = (Test_For_Fail, Neg_Conj_2),
	Neg_Atom = (Neg_Conj_1, (Ops_When_True ; Ops_When_Fail)),

	negate_atom(Conj_1, GoalVars, Result_Aux, Neg_Conj_1),
	negate_atom(Conj_2, GoalVars, Result, Neg_Conj_2).

negate_atom(Atom, GoalVars, Result, Neg_Atom) :-
	goal_is_disjunction(Atom, Disj_1, Disj_2), !,

	test_for_true(Test_For_True, Result_Aux),
	test_for_fail(Test_For_Fail, Result_Aux),
	generate_equality(Op, Result, Result_Aux),

	Ops_When_True = (Test_For_True, Neg_Disj_2),
	Ops_When_Fail = (Test_For_Fail, Op),
	Neg_Atom = (Neg_Disj_1, (Ops_When_Fail ; Ops_When_True)),

	negate_atom(Disj_1, GoalVars, Result_Aux, Neg_Disj_1),
	negate_atom(Disj_2, GoalVars, Result, Neg_Disj_2). 

negate_atom(Atom, GoalVars, Result, Neg_Atom) :-
	goal_is_equality(Atom, A_Left, A_Right, EQV, UQV), !,
	EQV = [], % No EQV variables allowed !!!
	functor_local(Addition, 'varsbag_addition', 3, [GoalVars |[UQV |[ New_EQV ]]]), 
	functor_local(Varsbag, 'varsbag', 4, [(A_Left, A_Right) |[New_EQV |[ [] |[ New_UQV]]]]),
	functor_local(Neg_Eq, 'diseq_euqv_adv', 5, [A_Left |[A_Right |[ New_EQV |[ New_UQV |[Result]]]]]),
	Neg_Atom = ((Addition, Varsbag), Neg_Eq).

negate_atom(Atom, GoalVars, Result, Neg_Atom) :-
	goal_is_disequality(Atom, A_Left, A_Right, EQV, UQV), !,
	EQV = [], % No EQV variables allowed !!!
	functor_local(Addition, 'varsbag_addition', 3, [GoalVars |[ UQV |[ New_EQV ]]]), 
	functor_local(Varsbag, 'varsbag', 4, [(A_Left, A_Right) |[ New_EQV |[ [] |[ New_UQV]]]]),
	functor_local(Neg_Diseq, 'eq_euqv_adv', 5, [A_Left |[A_Right |[ New_EQV |[ New_UQV |[Result]]]]]), % New_UQV = []
	Neg_Atom = ((Addition, Varsbag), Neg_Diseq).


negate_atom(Atom, GoalVars, Result, Neg_Atom) :-
	goal_is_negation(Atom, UQV, SubGoal, _Proposal), !,
	functor_local(Op_Append, 'append', 3, [UQV |[ GoalVars |[New_GoalVars]]]),
	Neg_Atom = (Op_Append, Neg_Atom_Aux),
	double_negation_atom(SubGoal, New_GoalVars, Result, Neg_Atom_Aux).

negate_atom(Atom, GoalVars, Result, Neg_Atom) :-
	goal_is_not_conj_disj_eq_diseq_dneg(Atom), !,
	functor(Atom, Name, Arity), !,
	cneg_main_and_aux_cl_names(Name, _Main_Cl_Name, Aux_Cl_Name),
	New_Arity is Arity + 2, 
	functor(Neg_Atom, Aux_Cl_Name, New_Arity),
	args_for_cneg_tr(New_Arity, Neg_Atom, GoalVars, Result),
	copy_args(Arity, Atom, Neg_Atom).

negate_atom(Atom, GoalVars, Result, Atom) :-
	echo_msg(1, 'cneg_tr_vpc', '', 'negate_atom(Atom, GoalVars, Result)', negate_atom(Atom, GoalVars, Result)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

double_negation_atom(Atom, GoalVars, Result, DN_Atom) :-
	goal_is_conjunction(Atom, Conj_1, Conj_2), !,

	test_for_true(Test_For_True, Result_Aux),
	test_for_fail(Test_For_Fail, Result_Aux),
	generate_equality(Op, Result, Result_Aux),

	Ops_When_True = (Test_For_True, DN_Conj_2),
	Ops_When_Fail = (Test_For_Fail, Op),
	DN_Atom = (DN_Conj_1, (Ops_When_Fail ; Ops_When_True)),

	double_negation_atom(Conj_1, GoalVars, Result_Aux, DN_Conj_1),
	double_negation_atom(Conj_2, GoalVars, Result, DN_Conj_2).

double_negation_atom(Atom, GoalVars, Result, DN_Atom) :-
	goal_is_disjunction(Atom, Disj_1, Disj_2), !,

	test_for_true(Test_For_True, Result_Aux),
	test_for_fail(Test_For_Fail, Result_Aux),
	generate_equality(Op, Result, Result_Aux),

	Ops_When_True = (Test_For_True, Op),
	Ops_When_Fail = (Test_For_Fail, DN_Disj_2),
	DN_Atom = (DN_Disj_1, (Ops_When_True ; Ops_When_Fail)),

	double_negation_atom(Disj_1, GoalVars, Result_Aux, DN_Disj_1),
	double_negation_atom(Disj_2, GoalVars, Result, DN_Disj_2). 

double_negation_atom(Atom, GoalVars, Result, DN_Atom) :-
	goal_is_negation(Atom, _Unconfigured_UQV, SubGoal, _Proposal), !, 
	negate_atom(SubGoal, GoalVars, Result, DN_Atom). % Problematic

double_negation_atom(Atom, GoalVars, Result, DN_Atom) :-
	goal_is_disequality(Atom, A_Left, A_Right, EQV, UQV), !,
	EQV = [], % No EQV variables allowed !!!
	functor_local(DN_Atom, 'diseq_euqv_adv', 5, [A_Left |[A_Right |[GoalVars |[ UQV |[Result ]]]]]).

double_negation_atom(Atom, GoalVars, Result, DN_Atom) :-
	goal_is_equality(Atom, A_Left, A_Right, EQV, UQV), !,
	EQV = [], % No EQV variables allowed !!!
	functor_local(DN_Atom, 'eq_euqv_adv', 5, [A_Left |[A_Right |[GoalVars |[ UQV |[Result ]]]]]).	

double_negation_atom(Atom, GoalVars, Result, DN_Atom) :-
	goal_is_not_conj_disj_eq_diseq_dneg(Atom), !,
	functor(Atom, Name, Arity), 
	New_Arity is Arity + 2,
	generate_double_negation_name(Name, New_Name),
	functor(DN_Atom, New_Name, New_Arity),
	args_for_cneg_tr(New_Arity, DN_Atom, GoalVars, Result),
	copy_args(Arity, Atom, DN_Atom).

double_negation_atom(Atom, GoalVars, Result, Atom) :-
	echo_msg(1, 'cneg_tr_vpc', '', 'double_negation_atom(Atom, GoalVars, Result)', negate_atom(Atom, GoalVars, Result)).

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

cneg_tr_generate_double_neg_bodies(List_Of_H_and_B, Cls_In, Cls_Out) :-
	echo_msg(0, 'cneg_tr_vpc', '', 'cneg_tr_generate_double_neg_bodies :: List_Of_H_and_B, Cls_In', (List_Of_H_and_B, Cls_In)),
	cneg_tr_generate_double_neg_bodies_aux(List_Of_H_and_B, Cls_In, Cls_Out).

% generate_dnb(List_Of_H_and_B, Cls_In, Cls_Out) :-
cneg_tr_generate_double_neg_bodies_aux([], Cls_In, Cls_In).
cneg_tr_generate_double_neg_bodies_aux([(Head, Body, Test, Counter) | List_Of_H_and_B], Cls_In, Cls_Out) :-
	echo_msg(0, 'cneg_tr_vpc', '', 'cneg_tr_generate_double_neg_body :: (Head, Body, Counter)', (Head, Body, Test, Counter)),
	cneg_tr_generate_double_neg_body(Head, Body, Counter, New_Cl), !,
	echo_msg(0, 'cneg_tr_vpc', '', 'cneg_tr_generate_double_neg_body :: New_Cl', New_Cl),
	% Recursive create the other clauses.
	cneg_tr_generate_double_neg_bodies_aux(List_Of_H_and_B, [New_Cl | Cls_In], Cls_Out).

cneg_tr_generate_double_neg_body(Head, Body, Counter, New_Cl) :-
	functor_local(Head, Name, Arity, _Args),
	generate_double_negation_name(Name, Aux_Name),
	generate_name_with_counter(Aux_Name, Counter, New_Name),
	New_Arity is Arity + 2,
	functor_local(New_Head, New_Name, New_Arity, _New_Args),
	copy_args(Arity, Head, New_Head),
	args_for_cneg_tr(New_Arity, New_Head, GoalVars, Result),

%	cneg_tr_generate_double_neg_body_aux(Body, Aux_Info, GoalVars, Result, New_Body),
	double_negation_atom(Body, GoalVars, Result, New_Body),
	functor_local(New_Cl, ':-', 2, [New_Head |[New_Body]]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cneg_tr_generate_double_neg_main_cls([], Cls, Cls) :- !.
cneg_tr_generate_double_neg_main_cls([(Name, Arity, Counter) | List_Of_Preds], Cls_In, Cls_Out) :-
	echo_msg(0, 'cneg_tr_vpc', '', 'cneg_tr_generate_double_neg_main_cls :: (Name, Arity, Counter)', (Name, Arity, Counter)),
	cneg_tr_generate_double_negation_main_cl(Name, Arity, Counter, DN_Main_Cl),
	echo_msg(0, 'cneg_tr_vpc', '', 'cneg_tr_generate_double_neg_main_cls :: Main_Cl', DN_Main_Cl),
	!, %Backtracking forbiden.
	cneg_tr_generate_double_neg_main_cls(List_Of_Preds, [DN_Main_Cl | Cls_In], Cls_Out).

cneg_tr_generate_double_negation_main_cl(Head_Name, Arity, Counter, Main_Cl) :-
	generate_double_negation_name(Head_Name, New_Head_Name),
	functor_local(Main_Cl, ':-', 2, [New_Head |[ SubCalls ]]),
	New_Arity is Arity + 2,
	functor(New_Head, New_Head_Name, New_Arity),
	args_for_cneg_tr(New_Arity, New_Head, GoalVars, Result),

	auxiliary_info(Aux_Info, Counter, New_Head, New_Head_Name, New_Arity, Arity),
	generate_double_negation_subcalls(1, Aux_Info, GoalVars, Result, SubCalls).

generate_double_negation_subcalls(Index, Aux_Info, _GoalVars, 'fail', 'fail') :-
	auxiliary_info(Aux_Info, Counter, _New_Head, _New_Head_Name, _New_Arity, _Arity),
	Counter < Index, !. % Security check.

generate_double_negation_subcalls(Index, Aux_Info, GoalVars, Result, SubCalls) :-
	auxiliary_info(Aux_Info, Counter, New_Head, New_Head_Name, New_Arity, Arity),
	Counter >= Index,

	SubCalls = (SubCall, (Ops_When_True ; Ops_When_Fail)), 
	Ops_When_True = (Test_For_True, Op),
	Ops_When_Fail = (Test_For_Fail, More_SubCalls),

	test_for_true(Test_For_True, Result_Aux),
	test_for_fail(Test_For_Fail, Result_Aux),
	generate_equality(Op, Result_Aux, Result),

	% Generate SubCall.
	generate_name_with_counter(New_Head_Name, Index, SubCall_Name),
	functor(SubCall, SubCall_Name, New_Arity),
	copy_args(Arity, New_Head, SubCall),
	args_for_cneg_tr(New_Arity, New_Head, GoalVars, Result_Aux),

	New_Index is Index + 1,
	generate_double_negation_subcalls(New_Index, Aux_Info, GoalVars, Result, More_SubCalls).

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

%status_operation([UQV_In |[UQV_Out |[Allowed_To_Fail |[Results]]]], 
%	UQV_In, UQV_Out, Allowed_To_Fail, Results).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
