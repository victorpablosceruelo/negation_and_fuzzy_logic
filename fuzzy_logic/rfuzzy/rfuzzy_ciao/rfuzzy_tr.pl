:- module(rfuzzy_tr,[rfuzzy_trans_sentence/3, rfuzzy_trans_clause/3],[]).

:- use_module(library(aggregates), [findall/3]).
:- use_module(library(terms),[copy_args/3]).
:- use_module(library('rfuzzy/rfuzzy_rt')).
:- include(library('clpqr-common/ops')).
:- include(library('rfuzzy/rfuzzy_ops')).

% Important info to be saved.
:- data predicate_definition/5.
:- data aggregators/1.
:- data sentences/2.
:- data defined_quantifiers_code/1.
% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

% translation_info(Category, Add_Args, Priority, Preffix_String)
% Priority = -1 means that it is not fixed.

translation_info('aggregator',             0, -1, "").
translation_info('defuzzification',       0, -1, "").
translation_info('function',                  1, -1, "").
translation_info('quantifier',                1, -1, "").

translation_info('crisp_rule',               0, -1, "").
translation_info('fuzzy_rule',              1, -1, "").

translation_info('crisp_rule_type',      0, -1, "rfuzzy_crisp_rule_type_").
translation_info('fuzzy_rule_type',     2, -1, "rfuzzy_fuzzy_rule_type_").

% For fuzzy rules
translation_info('fuzzy_rule_default_without_cond',   2, 0,        "rfuzzy_aux_").
translation_info('fuzzy_rule_default_with_cond',        2, 0.25,   "rfuzzy_aux_"). 
translation_info('fuzzy_rule_rule',                                2, 0.5,     "rfuzzy_aux_").
translation_info('fuzzy_rule_fuzzification',                  2, 0.75,   "rfuzzy_aux_").
translation_info('fuzzy_rule_db_value',                       2, 0.9,     "rfuzzy_aux_").
translation_info('fuzzy_rule_fact',                                2, 1,        "rfuzzy_aux_").
translation_info('fuzzy_rule_synonym',                        2, -1,       "rfuzzy_aux_").
translation_info('fuzzy_rule_antonym',                        2, -1,       "rfuzzy_aux_").
translation_info('fuzzy_rule_aux',                                2, -1,       "rfuzzy_aux_").
%translation_info('non_rfuzzy_fuzzy_rule', 0, -1,         "non_rfuzzy_fuzzy_rule").

% This produces unexpected results.
% translation_info(_X,                             _Y,               0, 0, 'no', 0,          "rfuzzy_error_error_error_").

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

save_fuzzy_rule_predicate_definition(Pred_Name, Pred_Arity, Pred_Type, Pred_Class, Selectors) :-
	print_msg('debug', 'save_fuzzy_rule_predicate_definition(Pred_Name, Pred_Arity, Pred_Type, Pred_Class)', (Pred_Name, Pred_Arity, Pred_Type, Pred_Class)),
	nonvar(Pred_Name), nonvar(Pred_Arity), nonvar(Pred_Class), nonvar(Pred_Type), nonvar(Selectors),

	% translate_predicate(Pred_Name, Pred_Arity, Pred_Class, New_Pred_Name, New_Pred_Arity).
	translate_predicate(Pred_Name, Pred_Arity, 'fuzzy_rule', Real_Pred_Name, Real_Pred_Arity),

	append_local(Pred_Type, ['rfuzzy_truth_value_type'], New_Pred_Type), 
	% save_predicate_definition(Pred_Name, Pred_Arity, Pred_Type, More_Info, IsNew)
	save_predicate_definition(Real_Pred_Name, Real_Pred_Arity, New_Pred_Type, [Pred_Class], Selectors),

	print_msg('debug', 'save_fuzzy_rule_predicate_definition', 'saved').

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

save_predicates_definition_list([], _Pred_Arity, _Pred_Type, _More_Info) :- !.
save_predicates_definition_list([Pred_Name | Pred_List], Pred_Arity, Pred_Type, More_Info) :-
	save_predicate_definition(Pred_Name, Pred_Arity, Pred_Type, More_Info, _IsNew), !,
	save_predicates_definition_list(Pred_List, Pred_Arity, Pred_Type, More_Info).

save_predicate_definition(Pred_Name, Pred_Arity, Pred_Type, More_Info, Selectors) :-
	print_msg('debug', 'save_predicate_definition(Pred_Name, Pred_Arity, Pred_Type, More_Info)', (Pred_Name, Pred_Arity, Pred_Type, More_Info)),
	check_save_predicate_definition_input(Pred_Name, Pred_Arity, Pred_Type, More_Info, Selectors),
	print_msg('debug', 'check_save_predicate_definition_input', 'ok'),
	(
	    (	 
		retract_fact(predicate_definition(Pred_Name, Pred_Arity, Old_Pred_Type, Old_More_Info, Old_Selectors)), !, % Retract last
		print_msg('debug', 'save_predicate_definition :: current', (Pred_Name, Pred_Arity, Old_Pred_Type, Old_More_Info, Old_Selectors)),
		append_local(More_Info, Old_More_Info, New_More_Info),
		append_types(Pred_Name, Pred_Arity, Pred_Type, Old_Pred_Type, New_Pred_Type),
		sets_union(Selectors, Old_Selectors, New_Selectors)
	    )
	;
	    (
		New_Pred_Type = [Pred_Type],
		New_More_Info = More_Info, 
		New_Selectors = Selectors
	    )
	), 
	assertz_fact(predicate_definition(Pred_Name, Pred_Arity, New_Pred_Type, New_More_Info, New_Selectors)),
	print_msg('debug', 'saved', save_predicate_definition(Pred_Name, Pred_Arity, New_Pred_Type, New_More_Info, New_Selectors)),
	!.

retrieve_predicate_info(Pred_Name, Pred_Arity, Pred_Type, Show_Error) :-
	print_msg('debug', 'retrieve_predicate_info(Pred_Name, Pred_Arity, Pred_Type, Show_Error)', retrieve_predicate_info(Pred_Name, Pred_Arity, Pred_Type, Show_Error)),
	(
	    (   predicate_definition(Pred_Name, Pred_Arity, Pred_Type, More_Info, Selectors), !,
		print_msg('debug', 'retrieved', retrieve_predicate_info(Pred_Name, Pred_Arity, Pred_Type, More_Info, Selectors))   
	    )
	;
	    (   Show_Error = 'no', !, 
		print_msg('debug', 'not retrieved', 'not showing error'),   fail  )
	;
	    (	Show_Error = 'true', !,
		print_msg('error', 'Predicate must be defined before use. Predicate ', Pred_Name/Pred_Arity), !, 
		fail
	    )
	).

retrieve_all_predicate_infos(Retrieved) :-
	findall((predicate_definition(Pred_Name, Pred_Arity, Pred_Type, More_Info, Selectors)),
	(retract_fact(predicate_definition(Pred_Name, Pred_Arity, Pred_Type, More_Info, Selectors))), Retrieved),
	 !.

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------
	
check_save_predicate_definition_input(Pred_Name, Pred_Arity, Pred_Type, More_Info, Selectors) :-
	print_msg('debug', 'check_save_predicate_definition_input(Pred_Name, Pred_Arity, Pred_Type, More_Info)', (Pred_Name, Pred_Arity, Pred_Type, More_Info)),
	( 
	    (	nonvar(Pred_Name), !    )
	;
	    (	var(Pred_Name), 
		print_msg('error', 'save_predicate_definition: Pred_Name cannot be a variable. Value', Pred_Name), !, fail
	    )
	),
	( 
	    (	nonvar(Pred_Arity), number(Pred_Arity), !	)
	;
	    (   nonvar(Pred_Arity),
		print_msg('error', 'save_predicate_definition: Pred_Arity must be a number. Value', Pred_Arity), !, fail
	    )
	; 
	    (	var(Pred_Arity), 
		print_msg('error', 'save_predicate_definition: Pred_Arity cannot be a variable. Value', Pred_Arity), !, fail
	    )
	),
	(
	    (	nonvar(Pred_Type), list(Pred_Type), !    )
	;
	    (	nonvar(Pred_Type),
		print_msg('error', 'save_predicate_definition: Pred_Type must be a list. Value', Pred_Type), !, fail
	    )
	;
	    (	var(Pred_Type),
		print_msg('error', 'save_predicate_definition: Pred_Type cannot be a variable. Value', Pred_Type), !, fail
	    )
	),
	(
	    (	nonvar(More_Info), list(More_Info), !    )
	;
	    (   nonvar(More_Info),
		print_msg('error', 'save_predicate_definition: More_Info must be a list. Value', More_Info), !, fail
	    )
	;
	    (	var(More_Info),
		print_msg('error', 'save_predicate_definition: More_Info cannot be a variable. Value', More_Info), !, fail
	    )
	),
	(
	    (	nonvar(Selectors), list(Selectors), !    )
	;
	    (   nonvar(Selectors),
		print_msg('error', 'save_predicate_definition: Selectors must be a list. Value', Selectors), !, fail
	    )
	;
	    (	var(Selectors),
		print_msg('error', 'save_predicate_definition: Selectors cannot be a variable. Value', Selectors), !, fail
	    )
	),
	(
	    (
		print_msg('debug', 'check_pred_type_aux(Pred_Arity, Pred_Type)', (Pred_Arity, Pred_Type)),
		check_pred_type_aux(Pred_Arity, Pred_Type), !
	    )
	;
	    (
		print_msg('error', 'Types in type definition do not sum up the arity value. (Pred_Name, Pred_Arity, Pred_Type)', (Pred_Name, Pred_Arity, Pred_Type)), 
		!, fail
	    )
	), !.

check_pred_type_aux(0, []) :- !.
check_pred_type_aux(1, [_Pred_Type]) :- !.
check_pred_type_aux(Pred_Arity, [_Pred_Type|More]) :-
	New_Pred_Arity is Pred_Arity -1,
	check_pred_type_aux(New_Pred_Arity, More).

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

append_types(_Pred_Name, _Pred_Arity, Pred_Type, [], [Pred_Type]) :- !.
append_types(_Pred_Name, _Pred_Arity, Pred_Type, [Pred_Type|PTL_In], [Pred_Type|PTL_In]) :- !.
append_types(Pred_Name, Pred_Arity, Pred_Type_In, [Pred_Type|PTL_In], [Pred_Type|PTL_Out]) :-
	get_nth_element_from_list(Pred_Arity, Pred_Type_In, Last_Type),
	get_nth_element_from_list(Pred_Arity, Pred_Type, Last_Type), !,
	append_types(Pred_Name, Pred_Arity, Pred_Type_In, PTL_In, PTL_Out).
append_types(Pred_Name, Pred_Arity, Pred_Type_In, [Pred_Type|_PTL_In], _PTL_Out) :-
	print_msg('error', 'You can not define different types for the same predicate. (Pred_Name, Pred_Arity)', (Pred_Name, Pred_Arity)), 
	print_msg('error', 'Previous type', Pred_Type),
 	print_msg('error', 'New        type', Pred_Type_In),
	!, fail. 

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

rfuzzy_trans_clause(Arg1, Arg1, _Arg3) :- 
	print_msg('debug', 'trans_fuzzy_clause: arg1', Arg1).

rfuzzy_trans_sentence(Arg1, Arg2, Arg3) :- 
	print_msg_nl('debug'),
	print_msg('debug', 'rfuzzy_trans_sent: arg1', Arg1),
	rfuzzy_trans_sent_aux(Arg1, Arg2), !,
	print_msg('debug', 'rfuzzy_trans_sent: arg2', Arg2),
	print_msg('debug', 'rfuzzy_trans_sent: arg3', Arg3),
	print_msg_nl('debug').

rfuzzy_trans_sentence(Arg, Arg, FileName) :- 
	print_msg('error', 'Impossible to translate input. Input', Arg),
	print_msg('error', 'in FileName', FileName),
	print_msg_nl('debug'),
	print_info('debug', Arg), 
	print_msg_nl('debug'),
	print_msg_nl('debug').

print_info(Level, Sentence) :-
	print_msg(Level, 'print_info', Sentence),
	functor(Sentence, Name, Arity),
	print_msg(Level, 'print_info: (Name, Arity)', (Name, Arity)),
	Sentence=..[Name|Args],
	print_list_info(Level, Args).
print_list_info(Level, []) :- !,
	print_msg(Level, 'print_list_info', 'empty list').
print_list_info(Level, [Arg | Args]) :- !,
	print_info(Level, Arg),
	print_list_info(Level, Args).

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

% We need to evaluate the whole program at the same time.
% Note that eat_2 uses info supplied by eat_1 and 
% eat_3 uses info supplied by eat_1 and eat_2.	
rfuzzy_trans_sent_aux(end_of_file, Fuzzy_Rules_3):-
	!,
	retrieve_all_predicate_infos(All_Predicate_Infos),
	print_msg('debug', 'all_predicate_info', All_Predicate_Infos),
	build_auxiliary_clauses(All_Predicate_Infos, Fuzzy_Rules_1),
	generate_introspection_predicate(All_Predicate_Infos, Fuzzy_Rules_1, Fuzzy_Rules_2),
	add_auxiliar_code(Fuzzy_Rules_2, Fuzzy_Rules_3).

rfuzzy_trans_sent_aux(0, []) :- !, 
	activate_rfuzzy_debug,
	print_msg_nl('info'), print_msg_nl('info'), 
	print_msg('info', 'Rfuzzy (Ciao Prolog package to compile Rfuzzy programs into a pure Prolog programs)', 'compiling ...'),
	print_msg_nl('info'),
	% save_predicate_definition(Pred_Name, Pred_Arity, Pred_Type, More_Info, IsNew)
	save_predicate_definition('rfuzzy_any_type', 1, ['null'], [], _IsNew1),
	save_predicate_definition('rfuzzy_truth_value_type', 1, ['null'], [], _IsNew2),
	save_predicate_definition('rfuzzy_credibility_value_type', 1, ['null'], [], _IsNew3),
	save_predicate_definition('rfuzzy_predicate_type', 1, ['null'], [], _IsNew4),
	save_predicate_definition('rfuzzy_number_type', 1, ['null'], [], _IsNew5),
	save_predicate_definition('fnot', 2, ['rfuzzy_predicate_type', 'rfuzzy_truth_value_type'], [], _IsNew6),

	save_predicate_definition('rfuzzy_string_type', 1, ['null'], [], _IsNew11),
	save_predicate_definition('rfuzzy_integer_type', 1, ['null'], [], _IsNew12),
	save_predicate_definition('rfuzzy_float_type', 1, ['null'], [], _IsNew13),
	save_predicate_definition('rfuzzy_enum_type', 1, ['null'], [], _IsNew14),
	save_predicate_definition('rfuzzy_boolean_type', 1, ['null'], [], _IsNew15),
	save_predicate_definition('rfuzzy_datetime_type', 1, ['null'], [], _IsNew16),

	rfuzzy_defined_aggregators(Defined_Aggregators_List),
	Aggregators_Type = ['rfuzzy_truth_value_type', 'rfuzzy_truth_value_type', 'rfuzzy_truth_value_type'],
	save_predicates_definition_list(Defined_Aggregators_List, 3, Aggregators_Type, []),
	
	rfuzzy_compute_defined_operators(Compute_Defined_Operators),
	save_predicate_definition('rfuzzy_compute_defined_operators', 0, [], Compute_Defined_Operators, _IsNew7),

	rfuzzy_defined_quantifiers(Defined_Quantifiers_List),
	save_rfuzzy_quantifiers_list(Defined_Quantifiers_List, Defined_Quantifiers_Code),
	assertz_fact(defined_quantifiers_code(Defined_Quantifiers_Code)).

rfuzzy_trans_sent_aux((:-activate_rfuzzy_debug), []) :- !,
	activate_rfuzzy_debug.
rfuzzy_trans_sent_aux((:-Whatever), [(:-Whatever)]) :- !.
rfuzzy_trans_sent_aux(Sentence, Translation) :-
	translate(Sentence, Translation).

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

% Unconditional default
translate((rfuzzy_default_value_for(Pred_Functor, Fixed_Truth_Value)), Cls) :- !,
	print_msg('debug', 'translate :: rfuzzy_default_value_for(Pred_Functor, Fixed_Truth_Value) ', (Pred_Functor, Fixed_Truth_Value)),
	translate_rfuzzy_default_value_for(Pred_Functor, Fixed_Truth_Value, _No_Condition_or_Thershold, Cls).

% Conditional default (for condition or thershold).
translate((rfuzzy_default_value_for(Pred_Functor, Fixed_Truth_Value) if Condition_or_Thershold), Cls) :- !,
	print_msg('debug', 'translate :: rfuzzy_default_value_for(Pred_Functor, Fixed_Truth_Value, Condition_or_Thershold) ', (Pred_Functor, Fixed_Truth_Value, Condition_or_Thershold)),
	translate_rfuzzy_default_value_for(Pred_Functor, Fixed_Truth_Value, Condition_or_Thershold, Cls).

% Fuzzy facts.
translate((Head value Fixed_Truth_Value), (Pred_Functor :- Assign_Truth_Value_SubGoal)):-
	!, % If patter matching, backtracking forbiden.
	print_msg('debug', 'fact conversion :: IN ',(Head value Fixed_Truth_Value)),
	nonvar(Head), nonvar(Fixed_Truth_Value), number(Fixed_Truth_Value),

	functor(Head, Pred_Name, Pred_Arity),
	Pred_Class = 'fuzzy_rule_fact',
	% translate_predicate(Pred_Name, Pred_Arity, Pred_Class, New_Pred_Name, New_Pred_Arity).
	translate_predicate(Pred_Name, Pred_Arity, Pred_Class, New_Pred_Name, New_Pred_Arity),
	% predicate_to_functor(Pred_Name, Pred_Arity, Pred_Class, Pred_Functor, Truth_Value).
	predicate_to_functor(New_Pred_Name, New_Pred_Arity, Pred_Class, Pred_Functor, Truth_Value),

	generate_assign_truth_value_subgoal(Fixed_Truth_Value, Truth_Value, Assign_Truth_Value_SubGoal),
	print_msg('debug', 'fact conversion :: OUT ', (Pred_Functor :- Assign_Truth_Value_SubGoal)),
	!. % Backtracking forbidden.

% Although aggregators are just crisp predicates of arity 3, 
% we use the following to ensure programmers do not use as aggregators
% fuzzy predicates (of arity 3 too). An error like that is very difficult to find.
translate((rfuzzy_aggregator(Aggregator_Name/Aggregator_Arity, TV_In_1, TV_In_2, TV_Out) :- Code), [Translation]) :-
	!, % If patter matching, backtracking forbiden.
	nonvar(Aggregator_Name), number(Aggregator_Arity), Aggregator_Arity = 3,

	functor(Aggregator, Aggregator_Name, Aggregator_Arity),
	arg(1, Aggregator, TV_In_1),
	arg(2, Aggregator, TV_In_2),
	arg(3, Aggregator, TV_Out),
	Translation = (Aggregator :- Code),

	Aggregator_Type = ['rfuzzy_truth_value_type', 'rfuzzy_truth_value_type', 'rfuzzy_truth_value_type'],
	% save_predicate_definition(Pred_Name, Pred_Arity, Pred_Type, More_Info, IsNew)
	save_predicate_definition(Aggregator_Name, Aggregator_Arity, Aggregator_Type, [], _IsNew),
	!.

% function definition.
translate((Head :# (Lower_Bound, List, Upper_Bound)), Cls) :-
	!, % If patter matching, backtracking forbiden.
	nonvar(Head), nonvar(List),
	% list(Lista),
	print_msg('debug', '(Head :# (Lower_Bound, List, Upper_Bound)) ', (Head :# (Lower_Bound, List, Upper_Bound))),

	functor(Head, Pred_Name, 0),
	Pred_Arity = 1,
	Pred_Class = 'function',
	% translate_predicate(Pred_Name, Pred_Arity, Pred_Class, New_Pred_Name, New_Pred_Arity).
	translate_predicate(Pred_Name, Pred_Arity, Pred_Class, New_Pred_Name, New_Pred_Arity),
	% predicate_to_functor(Pred_Name, Pred_Arity, Pred_Class, Pred_Functor, Truth_Value).
	predicate_to_functor(New_Pred_Name, New_Pred_Arity, Pred_Class, Pred_Functor, Truth_Value),

	arg(1, Pred_Functor, X),
	build_straight_lines(X, Truth_Value, Lower_Bound, List, Upper_Bound, Body),

	Pred_Type = ['rfuzzy_number_type', 'rfuzzy_truth_value_type'],
	Other_Info = [('lower_bound', Lower_Bound), ('upper_bound', Upper_Bound)],
	% save_predicate_definition(Pred_Name, Pred_Arity, Pred_Type, More_Info, _IsNew)
	save_predicate_definition(New_Pred_Name, New_Pred_Arity, Pred_Type, Other_Info, _IsNew), 
	Cls = (Pred_Functor :- (Body, print_msg('debug', 'function_call', Pred_Functor))),
	print_msg('debug', '(Head :# (Lower_Bound, List, Upper_Bound)) -> Cls', Cls).

% Predicate type(s) definition (Class = database).
translate(rfuzzy_define_database(Pred_Name/Pred_Arity, Description), Cls):- !,
	translate_rfuzzy_define_database(Pred_Name, Pred_Arity, Description, Cls).

% Predicate type(s) definition (Class \== database).
translate(rfuzzy_type_for(Pseudo_Class, Pred_Name/Pred_Arity, Pred_Type), Cls):-
	!, % If patter matching, backtracking forbiden.
	print_msg('debug', 'translate_rfuzzy_type_for(Class, Pred_Name, Pred_Arity, Pred_Type)', (Pseudo_Class, Pred_Name, Pred_Arity, Pred_Type)),
	nonvar(Pseudo_Class), nonvar(Pred_Name), number(Pred_Arity), nonvar(Pred_Type), 
	(
	    (
		Pseudo_Class = 'crisp_rule', !, 
		translate_rfuzzy_type_for_crisp_rule(Pred_Name, Pred_Arity, Pred_Type, [], Cls)
	    )
	;
	    (
		Pseudo_Class = 'fuzzy_rule', !,
		% save_fuzzy_rule_predicate_definition(Pred_Name, Pred_Arity, Pred_Type, Pred_Class, Cls)
		save_fuzzy_rule_predicate_definition(Pred_Name, Pred_Arity, Pred_Type, 'fuzzy_rule_type', Cls)
	    )
	;
	    (
		print_msg('error', 'Valid classes: crisp_rule or fuzzy_rule. Not a valid class', Pseudo_Class), !, fail
	    )
	),
	print_msg('debug', 'translate_rfuzzy_type_for(Class, Pred_Name, Pred_Arity, Pred_Type)', (Pseudo_Class, Pred_Name, Pred_Arity, Pred_Type)),
	print_msg('debug', 'translate_rfuzzy_type_for(Cls)', Cls).

% rules with credibility:
translate(((Head cred (Cred_Op, Cred)) :~ Body), Translation):-
	!, % If patter matching, backtracking forbiden.
	print_msg('debug', '(Head cred (Cred_Op, Cred)) :~ Body)', ((Head cred (Cred_Op, Cred))  :~ Body)),
	translate_rule(Head, Cred_Op, Cred, Body, Translation).

% rules without credibility:
translate((Head :~ Body), Translation):-
	!, % If patter matching, backtracking forbiden.
	print_msg('debug', '(Head :~ Body)', (Head  :~ Body)),
	translate_rule(Head, 'prod', 1, Body, Translation).

translate(rfuzzy_synonym(Pred2_Functor, Pred_Functor, Cred_Op, Cred), Cls):-
	!,
	print_msg('debug', 'translate(rfuzzy_synonym(Pred2_Functor, Pred_Functor, Cred_Op, Cred))) ', rfuzzy_synonym(Pred2_Functor, Pred_Functor, Cred_Op, Cred)),
	nonvar(Pred2_Functor), nonvar(Pred_Functor), nonvar(Cred_Op), nonvar(Cred), number(Cred),
	test_aggregator_is_defined(Cred_Op, 'true'),
	functor(Pred2_Functor, Pred2_Name, 1), arg(1, Pred2_Functor, Type_Functor_1), nonvar(Type_Functor_1),
	functor(Pred_Functor, Pred_Name, 1), arg(1, Pred_Functor, Type_Functor_2), nonvar(Type_Functor_2),
	functor(Type_Functor_1, Type_Name, 0), functor(Type_Functor_2, Type_Name, 0), 

	% retrieve_predicate_info(Pred_Name, Pred_Arity, Pred_Type, Show_Error),
	retrieve_predicate_info(Pred2_Name, Pred2_Arity, Pred2_Type, 'true'),
	( 
	    (	memberchk_local([Type_Name, _Unused_Type_Name], Pred2_Type), !    )
	;
	    (	print_msg('error', 'The type is not correctly defined for the predicate', Pred2_Name), !, fail    )
	),

	Pred_Class = 'fuzzy_rule_synonym', Pred_Arity is Pred2_Arity - 1,
	% translate_predicate(Pred_Name, Pred_Arity, Pred_Class, New_Pred_Name, New_Pred_Arity).
	translate_predicate(Pred_Name, Pred_Arity, Pred_Class, New_Pred_Name, New_Pred_Arity),
	% predicate_to_functor(Pred_Name, Pred_Arity, Pred_Class, Pred_Functor, Truth_Value).
	predicate_to_functor(New_Pred_Name, New_Pred_Arity, Pred_Class, New_Pred_Functor, Truth_Value_Out),
	% translate_predicate(Pred_Name, Pred_Arity, Pred_Class, New_Pred_Name, New_Pred_Arity).
	translate_predicate(Pred2_Name, Pred_Arity, Pred_Class, New_Pred2_Name, New_Pred2_Arity),
	% predicate_to_functor(Pred_Name, Pred_Arity, Pred_Class, Pred_Functor, Truth_Value).
	predicate_to_functor(New_Pred2_Name, New_Pred2_Arity, Pred_Class, New_Pred2_Functor, Truth_Value_In),

	copy_args(Pred2_Arity, New_Pred_Functor, New_Pred2_Functor),
	functor(Credibility_Functor, Cred_Op, 3), 
	Credibility_Functor=..[Cred_Op, Truth_Value_In, Cred, Truth_Value_Out],

	Cl = (New_Pred_Functor :- New_Pred2_Functor, Credibility_Functor, (Truth_Value_Out .>=. 0, Truth_Value_Out .=<. 1)),
	% save_fuzzy_rule_predicate_definition(Pred_Name, Pred_Arity, Pred_Type, Pred_Class, Cls)
	save_fuzzy_rule_predicate_definition(Pred_Name, Pred_Arity, [Type_Name], Pred_Class, Cls_Aux),
	Cls = [ Cl | Cls_Aux ], 
	!.

translate(rfuzzy_antonym(Pred2_Functor, Pred_Functor, Cred_Op, Cred), Cls):-
	!,
	print_msg('debug', 'translate(rfuzzy_antonym(Pred2_Functor, Pred_Functor, Cred_Op, Cred)) ', rfuzzy_antonym(Pred2_Functor, Pred_Functor, Cred_Op, Cred)),
	nonvar(Pred_Functor), nonvar(Pred2_Functor), nonvar(Cred_Op), nonvar(Cred), number(Cred),
	test_aggregator_is_defined(Cred_Op, 'true'),
	functor(Pred2_Functor, Pred2_Name, 1), arg(1, Pred2_Functor, Type_Functor_1), nonvar(Type_Functor_1),
	functor(Pred_Functor, Pred_Name, 1), arg(1, Pred_Functor, Type_Functor_2), nonvar(Type_Functor_2),
	functor(Type_Functor_1, Type_Name, 0), functor(Type_Functor_2, Type_Name, 0), 

	% retrieve_predicate_info(Pred_Name, Pred_Arity, Pred_Type, Show_Error),
	retrieve_predicate_info(Pred2_Name, Pred2_Arity, Pred2_Type, 'true'),
	( 
	    (	memberchk_local([Type_Name, _Unused_Type_Name], Pred2_Type), !    )
	;
	    (	print_msg('error', 'The type is not correctly defined for the predicate', Pred2_Name), !, fail    )
	),

	Pred_Class = 'fuzzy_rule_antonym', Pred_Arity is Pred2_Arity - 1,
	% translate_predicate(Pred_Name, Pred_Arity, Pred_Class, New_Pred_Name, New_Pred_Arity).
	translate_predicate(Pred_Name, Pred_Arity, Pred_Class, New_Pred_Name, New_Pred_Arity),
	% predicate_to_functor(Pred_Name, Pred_Arity, Pred_Class, Pred_Functor, Truth_Value).
	predicate_to_functor(New_Pred_Name, New_Pred_Arity, Pred_Class, New_Pred_Functor, Truth_Value_Out),
	% translate_predicate(Pred_Name, Pred_Arity, Pred_Class, New_Pred_Name, New_Pred_Arity).
	translate_predicate(Pred2_Name, Pred_Arity, Pred_Class, New_Pred2_Name, New_Pred2_Arity),
	% predicate_to_functor(Pred_Name, Pred_Arity, Pred_Class, Pred_Functor, Truth_Value).
	predicate_to_functor(New_Pred2_Name, New_Pred2_Arity, Pred_Class, New_Pred2_Functor, Truth_Value_In),

	copy_args(Pred2_Arity, New_Pred_Functor, New_Pred2_Functor),
	functor(Credibility_Functor, Cred_Op, 3), 
	Credibility_Functor=..[Cred_Op, Truth_Value_Aux, Cred, Truth_Value_Out],

	Cl = (New_Pred_Functor :- New_Pred2_Functor, (Truth_Value_Aux .=. 1 - Truth_Value_In), Credibility_Functor, (Truth_Value_Out .>=. 0, Truth_Value_Out .=<. 1)),
	% save_fuzzy_rule_predicate_definition(Pred_Name, Pred_Arity, Pred_Type, Pred_Class, Cls)
	save_fuzzy_rule_predicate_definition(Pred_Name, Pred_Arity, [Type_Name], Pred_Class, Cls_Aux),
	Cls = [ Cl | Cls_Aux ], 
	!.

translate((rfuzzy_quantifier(Pred_Name/Pred_Arity, Var_In, Var_Out) :- Code), Translation) :- !,
	print_msg('debug', 'translate: rfuzzy_quantifier(Pred_Name/Pred_Arity)', rfuzzy_quantifier(Pred_Name/Pred_Arity)),
	save_rfuzzy_quantifiers_list([(Pred_Name, Pred_Arity, Var_In, Var_Out, Code)], Translation),
	print_msg('debug', 'translate: rfuzzy_quantifier(Pred_Name/Pred_Arity)', rfuzzy_quantifier(Pred_Name/Pred_Arity)).

% fuzzification:
translate(rfuzzy_define_fuzzification(Pred_Functor, Crisp_Pred_Functor, Funct_Pred_Name), Cls):-
	!, % If patter matching, backtracking forbiden.
	nonvar(Pred_Functor), nonvar(Crisp_Pred_Functor), nonvar(Funct_Pred_Name),
	print_msg('debug', 'translate: rfuzzy_define_fuzzification(Pred_Functor, Crisp_Pred_Functor, Funct_Pred_Name)', rfuzzy_define_fuzzification(Pred_Functor, Crisp_Pred_Functor, Funct_Pred_Name)),
	functor(Pred_Functor, Pred_Name, 1), 
	arg(1, Pred_Functor, Pred_Type_11_Functor), nonvar(Pred_Type_11_Functor),
	functor(Pred_Type_11_Functor, Pred_Type_11, 0), nonvar(Pred_Type_11),
	functor(Crisp_Pred_Functor, Crisp_Pred_Name, 1), 
	arg(1, Crisp_Pred_Functor, Pred_Type_21_Functor), nonvar(Pred_Type_21_Functor),
	functor(Pred_Type_21_Functor, Pred_Type_21, 0), nonvar(Pred_Type_21),
	Pred_Type_11 = Pred_Type_21,

	% retrieve_predicate_info(Pred_Name, Pred_Arity, Pred_Type, Show_Error) 
	retrieve_predicate_info(Crisp_Pred_Name,  2, Pred_Type_2, 'true'),
	memberchk_local([Pred_Type_21, Pred_Type_22], Pred_Type_2),
	retrieve_predicate_info(Funct_Pred_Name, 2, Pred_Type_3, 'true'),
	memberchk_local([Pred_Type_31, Pred_Type_32], Pred_Type_3),
	(
	    (	var(Pred_Type_22), 
		print_msg('error', 'Types for predicates used in fuzzification must be defined before', (Crisp_Pred_Name)), 
		!, fail    )
	;
	    (   nonvar(Pred_Type_22), !   )
	),
	(
	    (	(var(Pred_Type_31)   ;   var(Pred_Type_32)   ), 
		 print_msg('error', 'Types for predicates used in fuzzification must be defined before', (Funct_Pred_Name)), 
		 !, fail    )
	;	
	    (    nonvar(Pred_Type_31),    nonvar(Pred_Type_32),   !    )
	),
	(
	    (   Pred_Type_22 = 'rfuzzy_integer_type', !   )
	;
	    (   Pred_Type_22 = 'rfuzzy_float_type', !   )
	;
	    (
		print_msg('error', 'Type of predicate is not suitable for fuzzification', Crisp_Pred_Name), 
		print_msg('debug', 'Pred_Type_22', Pred_Type_22), 
		!, fail    
	    )
	),
	(
	    (
		Pred_Type_31 = 'rfuzzy_number_type',
		Pred_Type_32 = 'rfuzzy_truth_value_type',
		!
	    )
	;
	    (
		print_msg('error', 'Type of predicate is not suitable for fuzzification', Funct_Pred_Name), 
		print_msg('debug', Pred_Type_31, Pred_Type_32), 
		!, fail    
	    )
	),

	Pred_Class = 'fuzzy_rule_fuzzification', Pred_Arity = 1,
	print_msg('debug', 'rfuzzy_define_fuzzification :: (Pred_Class, Pred_Arity)', (Pred_Class, Pred_Arity)), 
	% translate_predicate(Pred_Name, Pred_Arity, Pred_Class, New_Pred_Name, New_Pred_Arity).
	translate_predicate(Pred_Name, Pred_Arity, Pred_Class, New_Pred_Name, New_Pred_Arity),
	% predicate_to_functor(Pred_Name, Pred_Arity, Pred_Class, Pred_Functor, Truth_Value).
	predicate_to_functor(New_Pred_Name, New_Pred_Arity, Pred_Class, New_Pred_Functor, Truth_Value),

	% We need to do here as in other translations, so 
	% it generates aux and main predicates for fuzzifications too.
	functor(New_Crisp_Pred_Functor, Crisp_Pred_Name, 2),
	functor(New_Funct_Pred_Functor, Funct_Pred_Name, 2),
	retrieve_predicate_info(Pred_Type_11, Type_Check_Arity, _Type_Check_Type, 'true'),
	functor(Type_Check_Functor, Pred_Type_11, Type_Check_Arity),
	functor(Type_Check_Aux, '=', 2),

	print_msg('debug', 'rfuzzy_define_fuzzification :: linking', (New_Pred_Functor, New_Crisp_Pred_Functor, New_Funct_Pred_Functor)), 
	arg(1, New_Pred_Functor, Input),
	arg(1, New_Crisp_Pred_Functor, Input),
	arg(2, New_Crisp_Pred_Functor, Crisp_Value),
	arg(1, New_Funct_Pred_Functor, Crisp_Value),
	arg(2, New_Funct_Pred_Functor, Truth_Value),
	arg(1, Type_Check_Aux, Input),
	arg(2, Type_Check_Aux, Type_Check_Functor),

	Cl = (New_Pred_Functor :- (Type_Check_Functor, (Type_Check_Aux, (New_Crisp_Pred_Functor, New_Funct_Pred_Functor)))),

	Pred_Type = [Pred_Type_11],
	save_fuzzy_rule_predicate_definition(Pred_Name, Pred_Arity, Pred_Type, Pred_Class, Cls_Aux),
	Cls = [ Cl | Cls_Aux ], 
	!.

translate((rfuzzy_similarity_between(Element1, Element2, Truth_Value)), Translation) :-
	translate_rfuzzy_similarity_between(Element1, Element2, Truth_Value, 'prod', 1, Translation).
translate((rfuzzy_similarity_between(Element1, Element2, Truth_Value) cred (Credibility_Operator, Credibility)), Translation) :-
	translate_rfuzzy_similarity_between(Element1, Element2, Truth_Value, Credibility_Operator, Credibility, Translation).

% crisp predicates (non-facts) and crisp facts.
translate(Other, Other) :-
	print_msg('debug', 'Non-Rfuzzy predicate', Other),
	nonvar(Other), 
	(
	    (
		functor(Other, ':-', 2), !,
		arg(1, Other, Arg_1), 
		nonvar(Arg_1), 
		functor(Arg_1, Pred_Name, Pred_Arity)
	    )
	;
	    (
		functor(Other, Pred_Name, Pred_Arity), 
		Pred_Name \== ':-',
		Pred_Name \== ':~',
		Pred_Name \== ':#',
		Pred_Name \== 'value',
		Pred_Name \== 'fuzzify'
	    )
	),
	generate_fake_type(Pred_Arity, Pred_Type),
	% save_predicate_definition(Pred_Name, Pred_Arity, Pred_Type, More_Info, _IsNew)
	save_predicate_definition(Pred_Name, Pred_Arity, Pred_Type, [], _IsNew).

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

generate_fake_type(0, []) :- !.
generate_fake_type(1, [_Any]) :- !.
generate_fake_type(N, [_Any|More]) :-
	N > 1,
	NewN is N - 1,
	generate_fake_type(NewN, More).

extract_from_pred_functor_name_and_pred_type_1(Pred_Functor, Pred_Name, Type_Name, Type_Arity) :-
	print_msg('debug', 'extract_from_pred_functor_name_and_pred_type_1(Pred_Functor)', (Pred_Functor)),
	(
	    (	nonvar(Pred_Functor), !    )
	;
	    (	print_msg('error', 'Functor cannot be a variable. Functor', Pred_Functor), !, fail    )
	),
	functor(Pred_Functor, Pred_Name, Pred_Fake_Arity), 
	(
	    (	Pred_Fake_Arity = 1, !    )
	;
	    (	print_msg('error', 'Functor must be of arity 1. Functor', Pred_Functor), !, fail    )
	),
	arg(1, Pred_Functor, Type_Functor), 
	(
	    (	nonvar(Type_Functor), !    )
	;
	    (	print_msg('error', 'Functor type cannot be a variable. Functor', Pred_Functor), !, fail    )
	),
	functor(Type_Functor, Type_Name, Type_Fake_Arity),
	(
	    (	Type_Fake_Arity = 0, !    )
	;
	    (	print_msg('error', 'Functor type must be of arity 0. Functor', Pred_Functor), !, fail    )
	),
	% retrieve_predicate_info(Pred_Name, Pred_Arity, Pred_Type, Show_Error),
	retrieve_predicate_info(Type_Name, Type_Arity, _Type_Type, 'true'),
	print_msg('debug', 'extract_from_pred_functor_name_and_pred_type_1(Pred_Name, Type_Name, Type_Arity)', (Pred_Name, Type_Name, Type_Arity)), !.

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

translate_rfuzzy_default_value_for(Pred_Functor, Fixed_Truth_Value, Condition_or_Thershold, Cls) :-
	print_msg('debug', 'rfuzzy_default_value_for(Pred_Functor, Fixed_Truth_Value, Condition_or_Thershold) ', (Pred_Functor, Fixed_Truth_Value, Condition_or_Thershold)),
	!, % If patter matching, backtracking forbiden.
	(
	    (	nonvar(Fixed_Truth_Value), number(Fixed_Truth_Value), !   )
	;
	    (   print_msg('error', 'The truth value must be a number. Value', Fixed_Truth_Value), !, fail   )
	),
	extract_from_pred_functor_name_and_pred_type_1(Pred_Functor, Pred_Name, Type_1_Name, Type_1_Arity),

	(
	    (	var(Condition_or_Thershold), Pred_Class = 'fuzzy_rule_default_without_cond'   )
	;
	    (	nonvar(Condition_or_Thershold), Pred_Class = 'fuzzy_rule_default_with_cond'   )
	),
	Pred_Arity = 1,
	% translate_predicate(Pred_Name, Pred_Arity, Pred_Class, New_Pred_Name, New_Pred_Arity).
	translate_predicate(Pred_Name, Pred_Arity, Pred_Class, New_Pred_Name, New_Pred_Arity),
	% predicate_to_functor(Pred_Name, Pred_Arity, Pred_Class, Pred_Functor, Truth_Value).
	predicate_to_functor(New_Pred_Name, New_Pred_Arity, Pred_Class, New_Pred_Functor, Truth_Value),
	arg(1, New_Pred_Functor, Argument),

	(
	    translate_rfuzzy_default_value_aux(Condition_or_Thershold, Type_1_Name, Argument, Condition_Aux)
	;
	    Condition_Aux = 'true'
	),
	print_msg('debug', 'rfuzzy_default_value_for :: Condition_Aux', Condition_Aux),

	generate_check_types_subgoal(Type_1_Name, Type_1_Arity, Argument, Check_Types_SubGoal),
	generate_assign_truth_value_subgoal(Fixed_Truth_Value, Truth_Value, Assign_Truth_Value_SubGoal),
	print_msg('debug', 'rfuzzy_default_value_for :: Assign_Truth_Value_SubGoal', Assign_Truth_Value_SubGoal),
	Cls_2 = [(New_Pred_Functor :- Check_Types_SubGoal, Assign_Truth_Value_SubGoal, Condition_Aux)],
	print_msg('debug', 'rfuzzy_default_value_for :: Cls_2 ', Cls_2),

	Pred_Type = [   Type_1_Name   ],
	% save_fuzzy_rule_predicate_definition(Pred_Name, Pred_Arity, Pred_Type, Pred_Class, Cls)
	save_fuzzy_rule_predicate_definition(Pred_Name, Pred_Arity, Pred_Type, Pred_Class, Cls_1),
	print_msg('debug', 'rfuzzy_default_value_for :: Cls_1 ', Cls_1),
	append_local(Cls_1, Cls_2, Cls),
	!. % Backtracking forbidden.

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

translate_rfuzzy_default_value_aux(Thershold, Type_1_Name, Argument, Condition_Aux) :-
	nonvar(Thershold), 
	print_msg('debug', 'Thershold', Thershold),

	Thershold = thershold(Pred2_Functor, Cond, Thershold_Truth_Value), !,
	extract_from_pred_functor_name_and_pred_type_1(Pred2_Functor, Pred2_Name, Type_1_Name, _Type_1_Arity),
	
	Pred2_Class = 'fuzzy_rule', Pred2_Arity = 1,
	% translate_predicate(Pred_Name, Pred_Arity, Pred_Class, New_Pred_Name, New_Pred_Arity).
	translate_predicate(Pred2_Name, Pred2_Arity, Pred2_Class, New_Pred2_Name, New_Pred2_Arity),
	% predicate_to_functor(Pred_Name, Pred_Arity, Pred_Class, Pred_Functor, Truth_Value).
	predicate_to_functor(New_Pred2_Name, New_Pred2_Arity, Pred2_Class, New_Pred2_Functor, Truth_Value_For_Thershold),
	arg(1, New_Pred2_Functor, Argument),

	% retrieve_predicate_info(Pred_Name, Pred_Arity, Pred_Type, Show_Error)
	retrieve_predicate_info(New_Pred2_Name, New_Pred2_Arity, New_Pred2_Type, 'true'), 
	New_Pred2_Type = [ Type_1_Name, 'rfuzzy_truth_value_type' ],

	print_msg('debug', 'translate', 'condition (over | under)'),
	(
	    (
		Cond = 'over',
		functor(Pred3_Functor, '.>.', 2),
		Pred3_Functor=..['.>.', Truth_Value_For_Thershold, Thershold_Truth_Value]
	    )
	;
	    (
		Cond = 'under',
		functor(Pred3_Functor, '.<.', 2),
		Pred3_Functor=..['.<.', Truth_Value_For_Thershold, Thershold_Truth_Value]
	    )
	), 
	Condition_Aux = (New_Pred2_Functor, Pred3_Functor),
	print_msg('debug', 'Condition_Aux', Condition_Aux).

translate_rfuzzy_default_value_aux(Condition_Functor, Type_1_Name, Argument, Condition_Aux) :-
	nonvar(Condition_Functor),
	print_msg('debug', 'Condition', Condition_Functor),
	extract_from_pred_functor_name_and_pred_type_1(Condition_Functor, Condition_Name, Type_1_Name, _Type_1_Arity),

	Condition_Arity = 1,
	% retrieve_predicate_info(Pred_Name, Pred_Arity, Pred_Type, Show_Error),
	retrieve_predicate_info(Condition_Name, Condition_Arity, Condition_Type, 'true'),
	nonvar(Condition_Type), Condition_Type = [ Type_1_Name ],

	functor(Condition_Aux, Condition_Name, Condition_Arity),
	arg(1, Condition_Aux, Argument),
	print_msg('debug', 'Condition_Aux', Condition_Aux).

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

generate_check_types_subgoal(Type_1_Name, Type_1_Arity, Argument, Check_Types_SubGoal) :-
	print_msg('debug', 'generate_check_types_subgoal(Type_1_Name, Type_1_Arity, Argument)', (Type_1_Name, Type_1_Arity, Argument)),
	functor(Type_1_Functor, Type_1_Name, Type_1_Arity),
	Check_Types_SubGoal = (Type_1_Functor, (Argument = Type_1_Functor)).

generate_assign_truth_value_subgoal(Fixed_Truth_Value, Truth_Value, Assign_Truth_Value_SubGoal) :-
	print_msg('debug', 'generate_assign_truth_value_subgoal(Fixed_Truth_Value, Truth_Value)', (Fixed_Truth_Value, Truth_Value)),
	functor(Assign_Truth_Value_SubGoal, '.=.', 2),
	arg(1, Assign_Truth_Value_SubGoal, Truth_Value),
	arg(2, Assign_Truth_Value_SubGoal, Fixed_Truth_Value), !.

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

translate_rfuzzy_type_for_crisp_rule(Pred_Name, Pred_Arity, Pred_Type, Pred_More_Info, Cls) :-
	nonvar(Pred_Name), nonvar(Pred_Arity), nonvar(Pred_Type),
	print_msg('debug', 'rfuzzy_type_for :: (Pred_Name, Pred_Arity, Pred_Type)', (Pred_Name, Pred_Arity, Pred_Type)),
	
	% translate_predicate(Pred_Name, Pred_Arity, Pred_Class, New_Pred_Name, New_Pred_Arity).
	translate_predicate(Pred_Name, Pred_Arity, 'crisp_rule_type', New_Pred_Name, New_Pred_Arity),
	% predicate_to_functor(Pred_Name, Pred_Arity, Pred_Class, Pred_Functor, Truth_Value).
	predicate_to_functor(New_Pred_Name, New_Pred_Arity, 'crisp_rule_type', Pred_Functor, _Truth_Value),

	fix_functor_type(Pred_Functor, New_Pred_Arity, 1, Pred_Type, Body),
	Cl = (Pred_Functor :- Body),

	% translate_predicate(Pred_Name, Pred_Arity, Pred_Class, New_Pred_Name, New_Pred_Arity).
	translate_predicate(Pred_Name, Pred_Arity, 'crisp_rule', Real_Pred_Name, Real_Pred_Arity),
	
	(   % Generate cls only if necessary.
	    (   % it has been defined before.
		retrieve_predicate_info(Real_Pred_Name, Real_Pred_Arity, Retrieved_Pred_Type, 'no'), 
		nonvar(Retrieved_Pred_Type), !,
		Retrieved_Pred_Type = Pred_Type,
		Cls = []
	    )
	;
	    (   % Define it !!!
		% save_predicate_definition(Pred_Name, Pred_Arity, Pred_Type, More_Info, _IsNew)
		save_predicate_definition(Real_Pred_Name, Real_Pred_Arity, Pred_Type, Pred_More_Info, _IsNew),
		Cls = [Cl]
	    )
	), !.

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

translate_rfuzzy_define_database(Pred_Name, Pred_Arity, Description, Cls) :-
	nonvar(Pred_Name), nonvar(Pred_Arity), nonvar(Description),
	print_msg('debug', 'rfuzzy_define_database(Pred_Name/Pred_Arity, Description)', (Pred_Name/Pred_Arity, Description)),
	translate_db_description(Description, 1, Pred_Name, Pred_Arity, Pred_Type, Cls_2),
	translate_rfuzzy_type_for_crisp_rule(Pred_Name, Pred_Arity, Pred_Type, ['database'], Cls_1),
	print_msg('debug', 'rfuzzy_define_database :: Cls_1', Cls_1),
	print_msg('debug', 'rfuzzy_define_database :: Cls_2', Cls_2),
	append_local(Cls_1, Cls_2, Cls).

% translate_db_description(Description, Index, DB_Pred_Name, DB_Pred_Arity, Types, DB_Fields) 
translate_db_description([(Field_Name, Field_Type)], Index, DB_Pred_Name, DB_Pred_Arity, [Field_Type], Cls) :- 
	nonvar(Index), nonvar(DB_Pred_Arity), Index = DB_Pred_Arity, !,
	translate_field_description(Field_Name, Field_Type, DB_Pred_Name, DB_Pred_Arity, Index, Cls).

translate_db_description([(Field_Name, Field_Type) | Description], Index, DB_Pred_Name, DB_Pred_Arity, [Field_Type|Types], Cls) :-
	nonvar(Index), nonvar(DB_Pred_Arity), Index < DB_Pred_Arity, !,
	translate_field_description(Field_Name, Field_Type, DB_Pred_Name, DB_Pred_Arity, Index, Cls_1),
	New_Index is Index + 1,
	translate_db_description(Description, New_Index, DB_Pred_Name, DB_Pred_Arity, Types, Cls_2),
	append_local(Cls_1, Cls_2, Cls).

translate_field_description(Field_Name, Field_Type_2, DB_Pred_Name, DB_Pred_Arity, Index, [Cl | Cls]) :-
	nonvar(Field_Name), nonvar(Field_Type_2), nonvar(DB_Pred_Name), nonvar(DB_Pred_Arity), nonvar(Index),
	print_msg('debug', 'translate_field_description_for(Field_Type_2, DB_Pred_Name, DB_Pred_Arity, Index)', (Field_Type_2, DB_Pred_Name, DB_Pred_Arity, Index)),

	functor(DB_Pred_Functor, DB_Pred_Name, DB_Pred_Arity),
	arg(Index, DB_Pred_Functor, Value),
	functor(Mapping, '=', 2), arg(1, Mapping, Input), arg(2, Mapping, DB_Pred_Functor),
	functor(Test, '\\==', 2), arg(1, Test, Value), arg(2, Test, 'null'),

	Pred_Type = [DB_Pred_Name, Field_Type_2],
	print_msg('debug', 'translate_field_description_for(Field_Name, Pred_Type)', (Pred_Name, Pred_Type)),
	(
	    (	translate_field_description_aux(Pred_Type, Field_Name, Input, Value, Pred_Functor, Conversion, Cls), !    )
	;
	    (	print_msg('error', 'Error translating db definition for (Pred_Name, Pred_Type)', (Pred_Name, Pred_Type)), !, fail    )
	),
	
	Cl = (Pred_Functor :- (((Mapping, DB_Pred_Functor), Test), Conversion)),
	print_msg('debug', 'translate_field_description_for(Pred_Name, Cl)', (Pred_Name, Cl, Cls)).

translate_field_description_aux([DB_Pred_Type, Type], Pred_Name, Input, Value, Pred_Functor, Conversion, Cls) :-
	
	Type = 'rfuzzy_truth_value_type', !, 
	Pred_Class = 'fuzzy_rule_db_value', Pred_Arity=1,
	% translate_predicate(Pred_Name, Pred_Arity, Pred_Class, New_Pred_Name, New_Pred_Arity).
	translate_predicate(Pred_Name, Pred_Arity, Pred_Class, New_Pred_Name, New_Pred_Arity),
	% predicate_to_functor(Pred_Name, Pred_Arity, Pred_Class, Pred_Functor, Truth_Value).
	predicate_to_functor(New_Pred_Name, New_Pred_Arity, Pred_Class, Pred_Functor, Truth_Value),
	arg(1, Pred_Functor, Input),
	functor(Conversion, '.=.', 2), arg(1, Conversion, Value), arg(2, Conversion, Truth_Value),
	% save_fuzzy_rule_predicate_definition(Pred_Name, Pred_Arity, Pred_Type, Pred_Class, Cls)
	save_fuzzy_rule_predicate_definition(Pred_Name, Pred_Arity, [DB_Pred_Type, Type], Pred_Class, Cls).

translate_field_description_aux([DB_Pred_Type, Type], Pred_Name, Input, Value, Pred_Functor, Conversion, []) :-
	(
	    Type = 'rfuzzy_string_type' ; 
	    Type = 'rfuzzy_integer_type' ; 
	    Type = 'rfuzzy_enum_type' ;
	    Type = 'rfuzzy_boolean_type' ; 
	    Type = 'rfuzzy_datetime_type'
	), !, 
	Pred_Arity = 2,
	functor(Pred_Functor, Pred_Name, Pred_Arity),
	arg(1, Pred_Functor, Input), arg(2, Pred_Functor, Value),
	Conversion = 'true',
	% save_predicate_definition(Pred_Name, Pred_Arity, Pred_Type, More_Info, _IsNew)
	save_predicate_definition(Pred_Name, Pred_Arity, [DB_Pred_Type, Type], [(Type, Pred_Name, Pred_Arity)], _IsNew).

translate_field_description_aux([DB_Pred_Type, Type], Pred_Name, Input, Value, Pred_Functor, Conversion, []) :-
	Type = 'rfuzzy_float_type', !, 
	Pred_Arity = 2,
	functor(Pred_Functor, Pred_Name, Pred_Arity),
	arg(1, Pred_Functor, Input), arg(2, Pred_Functor, Value_Out),
	functor(Conversion, '.=.', 2), arg(1, Conversion, Value), arg(2, Conversion, Value_Out),
	% save_predicate_definition(Pred_Name, Pred_Arity, Pred_Type, More_Info, _IsNew)
	save_predicate_definition(Pred_Name, Pred_Arity, [DB_Pred_Type, Type], [(Type, Pred_Name, Pred_Arity)], _IsNew).

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

% translate_rfuzzy_similarity_between(Element1, Element2, Truth_Value, Credibility_Operator, Credibility, Translation).
translate_rfuzzy_similarity_between(Element1, Element2, Truth_Value, Credibility_Operator, Credibility, Translation) :-
	Translation = rfuzzy_computed_similarity_between(Element1, Element2, Truth_Value, Credibility_Operator, Credibility).

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

save_rfuzzy_quantifiers_list([], []) :- !.
save_rfuzzy_quantifiers_list([(Pred_Name, Pred_Arity, Truth_Value_In, Truth_Value_Out, Code) | More], [Translation | Translations]) :-
	nonvar(Pred_Name), nonvar(Pred_Arity), number(Pred_Arity), Pred_Arity = 2,

	functor(Quantifier, Pred_Name, Pred_Arity),
	arg(1, Quantifier, Fuzzy_Predicate_Functor_In),
	arg(2, Quantifier, Truth_Value_Out),

	Translation = ( Quantifier :-	
		      functor(Fuzzy_Predicate_Functor_In, _FP_Name, FP_Arity), 
		      arg(FP_Arity, Fuzzy_Predicate_Functor_In, Truth_Value_In),
		      Fuzzy_Predicate_Functor_In,
		      Code
		      ),

	Pred_Type = [rfuzzy_predicate_type, rfuzzy_truth_value_type],
	% save_predicate_definition(Pred_Name, Pred_Arity, Pred_Type, More_Info, _IsNew)
	save_predicate_definition(Pred_Name, Pred_Arity, Pred_Type, [], _IsNew), !,

	save_rfuzzy_quantifiers_list(More, Translations).

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

translate_rule(Head, Cred_Op, Cred, Body, Cls) :-
	print_msg('debug', 'translate_rule(Head, Cred_Op, Cred, Body) ', (translate_rule(Head, Cred_Op, Cred, Body))),
	nonvar(Head), nonvar(Cred_Op), nonvar(Body), number(Cred),

	extract_from_pred_functor_name_and_pred_type_1(Head, Pred_Name, Type_1_Name, _Type_1_Arity),

	Pred_Class = 'fuzzy_rule_rule', Pred_Arity = 1,
	% translate_predicate(Pred_Name, Pred_Arity, Pred_Class, New_Pred_Name, New_Pred_Arity).
	translate_predicate(Pred_Name, Pred_Arity, Pred_Class, New_Pred_Name, New_Pred_Arity),
	% predicate_to_functor(Pred_Name, Pred_Arity, Pred_Class, Pred_Functor, Truth_Value).
	predicate_to_functor(New_Pred_Name, New_Pred_Arity, Pred_Class, New_Pred_Functor, Truth_Value),

	% Translate all predicates in the body.
	extract_aggregator(Body, TV_Aggregator, Tmp_Body),

	translate_rule_body(Tmp_Body, TV_Aggregator, Type_1_Name, Truth_Value, Fuzzy_Body), 
	Cls = (New_Pred_Functor :- Fuzzy_Body),

%	save_fuzzy_rule_predicate_definition(Pred_Name, Pred_Arity, _Pred_Type, Pred_Class, _Cls_Unused),
	print_msg('debug', 'translate_rule(Cls) ', (translate_rule(Cls))).

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

extract_aggregator(Body, Aggregator_Op, Tmp_Body) :-
	print_msg('debug', 'extract_aggregator(Body)', extract_aggregator(Body)),
	extract_aggregator_aux(Body, Aggregator_Op, Tmp_Body),
	print_msg('debug', 'extract_aggregator(Body, Aggregator_Op, Tmp_Body)', extract_aggregator(Body, Aggregator_Op, Tmp_Body)).
	
extract_aggregator_aux(Body, Aggregator_Op_Name, Tmp_Body) :-
	nonvar(Body),
	functor(Body, Aggregator_Op_Name, 1),
	test_aggregator_is_defined(Aggregator_Op_Name, 'no'),
	arg(1, Body, Tmp_Body), !.
extract_aggregator_aux(Body, 'null', Body) :- !.

test_aggregator_is_defined(Pred_Name, Show_Error) :-
	nonvar(Pred_Name),
	Pred_Arity = 3,
	% retrieve_predicate_info(Pred_Name, Pred_Arity, Pred_Type, Show_Error),
	retrieve_predicate_info(Pred_Name, Pred_Arity, Pred_Type, Show_Error), !,
	nonvar(Pred_Type),
	Expected_Type = ['rfuzzy_truth_value_type', 'rfuzzy_truth_value_type', 'rfuzzy_truth_value_type'],
	memberchk_local(Expected_Type, Pred_Type).

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

% Security issues
translate_rule_body(Body_F, _TV_Aggregator, _Type_1_Name, _Truth_Value, _FB) :- 
	var(Body_F), 
	print_msg('error', 'Rule body cannot be a variable. Body', Body_F),
	!, fail. % If this is a variable the tranlate rules loop forever !!!

% Conjunction.
translate_rule_body((Tmp_Body_1, Tmp_Body_2), TV_Aggregator, Type_1_Name, Truth_Value, (FB_1, FB_2, Aggr_F)) :- !,
	print_msg('debug', 'translate_rule_body(Body, TV_Aggregator, Truth_Value) - conjunction',((Tmp_Body_1, Tmp_Body_2), TV_Aggregator, Truth_Value)),
	nonvar(TV_Aggregator),
	\+ ( TV_Aggregator = 'none' ),
	translate_rule_body(Tmp_Body_1, TV_Aggregator, Type_1_Name, TV_1, FB_1),
	translate_rule_body(Tmp_Body_2, TV_Aggregator, Type_1_Name, TV_2, FB_2),
	functor(Aggr_F, TV_Aggregator, 3),
	arg(1, Aggr_F, TV_1), 
	arg(2, Aggr_F, TV_2), 
	arg(3, Aggr_F, Truth_Value), !.

% Quantifier.
translate_rule_body(Body_F, _TV_Aggregator, Type_1_Name, Truth_Value, Translation) :-
	print_msg('debug', 'translate_rule_body(Body, Truth_Value) - with quantifier',(Body_F, Truth_Value)),
	nonvar(Body_F),
	functor(Body_F, Pred_Name, 1),

	Pred_Arity = 1, Pred_Class = 'quantifier',
	% translate_predicate(Pred_Name, Pred_Arity, Pred_Class, New_Pred_Name, New_Pred_Arity).
	translate_predicate(Pred_Name, Pred_Arity, Pred_Class, New_Pred_Name, New_Pred_Arity),
	% predicate_to_functor(Pred_Name, Pred_Arity, Pred_Class, Pred_Functor, Truth_Value).
	predicate_to_functor(New_Pred_Name, New_Pred_Arity, Pred_Class, Pred_Functor, Truth_Value),

	% retrieve_predicate_info(Pred_Name, Pred_Arity, Pred_Type, Show_Error) 
	retrieve_predicate_info(New_Pred_Name, New_Pred_Arity, Pred_Type, 'true'), 
	Valid_Pred_Type = ['rfuzzy_predicate_type', 'rfuzzy_truth_value_type'],
	nonvar(Pred_Type), memberchk_local(Valid_Pred_Type, Pred_Type),

	arg(1, Body_F, SubBody),
	print_msg('debug', 'translate_rule_body(SubBody)',(SubBody)),
	translate_rule_body(SubBody, 'none', Type_1_Name, _SubCall_Truth_Value, SubCall),
	print_msg('debug', 'translate_rule_body(SubBody, SubCall)',(SubBody, SubCall)),
	arg(1, Pred_Functor, SubCall),
	Translation = (Pred_Functor, (Truth_Value .>=. 0, Truth_Value .=<. 1)),
	print_msg('debug', 'translate_rule_body(Translation) - with quantifier',(Translation)).

% Normal.
translate_rule_body(Body_F, _TV_Aggregator, Type_1_Name, Truth_Value, Translation) :-
	print_msg('debug', 'translate_rule_body(Body, Truth_Value) - without quantifier',(Body_F, Truth_Value)),
	extract_from_pred_functor_name_and_pred_type_1(Body_F, Pred_Name, Type_1_Name, _Type_1_Arity),

	Pred_Class = 'fuzzy_rule', Pred_Arity = 1,
	% translate_predicate(Pred_Name, Pred_Arity, Pred_Class, New_Pred_Name, New_Pred_Arity).
	translate_predicate(Pred_Name, Pred_Arity, Pred_Class, New_Pred_Name, New_Pred_Arity),
	% predicate_to_functor(Pred_Name, Pred_Arity, Pred_Class, Pred_Functor, Truth_Value).
	predicate_to_functor(New_Pred_Name, New_Pred_Arity, Pred_Class, Pred_Functor, Truth_Value),

	% retrieve_predicate_info(Pred_Name, Pred_Arity, Pred_Type, Show_Error) 
	retrieve_predicate_info(New_Pred_Name, New_Pred_Arity, Pred_Type, 'true'), 
	nonvar(Pred_Type), Valid_Pred_Type = [Type_1_Name, 'rfuzzy_truth_value_type'],
	memberchk_local(Valid_Pred_Type, Pred_Type),

	Translation = (Pred_Functor, (Truth_Value .>=. 0, Truth_Value .=<. 1)),
	print_msg('debug', 'translate_rule_body(Body, Translation)',(Body_F, Translation)),
	print_msg_nl('debug').

translate_rule_body(Body_F, _TV_Aggregator, _Type_1_Name, _Truth_Value, _Result) :-
	print_msg('error', 'translating the rule subbody',(Body_F)), !, fail.

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

fix_functor_type(Functor, Arity, Actual, [Type], Type_F) :-
	print_msg('debug', 'fix_functor_type(Functor, Arity, Actual, Type)', (Functor, Arity, Actual, [Type]) ),
	Actual = Arity, !, % Security conditions.
	fix_functor_type_aux(Functor, Actual, Type, Type_F),
	!. % Backtracking not allowed.

fix_functor_type(Functor, Arity, Actual, [Type | More], (Type_F, More_F)) :-
	print_msg('debug', 'fix_functor_type(Functor, Arity, Actual, Type)', (Functor, Arity, Actual, [Type|More]) ),
	Actual < Arity, !,  % Security conditions.
	fix_functor_type_aux(Functor, Actual, Type, Type_F),
	NewActual is Actual + 1, % Next values.
	!,
	fix_functor_type(Functor, Arity, NewActual, More, More_F),
	!. % Backtracking not allowed here.

fix_functor_type(Functor, Arity, Actual, Types, _Type_F) :-
	print_msg('error', 'fix_functor_type(Functor, Arity, Actual, Types)', fix_functor_type(Functor, Arity, Actual, Types)),
	!, fail.

fix_functor_type_aux(Functor, Actual, Type, (Type_F)) :-
	print_msg('debug', 'fix_functor_type_aux(Functor, Actual, Type)', fix_functor_type_aux(Functor, Actual, Type)),
	fix_functor_type_aux_extract_Pred_Name(Type, Pred_Name),
	% retrieve_predicate_info(Pred_Name, Pred_Arity, Pred_Type, Show_Error),
	retrieve_predicate_info(Pred_Name, Pred_Arity, _Pred_Type, 'true'),
	functor(Type_F, Pred_Name, Pred_Arity), % Build functor.
	arg(1, Type_F, X), % Arguments of functor.
	arg(Actual, Functor, X), % Unify with Argument of functor.
	!.
fix_functor_type_aux(_Functor, _Actual, Type, (_Type_F)) :-
	print_msg('error', 'Not an adequate type name', Type), !, fail.

fix_functor_type_aux_extract_Pred_Name(Type, Pred_Name) :-
	functor(Type, _Name, 0),
	Pred_Name = Type.

fix_functor_type_aux_extract_Pred_Name(Type, Pred_Name) :-
	functor(Type, 'rfuzzy_enum_type', 1),
	arg(1, Type, Pred_Name).
% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

build_straight_lines(X, V, Lower_Bound, [(X1,V1) | List], Upper_Bound, Cls) :-
	(
	    (
		nonvar(Lower_Bound), nonvar(X1),
		Lower_Bound = X1, !,
		build_straight_lines_aux(X, V, [(X1,V1) | List], Upper_Bound, Cls)
	    )
	;
	    (
		print_msg('error', 'function: Lower_Bound =/= first element in list.', (Lower_Bound, X1)), !, fail
	    )
	), !.

build_straight_lines_aux(X, V, [(X1,V1),(X2,V2)], Upper_Bound, (Point1 ; Line ; Point2)) :- !,
	print_msg('debug', 'build_straight_lines', (build_straight_lines(X, V, [(X1,V1),(X2,V2)], (Point1, Line, Point2)))),
	build_point(X, V, X1, V1, Point1),
	build_line(X, V, X1, V1, X2, V2, Line),
	build_point(X, V, X2, V2, Point2), !,
	(
	    (
		nonvar(Upper_Bound), nonvar(X2),
		Upper_Bound = X2, !
	    )
	;
	    (
		print_msg('error', 'function: Lower_Bound =/= last element in list.', (Upper_Bound, X2)), !, fail
	    )
	), !.

build_straight_lines_aux(X, V, [(X1,V1),(X2,V2)|List], Upper_Bound, (Point ; Line ; More)) :-
	print_msg('debug', 'build_straight_lines', (build_straight_lines(X, V, [(X1,V1),(X2,V2)|List], (Point, Line, More)))),
	build_point(X, V, X1, V1, Point),
	build_line(X, V, X1, V1, X2, V2, Line),
	build_straight_lines_aux(X, V, [(X2,V2)|List], Upper_Bound, More).

build_point(X, V, X1, V1, (X .=. X1, V .=. V1)) :-
	print_msg('debug', 'build_point', build_point(X, V, X, V, (H :- (H :- X1 .=. X, V1 .=. V)))),
	nonvar(X1), nonvar(V1).

build_line(X, V, X1, V1, X2, V2, (X .>. X1, X .<. X2, Calculate_V)) :-
	print_msg('debug', 'build_line', (build_line(X, V, X1, V1, X2, V2, (X .>. X1, X .<. X2, Calculate_V)))),

	nonvar(X1), nonvar(X2), nonvar(V1), nonvar(V2), 
	number(X1), number(X2), number(V1), number(V2),
	X1 < X2, 

	!, % Backtracking is not allowed here.
	evaluate_V(X, V, X1, V1, X2, V2, Calculate_V).

evaluate_V(_X, V, _X1, Vf, _X2, Vf, (V .=. Vf)) :- !.

evaluate_V(X, V, X1, V1, X2, V2, (Pend .=. ((V2-V1)/(X2-X1)), V .=. V1+Pend*(X-X1))) :-
	X2 - X1 > 0,
	V1 \= V2.

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------


translate_predicate(Pred_Name, Pred_Arity, Pred_Class, New_Pred_Name, New_Pred_Arity) :-
	print_msg('debug', 'translate_predicate(Pred_Name, Pred_Arity, Pred_Class)', (Pred_Name, Pred_Arity, Pred_Class)),
	(
	    (	nonvar(Pred_Name), nonvar(Pred_Arity), nonvar(Pred_Class), number(Pred_Arity), !    )
	;
	    (
		print_msg('error', 'translate_predicate(Pred_Name, Pred_Arity, Pred_Class)', (Pred_Name, Pred_Arity, Pred_Class)),
		!, fail
	    )
	),
	translation_info(Pred_Class, Add_Args, Priority, Preffix_String),
	number(Add_Args), number(Priority), 
	New_Pred_Arity is Pred_Arity + Add_Args, 
	add_preffix_to_name(Pred_Name, Preffix_String, New_Pred_Name), % Change name
	print_msg('debug', 'translate_predicate :: (New_Pred_Name, New_Pred_Arity, Priority)', (New_Pred_Name, New_Pred_Arity, Priority)).

predicate_to_functor(Pred_Name, Pred_Arity, Pred_Class, Pred_Functor, Truth_Value) :-
	print_msg('debug', 'predicate_to_functor(Pred_Name, Pred_Arity, Pred_Class)', (Pred_Name, Pred_Arity, Pred_Class)),
	translation_info(Pred_Class, Add_Args, Priority, _Preffix_String),
	number(Add_Args), number(Priority), 
	functor(Pred_Functor, Pred_Name, Pred_Arity),
	(
	    (	Add_Args = 0, Truth_Value = 'no_truth_value_argument_added', !  )
	;
	    (	Add_Args = 1, arg(Pred_Arity, Pred_Functor, Truth_Value), !  )
	;
	    (	Add_Args = 2,
		arg(Pred_Arity, Pred_Functor, Truth_Value),
		(
		    (	Priority = -1  )
		;
		    (
			Priority >= 0,
			Pred_Arity_Aux is Pred_Arity - 1,
			arg(Pred_Arity_Aux, Pred_Functor, Priority)
		    )
		)
	    )
	), !,
	print_msg('debug', 'predicate_to_functor(Pred_Functor, Truth_Value)', (Pred_Functor, Truth_Value)).

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

add_preffix_to_name(Input, Preffix, Output) :-
	string(Preffix),
	atom(Input),
	atom_codes(Input, Input_Chars),
	append_local(Preffix, Input_Chars, Output_Chars),
	atom_codes(Output, Output_Chars), 
	atom(Output), !.
%	print_msg('debug', 'add_preffix_to_name', add_preffix_to_name(Prefix, Input, Output)).

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

build_auxiliary_clauses([], [end_of_file]) :- !.
build_auxiliary_clauses([Predicate_Def|Predicate_Defs], Cls) :-
	print_msg_nl('debug'),
	print_msg('debug', 'build_auxiliary_clauses IN (Predicate_Def)', (Predicate_Def)),
	Predicate_Def = predicate_definition(Pred_Name, Pred_Arity, _Pred_Type, _More_Info, Selectors),
	build_auxiliary_clauses_aux(Selectors, Pred_Name, Pred_Arity, Cls_1), !,
	print_msg('debug', 'build_auxiliary_clauses OUT (Cls_1)', Cls_1),
	build_auxiliary_clauses(Predicate_Defs, Cls_2),
	append_local(Cls_1, Cls_2, Cls).

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

build_auxiliary_clauses_aux([], _Pred_Name, _Pred_Arity, []) :- !.
build_auxiliary_clauses_aux([Selector | Selectors], Pred_Name, Pred_Arity, [Cl | Cls]) :-
	build_auxiliary_clauses_aux(Selectors, Pred_Name, Pred_Arity, Cls),

	Selector = (Aux_Pred_Name, Aux_Pred_Arity),
	functor(Pred_Functor, Pred_Name, Pred_Arity),
	functor(Aux_Pred_Functor, Aux_Pred_Name, Aux_Pred_Arity),
	Tmp_Pred_Arity is Pred_Arity - 1,
	copy_args(Tmp_Pred_Arity, Pred_Functor, Aux_Pred_Functor),

	arg(Pred_Arity, Pred_Functor, Truth_Value),
	arg(Aux_Pred_Arity, Aux_Pred_Functor, Truth_Value),
	
	Cl = [(
		  Pred_Functor :- (
				      print_msg('debug', 'Predicate called', Pred_Functor),
				      findall(Aux_Pred_Functor, Aux_Pred_Functor, Results), 
				      supreme(Results, Pred_Functor)
				  )
	      )], !.

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

get_nth_element_from_list(Position, [Head], Head) :-
	nonvar(Position), 
	Position = 1, !.
get_nth_element_from_list(Position, [Head | _Tail], Head) :-
	nonvar(Position), 
	Position = 1, !.
get_nth_element_from_list(Position, [_Head | Tail], Head) :-
	nonvar(Position), 
	Position > 1, !,
	NewPosition is Position - 1,
	get_nth_element_from_list(NewPosition, Tail, Head).

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

generate_introspection_predicate(Fuzzy_Rules_To_Build, Fuzzy_Rules_Built, Fuzzy_Rules) :-
	retrieve_all_predicate_infos(Retrieved),
	generate_introspection_predicate_aux(Fuzzy_Rules_To_Build, Fuzzy_Rules_Built, Fuzzy_Rules_Tmp),
	generate_introspection_predicate_aux(Retrieved, Fuzzy_Rules_Tmp, Fuzzy_Rules).

% generate_introspection_predicate_aux(Input_List, Accumulator_List, Result_List),
generate_introspection_predicate_aux([], Accumulator_List, Accumulator_List) :- !.
generate_introspection_predicate_aux([Input|Input_List], Accumulator_List, Result_List) :-
	generate_introspection_predicate_real(Input, Output),
	generate_introspection_predicate_aux(Input_List, [Output|Accumulator_List], Result_List).

% INFO: predicate_definition(Pred_Name, Pred_Arity, Pred_Type, New_More_Info, New_Needs_Head_Building)

generate_introspection_predicate_real(predicate_definition(Pred_Name, Pred_Arity, Pred_Type, More_Info_List, _NHB), Cl) :-
	nonvar(Pred_Name), nonvar(Pred_Arity), nonvar(Pred_Type), 
	Pred_Arity = 2,
	Pred_Type = [_Whatever, 'rfuzzy_enum_type'], 
	More_Info_List = [('rfuzzy_enum_type', Pred_Name, Pred_Arity)], !,

	functor(Pred_Functor, Pred_Name, Pred_Arity),
	arg(2, Pred_Functor, Enum_Value),
	Generator= (findall(Enum_Value, Pred_Functor, Enum_Values_List), 
	remove_list_dupplicates(Enum_Values_List, [], New_Enum_Values_List)),
	Cl = (rfuzzy_introspection(Pred_Name, Pred_Arity, Pred_Type, New_Enum_Values_List) :- Generator).

generate_introspection_predicate_real(predicate_definition(Pred_Name, Pred_Arity, Pred_Type, Pred_More_Info_List, _NHB), Cl) :-
	Cl = (rfuzzy_introspection(Pred_Name, Pred_Arity, Pred_Type, Pred_More_Info_List)).

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

add_auxiliar_code(Fuzzy_Rules_In, Fuzzy_Rules_Out) :-
	code_for_quantifier_fnot(Fuzzy_Rules_In, Fuzzy_Rules_Aux_1), 
	code_for_getting_attribute_values(Fuzzy_Rules_Aux_1, Fuzzy_Rules_Aux_2), 
	code_for_predefined_types(Fuzzy_Rules_Aux_2, Fuzzy_Rules_Aux_3),
	code_for_defined_quantifiers(Fuzzy_Rules_Aux_3, Fuzzy_Rules_Aux_4),
	code_for_rfuzzy_compute(Fuzzy_Rules_Aux_4, Fuzzy_Rules_Out).

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

code_for_getting_attribute_values(In, [Code_1, Code_2, Code_3|In]) :-
	Code_1 = (rfuzzy_var_truth_value(Var, Condition, Value) :-
		 print_msg('debug', 'rfuzzy_var_truth_value :: Var', Var),
		 var(Var),					 
		 dump_constraints(Var, Var, Dump), !,
		 print_msg('debug', 'rfuzzy_var_truth_value :: dump_constraints :: Dump', Dump),
		 rfuzzy_process_attribute_dump(Dump, Var, Condition, Value),
		 !),
		 Code_2 = (rfuzzy_var_truth_value(Var, 'constant', Var) :- nonvar(Var), !),
		 Code_3 = (rfuzzy_var_truth_value(Var, 'error', 0) :-
			  print_msg('error', 'rfuzzy_var_truth_value :: Var', Var),
			  !).

%    dump_internal(Var, Var, [cva(Var, Value)])
% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

code_for_quantifier_fnot(In, [Code | In]) :-
	Code = (
		   fnot(Fuzzy_Predicate_Functor, Truth_Value) :-
	       
	       print_msg('debug', 'fnot', 'Computing results list.'),
	       findall(Fuzzy_Predicate_Functor, Fuzzy_Predicate_Functor, Results_List), !,
	       print_msg('debug', 'fnot', Results_List),
	       reorder_by_truth_value(Results_List, [], Results_List_Aux),
	       print_msg('debug', 'reorder_by_truth_value', Results_List_Aux),
	       one_by_one_first_tail(Results_List_Aux, Result_Functor),
	       print_msg('debug', 'take_an_element', Result_Functor),
	       
	       functor(Fuzzy_Predicate_Functor, _FP_Name, FP_Arity), 
	       FP_Arity_Aux is FP_Arity - 1,
	       copy_args(FP_Arity_Aux, Result_Functor, Fuzzy_Predicate_Functor),
	       arg(FP_Arity, Result_Functor, SubCall_Truth_Value),
	       
	       print_msg('debug', 'fnot :: adjusting Truth_Value', Truth_Value),
	       Truth_Value .=. 1 - SubCall_Truth_Value,
	       Truth_Value .>=. 0, Truth_Value .=<. 1,
	       print_msg('debug', 'fnot :: result', Fuzzy_Predicate_Functor)
	       ).

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

code_for_defined_quantifiers(Code_In, Code_Out) :-
	retract_fact(defined_quantifiers_code(Defined_Quantifiers_Code)),
	append_local(Defined_Quantifiers_Code, Code_In, Code_Out), !.

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

code_for_rfuzzy_compute(In, [Code | In]) :-
	Code = (rfuzzy_compute(Operator, Elt1, Elt2, Truth_Value) :- 
	       findall(rfuzzy_computed_similarity_between(Elt1, Elt2, TV, Cred_Op, Cred),
	       rfuzzy_computed_similarity_between(Elt1, Elt2, TV, Cred_Op, Cred),
	       Computed_Similarities),
		rfuzzy_compute_aux(Operator, Elt1, Elt2, Computed_Similarities, Truth_Value)
	       ).

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

code_for_predefined_types(In, [Type_1, Type_2, Type_3, Type_4, Type_5, Type_11, Type_12, Type_13, Type_14, Type_15, Type_16|In]) :-
	Type_1 = (rfuzzy_any_type(_Any_1)),
	Type_2 = (rfuzzy_truth_value_type(_Any_2)), 
	Type_3 = (rfuzzy_credibility_value_type(_Any_3)), 
	Type_4 = (rfuzzy_predicate_type(_Any_4)), 
	Type_5 = (rfuzzy_number_type(_Any_4)),

	Type_11 = (rfuzzy_string_type(_Any_11)), 
	Type_12 = (rfuzzy_integer_type(_Any_12)), 
	Type_13 = (rfuzzy_float_type(_Any_13)), 
	Type_14 = (rfuzzy_enum_type(_Any_14)), 
	Type_15 = (rfuzzy_boolean_type(_Any_15)), 
	Type_16 = (rfuzzy_datetime_type(_Any_16)), 

	!.

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------
