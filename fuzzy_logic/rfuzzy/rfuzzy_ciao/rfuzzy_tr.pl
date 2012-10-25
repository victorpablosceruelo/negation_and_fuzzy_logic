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
%translation_info('non_rfuzzy_fuzzy_rule', 0, -1,         "non_rfuzzy_fuzzy_rule").

% This produces unexpected results.
% translation_info(_X,                             _Y,               0, 0, 'no', 0,          "rfuzzy_error_error_error_").

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

save_fuzzy_rule_predicate_definition(Pred_Name, Pred_Arity, Pred_Type, Pred_Class, Gen_Type_Cls, Cls) :-
	print_msg('debug', 'save_fuzzy_rule_predicate_definition(Pred_Name, Pred_Arity, Pred_Type, Pred_Class)', (Pred_Name, Pred_Arity, Pred_Type, Pred_Class)),
	nonvar(Pred_Name), nonvar(Pred_Arity), nonvar(Pred_Class),
	% translate_predicate(Pred_Name, Pred_Arity, Pred_Class, New_Pred_Name, New_Pred_Arity).
	translate_predicate(Pred_Name, Pred_Arity, 'fuzzy_rule', Real_Pred_Name, Real_Pred_Arity),
	% translate_predicate(Pred_Name, Pred_Arity, Pred_Class, New_Pred_Name, New_Pred_Arity).
	translate_predicate(Pred_Name, Pred_Arity, Pred_Class, New_Pred_Name, New_Pred_Arity),

	(
	    (
		Pred_Class = 'fuzzy_rule_type', !,
		More_Info = []
	    )
	;
	    (
		Pred_Class \== 'fuzzy_rule_type', !,
		More_Info = [(Pred_Class, New_Pred_Name, New_Pred_Arity)]
	    )
	),
	% save_predicate_definition(Pred_Name, Pred_Arity, Pred_Type, More_Info, Needs_Head_Building)
	save_predicate_definition(Real_Pred_Name, Real_Pred_Arity, _Not_Yet_Pred_Type, More_Info, 'true'),

	(
	    (   % Pred_Type is still undefined or we do  not want to generate Cls.  
		(    var(Pred_Type)  ;   Gen_Type_Cls = 'no'   ), !, 
		     Cls = []
	    )
	;
	    (
		nonvar(Pred_Type), 
		save_fuzzy_rule_predicate_definition_gen_type_cls(Pred_Name, Pred_Arity, Pred_Type, Cls)
	    )
	), !.

save_fuzzy_rule_predicate_definition_gen_type_cls(Pred_Name, Pred_Arity, Pred_Type_In, Cls) :-
	print_msg('debug', 'save_fuzzy_rule_predicate_definition_gen_type_cls :: (Pred_Name, Pred_Arity, Pred_Type_In)', (Pred_Name, Pred_Arity, Pred_Type_In)),
	nonvar(Pred_Name), number(Pred_Arity), nonvar(Pred_Type_In), 

	% translate_predicate(Pred_Name, Pred_Arity, Pred_Class, New_Pred_Name, New_Pred_Arity).
	translate_predicate(Pred_Name, Pred_Arity, 'fuzzy_rule', Real_Pred_Name, Real_Pred_Arity),

	append_local(Pred_Type_In, ['rfuzzy_truth_value_type'], Pred_Type),
	(   % Generate cls only if necessary.
	    (   % It has been defined before.
		retrieve_predicate_info(Real_Pred_Name, Real_Pred_Arity, Pred_Type, More_Info_Aux, _NBH_3, 'no'),
		print_msg('debug', 'save_fuzzy_rule_predicate_definition_gen_type_cls :: More_Info_Aux', More_Info_Aux),
		memberchk_local(('fuzzy_rule_type', _Ignored_Name, _Ignored_Arity), More_Info_Aux), !,
		Cls = []
	    )
	;
	    (   % Define it !!!
		save_fuzzy_rule_pred_def_gen_type_cls_real(Pred_Name, Pred_Arity, Pred_Type_In, Type_Pred_Name, Type_Pred_Arity, Cl),
		More_Info = [('fuzzy_rule_type', Type_Pred_Name, Type_Pred_Arity)],
		% save_predicate_definition(Pred_Name, Pred_Arity, Pred_Type, More_Info, Needs_Head_Building)
		save_predicate_definition(Real_Pred_Name, Real_Pred_Arity, Pred_Type, More_Info, 'true'),
		Cls = [Cl]
	    )
	).

save_fuzzy_rule_pred_def_gen_type_cls_real(Pred_Name, Pred_Arity, Pred_Type, Type_Pred_Name, Type_Pred_Arity, Cl) :-
	nonvar(Pred_Name), number(Pred_Arity), nonvar(Pred_Type), 
	Pred_Class = 'fuzzy_rule_type', 
	% translate_predicate(Pred_Name, Pred_Arity, Pred_Class, New_Pred_Name, New_Pred_Arity).
	translate_predicate(Pred_Name, Pred_Arity, Pred_Class, Type_Pred_Name, Type_Pred_Arity),
	% predicate_to_functor(Pred_Name, Pred_Arity, Pred_Class, Pred_Functor, Truth_Value).
	predicate_to_functor(Type_Pred_Name, Type_Pred_Arity, Pred_Class, Pred_Functor, _Truth_Value),

	append_local(Pred_Type, ['rfuzzy_credibility_value_type', 'rfuzzy_truth_value_type'], Type_Pred_Type),
	fix_functor_type(Pred_Functor, Type_Pred_Arity, 1, Type_Pred_Type, Body),
	Cl = (Pred_Functor :- Body), !.

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

save_predicates_definition_list([], _Pred_Arity, _Pred_Type, _More_Info, _Needs_Head_Building) :- !.
save_predicates_definition_list([Pred_Name | Pred_List], Pred_Arity, Pred_Type, More_Info, Needs_Head_Building) :-
	save_predicate_definition(Pred_Name, Pred_Arity, Pred_Type, More_Info, Needs_Head_Building), !,
	save_predicates_definition_list(Pred_List, Pred_Arity, Pred_Type, More_Info, Needs_Head_Building).

save_predicate_definition(Pred_Name, Pred_Arity, Pred_Type, More_Info, Needs_Head_Building) :-
	print_msg('debug', 'save_predicate_definition(Pred_Name, Pred_Arity, Pred_Type, More_Info, Needs_Head_Building)', (Pred_Name, Pred_Arity, Pred_Type, More_Info, Needs_Head_Building)),
	check_save_predicate_definition_input(Pred_Name, Pred_Arity, Pred_Type),
	(
	    (	 
		list(More_Info),
		retract_fact(predicate_definition(Pred_Name, Pred_Arity, Pred_Type, Old_More_Info, Old_Needs_Head_Building)), !, % Retract last
		print_msg('debug', 'save_predicate_definition :: current', (Pred_Name, Pred_Arity, Pred_Type, Old_More_Info, Old_Needs_Head_Building)),
		append_local(More_Info, Old_More_Info, New_More_Info),
		boolean_or(Old_Needs_Head_Building, Needs_Head_Building, New_Needs_Head_Building)
	    )
	;
	    (
		list(More_Info),
		New_More_Info = More_Info, 
		New_Needs_Head_Building = Needs_Head_Building
	    )
	), 
	assertz_fact(predicate_definition(Pred_Name, Pred_Arity, Pred_Type, New_More_Info, New_Needs_Head_Building)),
	print_msg('debug', 'saved', save_predicate_definition(Pred_Name, Pred_Arity, Pred_Type, New_More_Info, New_Needs_Head_Building)),
	!.

retrieve_predicate_info(Pred_Name, Pred_Arity, Pred_Type, More_Info, Needs_Head_Building, Show_Error) :-
	print_msg('debug', 'retrieve_predicate_info(Pred_Name, Pred_Arity, Pred_Type, More_Info, Needs_Head_Building, Show_Error)', retrieve_predicate_info(Pred_Name, Pred_Arity, Pred_Type, More_Info, Needs_Head_Building, Show_Error)),
	(
	    (   predicate_definition(Pred_Name, Pred_Arity, Pred_Type, More_Info, Needs_Head_Building), !,
		print_msg('debug', 'retrieved', retrieve_predicate_info(Pred_Name, Pred_Arity, Pred_Type, More_Info, Needs_Head_Building))   
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
	findall((predicate_definition(Pred_Name, Pred_Arity, Pred_Type, More_Info, Needs_Head_Building)),
	(retract_fact(predicate_definition(Pred_Name, Pred_Arity, Pred_Type, More_Info, Needs_Head_Building))), Retrieved),
	 !.

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------
	
check_save_predicate_definition_input(Pred_Name, Pred_Arity, _Pred_Type) :-
	( 
	    (
		var(Pred_Name), 
		print_msg('error', 'save_predicate_definition: Pred_Name cannot be a variable. Value', Pred_Name)
	    )
	; 
	    (
		var(Pred_Arity),
		print_msg('error', 'save_predicate_definition: Pred_Arity cannot be a variable. Value', Pred_Arity)
	    )
	),
	!, fail.

check_save_predicate_definition_input(Pred_Name, Pred_Arity, Pred_Type) :-
	nonvar(Pred_Arity), nonvar(Pred_Name), number(Pred_Arity), 
	(
	    (
		var(Pred_Type), !
	    )
	;
	    (
		nonvar(Pred_Type), 
		check_pred_type_aux(Pred_Arity, Pred_Type), !
	    )
	;
	    (
		nonvar(Pred_Type),
		print_msg('error', 'Types in type definition do not sum up the arity value. (Pred_Name, Pred_Arity, Pred_Type)', (Pred_Name, Pred_Arity, Pred_Type)), 
		!, fail
	    )
	).

check_pred_type_aux(1, [_Pred_Type]) :- !.
check_pred_type_aux(Pred_Arity, [_Pred_Type|More]) :-
	New_Pred_Arity is Pred_Arity -1,
	check_pred_type_aux(New_Pred_Arity, More).
	
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
	print_msg_nl('info'), print_msg_nl('info'), 
	print_msg('info', 'Rfuzzy (Ciao Prolog package to compile Rfuzzy programs into a pure Prolog programs)', 'compiling ...'),
	print_msg_nl('info'),
	% save_predicate_definition(Pred_Name, Pred_Arity, Pred_Type, More_Info, Needs_Head_Building)
	save_predicate_definition('rfuzzy_any_type', 1, _Pred_Type1, [], 'no'),
	save_predicate_definition('rfuzzy_truth_value_type', 1, _Pred_Type2, [], 'no'),
	save_predicate_definition('rfuzzy_credibility_value_type', 1, _Pred_Type3, [], 'no'),
	save_predicate_definition('rfuzzy_predicate_type', 1, _Pred_Type4, [], 'no'),
	save_predicate_definition('rfuzzy_number_type', 1, _Pred_Type5, [], 'no'),
	save_predicate_definition('fnot', 2, ['rfuzzy_predicate_type', 'rfuzzy_truth_value_type'], [], 'no'),

	save_predicate_definition('rfuzzy_string_type', 1, _Pred_Type11, [], 'no'),
	save_predicate_definition('rfuzzy_integer_type', 1, _Pred_Type12, [], 'no'),
	save_predicate_definition('rfuzzy_float_type', 1, _Pred_Type13, [], 'no'),
	save_predicate_definition('rfuzzy_enum_type', 1, _Pred_Type14, [], 'no'),
	save_predicate_definition('rfuzzy_boolean_type', 1, _Pred_Type15, [], 'no'),
	save_predicate_definition('rfuzzy_datetime_type', 1, _Pred_Type16, [], 'no'),

	rfuzzy_defined_aggregators(Defined_Aggregators_List),
	Aggregators_Type = ['rfuzzy_truth_value_type', 'rfuzzy_truth_value_type', 'rfuzzy_truth_value_type'],
	save_predicates_definition_list(Defined_Aggregators_List, 3, Aggregators_Type, [], 'no'),
	
	rfuzzy_compute_defined_operators(Compute_Defined_Operators),
	save_predicate_definition('rfuzzy_compute_defined_operators', 0, _Rfuzzy_Compute_Type, Compute_Defined_Operators, 'no'),

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
translate((rfuzzy_default_value_for(Pred_Functor, Fixed_Truth_Value)), Cls) :- 
	print_msg('debug', 'translate :: rfuzzy_default_value_for(Pred_Name, Truth_Value) ', (Pred_Name, Fixed_Truth_Value)),
	!, % If patter matching, backtracking forbiden.
	nonvar(Fixed_Truth_Value), number(Fixed_Truth_Value), nonvar(Pred_Functor), 
	functor(Pred_Functor, Pred_Arity, Pred_Name), Pred_Arity = 1,
	arg(1, Pred_Functor, Type_1_Functor), nonvar(Type_1_Functor),
	functor(Type_1_Functor, 0, Type_1_Name)

	% retrieve_predicate_info(Pred_Name, Pred_Arity, Pred_Type, More_Info, Needs_Head_Building, Show_Error),
	retrieve_predicate_info(Type_1_Name, Type_1_Arity, _Type_1_Type, _MI, _NHB, 'true'),

	Pred_Class = 'fuzzy_rule_default_without_cond',
	% translate_predicate(Pred_Name, Pred_Arity, Pred_Class, New_Pred_Name, New_Pred_Arity).
	translate_predicate(Pred_Name, Pred_Arity, Pred_Class, New_Pred_Name, New_Pred_Arity),
	% predicate_to_functor(Pred_Name, Pred_Arity, Pred_Class, Pred_Functor, Truth_Value).
	predicate_to_functor(New_Pred_Name, New_Pred_Arity, Pred_Class, New_Pred_Functor, Truth_Value),

	(
	    translate_rfuzzy_default_value_condition(Condition, Condition_Aux)
	;
	    translate_rfuzzy_default_value_thershold(Thershold, Condition_Aux)
	;
	    Condition_Aux = 'true'
	),

	arg(1, New_Pred_Functor, Argument),
	generate_check_types_subgoal(Type_1_Name, Type_1_Arity, Argument, Check_Types_SubGoal),
	generate_assign_truth_value_subgoal(Fixed_Truth_Value, Truth_Value, Assign_Truth_Value_SubGoal),
	Cls_1 = [(Pred_Functor :- Check_Types_SubGoal, Assign_Truth_Value_SubGoal, Condition_Aux)],
	print_msg('debug', 'translate :: Cls ', Cls),

	% save_fuzzy_rule_predicate_definition(Pred_Name, Pred_Arity, Pred_Type, Pred_Class)
	save_fuzzy_rule_predicate_definition(Pred_Name, Pred_Arity, Pred_Type, Pred_Class, 'no', Cls_2),
	append(Cls_1, Cls_2, Cls)
	!. % Backtracking forbidden.

% Conditional default
translate((rfuzzy_default_value_for(Pred_Name, Fixed_Truth_Value) if Pred2_Name/Pred2_Arity), Cls) :-
	print_msg('debug', 'translate :: rfuzzy_default_value_for(Pred_Name, Truth_Value) if Pred2_Name/Pred2_Arity', (rfuzzy_default_value_for(Pred_Name, Fixed_Truth_Value) if Pred2_Name/Pred2_Arity)),
	print_msg('debug', 'translate', 'if detected'),
	!, % If patter matching, backtracking forbiden.
	nonvar(Fixed_Truth_Value), number(Fixed_Truth_Value), nonvar(Pred_Name), 
	nonvar(Pred2_Name), nonvar(Pred2_Arity), number(Pred2_Arity),  

	% retrieve_predicate_info(Pred_Name, Pred_Arity, Pred_Type, More_Info, Needs_Head_Building, Show_Error),
	retrieve_predicate_info(Pred_Name, Tmp_Pred_Arity, Pred_Type, _MI, _NHB, 'true'),
	Pred_Arity is Tmp_Pred_Arity - 1, Pred_Arity = Pred2_Arity,

	Pred_Class = 'fuzzy_rule_default_with_cond',
	% translate_predicate(Pred_Name, Pred_Arity, Pred_Class, New_Pred_Name, New_Pred_Arity).
	translate_predicate(Pred_Name, Pred_Arity, Pred_Class, New_Pred_Name, New_Pred_Arity),
	% predicate_to_functor(Pred_Name, Pred_Arity, Pred_Class, Pred_Functor, Truth_Value).
	predicate_to_functor(New_Pred_Name, New_Pred_Arity, Pred_Class, Pred_Functor, Truth_Value),

	functor(Pred2_Functor, Pred2_Name, Pred2_Arity),
	copy_args(Pred_Arity, Pred_Functor, Pred2_Functor),    % Copy args from main functor.
	generate_truth_value_equal_to_functor(Fixed_Truth_Value, Truth_Value, Truth_Value_Functor),
	Cls = (Pred_Functor :- (Pred2_Functor, Truth_Value_Functor)),
	print_msg('debug', 'translate :: Cls ', Cls),

	% save_fuzzy_rule_predicate_definition(Pred_Name, Pred_Arity, Pred_Type, Pred_Class)
	save_fuzzy_rule_predicate_definition(Pred_Name, Pred_Arity, Pred_Type, Pred_Class, 'no', _Cls_Unused),
	!.

translate((rfuzzy_default_value_for(Pred_Name, Fixed_Truth_Value) if thershold(Pred2_Name, Cond, Thershold_Truth_Value)), Cls) :-
	print_msg('debug', 'translate :: (rfuzzy_default_value_for(Pred_Name, Truth_Value) if thershold(Pred2_Name, Cond, Thershold_Truth_Value))', (rfuzzy_default_value_for(Pred_Name, Fixed_Truth_Value) if thershold(Pred2_Name, Cond, Thershold_Truth_Value))),
	!, % If patter matching, backtracking forbiden.
	nonvar(Fixed_Truth_Value), number(Fixed_Truth_Value), nonvar(Pred_Name), nonvar(Pred2_Name), 

	% retrieve_predicate_info(Pred_Name, Pred_Arity, Pred_Type, More_Info, Needs_Head_Building, Show_Error),
	retrieve_predicate_info(Pred_Name,   Tmp_Pred_Arity, Pred_Type_1, _MI_1, _NHB_1, 'true'), nonvar(Pred_Type_1),
	% retrieve_predicate_info(Pred_Name, Pred_Arity, Pred_Type, More_Info, Needs_Head_Building, Show_Error),
	retrieve_predicate_info(Pred2_Name, Tmp_Pred_Arity, Pred_Type_2, _MI_2, _NHB_2, 'true'), nonvar(Pred_Type_2),
	Pred_Arity is Tmp_Pred_Arity - 1, Pred_Type = Pred_Type_1, Pred_Type = Pred_Type_2, 
	
	Pred_Class = 'fuzzy_rule_default_with_cond',
	% translate_predicate(Pred_Name, Pred_Arity, Pred_Class, New_Pred_Name, New_Pred_Arity).
	translate_predicate(Pred_Name, Pred_Arity, Pred_Class, New_Pred_Name, New_Pred_Arity),
	% predicate_to_functor(Pred_Name, Pred_Arity, Pred_Class, Pred_Functor, Truth_Value).
	predicate_to_functor(New_Pred_Name, New_Pred_Arity, Pred_Class, Pred_Functor, Truth_Value),

	Pred2_Class = 'fuzzy_rule',
	% translate_predicate(Pred_Name, Pred_Arity, Pred_Class, New_Pred_Name, New_Pred_Arity).
	translate_predicate(Pred2_Name, Pred_Arity, Pred2_Class, New_Pred2_Name, New_Pred2_Arity),
	% predicate_to_functor(Pred_Name, Pred_Arity, Pred_Class, Pred_Functor, Truth_Value).
	predicate_to_functor(New_Pred2_Name, New_Pred2_Arity, Pred2_Class, Pred2_Functor, Truth_Value_For_Thershold),

	copy_args(Pred_Arity, Pred_Functor, Pred2_Functor),
	print_msg('debug', 'translate', 'condition (over | under)'),
	(
	    (
		Cond = 'over',
		functor(H_Pred3, '.>.', 2),
		Pred3_Functor=..['.>.', Truth_Value_For_Thershold, Thershold_Truth_Value]
	    )
	;
	    (
		Cond = 'under',
		functor(H_Pred3, '.<.', 2),
		Pred3_Functor=..['.<.', Truth_Value_For_Thershold, Thershold_Truth_Value]
	    )
	), 
	generate_truth_value_equal_to_functor(Fixed_Truth_Value, Truth_Value, Truth_Value_Functor),
	% retrieve_predicate_info(Pred_Name, Pred_Arity, Pred_Type, More_Info, Needs_Head_Building, Show_Error)
	retrieve_predicate_info(New_Pred2_Name, New_Pred2_Arity, _Pred2_Functor_Type, _List, _NHB, 'true'), 

	Cls = (Pred_Functor :- ((Pred2_Functor, Pred3_Functor), Truth_Value_Functor)),
	% save_fuzzy_rule_predicate_definition(Pred_Name, Pred_Arity, Pred_Type, Pred_Class)
	save_fuzzy_rule_predicate_definition(Pred_Name, Pred_Arity, Pred_Type, Pred_Class, 'no', _Cls_Unused), !,
	print_msg('debug', 'translate :: (rfuzzy_default_value_for(Pred_Name, Truth_Value) if thershold(Pred2_Name, Cond, Thershold_Truth_Value))', (rfuzzy_default_value_for(Pred_Name, Fixed_Truth_Value) if thershold(Pred2_Name, Cond, Thershold_Truth_Value))),
	print_msg('debug', 'translate :: (Cls)', Cls).

% Fuzzy facts.
translate((Head value Fixed_Truth_Value), (Pred_Functor :- Truth_Value_Functor)):-
	!, % If patter matching, backtracking forbiden.
	print_msg('debug', 'fact conversion :: IN ',(Head value Fixed_Truth_Value)),
	nonvar(Head), nonvar(Fixed_Truth_Value), number(Fixed_Truth_Value),

	functor(Head, Pred_Name, Pred_Arity),
	Pred_Class = 'fuzzy_rule_fact',
	% translate_predicate(Pred_Name, Pred_Arity, Pred_Class, New_Pred_Name, New_Pred_Arity).
	translate_predicate(Pred_Name, Pred_Arity, Pred_Class, New_Pred_Name, New_Pred_Arity),
	% predicate_to_functor(Pred_Name, Pred_Arity, Pred_Class, Pred_Functor, Truth_Value).
	predicate_to_functor(New_Pred_Name, New_Pred_Arity, Pred_Class, Pred_Functor, Truth_Value),

	generate_truth_value_equal_to_functor(Fixed_Truth_Value, Truth_Value, Truth_Value_Functor),
	print_msg('debug', 'fact conversion :: OUT ', (Pred_Functor :- Truth_Value_Functor)),
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
	% save_predicate_definition(Pred_Name, Pred_Arity, Pred_Type, More_Info, Needs_Head_Building)
	save_predicate_definition(Aggregator_Name, Aggregator_Arity, Aggregator_Type, [], 'no'),
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
	% save_predicate_definition(Pred_Name, Pred_Arity, Pred_Type, More_Info, Needs_Head_Building)
	save_predicate_definition(New_Pred_Name, New_Pred_Arity, Pred_Type, Other_Info, 'no'), 
	Cls = (Pred_Functor :- (Body, print_msg('debug', 'function_call', Pred_Functor))),
	print_msg('debug', '(Head :# (Lower_Bound, List, Upper_Bound)) -> Cls', Cls).

% Predicate type(s) definition (Class = database).
translate(rfuzzy_define_database(Pred_Name/Pred_Arity, Description), Cls):- !,
	nonvar(Pred_Name), nonvar(Pred_Arity), nonvar(Description),
	print_msg('debug', 'rfuzzy_db_description(Pred_Name/Pred_Arity, Description)', (Pred_Name/Pred_Arity, Description)),
	translate_db_description(Description, 1, Pred_Arity, Pred_Type, DB_Fields),
	translate_rfuzzy_type_for_crisp_rule(Pred_Name, Pred_Arity, Pred_Type, ['database'], Cls_1),
	translate_db_fields(DB_Fields, 1, Pred_Arity, Pred_Name, Cls_2),
	print_msg('debug', 'rfuzzy_db_description :: Cls_1', Cls_1),
	print_msg('debug', 'rfuzzy_db_description :: Cls_2', Cls_2),
	append_local(Cls_1, Cls_2, Cls).

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
		% save_fuzzy_rule_predicate_definition(Pred_Name, Pred_Arity, Pred_Type, Pred_Class, Gen_Type_Cls, Cls)
		save_fuzzy_rule_predicate_definition(Pred_Name, Pred_Arity, Pred_Type, 'fuzzy_rule_type', 'true', Cls)
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

translate(rfuzzy_synonym(Pred2_Name, Pred_Name, Cred_Op, Cred), Cls):-
	!,
	print_msg('debug', 'translate(rfuzzy_synonym(Pred2_Name, Pred_Name, Cred_Op, Cred))) ', rfuzzy_synonym(Pred2_Name, Pred_Name, Cred_Op, Cred)),
	nonvar(Pred2_Name), nonvar(Pred_Name), nonvar(Cred_Op), nonvar(Cred), number(Cred),
	test_aggregator_is_defined(Cred_Op, 'true'),

	% retrieve_predicate_info(Pred_Name, Pred_Arity, Pred_Type, More_Info, Needs_Head_Building, Show_Error),
	retrieve_predicate_info(Pred2_Name, Pred2_Arity, Pred2_Type, _MI_2, _NHB_2, 'true'),
	( 
	    (
		nonvar(Pred2_Arity), nonvar(Pred2_Type), !
	    )
	;
	    (
		print_msg('error', 'You must define the type for the predicate', Pred2_Name), !, fail
	    )
	),

	Pred_Class = 'fuzzy_rule_synonym', Pred_Arity is Pred2_Arity - 1,
	% translate_predicate(Pred_Name, Pred_Arity, Pred_Class, New_Pred_Name, New_Pred_Arity).
	translate_predicate(Pred_Name, Pred_Arity, Pred_Class, New_Pred_Name, New_Pred_Arity),
	% predicate_to_functor(Pred_Name, Pred_Arity, Pred_Class, Pred_Functor, Truth_Value).
	predicate_to_functor(New_Pred_Name, New_Pred_Arity, Pred_Class, Pred_Functor, Truth_Value_Out),

	functor(Credibility_Functor, Cred_Op, 3), 
	Credibility_Functor=..[Cred_Op, Truth_Value_In, Cred, Truth_Value_Out],

	add_preffix_to_name(Pred2_Name, "rfuzzy_aux_", Pred2_Name_Aux),
	functor(Pred2_Functor_Aux, Pred2_Name_Aux, New_Pred_Arity),
	copy_args(Pred2_Arity, Pred_Functor, Pred2_Functor_Aux),
	arg(New_Pred_Arity, Pred2_Functor_Aux, Truth_Value_In),

	Cl = (Pred_Functor :- Pred2_Functor_Aux, Credibility_Functor, (Truth_Value_Out .>=. 0, Truth_Value_Out .=<. 1)),
	append_local(Pred_Type, ['rfuzzy_truth_value_type'], Pred2_Type),
	% save_fuzzy_rule_predicate_definition(Pred_Name, Pred_Arity, Pred_Type, Pred_Class)
	save_fuzzy_rule_predicate_definition(Pred_Name, Pred_Arity, Pred_Type, Pred_Class, 'true', Cls_Aux),
	Cls = [ Cl | Cls_Aux ], 
	!.

translate(rfuzzy_antonym(Pred2_Name, Pred_Name, Cred_Op, Cred), Cls):-
	!,
	print_msg('debug', 'translate(rfuzzy_antonym(Pred2_Name, Pred_Name, Cred_Op, Cred)) ', rfuzzy_antonym(Pred2_Name, Pred_Name, Cred_Op, Cred)),
	nonvar(Pred_Name), nonvar(Pred2_Name), nonvar(Cred_Op), nonvar(Cred), number(Cred),
	test_aggregator_is_defined(Cred_Op, 'true'),

	% retrieve_predicate_info(Pred_Name, Pred_Arity, Pred_Type, More_Info, Needs_Head_Building, Show_Error),
	retrieve_predicate_info(Pred2_Name, Pred2_Arity, Pred2_Type, _MI_2, _NHB_2, 'true'),
	( 
	    (
		nonvar(Pred2_Arity), nonvar(Pred2_Type), !
	    )
	;
	    (
		print_msg('error', 'You must define the type for the predicate', Pred2_Name), !, fail
	    )
	),

	Pred_Class = 'fuzzy_rule_antonym', Pred_Arity is Pred2_Arity - 1,
	% translate_predicate(Pred_Name, Pred_Arity, Pred_Class, New_Pred_Name, New_Pred_Arity).
	translate_predicate(Pred_Name, Pred_Arity, Pred_Class, New_Pred_Name, New_Pred_Arity),
	% predicate_to_functor(Pred_Name, Pred_Arity, Pred_Class, Pred_Functor, Truth_Value).
	predicate_to_functor(New_Pred_Name, New_Pred_Arity, Pred_Class, Pred_Functor, Truth_Value_Out),

	functor(Credibility_Functor, Cred_Op, 3), 
	Credibility_Functor=..[Cred_Op, Truth_Value_Aux, Cred, Truth_Value_Out],

	add_preffix_to_name(Pred2_Name, "rfuzzy_aux_", Pred2_Name_Aux),
	functor(Pred2_Functor_Aux, Pred2_Name_Aux, New_Pred_Arity),
	copy_args(Pred2_Arity, Pred_Functor, Pred2_Functor_Aux),
	arg(New_Pred_Arity, Pred2_Functor_Aux, Truth_Value_In),

	Cl = (Pred_Functor :- Pred2_Functor_Aux, (Truth_Value_Aux .=. 1 - Truth_Value_In), Credibility_Functor, (Truth_Value_Out .>=. 0, Truth_Value_Out .=<. 1)),
	append_local(Pred_Type, ['rfuzzy_truth_value_type'], Pred2_Type),
	% save_fuzzy_rule_predicate_definition(Pred_Name, Pred_Arity, Pred_Type, Pred_Class)
	save_fuzzy_rule_predicate_definition(Pred_Name, Pred_Arity, Pred_Type, Pred_Class, 'true', Cls_Aux),
	Cls = [ Cl | Cls_Aux ], 
	!.

translate((rfuzzy_quantifier(Pred_Name/Pred_Arity, Var_In, Var_Out) :- Code), Translation) :- !,
	print_msg('debug', 'translate: rfuzzy_quantifier(Pred_Name/Pred_Arity)', rfuzzy_quantifier(Pred_Name/Pred_Arity)),
	save_rfuzzy_quantifiers_list([(Pred_Name, Pred_Arity, Var_In, Var_Out, Code)], Translation),
	print_msg('debug', 'translate: rfuzzy_quantifier(Pred_Name/Pred_Arity)', rfuzzy_quantifier(Pred_Name/Pred_Arity)).

% fuzzification:
translate(rfuzzy_define_fuzzification(Pred_Name, Crisp_Pred_Name, Funct_Pred_Name), Cls):-
	!, % If patter matching, backtracking forbiden.
	print_msg('debug', 'translate: rfuzzy_define_fuzzification(Pred_Name, Crisp_Pred_Name, Funct_Pred_Name)', rfuzzy_define_fuzzification(Pred_Name, Crisp_Pred_Name, Funct_Pred_Name)),
	% retrieve_predicate_info(Pred_Name, Pred_Arity, Pred_Type, More_Info, Needs_Head_Building, Show_Error) 
	retrieve_predicate_info(Crisp_Pred_Name,  2, Pred_Type_1, _MI_1, _NHB_1, 'true'),
	retrieve_predicate_info(Funct_Pred_Name, 2, Pred_Type_2, _MI_2, _NBH_2, 'true'),
	(
	    (	var(Pred_Type_1), 
		print_msg('error', 'Types for predicates used in fuzzification must be defined before', (Crisp_Pred_Name)), 
		!, fail    )
	;
	    nonvar(Pred_Type_1)
	),
	(
	    (	var(Pred_Type_2), 
		print_msg('error', 'Types for predicates used in fuzzification must be defined before', (Funct_Pred_Name)), 
		!, fail    )
	;	
	    nonvar(Pred_Type_2)
	),

	get_nth_element_from_list(1, Pred_Type_1, Type_1),
 	get_nth_element_from_list(2, Pred_Type_1, Type_2_Pred_1),
 	get_nth_element_from_list(1, Pred_Type_2, Type_2_Pred_2),
 	get_nth_element_from_list(2, Pred_Type_2, Type_3),
	(
	    Type_2_Pred_1 = 'rfuzzy_integer_type'
	;
	    Type_2_Pred_1 = 'rfuzzy_float_type'
	;
	    (
		print_msg('error', 'Type of predicate is not suitable for fuzzification', Crisp_Pred_Name), 
		!, fail    
	    )
	),
	(
	    (
		Type_2_Pred_2 = 'rfuzzy_number_type',
		Type_3 = 'rfuzzy_truth_value_type'
	    )
	;
	    (
		print_msg('error', 'Type of predicate is not suitable for fuzzification', Funct_Pred_Name), 
		!, fail    
	    )
	),

	Pred_Class = 'fuzzy_rule_fuzzification', Pred_Arity = 1,
	% translate_predicate(Pred_Name, Pred_Arity, Pred_Class, New_Pred_Name, New_Pred_Arity).
	translate_predicate(Pred_Name, Pred_Arity, Pred_Class, New_Pred_Name, New_Pred_Arity),
	% predicate_to_functor(Pred_Name, Pred_Arity, Pred_Class, Pred_Functor, Truth_Value).
	predicate_to_functor(New_Pred_Name, New_Pred_Arity, Pred_Class, Pred_Functor, Truth_Value),

	% We need to do here as in other translations, so 
	% it generates aux and main predicates for fuzzifications too.
	functor(Crisp_Pred_Functor, Crisp_Pred_Name, 2),
	functor(Funct_Pred_Functor, Funct_Pred_Name, 2),

	arg(1, Pred_Functor, Input),
	arg(1, Crisp_Pred_Functor, Input),
	arg(2, Crisp_Pred_Functor, Crisp_Value),
	arg(1, Funct_Pred_Functor, Crisp_Value),
	arg(2, Funct_Pred_Functor, Truth_Value),

	Cl = (Pred_Functor :- (Crisp_Pred_Functor, Funct_Pred_Functor)),

	Pred_Type = [Type_1],
	save_fuzzy_rule_predicate_definition(Pred_Name, Pred_Arity, Pred_Type, Pred_Class, 'true', Cls_Aux),
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
	% save_predicate_definition(Pred_Name, Pred_Arity, Pred_Type, More_Info, Needs_Head_Building)
	save_predicate_definition(Pred_Name, Pred_Arity, _Pred_Type, [], 'no').

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
		retrieve_predicate_info(Real_Pred_Name, Real_Pred_Arity, Retrieved_Pred_Type, _More_Info, _NBH_3, 'no'), 
		nonvar(Retrieved_Pred_Type), !,
		Retrieved_Pred_Type = Pred_Type,
		Cls = []
	    )
	;
	    (   % Define it !!!
		% save_predicate_definition(Pred_Name, Pred_Arity, Pred_Type, More_Info, Needs_Head_Building)
		save_predicate_definition(Real_Pred_Name, Real_Pred_Arity, Pred_Type, Pred_More_Info, 'no'),
		Cls = [Cl]
	    )
	), !.

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

% translate_db_description(Description, Index, Max_Index, Types, DB_Fields) 
translate_db_description([(Field_Name, Field_Type)], Index, Index, [Field_Type], [Field_Name]) :- nonvar(Index), !.
translate_db_description([(Field_Name, Field_Type) | Description], Index, Max_Index, [Field_Type|Types], [Field_Name|DB_Fields]) :-
	nonvar(Index), nonvar(Max_Index), Index < Max_Index, !,
	New_Index is Index + 1,
	translate_db_description(Description, New_Index, Max_Index, Types, DB_Fields).
% translate_db_fields(DB_Fields, Index, Max_Index, DB_Pred_Name, Cls) 
translate_db_fields([Pred_Name], Index, Index, DB_Pred_Name, [Cl]) :- 
	print_msg('debug', 'translate_db_fields([Pred_Name], Index, Index, DB_Pred_Name)', ([Pred_Name], Index, Index, DB_Pred_Name)),
	nonvar(Index), !,
	translate_rfuzzy_db_value_for(Pred_Name, DB_Pred_Name, Index, Cl).
translate_db_fields([Pred_Name|DB_Fields], Index, Max_Index, DB_Pred_Name, [Cl | Cls]) :-
	print_msg('debug', 'translate_db_fields([Pred_Name], Index, Max_Index, DB_Pred_Name)', ([Pred_Name], Index, Max_Index, DB_Pred_Name)),
	nonvar(Index), nonvar(Max_Index), Index < Max_Index, !,
	New_Index is Index + 1,
	translate_rfuzzy_db_value_for(Pred_Name, DB_Pred_Name, Index, Cl),
	translate_db_fields(DB_Fields, New_Index, Max_Index, DB_Pred_Name, Cls).

translate_rfuzzy_db_value_for(Pred_Name, Database_Pred_Name, Position, Cl) :-
	print_msg('debug', 'translate_rfuzzy_db_value_for(Pred_Name, Database_Pred_Name, Position)', (Pred_Name, Database_Pred_Name, Position)),
	nonvar(Pred_Name), nonvar(Database_Pred_Name), nonvar(Position), 
	% retrieve_predicate_info(Pred_Name, Pred_Arity, Pred_Type, More_Info, Needs_Head_Building, Show_Error)
	retrieve_predicate_info(Database_Pred_Name, Database_Pred_Arity, Database_Pred_Type, _List, _NHB, 'true'), !,
	print_msg('debug', 'translate_rfuzzy_db_value_for(Database_Pred_Arity, Database_Pred_Type)', (Database_Pred_Arity, Database_Pred_Type)),	
	(
	    (	nonvar(Database_Pred_Arity), nonvar(Database_Pred_Type), !     )
	;
	    (	print_msg('error', 'You must define the database type before. Database', Database_Pred_Name), !, fail     )
	),
	Position =< Database_Pred_Arity, 
	get_nth_element_from_list(Position, Database_Pred_Type, Type),
	print_msg('debug', 'translate_rfuzzy_db_value_for(Position, Type, Pred_Name)', (Position, Type, Pred_Name)),

	functor(DB_Pred_Functor, Database_Pred_Name, Database_Pred_Arity),
	arg(Position, DB_Pred_Functor, Value),
	functor(Mapping, '=', 2), arg(1, Mapping, Input), arg(2, Mapping, DB_Pred_Functor),
	functor(Test, '\\==', 2), arg(1, Test, Value), arg(2, Test, 'null'),

	Pred_Type = [Database_Pred_Name, Type],
	print_msg('debug', 'translate_rfuzzy_db_value_for(Pred_Name, Pred_Type)', (Pred_Name, Pred_Type)),
	(
	    (	translate_rfuzzy_db_value_aux(Type, Pred_Name, Pred_Type, Input, Value, Pred_Functor, Conversion), !    )
	;
	    (	print_msg('error', 'Error translating db definition for (Pred_Name, Pred_Type)', (Pred_Name, Pred_Type)), !, fail    )
	),
	
	Cl = (Pred_Functor :- (((Mapping, DB_Pred_Functor), Test), Conversion)),
	print_msg('debug', 'translate_rfuzzy_db_value_for(Pred_Name, Cl)', (Pred_Name, Cl)).

translate_rfuzzy_db_value_aux(Type, Pred_Name, Pred_Type, Input, Value, Pred_Functor, Conversion) :-
	
		nonvar(Type), nonvar(Pred_Name), (Type = 'rfuzzy_truth_value_type'), !, 
		Pred_Class = 'fuzzy_rule_db_value', Pred_Arity=1,
		% translate_predicate(Pred_Name, Pred_Arity, Pred_Class, New_Pred_Name, New_Pred_Arity).
		translate_predicate(Pred_Name, Pred_Arity, Pred_Class, New_Pred_Name, New_Pred_Arity),
		% predicate_to_functor(Pred_Name, Pred_Arity, Pred_Class, Pred_Functor, Truth_Value).
		predicate_to_functor(New_Pred_Name, New_Pred_Arity, Pred_Class, Pred_Functor, Truth_Value),
		arg(1, Pred_Functor, Input),
		functor(Conversion, '.=.', 2), arg(1, Conversion, Value), arg(2, Conversion, Truth_Value),
		% save_fuzzy_rule_predicate_definition(Pred_Name, Pred_Arity, Pred_Type, Pred_Class)
		save_fuzzy_rule_predicate_definition(Pred_Name, Pred_Arity, Pred_Type, Pred_Class, 'no', _Cls_Unused).

translate_rfuzzy_db_value_aux(Type, Pred_Name, Pred_Type, Input, Value, Pred_Functor, Conversion) :-
		nonvar(Type), nonvar(Pred_Name), 
		(Type = 'rfuzzy_string_type' ; Type = 'rfuzzy_integer_type' ; Type = 'rfuzzy_enum_type' ;
		    Type = 'rfuzzy_boolean_type' ; Type = 'rfuzzy_datetime_type'), !, 
		Pred_Arity = 2,
		functor(Pred_Functor, Pred_Name, Pred_Arity),
		arg(1, Pred_Functor, Input), arg(2, Pred_Functor, Value),
		Conversion = 'true',
		% save_predicate_definition(Pred_Name, Pred_Arity, Pred_Type, More_Info, Needs_Head_Building)
		save_predicate_definition(Pred_Name, Pred_Arity, Pred_Type, [(Type, Pred_Name, Pred_Arity)], 'no').

translate_rfuzzy_db_value_aux(Type, Pred_Name, Pred_Type, Input, Value, Pred_Functor, Conversion) :-
		nonvar(Type), nonvar(Pred_Name), (Type = 'rfuzzy_float_type'), !, 
		Pred_Arity = 2,
		functor(Pred_Functor, Pred_Name, Pred_Arity),
		arg(1, Pred_Functor, Input), arg(2, Pred_Functor, Value_Out),
		functor(Conversion, '.=.', 2), arg(1, Conversion, Value), arg(2, Conversion, Value_Out),
		% save_predicate_definition(Pred_Name, Pred_Arity, Pred_Type, More_Info, Needs_Head_Building)
		save_predicate_definition(Pred_Name, Pred_Arity, Pred_Type, [(Type, Pred_Name, Pred_Arity)], 'no').

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
	% save_predicate_definition(Pred_Name, Pred_Arity, Pred_Type, More_Info, Needs_Head_Building)
	save_predicate_definition(Pred_Name, Pred_Arity, Pred_Type, [], 'no'), !,

	save_rfuzzy_quantifiers_list(More, Translations).

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

translate_rule(Head, Cred_Op, Cred, Body, Cls) :-
	print_msg('debug', 'translate_rule(Head, Cred_Op, Cred, Body) ', (translate_rule(Head, Cred_Op, Cred, Body))),
	nonvar(Head), nonvar(Cred_Op), nonvar(Body), number(Cred),

	functor(Head, Pred_Name, Pred_Arity),
	Pred_Class = 'fuzzy_rule_rule', 
	% translate_predicate(Pred_Name, Pred_Arity, Pred_Class, New_Pred_Name, New_Pred_Arity).
	translate_predicate(Pred_Name, Pred_Arity, Pred_Class, New_Pred_Name, New_Pred_Arity),
	% predicate_to_functor(Pred_Name, Pred_Arity, Pred_Class, Pred_Functor, Truth_Value).
	predicate_to_functor(New_Pred_Name, New_Pred_Arity, Pred_Class, Pred_Functor, Truth_Value),

	% Translate all predicates in the body.
	extract_aggregator(Body, TV_Aggregator, Tmp_Body),

	translate_rule_body(Tmp_Body, TV_Aggregator, Truth_Value, Fuzzy_Body), 
	Cls = (Pred_Functor :- Fuzzy_Body),

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
	% retrieve_predicate_info(Pred_Name, Pred_Arity, Pred_Type, More_Info, Needs_Head_Building, Show_Error),
	retrieve_predicate_info(Pred_Name, Pred_Arity, Pred_Type, _MI, _NHB, Show_Error),
	nonvar(Pred_Type), 
	Pred_Type = ['rfuzzy_truth_value_type', 'rfuzzy_truth_value_type', 'rfuzzy_truth_value_type'].

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

% Security issues
translate_rule_body(Body_F, TV_Aggregator, _Truth_Value, _FB) :- 
	print_msg('debug', 'translate_rule_body(Body_F, TV_Aggregator) - variable problem', (Body_F, TV_Aggregator)),
	var(Body_F), !, fail. % If this is a variable the tranlate rules loop forever !!!

% Conjunction.
translate_rule_body((Tmp_Body_1, Tmp_Body_2), TV_Aggregator, Truth_Value, (FB_1, FB_2, Aggr_F)) :- !,
	print_msg('debug', 'translate_rule_body(Body, TV_Aggregator, Truth_Value) - conjunction',((Tmp_Body_1, Tmp_Body_2), TV_Aggregator, Truth_Value)),
	nonvar(TV_Aggregator),
	\+ ( TV_Aggregator = 'none' ),
	translate_rule_body(Tmp_Body_1, TV_Aggregator, TV_1, FB_1),
	translate_rule_body(Tmp_Body_2, TV_Aggregator, TV_2, FB_2),
	functor(Aggr_F, TV_Aggregator, 3),
	arg(1, Aggr_F, TV_1), 
	arg(2, Aggr_F, TV_2), 
	arg(3, Aggr_F, Truth_Value), !.

% Quantifier.
translate_rule_body(Body_F, _TV_Aggregator, Truth_Value, Translation) :-
	print_msg('debug', 'translate_rule_body(Body, Truth_Value) - with quantifier',(Body_F, Truth_Value)),
	nonvar(Body_F),
	functor(Body_F, Pred_Name, 1),

	Pred_Arity = 1, Pred_Class = 'quantifier',
	% translate_predicate(Pred_Name, Pred_Arity, Pred_Class, New_Pred_Name, New_Pred_Arity).
	translate_predicate(Pred_Name, Pred_Arity, Pred_Class, New_Pred_Name, New_Pred_Arity),
	% predicate_to_functor(Pred_Name, Pred_Arity, Pred_Class, Pred_Functor, Truth_Value).
	predicate_to_functor(New_Pred_Name, New_Pred_Arity, Pred_Class, Pred_Functor, Truth_Value),

	% retrieve_predicate_info(Pred_Name, Pred_Arity, Pred_Type, More_Info, Needs_Head_Building, Show_Error) 
	retrieve_predicate_info(New_Pred_Name, New_Pred_Arity, Pred_Type, _MI, _NHB, 'true'), 
	nonvar(Pred_Type), Pred_Type = ['rfuzzy_predicate_type', 'rfuzzy_truth_value_type'],

	arg(1, Body_F, SubBody),
	print_msg('debug', 'translate_rule_body(SubBody)',(SubBody)),
	translate_rule_body(SubBody, 'none', _SubCall_Truth_Value, SubCall),
	print_msg('debug', 'translate_rule_body(SubBody, SubCall)',(SubBody, SubCall)),
	arg(1, Pred_Functor, SubCall),
	Translation = (Pred_Functor, (Truth_Value .>=. 0, Truth_Value .=<. 1)),
	print_msg('debug', 'translate_rule_body(Translation) - with quantifier',(Translation)).

% Normal.
translate_rule_body(Body_F, _TV_Aggregator, Truth_Value, Translation) :-
	print_msg('debug', 'translate_rule_body(Body, Truth_Value) - without quantifier',(Body_F, Truth_Value)),
	nonvar(Body_F),
	functor(Body_F, Pred_Name, Pred_Arity),

	Pred_Class = 'fuzzy_rule',
	% translate_predicate(Pred_Name, Pred_Arity, Pred_Class, New_Pred_Name, New_Pred_Arity).
	translate_predicate(Pred_Name, Pred_Arity, Pred_Class, New_Pred_Name, New_Pred_Arity),
	% predicate_to_functor(Pred_Name, Pred_Arity, Pred_Class, Pred_Functor, Truth_Value).
	predicate_to_functor(New_Pred_Name, New_Pred_Arity, Pred_Class, Pred_Functor, Truth_Value),

	% retrieve_predicate_info(Pred_Name, Pred_Arity, Pred_Type, More_Info, Needs_Head_Building, Show_Error) 
	retrieve_predicate_info(New_Pred_Name, New_Pred_Arity, Pred_Type, _MI, _NHB, 'true'), 
	nonvar(Pred_Type),
	get_nth_element_from_list(New_Pred_Arity, Pred_Type, 'rfuzzy_truth_value_type'),

	Translation = (Pred_Functor, (Truth_Value .>=. 0, Truth_Value .=<. 1)),
	print_msg('debug', 'translate_rule_body(Body, Translation)',(Body_F, Translation)),
	print_msg_nl('debug').

translate_rule_body(Body_F, _TV_Aggregator, _Truth_Value, _Result) :-
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
	% retrieve_predicate_info(Pred_Name, Pred_Arity, Pred_Type, More_Info, Needs_Head_Building, Show_Error),
	retrieve_predicate_info(Pred_Name, Pred_Arity, _Pred_Type, _More_Info, _Needs_Head_Building, 'true'),
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
	    (	nonvar(Pred_Name), nonvar(Pred_Arity), nonvar(Pred_Class), !    )
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
build_auxiliary_clauses([Predicate_Def|Predicate_Defs], Clauses) :-
	print_msg_nl('debug'),
	print_msg('debug', 'build_auxiliary_clauses IN (Predicate_Def)', (Predicate_Def)),
	predicate_definition_does_not_need_auxiliar(Predicate_Def), !,
	print_msg('debug', 'build_auxiliary_clauses OUT', 'nothing'),
	build_auxiliary_clauses(Predicate_Defs, Clauses).	
build_auxiliary_clauses([Predicate_Def|Predicate_Defs], [Pred_Main, Pred_Aux | Clauses]) :-
	print_msg_nl('debug'),
	print_msg('debug', 'build_auxiliary_clauses IN (Predicate_Def)', (Predicate_Def)),
	build_auxiliary_clause(Predicate_Def, Pred_Main, Pred_Aux), !,
	print_msg('debug', 'build_auxiliary_clauses OUT (Pred_Main, Pred_Aux)', (Pred_Main, Pred_Aux)),
	build_auxiliary_clauses(Predicate_Defs, Clauses).	

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------
predicate_definition_does_not_need_auxiliar(predicate_definition(_Pred_Name, _Pred_Arity, _Pred_Type, _List, 'no')).

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

build_auxiliary_clause(predicate_definition(Pred_Name, Pred_Arity, _Pred_Type, List, 'true'), Fuzzy_Cl_Main, Fuzzy_Cl_Aux) :-

	% Build MAIN functor.
	functor(Pred_Functor, Pred_Name, Pred_Arity),
	% Build AUXILIAR functor
	add_preffix_to_name(Pred_Name, "rfuzzy_aux_", Aux_Pred_Name),
	Aux_Pred_Arity is Pred_Arity + 1,
	functor(Aux_Pred_Functor, Aux_Pred_Name, Aux_Pred_Arity),
	arg(Aux_Pred_Arity, Aux_Pred_Functor, Aux_Pred_Truth_Value_Arg),
	arg(Pred_Arity,         Aux_Pred_Functor, Aux_Pred_Priority_Arg),
	% Unify crisp args of MAIN and AUXILIAR functors.
	Pred_Crisp_Arity is Pred_Arity - 1,
	copy_args(Pred_Crisp_Arity, Pred_Functor, Aux_Pred_Functor),

	print_msg('debug', 'Now building functor from (List, Pred_Name, Aux_Pred_Functor)', (List, Pred_Name, Aux_Pred_Functor)),
	build_functors(List, 'fuzzy_rule_type',                 'true', Aux_Pred_Functor, Fuzzy_Pred_Types, [], Def_1, [], NDef_1),
	build_functors(List, 'fact',                              'fail',  Aux_Pred_Functor, Fuzzy_Pred_Fact, Def_1, Def_2, NDef_1, NDef_2),
	build_functors(List, 'fuzzification',                'fail',  Aux_Pred_Functor, Fuzzy_Pred_Fuzzification, Def_2, Def_3, NDef_2, NDef_3),
	build_functors(List, 'rule',                              'fail',  Aux_Pred_Functor, Fuzzy_Pred_Rule, Def_3, Def_4, NDef_3, NDef_4),
	build_functors(List, 'default_with_cond',      'fail',  Aux_Pred_Functor, Fuzzy_Pred_Default_With_Cond, Def_4, Def_5, NDef_4, NDef_5),
	build_functors(List, 'default_without_cond', 'fail',  Aux_Pred_Functor, Fuzzy_Pred_Default_Without_Cond, Def_5, Def_6, NDef_5, NDef_6),
	build_functors(List, 'synonym',                     'fail',  Aux_Pred_Functor, Fuzzy_Pred_Synonym, Def_6, Def_7, NDef_6, NDef_7),
	build_functors(List, 'antonym',                      'fail',  Aux_Pred_Functor, Fuzzy_Pred_Antonym, Def_7, Def_8, NDef_7, NDef_8),
	build_functors(List, 'fuzzy_rule_db_value',  'fail',  Aux_Pred_Functor, Fuzzy_Pred_DB_Value, Def_8, Def, NDef_8, NDef),
	build_functors_notify_missing_facilities(Pred_Name, Def, NDef),

	(Fuzzy_Cl_Main = (
			     (
				 Pred_Functor :- (
						     print_msg('debug', 'Predicate called', Pred_Functor),
						     findall(Aux_Pred_Functor, Aux_Pred_Functor, Results), 
						     supreme(Results, Pred_Functor)
						 )		     
			     ) % Main Fuzzy Pred
			 )
	),
	(Fuzzy_Cl_Aux = ( 
			    ( 
				Aux_Pred_Functor :- ( 
							Fuzzy_Pred_Types,
							(   
							    Fuzzy_Pred_Fact ; 
							    Fuzzy_Pred_Fuzzification ;
							    Fuzzy_Pred_Rule ;
							    Fuzzy_Pred_Default_With_Cond ; 
							    Fuzzy_Pred_Default_Without_Cond ;
							    Fuzzy_Pred_Synonym ;
							    Fuzzy_Pred_Antonym ;
							    Fuzzy_Pred_DB_Value
							),
							% Security conditions.
							Aux_Pred_Truth_Value_Arg .>=. 0,
							Aux_Pred_Truth_Value_Arg .=<. 1,
							Aux_Pred_Priority_Arg .>=. 0,
							Aux_Pred_Priority_Arg .=<. 1
						    )
			    )
			)
	).

build_auxiliary_clause(Predicate_Definition, _Fuzzy_Cl_Main, _Fuzzy_Cl_Aux) :-
	print_msg('error', 'Error building auxiliary clauses for predicate definition', Predicate_Definition),
	!, fail.

build_functors([], Category, On_Error, _Functor_In, On_Error, Def_In, Def_In, NDef_In, [Category | NDef_In]) :- !.
	
build_functors([(Category, Sub_Pred_Name, Sub_Pred_Arity)|_List], Category, _On_Error, Functor_In, Functor, Def_In, [Category | Def_In], NDef_In, NDef_In) :-

	!, % Backtracking not allowed.
	functor(Functor, Sub_Pred_Name, Sub_Pred_Arity),  % Create functor
	copy_args(Sub_Pred_Arity, Functor_In, Functor).              % Unify args with the auxiliar one.

build_functors([(_Other_Category, _Sub_Pred_Name, _Sub_Pred_Arity)|List], Category, On_Error, Functor_In, Functor, Def_In, Def_Out, NDef_In, NDef_Out) :-
	build_functors(List, Category, On_Error, Functor_In, Functor, Def_In, Def_Out, NDef_In, NDef_Out).

% build_functors_notify_missing_facilities(Pred_Name, Def, NDef),
build_functors_notify_missing_facilities(Pred_Name, _Def, NDef_In) :-
	% Not an error if the missing information belongs to the following categories:
	NDef_Ignore=['fact', 'function', 'fuzzification', 'rule', 'default_with_cond', 'synonym', 'antonym', 'fuzzy_rule_db_value'],
	lists_substraction(NDef_In, NDef_Ignore, NDef_Out),
	(
	    (   NDef_Out = [], !  )
	;
	    print_msg('info', 'Facilities not defined for the predicate :: (Predicate_Name, Facilities) ', (Pred_Name, NDef_Out))
	), !.

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

lists_substraction([], _List_2, []) :- !.
lists_substraction([Head | Tail ], List_2, Result_List) :-
	memberchk_local(Head, List_2), !, 
	lists_substraction(Tail, List_2, Result_List).
lists_substraction([Head | Tail ], List_2, [Head | Result_List]) :-
	lists_substraction(Tail, List_2, Result_List).

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

boolean_or('true', _Any, 'true').
boolean_or(_Any, 'true', 'true').
boolean_or('no', 'no', 'no').

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

generate_truth_value_equal_to_functor(Fixed_Truth_Value, Truth_Value, Truth_Value_Functor) :-
	functor(Truth_Value_Functor, '.=.', 2),
	arg(1, Truth_Value_Functor, Truth_Value),
	arg(2, Truth_Value_Functor, Fixed_Truth_Value), !.

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
