:- module(rfuzzy_tr,[rfuzzy_trans_sentence/3, rfuzzy_trans_clause/3],[]).

:- use_module(library(aggregates), [findall/3]).
:- use_module(library(terms),[copy_args/3]).
:- use_module(library('rfuzzy/rfuzzy_rt')).
:- include(library('clpqr-common/ops')).
:- include(library('rfuzzy/rfuzzy_ops')).

% Important info to be saved.
:- data predicate_definition/5.
:- data connectives/1.
:- data sentences/2.
:- data defined_modifiers_code/1.
:- data defined_negation_ops_code/1.

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

clean_up_asserted_facts :-
	findall('done', retract_fact(predicate_definition(_11, _12, _13, _14, _15)), _Removed_1),
	findall('done', retract_fact(connectives(_21)), _Removed_2),
	findall('done', retract_fact(sentences(_31, _32)), _Removed_3),
	findall('done', retract_fact(defined_modifiers_code(_41)), _Removed_4),
	!.

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

get_auxiliar_suffix('rfuzzy_aux').

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

save_predicates_definition_list([], _P_A, _P_T, _P_O, _P_MI) :- !.
save_predicates_definition_list([P_N | Pred_List], P_A, P_T, P_O, P_MI) :-
	save_predicate_definition(P_N, P_A, P_T, P_O, P_MI), !,
	save_predicates_definition_list(Pred_List, P_A, P_T, P_O, P_MI).

save_predicate_definition(P_N, P_A, P_T, P_O, P_MI) :-
	print_msg('debug', 'save_predicate_definition(P_N, P_A, P_T, P_O, P_MI)', (P_N, P_A, P_T, P_O, P_MI)),
	check_save_predicate_definition_input(P_N, P_A, P_T, P_O, P_MI),
	print_msg('debug', 'check_save_predicate_definition_input', 'ok'),
	!,
	(
	    (	 
		retract_fact(predicate_definition(P_N, P_A, Old_P_T, Old_P_O, Old_P_MI)), !, % Retract last
		print_msg('debug', 'save_predicate_definition :: current', (P_N, P_A, Old_P_T, Old_P_O, Old_P_MI)),
		add_element_to_set_if_any(P_T, Old_P_T, New_P_T),
		add_element_to_set_if_any(P_O, Old_P_O, New_P_O),
		add_element_to_set_if_any(P_MI, Old_P_MI, New_P_MI)
	    )
	;
	    (
		add_element_to_set_if_any(P_T, [], New_P_T),
		add_element_to_set_if_any(P_O, [], New_P_O),
		add_element_to_set_if_any(P_MI, [], New_P_MI)
	    )
	), 
	assertz_fact(predicate_definition(P_N, P_A, New_P_T, New_P_O, New_P_MI)),
	print_msg('debug', 'saved', save_predicate_definition(P_N, P_A, New_P_T, New_P_O, New_P_MI)),
	!.

% sets_union_if_non_empty(New_SubSet, Set, New_Set).
add_element_to_set_if_any(Element, Set, _New_Set) :- 
	(
	    (
		var(Element),
		print_msg('error', 'sets_union_if_non_empty', 'New_SubSet cannot be a variable')
	    )
	;
	    (
		var(Set),
		print_msg('error', 'sets_union_if_non_empty', 'Set cannot be a variable')
	    )
	),
	!, fail.

add_element_to_set_if_any([], Set, Set) :- !.
add_element_to_set_if_any(Element, Set, New_Set) :- 
	sets_union([Element], Set, New_Set), !.

retrieve_predicate_info(P_N, P_A, P_T, Show_Error) :-
	print_msg('debug', 'retrieve_predicate_info(P_N, P_A, P_T, Show_Error)', retrieve_predicate_info(P_N, P_A, P_T, Show_Error)),
	(
	    (   predicate_definition(P_N, P_A, P_T, P_O, P_MI), !,
		print_msg('debug', 'retrieved', retrieve_predicate_info(P_N, P_A, P_T, P_O, P_MI))   
	    )
	;
	    (   Show_Error = 'no', !, 
		print_msg('debug', 'not retrieved', 'not showing error'),   fail  )
	;
	    (	Show_Error = 'true', !,
		print_msg('error', 'Predicate must be defined before use. Predicate ', P_N/P_A), !, 
		fail
	    )
	).

retrieve_all_predicate_infos(P_N, P_A, Retrieved) :-
	findall((predicate_definition(P_N, P_A, P_T, P_O, P_MI)), 
	predicate_definition(P_N, P_A, P_T, P_O, P_MI), Retrieved), !.
%	(retract_fact(predicate_definition(P_N, P_A, P_T, P_MI))), Retrieved),
%	 !.

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------
	
check_save_predicate_definition_input(P_N, P_A, P_T, P_O, P_MI) :-
	print_msg('debug', 'check_save_predicate_definition_input(P_N, P_A, P_T, P_O)', (P_N, P_A, P_T, P_O)),
	( 
	    (	nonvar(P_N), !    )
	;
	    (	var(P_N), 
		print_msg('error', 'save_predicate_definition: P_N cannot be a variable. Value', P_N), !, fail
	    )
	),
	( 
	    (	nonvar(P_A), number(P_A), !	)
	;
	    (   nonvar(P_A),
		print_msg('error', 'save_predicate_definition: P_A must be a number. Value', P_A), !, fail
	    )
	; 
	    (	var(P_A), 
		print_msg('error', 'save_predicate_definition: P_A cannot be a variable. Value', P_A), !, fail
	    )
	),
	(
	    (	var(P_T),
		print_msg('error', 'save_predicate_definition: P_T cannot be a variable. Value', P_T), !, fail
	    )
	;
	    (	nonvar(P_T), list(P_T), !   )
	;
	    (	nonvar(P_T),
		print_msg('error', 'save_predicate_definition: P_T must be a list. Value', P_T), !, fail
	    )
	), 
	(
	    (	var(P_O),
		print_msg('error', 'save_predicate_definition: P_O cannot be a variable. Value', P_O), !, fail
	    )
	;
	    (	list(P_O),
		print_msg('error', 'save_predicate_definition: P_O cannot be a list. Value', P_O), !, fail
	    )
	;   
	    (	nonvar(P_O), !     )
	),
	(
	    print_msg('debug', 'check_pred_type_aux(P_A, P_T)', (P_A, P_T)),
	    check_pred_type_aux(P_A, P_T), !
	;
	    (
		print_msg('error', 'Types in type definition do not sum up the arity value. (P_N, P_A, P_T)', (P_N, P_A, P_T)), 
		!, fail
	    )
	), 
	(
	    (
		nonvar(P_MI), P_MI = []
	    )
	;
	    (
		nonvar(P_MI), P_MI = (Selector, Options), nonvar(Selector), list(Options)
	    )
	;
	    (
		print_msg('error', 'Predicate More Information is not a tuple of an element and a list. (P_N, P_A, P_T, P_MI)', (P_N, P_A, P_T, P_MI)), 
		!, fail
	    )
	),
	!.

check_save_predicate_definition_input(P_N, P_A, P_T, P_O, P_MI) :-
	print_msg('debug', 'check_save_predicate_definition_input :: (P_N, P_A, P_T, P_O, P_MI)', (P_N, P_A, P_T, P_O, P_MI)), !, fail.

check_pred_type_aux(0, []) :- !.
check_pred_type_aux(1, [_P_T]) :- !.
check_pred_type_aux(P_A, [_P_T|More]) :-
	New_P_A is P_A -1,
	check_pred_type_aux(New_P_A, More).

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
rfuzzy_trans_sent_aux(end_of_file, Code_Before_EOF_with_EOF):-
	!,
	retrieve_all_predicate_infos(_Any_P_N, _Any_P_A, All_Predicate_Infos),
	print_msg('debug', 'all_predicate_info', All_Predicate_Infos),
	generate_code_from_saved_info(All_Predicate_Infos, [], Cls_1, [end_of_file], Cls_2),
	append_local(Cls_1, Cls_2, Generated_Code),
	print_msg('debug', 'Generated_Code', Generated_Code),
	add_auxiliar_code(Generated_Code, Code_Before_EOF_with_EOF), 
	clean_up_asserted_facts.

rfuzzy_trans_sent_aux(0, []) :- !, 
	% activate_rfuzzy_debug,
	clean_up_asserted_facts,
	print_msg_nl('info'), print_msg_nl('info'), 
	print_msg('info', 'Rfuzzy (Ciao Prolog package to compile Rfuzzy programs into a pure Prolog programs)', 'compiling ...'),
	print_msg_nl('info'),
	% save_predicate_definition(P_N, P_A, P_T, P_O, P_MI)
	save_predicate_definition('rfuzzy_any_type', 1, ['null'], 'type', []),
	save_predicate_definition('rfuzzy_truth_value_type', 1, ['null'], 'type', []),
	save_predicate_definition('rfuzzy_credibility_value_type', 1, ['null'], 'type', []),
	save_predicate_definition('rfuzzy_predicate_type', 1, ['null'], 'type', []),
	save_predicate_definition('rfuzzy_number_type', 1, ['null'], 'type', []),
	save_predicate_definition('fnot', 2, ['rfuzzy_predicate_type', 'rfuzzy_truth_value_type'], 'negation', []),

	save_predicate_definition('rfuzzy_string_type', 1, ['null'], 'type', []),
	save_predicate_definition('rfuzzy_integer_type', 1, ['null'], 'type', []),
	save_predicate_definition('rfuzzy_float_type', 1, ['null'], 'type', []),
	save_predicate_definition('rfuzzy_enum_type', 1, ['null'], 'type', []),
	save_predicate_definition('rfuzzy_boolean_type', 1, ['null'], 'type', []),
	save_predicate_definition('rfuzzy_datetime_type', 1, ['null'], 'type', []),

	rfuzzy_defined_connectives(Defined_Aggregators_List),
	Aggregators_Type = ['rfuzzy_truth_value_type', 'rfuzzy_truth_value_type', 'rfuzzy_truth_value_type'],
	save_predicates_definition_list(Defined_Aggregators_List, 3, Aggregators_Type, 'connective', []),
	
	rfuzzy_compute_defined_comparators(Compute_Defined_Comparators),
	save_predicate_definition('rfuzzy_compute_defined_operators', 0, [], 'framework', ('defined_operators', Compute_Defined_Comparators)),

	rfuzzy_defined_negation_ops(Defined_Negators_List),
	save_rfuzzy_negation_ops_list(Defined_Negators_List, Defined_Negators_Code),
	assertz_fact(defined_negation_ops_code(Defined_Negators_Code)),

	rfuzzy_defined_modifiers(Defined_Modifiers_List),
	save_rfuzzy_modifiers_list(Defined_Modifiers_List, Defined_Modifiers_Code),
	assertz_fact(defined_modifiers_code(Defined_Modifiers_Code)).

rfuzzy_trans_sent_aux((:-activate_rfuzzy_debug), []) :- !,
	activate_rfuzzy_debug.
rfuzzy_trans_sent_aux((:-Whatever), [(:-Whatever)]) :- !.
rfuzzy_trans_sent_aux(Sentence, Translation) :-
	translate(Sentence, Translation).

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

%
% P_F -> Predicate Functor
% P_N -> Predicate Name
% P_A -> Predicate Arity
% P_TN -> Predicate Type Name
% P_TA -> Predicate Type Arity
% P_B -> Predicate Body
% NP_F -> New Predicate Functor
% NP_N -> New Predicate Name
% NP_A -> New Predicate Arity
% If_Cond -> If Condition
% Cred_Op -> Credibility Operator
% Cred_value -> Credibility Value
% UN -> User Name (for personalization)
rfuzzy_pred_info(pred_info(P_F, P_N, P_A, P_TN, P_TA, P_B, NP_F, NP_N, NP_A, If_Cond, Cred_Op, Cred_Value, UN), 
	P_F, P_N, P_A, P_TN, P_TA, P_B, NP_F, NP_N, NP_A, If_Cond, Cred_Op, Cred_Value, UN) :- !.

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

% General syntax: 
% Pred_Functor :~ P_B if Pred_Condition with_credibility (Op, Cred) only_for_user UserName
translate((P_F :~ PB_In), Cls) :- !,
	% Extract P_N, P_T_Name, P_T_Arity
	nonvar(P_F),
	extract_from_PF_values_PN_PA_PTN_PTA(P_F, P_N, P_A, P_TN, P_TA),
	nonvar(P_N), nonvar(P_A), nonvar(P_TN), nonvar(P_TA),
	
	% Extract User Name (if any).
	nonvar(PB_In),
	(
	    (
		PB_In = (PB_In_1 only_for_user UN), 
		nonvar(UN), 
		(
		    (
			UN = 'null',
			print_msg('warning', 'invalid UserName for rule.', 'Taken default instead.')
		    )
		;
		    true
		), !
	    )
	;
	    (
		UN = null,
		PB_In_1 = PB_In
	    )
	),
	
	% Extract credibility if any.
	nonvar(PB_In_1),
	(
	    (
		PB_In_1 = (PB_In_2 with_credibility (Cred_Op, Cred_Value)), 
		nonvar(Cred_Op), nonvar(Cred_Value), !
	    )
	;
	    (
		PB_In_2 = PB_In_1,
		Cred_Op = 'prod', Cred_Value = 1
	    )
	),

	% Remove if condition (if any)
	nonvar(PB_In_2),
	(
	    (
		PB_In_2 = (PB_In_3 if If_Cond), 
		nonvar(If_Cond), !
	    )
	;
	    (
		If_Cond = 'true',
		PB_In_3 = PB_In_2
	    )
	),

	nonvar(PB_In_3),
	P_B = PB_In_3,
	rfuzzy_pred_info(Pred_Info,	P_F, P_N, P_A, P_TN, P_TA, P_B, _NP_F, _NP_N, _NP_A, If_Cond, Cred_Op, Cred_Value, UN),
	!,

	translate_fuzzy(Pred_Info, Cls).

% Although connectives are just crisp predicates of arity 3, 
% we use the following to ensure programmers do not use as connectives
% fuzzy predicates (of arity 3 too). An error like that is very difficult to find.
translate((define_connective(Aggregator_Name/Aggregator_Arity, TV_In_1, TV_In_2, TV_Out) :- Code), [Translation]) :-
	!, % If patter matching, backtracking forbiden.
	nonvar(Aggregator_Name), number(Aggregator_Arity), Aggregator_Arity = 3,

	functor(Aggregator, Aggregator_Name, Aggregator_Arity),
	arg(1, Aggregator, TV_In_1),
	arg(2, Aggregator, TV_In_2),
	arg(3, Aggregator, TV_Out),
	Translation = (Aggregator :- Code),

	Aggregator_Type = ['rfuzzy_truth_value_type', 'rfuzzy_truth_value_type', 'rfuzzy_truth_value_type'],
	% save_predicate_definition(P_N, P_A, P_T, P_MI)
	save_predicate_definition(Aggregator_Name, Aggregator_Arity, Aggregator_Type, 'connective', []),
	!.

translate((define_modifier(P_N/P_A, Var_In, Var_Out) :- Code), Translation) :- !,
	print_msg('debug', 'translate: rfuzzy_modifier(P_N/P_A)', rfuzzy_modifier(P_N/P_A)),
	save_rfuzzy_modifiers_list([(P_N, P_A, Var_In, Var_Out, Code)], Translation),
	print_msg('debug', 'translate: rfuzzy_modifier(P_N/P_A)', rfuzzy_modifier(P_N/P_A)).

translate((define_negation_op(P_N/P_A, Var_In, Var_Out) :- Code), Translation) :- !,
	print_msg('debug', 'translate: define_negation_op(P_N/P_A)', define_negation_op(P_N/P_A)),
	save_rfuzzy_negation_ops_list([(P_N, P_A, Var_In, Var_Out, Code)], Translation),
	print_msg('debug', 'translate: define_negation_op(P_N/P_A)', define_negation_op(P_N/P_A)).

% Predicate type(s) definition (Class = database).
translate(define_database(P_N/P_A, Description), Cls):- !,
	translate_rfuzzy_define_database(P_N, P_A, Description, Cls).

% Predicate type(s) definition (Class \== database).
translate(define_type_for(P_N/P_A, P_T), Cls):-
	!, % If patter matching, backtracking forbiden.
	print_msg('debug', 'translate_rfuzzy_type_for(P_N, P_A, P_T)', (P_N, P_A, P_T)),
	nonvar(P_N), nonvar(P_A), number(P_A), nonvar(P_T), 
	translate_rfuzzy_type_for_crisp_rule(P_N, P_A, P_T, [], Cls),
	print_msg('debug', 'translate_rfuzzy_type_for(P_N, P_A, P_T)', (P_N, P_A, P_T)),
	print_msg('debug', 'translate_rfuzzy_type_for(Cls)', Cls).

% Similarity between elements.
translate((define_similarity_between(Database, Element1, Element2, Truth_Value)), []) :-
	translate_rfuzzy_similarity_between(Database, Element1, Element2, Truth_Value, 'prod', 1).
translate((define_similarity_between(Database, Element1, Element2, Truth_Value) cred (Credibility_Operator, Credibility)), []) :-
	translate_rfuzzy_similarity_between(Database, Element1, Element2, Truth_Value, Credibility_Operator, Credibility).

% crisp predicates (non-facts) and crisp facts.
translate(Other, Other) :-
	print_msg('debug', 'Non-Rfuzzy predicate', Other),
	nonvar(Other), 
	(
	    (
		functor(Other, ':-', 2), !,
		arg(1, Other, Arg_1), 
		nonvar(Arg_1), 
		functor(Arg_1, P_N, P_A)
	    )
	;
	    (
		functor(Other, P_N, P_A), 
		P_N \== ':-',
		P_N \== ':~',
		P_N \== ':#',
		P_N \== 'value',
		P_N \== 'fuzzify'
	    )
	),
	generate_fake_type(P_A, P_T),
	% save_predicate_definition(P_N, P_A, P_T, P_MI)
	save_predicate_definition(P_N, P_A, P_T, 'type', []).

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

translate_fuzzy(Pred_Info, Cls) :-
	print_msg('debug', 'translate_fuzzy ::  (Pred_Info)', (Pred_Info)),
	% Info previously retrieved or to be filled in.
	rfuzzy_pred_info(Pred_Info,	_P_F, P_N, _P_A, P_TN, P_TA, P_B, NP_F, NP_N, NP_A, If_Cond, Cred_Op, Cred_Value, UN),

	% Prepare the predicate head
	get_auxiliar_suffix(Suffix),
	add_preffix_to_name(P_N, Suffix, NP_N), % Change name
	NP_A = 3,
	functor(NP_F, NP_N, NP_A),
	arg(1, NP_F, NP_Arg_Input),
	arg(2, NP_F, NP_Arg_Prio),
	arg(3, NP_F, NP_Arg_TV),
	
	nonvar(P_B),
	functor(P_B, P_B_Name, P_B_Arity), 
	(
	    (   % Fuzzy fact.
		P_B_Name = 'value', P_B_Arity = 1,
		arg(1, P_B, Fixed_Truth_Value),
		translate_rfuzzy_fact(Pred_Info, Fixed_Truth_Value, Cl_Body, Cl_Body_TV),
		Cl_Body_Prio = 1
	    )
	;
	    (   % Fuzzification function.
		P_B_Name = 'function', P_B_Arity = 2,
		arg(1, P_B, Defined_Pred),
		arg(2, P_B, Function_Body),
		translate_rfuzzy_fuzzification(Pred_Info, NP_Arg_Input, Defined_Pred, Function_Body, Cl_Body, Cl_Body_TV),
		Cl_Body_Prio = 0.6
	    )
	;
	    (   % Fuzzy Rule
		P_B_Name = 'rule', P_B_Arity = 2,
		arg(1, P_B, Rule_Op),
		arg(2, P_B, Rule_Body),
		test_connective_is_defined(Rule_Op, 'yes'),
		translate_rfuzzy_rule_body(Rule_Body, Rule_Op, NP_Arg_Input, P_TN, Cl_Body_TV, Cl_Body),
		Cl_Body_Prio = 0.4
	    )	
	;
	    (   % Fuzzy Rule (without truth values connective) 
		P_B_Name = 'rule', P_B_Arity = 1,
		Rule_Op = 'prod',
		arg(1, P_B, Rule_Body),
		test_connective_is_defined(Rule_Op, 'yes'),
		translate_rfuzzy_rule_body(Rule_Body, Rule_Op, NP_Arg_Input, P_TN, Cl_Body_TV, Cl_Body),
		Cl_Body_Prio = 0.4
	    )	
	;

	    (   % Fuzzy Default
		P_B_Name = 'defaults_to', P_B_Arity = 1,
		arg(1, P_B, Fixed_Truth_Value),
		translate_rfuzzy_default_value_for(Pred_Info, Fixed_Truth_Value, Cl_Body, Cl_Body_TV),
		Cl_Body_Prio = 0.2
	    )
	;
	    (
		P_B_Name = 'synonym_of', P_B_Arity = 3,
		arg(1, P_B, Defined_Pred),
		arg(2, P_B, TV_Op),
		test_connective_is_defined(TV_Op, 'true'),
		arg(3, P_B, TV_Val),
		translate_rfuzzy_rule_synonym(Pred_Info, Defined_Pred, TV_Op, TV_Val, Cl_Body, Cl_Body_TV, Cl_Body_Prio)
	    )
	;
	    (
		P_B_Name = 'antonym_of', P_B_Arity = 3,
		arg(1, P_B, Defined_Pred),
		arg(2, P_B, TV_Op),
		test_connective_is_defined(TV_Op, 'true'),
		arg(3, P_B, TV_Val),
		translate_rfuzzy_rule_antonym(Pred_Info, Defined_Pred, TV_Op, TV_Val, Cl_Body, Cl_Body_TV, Cl_Body_Prio)
	    )
	), !,

	print_msg('debug', 'translate_fuzzy ::  (Cl_Body, Cl_Body_TV, Cl_Body_Prio)', (Cl_Body, Cl_Body_TV, Cl_Body_Prio)),
	generate_type_test_subCl(NP_Arg_Input, P_TN, P_TA, SubCl_TypeTest),
	generate_credibility_subCl(Cred_Op, Cred_Value, Cl_Body_TV, NP_Arg_TV, SubCl_Credibility),
	generate_ifcondition_subCl(If_Cond, P_TN, P_TA, NP_Arg_Input, SubCl_IfCondition),
	generate_username_subCl(UN, SubCl_UserName),
	generate_priority_subCl(Cl_Body_Prio, NP_Arg_Prio, SubCl_IfCondition, SubCl_UserName, SubCl_Prio),
	generate_on_error_subCl(NP_Arg_TV, NP_Arg_Prio, Cl_Body_On_Error),
	generate_debug_helper(P_B_Name, NP_F, SubCl_Debug),
	Cls = [(NP_F :- SubCl_TypeTest, (
					    (Cl_Body, SubCl_Credibility, SubCl_IfCondition, SubCl_UserName, SubCl_Prio, SubCl_Debug) 
					; 
					    (Cl_Body_On_Error)
					))],
	print_msg('debug', 'translate_fuzzy', Cls),

	save_predicate_definition(P_N, 2, [P_TN, 'rfuzzy_truth_value_type'], P_B_Name, ('fuzzy_rule', [(NP_N, NP_A)])),
	print_msg('debug', 'translate_fuzzy ', ' ').

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

generate_debug_helper(P_B_Name, NP_F, SubCl_Debug) :-
	SubCl_Debug = print_msg('debug', P_B_Name, NP_F).

generate_on_error_subCl(Cl_Body_TV, Cl_Body_Prio, Cl_Body_On_Error) :-
	Cl_Body_On_Error = (Cl_Body_TV .=. 0, Cl_Body_Prio .=. 0).

generate_type_test_subCl(NP_Arg_Input, P_TN, P_TA, SubCl_TypeTest) :-
	functor(Type_F, P_TN, P_TA),
	SubCl_TypeTest = ((Type_F = NP_Arg_Input), Type_F).

generate_credibility_subCl(Cred_Op, Cred_Value, Cl_Body_TV, NP_Arg_TV, SubCl_Credibility) :-
	(   % Trivial cases (optimizations).
	    (   Cred_Op = 'prod', Cred_Value = 1   )
	;
	    (   Cred_Op = 'min', Cred_Value = 1   )
	), !, 
	NP_Arg_TV = Cl_Body_TV, 
	SubCl_Credibility = true.

generate_credibility_subCl(Cred_Op, Cred_Value, Cl_Body_TV, NP_Arg_TV, SubCl_Credibility) :-
	functor(Cred_F, Cred_Op, 3), 
	arg(1, Cred_F, Cred_Value),
	arg(2, Cred_F, Cl_Body_TV),
	arg(3, Cred_F, NP_Arg_TV),
	SubCl_Credibility = (Cred_F, NP_Arg_TV .>=. 0, NP_Arg_TV .=<. 1).

generate_ifcondition_subCl(If_Cond, _P_TN, _P_TA, _NP_Arg_Input, _SubCl_IfCondition) :-
	var(If_Cond), 
	print_msg('debug', 'The if condition cannot be a variable', If_Cond).

generate_ifcondition_subCl('true', _P_TN, _P_TA, _NP_Arg_Input, 'true') :- !. % No condition.
	
generate_ifcondition_subCl((If_Cond_1, If_Cond_2), P_TN, P_TA, NP_Arg_Input, (SubCl_IfCondition_1, SubCl_IfCondition_2)) :-
	!,
	generate_ifcondition_subCl(If_Cond_1, P_TN, P_TA, NP_Arg_Input, SubCl_IfCondition_1),
	generate_ifcondition_subCl(If_Cond_2, P_TN, P_TA, NP_Arg_Input, SubCl_IfCondition_2).

generate_ifcondition_subCl((Functor equals Value), P_TN, P_TA, NP_Arg_Input, SubCl_IfCondition) :-
	extract_from_PF_values_PN_PA_PTN_PTA(Functor, FP_N, _FP_A, P_TN, P_TA),
	functor(Get_Value, FP_N, 2),
	arg(1, Get_Value, NP_Arg_Input),
	arg(2, Get_Value, Obtained_Value),
	nonvar(Value), number(Value), !,
	SubCl_IfCondition = (Get_Value, (Obtained_Value .=. Value)).

generate_ifcondition_subCl((Functor equals Value), P_TN, P_TA, NP_Arg_Input, SubCl_IfCondition) :-
	extract_from_PF_values_PN_PA_PTN_PTA(Functor, FP_N, _FP_A, P_TN, P_TA),
	functor(Get_Value, FP_N, 2),
	arg(1, Get_Value, NP_Arg_Input),
	arg(2, Get_Value, Obtained_Value),
	nonvar(Value), !,
	SubCl_IfCondition = (Get_Value, (Obtained_Value = Value)).

generate_ifcondition_subCl((Functor is_not Value), P_TN, P_TA, NP_Arg_Input, SubCl_IfCondition) :-
	extract_from_PF_values_PN_PA_PTN_PTA(Functor, FP_N, _FP_A, P_TN, P_TA),
	functor(Get_Value, FP_N, 2),
	arg(1, Get_Value, NP_Arg_Input),
	arg(2, Get_Value, Obtained_Value),
	nonvar(Value), number(Value), !,
	SubCl_IfCondition = (Get_Value, (Obtained_Value .<>. Value)).

generate_ifcondition_subCl((Functor is_not Value), P_TN, P_TA, NP_Arg_Input, SubCl_IfCondition) :-
	extract_from_PF_values_PN_PA_PTN_PTA(Functor, FP_N, _FP_A, P_TN, P_TA),
	functor(Get_Value, FP_N, 2),
	arg(1, Get_Value, NP_Arg_Input),
	arg(2, Get_Value, Obtained_Value),
	nonvar(Value), !,
	SubCl_IfCondition = (Get_Value, (Obtained_Value \== Value)).

generate_ifcondition_subCl((Functor is_over Value), P_TN, P_TA, NP_Arg_Input, SubCl_IfCondition) :-
	extract_from_PF_values_PN_PA_PTN_PTA(Functor, FP_N, _FP_A, P_TN, P_TA),	
	functor(Get_Value, FP_N, 2),
	arg(1, Get_Value, NP_Arg_Input),
	arg(2, Get_Value, Obtained_Value),
	nonvar(Value), number(Value), !,
	SubCl_IfCondition = (Get_Value, (Obtained_Value .>. Value)).

generate_ifcondition_subCl((Functor is_under Value), P_TN, P_TA, NP_Arg_Input, SubCl_IfCondition) :-
	extract_from_PF_values_PN_PA_PTN_PTA(Functor, FP_N, _FP_A, P_TN, P_TA),	
	functor(Get_Value, FP_N, 2),
	arg(1, Get_Value, NP_Arg_Input),
	arg(2, Get_Value, Obtained_Value),
	nonvar(Value), number(Value), !, 
	SubCl_IfCondition = (Get_Value, (Obtained_Value .<. Value)).

% generate_username_subCl(UN, SubCl_UserName)
generate_username_subCl(UN, 'true') :-
	nonvar(UN), UN = 'null', !.
generate_username_subCl(UN, SubCl_UserName) :-
	nonvar(UN), UN \== 'null', !, 
	functor(Obtain_UserName, 'isUserNameLocalUserName', 1),
	arg(1, Obtain_UserName, Obtined_UserName),
	SubCl_UserName = (Obtain_UserName, (Obtined_UserName = UN)).

generate_priority_subCl(Cl_Body_Prio, NP_Arg_Prio, SubCl_IfCondition, SubCl_UserName, SubCl_Prio) :- 
	nonvar(SubCl_IfCondition), nonvar(SubCl_UserName), !,
	(
	    (
		SubCl_IfCondition = 'true', 
		SubCl_UserName = 'true',
		Value = 0.15
	    )
	;
	    (
		SubCl_IfCondition = 'true', 
		SubCl_UserName \== 'true',
		Value = 0.1
	    )
	;
	    (
		SubCl_IfCondition \== 'true', 
		SubCl_UserName = 'true',
		Value = 0.05
	    )
	;
	    (
		SubCl_IfCondition \== 'true', 
		SubCl_UserName \== 'true',
		Value = 0
	    )
	), !, 
	SubCl_Prio = (
			 (NP_Arg_Prio_Tmp .=. Cl_Body_Prio - Value), 
			  (
			      (NP_Arg_Prio_Tmp .>. 0, NP_Arg_Prio .=. NP_Arg_Prio_Tmp) 
			  ; 
			      (NP_Arg_Prio_Tmp .=<. 0, NP_Arg_Prio .=. 0)
			  )), !.

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

translate_rfuzzy_fact(_Pred_Info, Fixed_Truth_Value, Cl_Body, Cl_Body_TV) :-
	nonvar(Fixed_Truth_Value), number(Fixed_Truth_Value),
	generate_assign_truth_value_subgoal(Fixed_Truth_Value, Cl_Body_TV, Cl_Body),
	!. % Backtracking forbidden.

translate_rfuzzy_rule_synonym(Pred_Info, Defined_Pred, TV_Op, TV_Val, Cl_Body, Cl_Body_TV, Cl_Body_Prio) :-
	rfuzzy_pred_info(Pred_Info,	_P_F, _P_N, _P_A, P_TN, P_TA, _P_B, NP_F, _NP_N, _NP_A, _If_Cond, _Cred_Op, _Cred_Value, _UN),

	nonvar(Defined_Pred), 
	extract_from_PF_values_PN_PA_PTN_PTA(Defined_Pred, Defined_P_N, _Fake_P_A, P_TN, P_TA),
	retrieve_predicate_info(Defined_P_N, 2, Defined_P_T, 'true'),
	( 
	    (	memberchk_local([P_TN, _Unused_Type_Name], Defined_P_T), !    )
	;
	    (	print_msg('error', 'The type is not correctly defined for the predicate', Defined_P_N), !, fail    )
	),

	% Prepare the predicate head
	get_auxiliar_suffix(Suffix),
	add_preffix_to_name(Defined_P_N, Suffix, Defined_NP_N), % Change name
	Defined_NP_A = 3,
	functor(Defined_NP_F, Defined_NP_N, Defined_NP_A),
	arg(1, Defined_NP_F, Defined_NP_Arg_Input),
	arg(2, Defined_NP_F, Defined_NP_Arg_Prio),
	arg(3, Defined_NP_F, Defined_NP_Arg_TV),
	print_msg('debug', 'translate_rfuzzy_rule_synonym :: created defined functor Defined_NP_F', Defined_NP_F),

	arg(1, NP_F, Defined_NP_Arg_Input),
	Cl_Body_Prio = Defined_NP_Arg_Prio,
	functor(Compute_TV, TV_Op, 3),
	print_msg('debug', 'translate_rfuzzy_rule_synonym :: created compute TV functor Compute_TV', Compute_TV),
	arg(1, Compute_TV, Defined_NP_Arg_TV),
	arg(2, Compute_TV, TV_Val), 
	arg(3, Compute_TV, Cl_Body_TV),
	Cl_Body = (Defined_NP_F, (Defined_NP_Arg_TV .>=. 0, Defined_NP_Arg_TV .=<. 1), Compute_TV),
	print_msg('debug', 'translate_rfuzzy_rule_synonym :: Cl_Body', Cl_Body),
	!.

translate_rfuzzy_rule_antonym(Pred_Info, Defined_Pred, TV_Op, TV_Val, Cl_Body, Cl_Body_TV, Cl_Body_Prio) :-
	rfuzzy_pred_info(Pred_Info,	_P_F, _P_N, _P_A, P_TN, P_TA, _P_B, NP_F, _NP_N, _NP_A, _If_Cond, _Cred_Op, _Cred_Value, _UN),

	nonvar(Defined_Pred), 
	extract_from_PF_values_PN_PA_PTN_PTA(Defined_Pred, Defined_P_N, _Fake_P_A, P_TN, P_TA),
	retrieve_predicate_info(Defined_P_N, 2, Defined_P_T, 'true'),
	( 
	    (	memberchk_local([P_TN, _Unused_Type_Name], Defined_P_T), !    )
	;
	    (	print_msg('error', 'The type is not correctly defined for the predicate', Defined_P_N), !, fail    )
	),

	% Prepare the predicate head
	get_auxiliar_suffix(Suffix),
	add_preffix_to_name(Defined_P_N, Suffix, Defined_NP_N), % Change name
	Defined_NP_A = 3,
	functor(Defined_NP_F, Defined_NP_N, Defined_NP_A),
	arg(1, Defined_NP_F, Defined_NP_Arg_Input),
	arg(2, Defined_NP_F, Defined_NP_Arg_Prio),
	arg(3, Defined_NP_F, Defined_NP_Arg_TV),
	print_msg('debug', 'translate_rfuzzy_rule_antonym :: created defined functor Defined_NP_F', Defined_NP_F),

	arg(1, NP_F, Defined_NP_Arg_Input),
	Cl_Body_Prio = Defined_NP_Arg_Prio,
	functor(Compute_TV, TV_Op, 3),
	print_msg('debug', 'translate_rfuzzy_rule_antonym :: created compute TV functor Compute_TV', Compute_TV),
	arg(1, Compute_TV, Defined_NP_Arg_TV_Aux),
	arg(2, Compute_TV, TV_Val), 
	arg(3, Compute_TV, Cl_Body_TV),
	Antonym = (Defined_NP_Arg_TV_Aux .=. 1 - Defined_NP_Arg_TV),
	Cl_Body = (Defined_NP_F, (Defined_NP_Arg_TV .>=. 0, Defined_NP_Arg_TV .=<. 1), Antonym, Compute_TV),
	print_msg('debug', 'translate_rfuzzy_rule_antonym :: Cl_Body', Cl_Body),
	!.

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

% translate_rfuzzy_default_value_for(Pred_Functor, Fixed_Truth_Value, Condition_or_Thershold, Cls) :-
translate_rfuzzy_default_value_for(_Pred_Info, Fixed_Truth_Value, Cl_Body, Cl_Body_TV) :-
	(
	    (	nonvar(Fixed_Truth_Value), number(Fixed_Truth_Value), !   )
	;
	    (   print_msg('error', 'The truth value must be a number. Value', Fixed_Truth_Value), !, fail   )
	),
	Cl_Body = (Cl_Body_TV .=. Fixed_Truth_Value),
	!. % Backtracking forbidden.

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

generate_fake_type(0, []) :- !.
generate_fake_type(1, [_Any]) :- !.
generate_fake_type(N, [_Any|More]) :-
	N > 1,
	NewN is N - 1,
	generate_fake_type(NewN, More).

extract_from_PF_values_PN_PA_PTN_PTA(P_F, P_N, P_A, PT_N, PT_A) :-
	print_msg('debug', 'extract_from_PF_values_PN_PA_PTN_PTA:: (P_F)', (P_F)),
	(
	    (	nonvar(P_F), !    )
	;
	    (	print_msg('error', 'Functor cannot be a variable. Functor', P_F), !, fail    )
	),
	functor(P_F, P_N, P_A), 
	(
	    (	P_A = 1, !    )
	;
	    (	print_msg('error', 'Functor must be of arity 1. Functor', P_F), !, fail    )
	),
	arg(1, P_F, PT_F), 
	(
	    (	nonvar(PT_F), !    )
	;
	    (	print_msg('error', 'Functor type cannot be a variable. Functor', P_F), !, fail    )
	),
	functor(PT_F, PT_N, PT_Fake_Arity),
	(
	    (	PT_Fake_Arity = 0, !    )
	;
	    (	print_msg('error', 'Functor type must be of arity 0. Functor', P_F), !, fail    )
	),
	% retrieve_predicate_info(P_N, P_A, P_T, Show_Error),
	retrieve_predicate_info(PT_N, PT_A, _PT_Type, 'true'),
	print_msg('debug', 'extract_from_PF_values_PN_PA_PTN_PTA(P_F, P_N, P_A, PT_N, PT_A)', (P_F, P_N, P_A, PT_N, PT_A)), !.

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

generate_assign_truth_value_subgoal(Fixed_Truth_Value, Truth_Value, Assign_Truth_Value_SubGoal) :-
	print_msg('debug', 'generate_assign_truth_value_subgoal(Fixed_Truth_Value, Truth_Value)', (Fixed_Truth_Value, Truth_Value)),
	functor(Assign_Truth_Value_SubGoal, '.=.', 2),
	arg(1, Assign_Truth_Value_SubGoal, Truth_Value),
	arg(2, Assign_Truth_Value_SubGoal, Fixed_Truth_Value), !.

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

translate_rfuzzy_type_for_crisp_rule(P_N, P_A, P_T, P_MI, [Cl]) :-
	nonvar(P_N), nonvar(P_A), number(P_A), nonvar(P_T),
	print_msg('debug', 'rfuzzy_type_for :: (P_N, P_A, P_T)', (P_N, P_A, P_T)),
	
	test_type_definition(P_A, 1, P_T),
	functor(P_F, P_N, P_A),
	Cl = (P_F :- fail), % This is just to invite prolog to complaint if any rule has same name but different arity.

	(   
	    (   % it has been defined before and our definition is included.
		retrieve_predicate_info(P_N, P_A, Retrieved_P_T, 'no'), 
		nonvar(Retrieved_P_T), !,
		memberchk_local(P_T, Retrieved_P_T)
	    )
	;
	    (   % Define it or redefine it (Include the new type definition)
		% save_predicate_definition(P_N, P_A, P_T, P_MI)
		save_predicate_definition(P_N, P_A, P_T, 'type', P_MI)
	    )
	), !.

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

translate_rfuzzy_define_database(P_N, P_A, Description, Cls) :-
	print_msg('debug', 'rfuzzy_define_database(P_N/P_A, Description)', (P_N/P_A, Description)),
	nonvar(P_N), nonvar(P_A), nonvar(Description),
	print_msg('debug', 'rfuzzy_define_database :: translate_db_description(Description)', (Description)),
	translate_db_description(Description, 1, P_N, P_A, P_T, Fields_Names),
	print_msg('debug', 'rfuzzy_define_database :: translate_rfuzzy_type_for_crisp_rule(P_N, P_A, P_T)', (P_N, P_A, P_T)),
	translate_rfuzzy_type_for_crisp_rule(P_N, P_A, P_T, ('database', Fields_Names), Cls).

% translate_db_description(Description, Index, DB_P_N, DB_P_A, Types, DB_Fields) 
translate_db_description([(Field_Name, Field_Type)], Index, DB_P_N, DB_P_A, [Field_Type], [Field_Name]) :- 
	nonvar(Index), nonvar(DB_P_A), Index = DB_P_A, !,
	save_field_description(Field_Name, Field_Type, DB_P_N, DB_P_A, Index).

translate_db_description([(Field_Name, Field_Type) | Description], Index, DB_P_N, DB_P_A, [Field_Type|Types], [Field_Name | Field_Names]) :-
	nonvar(Index), nonvar(DB_P_A), Index < DB_P_A, !,
	save_field_description(Field_Name, Field_Type, DB_P_N, DB_P_A, Index),
	New_Index is Index + 1,
	translate_db_description(Description, New_Index, DB_P_N, DB_P_A, Types, Field_Names).

translate_db_description(_List_In, Index, _DB_P_N, DB_P_A, _Types, _Field_Names) :-
	nonvar(Index), nonvar(DB_P_A), Index >= DB_P_A, !,
	print_msg('error', 'There are more database fields than the number of fields you specify in the program file', DB_P_A),
	!, fail. % If this is a variable the tranlate rules loop forever !!!

translate_db_description(_List_In, Index, _DB_P_N, DB_P_A, _Types, _Field_Names) :-
	(
	    var(Index)
	;
	    var(DB_P_A)
	), !,
	print_msg('error', 'translate_db_description: Index or DB_P_A are variables when they should not. (Index, DB_P_A) ', (Index, DB_P_A)),
	!, fail. % If this is a variable the tranlate rules loop forever !!!

save_field_description(Field_Name, Field_Type, DB_P_N, DB_P_A, Index) :-
	nonvar(Field_Name), nonvar(Field_Type), nonvar(DB_P_N), nonvar(DB_P_A), nonvar(Index),

	% retrieve_predicate_info(P_N, P_A, P_T, Show_Error),
	retrieve_predicate_info(Field_Type, 1, _P_T, 'true'), !,

	P_MI = ('rfuzzy_db_field', [(Field_Name, Field_Type, DB_P_N, DB_P_A, Index)]),
	save_predicate_definition(Field_Name, 2, [DB_P_N, Field_Type], 'db_field', P_MI).

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

% translate_rfuzzy_similarity_between(Element1, Element2, Truth_Value, Credibility_Operator, Credibility, []).
translate_rfuzzy_similarity_between(Database, Element1, Element2, Truth_Value, Credibility_Operator, Credibility) :-
	nonvar(Database), nonvar(Element1), nonvar(Element2), nonvar(Truth_Value), nonvar(Credibility_Operator), nonvar(Credibility),
	Real_P_N = 'rfuzzy_computed_similarity_between',
	% add_preffix_to_name(Database, Real_P_N, P_N),
	% functor(Translation, P_N, 5),
	functor(Translation, Real_P_N, 6),
	arg(1, Translation, Database), 
	arg(2, Translation, Element1), 
	arg(3, Translation, Element2), 
	arg(4, Translation, Truth_Value), 
	arg(5, Translation, Credibility_Operator), 
	arg(6, Translation, Credibility),
	
	Real_P_T = ['rfuzzy_predicate_type', 'rfuzzy_predicate_type', 'rfuzzy_predicate_type', 'rfuzzy_truth_value_type', 'rfuzzy_predicate_type', 'rfuzzy_credibility_value_type'],
	save_predicate_definition(Real_P_N, 6, Real_P_T, 'similarity', ('rfuzzy_similarity_clause', [Translation])).

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

save_rfuzzy_modifiers_list([], []) :- !.
save_rfuzzy_modifiers_list([(P_N, P_A, Truth_Value_In, Truth_Value_Out, Code) | More], [Translation | Translations]) :-
	nonvar(P_N), nonvar(P_A), number(P_A), P_A = 2,

	functor(Modifier, P_N, P_A),
	arg(1, Modifier, Fuzzy_Predicate_Functor_In),
	arg(2, Modifier, Truth_Value_Out),

	Translation = ( Modifier :-	
		      functor(Fuzzy_Predicate_Functor_In, _FP_Name, FP_Arity), 
		      arg(FP_Arity, Fuzzy_Predicate_Functor_In, Truth_Value_In),
		      Fuzzy_Predicate_Functor_In,
		      Code,
		      Truth_Value_Out .>=. 0, 
		      Truth_Value_Out .=<. 1
		      ),

	P_T = [rfuzzy_predicate_type, rfuzzy_truth_value_type],
	% save_predicate_definition(P_N, P_A, P_T, P_MI)
	save_predicate_definition(P_N, P_A, P_T, 'modifier', []), !,

	save_rfuzzy_modifiers_list(More, Translations).

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

save_rfuzzy_negation_ops_list([], []) :- !.
save_rfuzzy_negation_ops_list([(P_N, P_A, Truth_Value_In, Truth_Value_Out, Code) | More], [Translation | Translations]) :-
	nonvar(P_N), nonvar(P_A), number(P_A), P_A = 2,

	functor(Negator, P_N, P_A),
	arg(1, Negator, Fuzzy_Predicate_Functor),
	arg(2, Negator, Truth_Value_Out),

	Translation = ( Negator :-	
		      print_msg('debug', P_N, 'Computing results list.'),
		      findall(Fuzzy_Predicate_Functor, Fuzzy_Predicate_Functor, Results_List), !,
		      print_msg('debug', P_N, Results_List),
		      reorder_by_truth_value(Results_List, [], Results_List_Aux),
		      print_msg('debug', 'reorder_by_truth_value', Results_List_Aux),
		      one_by_one_first_tail(Results_List_Aux, Result_Functor),
		      print_msg('debug', 'take_an_element', Result_Functor),
	       
		      functor(Fuzzy_Predicate_Functor, _FP_Name, FP_Arity), 
		      FP_Arity_Aux is FP_Arity - 1,
		      copy_args(FP_Arity_Aux, Result_Functor, Fuzzy_Predicate_Functor),
		      arg(FP_Arity, Result_Functor, Truth_Value_In),
	       
		      print_msg('debug', 'fnot :: adjusting Truth_Value', Truth_Value_Out),
		      Code,
		      Truth_Value_Out .>=. 0, Truth_Value_Out .=<. 1,
		      print_msg('debug', 'negation :: result', Fuzzy_Predicate_Functor)
		      ),

	P_T = [rfuzzy_predicate_type, rfuzzy_truth_value_type],
	% save_predicate_definition(P_N, P_A, P_T, P_MI)
	save_predicate_definition(P_N, P_A, P_T, 'negation', []), !,

	save_rfuzzy_negation_ops_list(More, Translations).


% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

test_connective_is_defined(P_N, Show_Error) :-
	nonvar(P_N),
	P_A = 3,
	% retrieve_predicate_info(P_N, P_A, P_T, Show_Error),
	retrieve_predicate_info(P_N, P_A, P_T, Show_Error), !,
	nonvar(P_T),
	Expected_Type = ['rfuzzy_truth_value_type', 'rfuzzy_truth_value_type', 'rfuzzy_truth_value_type'],
	memberchk_local(Expected_Type, P_T).

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

% Security issues
translate_rfuzzy_rule_body(Body_F, _TV_Aggregator, _NP_Arg_Input, _P_TN, _Truth_Value, _FB) :- 
	var(Body_F), 
	print_msg('error', 'Rule body cannot be a variable. Body', Body_F),
	!, fail. % If this is a variable the tranlate rules loop forever !!!

% Conjunction.
translate_rfuzzy_rule_body((Tmp_Body_1, Tmp_Body_2), TV_Aggregator, NP_Arg_Input, P_TN, Truth_Value, (FB_1, FB_2, Aggr_F)) :- !,
	print_msg('debug', 'translate_rfuzzy_rule_body(Body, TV_Aggregator, Truth_Value) - conjunction',((Tmp_Body_1, Tmp_Body_2), TV_Aggregator, Truth_Value)),
	nonvar(TV_Aggregator),
	\+ ( TV_Aggregator = 'none' ),
	translate_rfuzzy_rule_body(Tmp_Body_1, TV_Aggregator, NP_Arg_Input, P_TN, TV_1, FB_1),
	translate_rfuzzy_rule_body(Tmp_Body_2, TV_Aggregator, NP_Arg_Input, P_TN, TV_2, FB_2),
	functor(Aggr_F, TV_Aggregator, 3),
	arg(1, Aggr_F, TV_1), 
	arg(2, Aggr_F, TV_2), 
	arg(3, Aggr_F, Truth_Value), !.

% Rule Body Conjunct with Modifier.
translate_rfuzzy_rule_body(Body_F, _TV_Aggregator, NP_Arg_Input, P_TN, Truth_Value, Translation) :-
	print_msg('debug', 'translate_rfuzzy_rule_body(Body, Truth_Value) - with modifier',(Body_F, Truth_Value)),
	nonvar(Body_F),
	functor(Body_F, QP_N, QP_Arity),
	QP_Arity = 1, % If not then it is not a modifier.
	arg(1, Body_F, SubBody),
	functor(SubBody, _BodyP_N, BodyP_Arity),
	BodyP_Arity > 0, % If not then it is not a modifier.
	!,

	% retrieve_predicate_info(P_N, P_A, P_T, Show_Error) 
	retrieve_predicate_info(QP_N, 2, QP_T, 'true'), 
	nonvar(QP_T), 
	memberchk_local(['rfuzzy_predicate_type', 'rfuzzy_truth_value_type'], QP_T),
	!,
	
	print_msg('debug', 'translate_rfuzzy_rule_body(SubBody)',(SubBody)),
	translate_rfuzzy_rule_body(SubBody, 'none', NP_Arg_Input, P_TN, _SubCall_Truth_Value, SubCall),
	print_msg('debug', 'translate_rfuzzy_rule_body(SubBody, SubCall)',(SubBody, SubCall)),

	functor(QP_F, QP_N, 2),
	arg(1, QP_F, SubCall),
	arg(2, QP_F, Truth_Value),
	Translation = QP_F, % Note this can be a sub-body: it must be a functor, but not a conjunction of them.
	print_msg('debug', 'translate_rfuzzy_rule_body(Translation) - with modifier',(Translation)).

% Rule Body Conjunct without Modifier.
translate_rfuzzy_rule_body(Body_F, _TV_Aggregator, NP_Arg_Input, P_TN, Truth_Value, Translation) :-
	print_msg('debug', 'translate_rfuzzy_rule_body(Body, Truth_Value) - without modifier',(Body_F, Truth_Value)),
	extract_from_PF_values_PN_PA_PTN_PTA(Body_F, P_N, _Fake_P_A, P_TN, _Type_1_Arity),

	% retrieve_predicate_info(P_N, P_A, P_T, Show_Error) 
	retrieve_predicate_info(P_N, P_A, P_T, 'true'), 
	nonvar(P_T), 
	memberchk_local([P_TN, 'rfuzzy_truth_value_type'], P_T),

	functor(P_F, P_N, P_A),
	arg(1, P_F, NP_Arg_Input),
	arg(2, P_F, Truth_Value),
	Translation = P_F, % Note this can be a sub-body: it must be a functor, but not a conjunction of them.
	print_msg('debug', 'translate_rfuzzy_rule_body(Body, Translation)',(Body_F, Translation)),
	print_msg_nl('debug').

translate_rfuzzy_rule_body(Body_F, _TV_Aggregator, _NP_Arg_Input, _P_TN, _Truth_Value, _Result) :-
	print_msg('error', 'translating the rule subbody',(Body_F)), !, fail.

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

test_type_definition(P_A, Actual, [Type]) :-
	print_msg('debug', 'test_type_definition(P_A, Actual, Type)', (P_A, Actual, [Type]) ),
	Actual = P_A, !, % Security conditions.
	test_type_definition_aux_1(Type),
	!. % Backtracking not allowed.

test_type_definition(P_A, Actual, [Type | More]) :-
	print_msg('debug', 'test_type_definition(P_A, Actual, Type)', (P_A, Actual, [Type|More]) ),
	Actual < P_A, !,  % Security conditions.
	test_type_definition_aux_1(Type),
	NewActual is Actual + 1, % Next values.
	!,
	test_type_definition(P_A, NewActual, More),
	!. % Backtracking not allowed here.

test_type_definition(_P_A, _Actual, Types) :-
	print_msg('error', 'test_type_definition :: Types', Types),
	!, fail.

test_type_definition_aux_1(Type/1) :-
	test_type_definition_aux_2(Type), !.

test_type_definition_aux_1(Type) :-
	test_type_definition_aux_2(Type), !.

test_type_definition_aux_2(Type) :-
	print_msg('debug', 'test_type_definition_aux :: Type', Type),
	functor(Type, P_N, Fake_P_A),
	print_msg('debug', 'test_type_definition_aux :: (P_N, Fake_P_A)', (P_N, Fake_P_A)),
	nonvar(P_N), nonvar(Fake_P_A),
	P_N \== '/',
	% retrieve_predicate_info(P_N, P_A, P_T, Show_Error),
	retrieve_predicate_info(P_N, _P_A, _P_T, 'true'),
	!.
test_type_definition_aux_2(Type/1) :-
	print_msg('error', 'Not an adequate type name', Type), !, fail.

test_type_definition_aux_2(Type) :-
	print_msg('error', 'Type must have the format Name/Arity, where arity must be 1', Type), !, fail.

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

% translate_rfuzzy_fuzzification(Pred_Functor, Defined_Pred_F, UserName, Function_List, Cls) :-
translate_rfuzzy_fuzzification(Pred_Info, NP_Arg_Input, Defined_Pred_F, Function_Body, Cl_Body, Cl_Body_TV) :-
	nonvar(Defined_Pred_F), nonvar(Function_Body),
	print_msg('debug', 'translate: rfuzzy_fuzzification(Pred_Info, Defined_Pred_F)', (Pred_Info, Defined_Pred_F)),
	print_msg('debug', 'translate: rfuzzy_fuzzification(Function_Body)', Function_Body),

	rfuzzy_pred_info(Pred_Info, _P_F, _P_N, _P_A, P_TN, P_TA, _P_B, _NP_F, _NP_N, _NP_A, _If_Cond, _Cred_Op, _Cred_Value, _UN),
	extract_from_PF_values_PN_PA_PTN_PTA(Defined_Pred_F, DP_N, _DP_A, P_TN, P_TA),

	retrieve_predicate_info(DP_N,  2, DP_Type, 'true'),
	memberchk_local([P_TN, P_TN_2], DP_Type),
	(
	    (	var(P_TN_2), 
		print_msg('error', 'Types for predicates used in fuzzification must be defined before', (DP_N)), 
		!, fail    )
	;
	    (   nonvar(P_TN_2), !   )
	),
	(
	    (   P_TN_2 = 'rfuzzy_integer_type', !   )
	;
	    (   P_TN_2 = 'rfuzzy_float_type', !   )
	;
	    (
		print_msg('error', 'Type of predicate is not suitable for fuzzification', DP_N), 
		print_msg('debug', 'P_TN_2', P_TN_2), 
		!, fail    
	    )
	),

	functor(NDP_F, DP_N, 2),
	arg(1, NDP_F, NP_Arg_Input),
	arg(2, NDP_F, Crisp_Value),

	print_msg('debug', 'build_straight_lines(Crisp_Value, Cl_Body_TV, Function_Body)', (Crisp_Value, Cl_Body_TV, Function_Body)), 
	build_straight_lines(Crisp_Value, Cl_Body_TV, Function_Body, List_Body),
	print_msg('debug', 'build_straight_lines(List_Body)', (List_Body)), 
	Cl_Body = (NDP_F, List_Body),
	!.

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------


build_straight_lines(X, V, List, Cls) :-
	(
	    (
		nonvar(List), list(List),
		build_straight_lines_aux(X, V, List, Cls), !
	    )
	;
	    (
		print_msg('error', 'function definition contains errors. Please fix them', function(List)), !, fail
	    )
	), !.

build_straight_lines_aux(X, V, [(X1,V1),(X2,V2)], (Point1 ; Line ; Point2)) :- !,
	print_msg('debug', 'build_straight_lines', (build_straight_lines(X, V, [(X1,V1),(X2,V2)], (Point1, Line, Point2)))),
	build_point(X, V, X1, V1, Point1),
	build_line(X, V, X1, V1, X2, V2, Line),
	build_point(X, V, X2, V2, Point2), !.

build_straight_lines_aux(X, V, [(X1,V1),(X2,V2)|List], (Point ; Line ; More)) :- !,
	print_msg('debug', 'build_straight_lines', (build_straight_lines(X, V, [(X1,V1),(X2,V2)|List], (Point, Line, More)))),
	build_point(X, V, X1, V1, Point),
	build_line(X, V, X1, V1, X2, V2, Line),
	build_straight_lines_aux(X, V, [(X2,V2)|List], More).

build_point(X, V, X1, V1, (Point, Debug)) :-
	print_msg('debug', 'build_point', build_point(X, V, X, V, (H :- (H :- X1 .=. X, V1 .=. V)))),
	nonvar(X1), nonvar(V1), !,
	Point = (X .=. X1, V .=. V1),
	Debug = (print_msg('debug', 'function point', Point)).

build_line(X, V, X1, V1, X2, V2, (Line, Debug)) :-
	print_msg('debug', 'build_line', (build_line(X, V, X1, V1, X2, V2, (X .>. X1, X .<. X2, Calculate_V)))),

	nonvar(X1), nonvar(X2), nonvar(V1), nonvar(V2), 
	number(X1), number(X2), number(V1), number(V2),
	%	X1 .<. X2, 

	!, % Backtracking is not allowed here.
	Calculate_V = (Pend .=. ((V2-V1)/(X2-X1)), V .=. V1+Pend*(X-X1)),
	Line = (X .>. X1, X .<. X2, Calculate_V),
	Debug = (print_msg('debug', 'function line', Line)).

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

generate_code_from_saved_info([], Cls_1_In, Cls_1_In, Cls_2_In, Cls_2_In) :- !,
	print_msg('debug', 'generate_code_from_saved_info', 'END').
generate_code_from_saved_info([Predicate_Def|Predicate_Defs], Cls_1_In, Cls_1_Out, Cls_2_In, Cls_2_Out) :-
	print_msg('debug', 'generate_code_from_saved_info :: Predicate_Def', Predicate_Def),
	Predicate_Def = predicate_definition(P_N, P_A, P_T, P_O, P_MI), !,
	print_msg('debug', 'generate_code_from_saved_info :: P_MI', P_MI),
	generate_code_main(P_N, P_A, P_T, P_O, P_MI, Cls_1_In, Cls_1_Aux, Cls_2_In, Cls_2_Aux),
%	print_msg('debug', 'generate_code_from_saved_info :: Cls_1_Aux', Cls_1_Aux),
%	print_msg('debug', 'generate_code_from_saved_info :: Cls_2_Aux', Cls_2_Aux),
	generate_code_from_saved_info(Predicate_Defs, Cls_1_Aux, Cls_1_Out, Cls_2_Aux, Cls_2_Out).

generate_code_from_saved_info([Predicate_Def|Predicate_Defs], Cls_1_In, Cls_1_Out, Cls_2_In, Cls_2_Out) :- !,
	print_msg('error', 'generate_code_from_saved_info :: cannot parse', Predicate_Def),
	generate_code_from_saved_info(Predicate_Defs, Cls_1_In, Cls_1_Out, Cls_2_In, Cls_2_Out).

generate_code_main(P_N, P_A, P_T, P_O, P_MI, Cls_1_In, Cls_1_Out, Cls_2_In, Cls_2_Out) :-
	print_msg('debug', 'generate_code_main :: (P_N, P_A, P_T, P_MI)', (P_N, P_A, P_T, P_MI)),
	generate_code_from_P_MIs(P_MI, P_N, P_A, P_T, [], Cls_1_In, Cls_1_Out, PI_Body_List),
	print_msg('debug', 'generate_code_main :: (P_N, P_A, P_T, P_MI)', (P_N, P_A, P_T, P_MI)),
	print_msg('debug', 'generate_code_main :: PI_Body_List', PI_Body_List),
	build_introspection_clause(P_N, P_A, P_T, P_O, P_MI, PI_Body_List, Cls_2_In, Cls_2_Out).

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

build_introspection_clause(P_N, P_A, P_T, P_O, P_MI, [], Cls_In, [Cl | Cls_In]) :- !,
	Cl = (rfuzzy_introspection(P_N, P_A, P_T, P_O, P_MI)).
build_introspection_clause(P_N, P_A, P_T, P_O, P_MI, PI_Body_List, Cls_In, [Cl | Cls_In]) :-
	list_to_disjunction(PI_Body_List, PI_Body, Type_Var, Enum_Value_Var),

	Generator = (findall((Type_Var, Enum_Value_Var), PI_Body, Enum_Values_List), 
	remove_list_dupplicates(Enum_Values_List, [], New_Enum_Values_List)),
	Cl = (rfuzzy_introspection(P_N, P_A, P_T, P_O, [('rfuzzy_enum_type_values', New_Enum_Values_List) | P_MI]) :- Generator).
	
list_to_disjunction([], 'false', _Type_Var, _Enum_Value_Var) :- !.
list_to_disjunction([(Type_Var, Enum_Value_Var, Body) | PI_Body_List], (Body ; PI_Body), Type_Var, Enum_Value_Var) :-
	list_to_disjunction(PI_Body_List, PI_Body, Type_Var, Enum_Value_Var), !.

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

list_head([Head | _List], Head).

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

generate_code_from_P_MIs([], _P_N, _P_A, _P_T, _History, Cls_In, Cls_In, []) :- !.
generate_code_from_P_MIs([(Selector, Details_List) | P_MI], P_N, P_A, P_T, History_In, Cls_In, Cls_Out, PI_Body_List_Out) :-
	print_msg('debug', 'generate_code_from_P_MIs :: (Selector, Details_List)', (Selector, Details_List)),
	list_head(Details_List, Details),
	nonvar(Selector), 
	(
	    (
		Selector = 'fuzzy_rule', !,
		(
		    memberchk_local(Selector, History_In), !,
		    Cls_Aux = Cls_In,
		    History_Out = History_In,
		    PI_Body_List_Out = PI_Body_List_In
		;
		    build_fuzzy_rule_main_clause(P_N, P_A, P_T, Details, Cls_In, Cls_Aux),
		    History_Out = [ Selector | History_In ],
		    PI_Body_List_Out = PI_Body_List_In
		)
	    )
	;
	    (
		Selector = 'rfuzzy_similarity_clause', !,
		build_similarity_clause(P_N, P_A, P_T, Details, Cls_In, Cls_Aux),
		History_Out = History_In,
		PI_Body_List_Out = PI_Body_List_In
	    )
	;
	    (
		Selector = 'rfuzzy_db_field', !,
		build_db_field_cls_and_retrievers(P_N, P_A, P_T, Details, Cls_In, Cls_Aux, PI_Body_List_In, PI_Body_List_Out),
		History_Out = History_In
	    )
	;
	    (
		Cls_Aux = Cls_In,
		History_Out = History_In,
		PI_Body_List_Out = PI_Body_List_In
	    )
	),
	generate_code_from_P_MIs(P_MI, P_N, P_A, P_T, History_Out, Cls_Aux, Cls_Out, PI_Body_List_In).

build_fuzzy_rule_main_clause(P_N, P_A, _P_T, Details, Cls_In, [Cl | Cls_In]) :-
	Details = (Aux_P_N, Aux_P_A),
	functor(Pred_Functor, P_N, P_A),
	functor(Aux_Pred_Functor, Aux_P_N, Aux_P_A),
	Tmp_P_A is P_A - 1,
	copy_args(Tmp_P_A, Pred_Functor, Aux_Pred_Functor),

	arg(P_A, Pred_Functor, Truth_Value),
	arg(Aux_P_A, Aux_Pred_Functor, Truth_Value),
	
	Cl = (
		 Pred_Functor :- (
				     print_msg('debug', 'Predicate called', Pred_Functor),
				     findall(Aux_Pred_Functor, Aux_Pred_Functor, Results), 
				     supreme(Results, Pred_Functor)
				 )
	     ), !.

build_similarity_clause(_P_N, _P_A, _P_T, Details, Cls_In, [Details | Cls_In]) :- !.

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

build_db_field_cls_and_retrievers(P_N, P_A, P_T, Details, Cls_In, [Cl | Cls_In], PI_Body_List_In, PI_Body_List_Out) :-
	print_msg('debug', 'build_db_field_cls_and_retrievers :: (P_N, P_A, P_T, Details)', (P_N, P_A, P_T, Details)),
	P_A = 2,
	Details = (Field_Name, Field_Type, DB_P_N, DB_P_A, Index),
	P_N = Field_Name,

	functor(Field_Functor, P_N, P_A),
	arg(1, Field_Functor, Input_Value),
	arg(2, Field_Functor, Output_Value),

	functor(DB_P_F, DB_P_N, DB_P_A),
	arg(Index, DB_P_F, DB_Value),

	Mapping = (Input_Value = DB_P_F),
	Test_Valid_Value = (DB_Value \== 'null'),
	generate_db_value_conversor(Field_Type, DB_Value, Output_Value, Conversion),
	Cl = (Field_Functor :- (((Mapping, DB_P_F), Test_Valid_Value), Conversion)),

	generate_pl_body_when_enum_type(Field_Type, DB_P_N, DB_P_A, P_N, PI_Body_List_In, PI_Body_List_Out),
	print_msg('debug', 'translate_field_description_for(P_N, Cls)', (P_N, Cl)).

generate_db_value_conversor(Field_Type, DB_Value, Output_Value, Conversion) :-
	Field_Type = 'rfuzzy_truth_value_type', !, 
	Conversion = (Output_Value .=. DB_Value).

generate_db_value_conversor(Field_Type, DB_Value, Output_Value, Conversion) :-
	(
	    Field_Type = 'rfuzzy_string_type' ; 
	    Field_Type = 'rfuzzy_integer_type' ; 
	    Field_Type = 'rfuzzy_enum_type' ;
	    Field_Type = 'rfuzzy_boolean_type' ; 
	    Field_Type = 'rfuzzy_datetime_type' % Not sure this is the right conversion ...
	), !, 
	Conversion = (Output_Value = DB_Value).

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

generate_pl_body_when_enum_type(Field_Type, _DB_P_N, _DB_P_A, _P_N, PI_Body_List_In, PI_Body_List_In) :-
	nonvar(Field_Type), Field_Type \== 'rfuzzy_enum_type', !.
generate_pl_body_when_enum_type(Field_Type, DB_P_N, DB_P_A, P_N, PI_Body_List_In, [Pl_Body | PI_Body_List_In]) :-
	nonvar(Field_Type), Field_Type = 'rfuzzy_enum_type', !,
	Pl_Body = (Type_Var, Enum_Value_Var, Body),

	functor(DB_P_F, DB_P_N, DB_P_A),
	Mapping = (Input_Value = DB_P_F),
	functor(P_F, P_N, 2), 
	arg(1, P_F, Input_Value),
	arg(2, P_F, Enum_Value_Var),

	Body = (((Mapping, DB_P_F), P_F), (Type_Var = DB_P_N)).

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

add_auxiliar_code(Cls_In, Cls_Out) :-
	print_msg('debug', 'add_auxiliar_code :: Cls_In', Cls_In),
	code_for_defined_negation_ops(Cls_In, Cls_Aux_1), 
	code_for_getting_attribute_values(Cls_Aux_1, Cls_Aux_2), 
	code_for_predefined_types(Cls_Aux_2, Cls_Aux_3),
	code_for_defined_modifiers(Cls_Aux_3, Cls_Aux_4),
	print_msg('debug', 'add_auxiliar_code :: Cls_Aux_4', Cls_Aux_4),
%	code_for_rfuzzy_compute_1(Cls_Aux_4, Cls_Aux_5),
%	print_msg('debug', 'add_auxiliar_code :: Cls_Aux_5', Cls_Aux_5),
	code_for_rfuzzy_compute_2(Cls_Aux_4, Cls_Aux_6),
	code_for_assert_local_user_name(Cls_Aux_6, Cls_Aux_7),
%	print_msg('debug', 'add_auxiliar_code :: Cls_Aux_7', Cls_Aux_7),
	code_for_testing_program_introspection(Cls_Aux_7, Cls_Out),
	print_msg('debug', 'add_auxiliar_code :: Cls_Out', Cls_Out).

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

code_for_testing_program_introspection(Cls_In, [Cl_1, Cl_2, Cl_3, Cl_4, Cl_5, Cl_6 | Cls_In]) :-
	Cl_1 = (test_program_introspection :- findall(rfuzzy_introspection(PN, PA, PT, P_O, P_MI), rfuzzy_introspection(PN, PA, PT, P_O, P_MI), L), test_program_introspection_aux_1(L)),
	Cl_2 = (test_program_introspection_aux_1([])), 
	Cl_3 = (test_program_introspection_aux_1([H|T]) :- test_program_introspection_aux_2(H), test_program_introspection_aux_1(T)),
	Cl_4 = (test_program_introspection_aux_2(rfuzzy_introspection(_PN, _PA, _PT, _P_O, P_MI)) :- test_program_introspection_aux_3(P_MI)),
	Cl_5 = (test_program_introspection_aux_3([])),
	Cl_6 = (test_program_introspection_aux_3([(Selector, Info) | P_MI]) :- list(Info), print_msg('debug', '(Selector, Info)', (Selector, Info)), test_program_introspection_aux_3(P_MI)).

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

code_for_assert_local_user_name(Code, [Cl_1, Cl_2 | Code]) :-
	Cl_1 = (assertLocalUserName(UserName) :- assertLocalUserNameAux(UserName)),
	Cl_2 = (isUserNameLocalUserName(UserName) :- isUserNameLocalUserNameAux(UserName, _Kind, 'ok')).

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

code_for_getting_attribute_values(In, In) :- !. % Without use.
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

code_for_defined_negation_ops(Code_In, Code_Out) :-
	retract_fact(defined_negation_ops_code(Defined_Neg_Ops_Code)),
	append_local(Defined_Neg_Ops_Code, Code_In, Code_Out), !.

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

code_for_defined_modifiers(Code_In, Code_Out) :-
	retract_fact(defined_modifiers_code(Defined_Modifiers_Code)),
	append_local(Defined_Modifiers_Code, Code_In, Code_Out), !.

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

code_for_rfuzzy_compute_2(In, [Code | In]) :-
	Code = (rfuzzy_compute(Operator, Elt1_In, Elt2_In, Database, Truth_Value) :- 
	       nonvar(Operator), nonvar(Database),
	       functor(Elt2_In, Name, 1),
	       functor(Aux_Elt2, Name, 2),
	       print_msg('debug', 'rfuzzy_compute_aux :: rfuzzy_introspection(Name, 2)', (Name, 2)),
	       rfuzzy_introspection(Name, 2, P_T, _P_O, _Pred_MI_1_List), !,
	       print_msg('debug', 'rfuzzy_compute_aux :: rfuzzy_introspection(Name, 2, P_T)', (Name, 2, P_T)),
	       memberchk_local([Database, Arg_Type], P_T),
	       print_msg('debug', 'rfuzzy_compute_aux :: Arg_Type', (Arg_Type)),
	       arg(1, Elt2_In, Elt2_Arg1), 
	       arg(1, Aux_Elt2, Elt2_Arg1),
	       arg(2, Aux_Elt2, Elt2_Arg2),
	       print_msg('debug', 'rfuzzy_compute_aux :: calling Aux_Elt2', Aux_Elt2),
	       Aux_Elt2, 
	       (
		   (   Operator = '=~=', !,
		       print_msg('debug', 'rfuzzy_compute_aux :: operator is =~=', Operator),
		       functor(Elt1, Name, 1),
		       arg(1, Elt1, Elt1_In),
		       functor(Elt2, Name, 1),
		       arg(1, Elt2, Elt2_Arg2),
		       Template = rfuzzy_computed_similarity_between(Database, Elt1, Elt2, _TV, _Cred_Op, _Cred),
		       findall(Template, Template, Computed_Similarities)
		   )
	       ;
		   (   Operator \== '=~=', !,
		       print_msg('debug', 'rfuzzy_compute_aux :: operator is NOT =~=', Operator),
		       Elt1 = Elt1_In,
		       Elt2 = Elt2_Arg2,
		       Computed_Similarities = []
		   )
	       ),
	       print_msg('debug', 'rfuzzy_compute_aux(Operator, Elt1, Elt2, Computed_Similarities)', (Operator, Elt1, Elt2, Computed_Similarities)),
	       rfuzzy_compute_aux(Operator, Arg_Type, Elt1, Elt2, Computed_Similarities, Truth_Value),
	       print_msg('debug', 'rfuzzy_compute_aux :: Truth_Value', Truth_Value)
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
