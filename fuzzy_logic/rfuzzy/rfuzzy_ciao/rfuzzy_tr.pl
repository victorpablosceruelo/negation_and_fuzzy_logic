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
translation_info('fuzzy_rule_default_without_cond',   2, 0,        "rfuzzy_fuzzy_rule_default_without_cond_").
translation_info('fuzzy_rule_default_with_cond',        2, 0.25,   "rfuzzy_fuzzy_rule_default_with_cond_"). 
translation_info('fuzzy_rule_rule',                                2, 0.5,     "rfuzzy_fuzzy_rule_rule_").
translation_info('fuzzy_rule_fuzzification',                  2, 0.75,   "rfuzzy_fuzzy_rule_fuzzification_").
translation_info('fuzzy_rule_db_value',                       2, 0.9,     "rfuzzy_fuzzy_rule_db_value_").
translation_info('fuzzy_rule_fact',                                2, 1,        "rfuzzy_fuzzy_rule_fact_").
translation_info('fuzzy_rule_synonym',                        2, -1,       "rfuzzy_fuzzy_rule_sinonym_").
translation_info('fuzzy_rule_antonym',                        2, -1,       "rfuzzy_fuzzy_rule_antonym_").
%translation_info('non_rfuzzy_fuzzy_rule', 0, -1,         "non_rfuzzy_fuzzy_rule").

% This produces unexpected results.
% translation_info(_X,                             _Y,               0, 0, 'no', 0,          "rfuzzy_error_error_error_").

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

save_fuzzy_rule_predicate_definition(Pred_Name, Pred_Arity, Pred_Type, Pred_Class) :-
	% translate_predicate(Pred_Name, Pred_Arity, Pred_Class, New_Pred_Name, New_Pred_Arity).
	translate_predicate(Pred_Name, Pred_Arity, 'fuzzy_rule', Real_Pred_Name, Real_Pred_Arity),
	% translate_predicate(Pred_Name, Pred_Arity, Pred_Class, New_Pred_Name, New_Pred_Arity).
	translate_predicate(Pred_Name, Pred_Arity, Pred_Class, New_Pred_Name, New_Pred_Arity),
	
	More_Info = [(Pred_Class, New_Pred_Name, New_Pred_Arity)],
	Needs_Head_Building = 'true',
	save_predicate_definition(Real_Pred_Name, Real_Pred_Arity, Pred_Type, More_Info, Needs_Head_Building).

save_predicate_definition(Pred_Name, Pred_Arity, Pred_Type, More_Info, Needs_Head_Building) :-
	print_msg('debug', 'save_predicate_definition(Pred_Name, Pred_Arity, Pred_Type, More_Info, Needs_Head_Building)', (Pred_Name, Pred_Arity, Pred_Type, More_Info, Needs_Head_Building)),
	(
	    (	 
		list(More_Info),
		retract_fact(predicate_definition(Pred_Name, Pred_Arity, Pred_Type, Old_More_Info, Old_Needs_Head_Building)), !, % Retract last
		print_msg('debug', 'save_predicate_definition', (Pred_Name, Pred_Arity, Pred_Type, Old_More_Info, Old_Needs_Head_Building)),
		append_local(More_Info, Old_More_Info, New_More_Info),
		boolean_or(Old_Needs_Head_Building, Needs_Head_Building, New_Needs_Head_Building),
		assertz_fact(predicate_definition(Pred_Name, Pred_Arity, Pred_Type, New_More_Info, New_Needs_Head_Building))
	    )
	;
	    (
		list(More_Info),
		New_More_Info = More_Info, 
		New_Needs_Head_Building = Needs_Head_Building,
		assertz_fact(predicate_definition(Pred_Name, Pred_Arity, Pred_Type, New_More_Info, New_Needs_Head_Building))
	    )
	), 
	print_msg('debug', 'saved', save_predicate_definition(Pred_Name, Pred_Arity, Pred_Type, New_More_Info, New_Needs_Head_Building)),
	!.

retrieve_predicate_info(Pred_Name, Pred_Arity, Pred_Type, More_Info, Needs_Head_Building, Show_Error) :-
	print_msg('debug', 'retrieve_predicate_info(Pred_Name, Pred_Arity, Pred_Type, More_Info, Needs_Head_Building, Show_Error)', retrieve_predicate_info(Pred_Name, Pred_Arity, Pred_Type, More_Info, Needs_Head_Building, Show_Error)),
	(
	    (   predicate_definition(Pred_Name, Pred_Arity, Pred_Type, More_Info, Needs_Head_Building), !   )
	;
	    (   Show_Error = 'no', !, fail  )
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

rfuzzy_trans_clause(Arg1, Arg1, _Arg3) :- 
	print_msg('debug', 'trans_fuzzy_clause: arg1', Arg1).

rfuzzy_trans_sentence(Arg1, Arg2, Arg3) :- 
	print_msg('debug', 'rfuzzy_trans_sent: arg1', Arg1),
	rfuzzy_trans_sent_aux(Arg1, Arg2), !,
	print_msg('debug', 'rfuzzy_trans_sent: arg2', Arg2),
	print_msg('debug', 'rfuzzy_trans_sent: arg3', Arg3),
	print_msg_nl('debug').

rfuzzy_trans_sentence(Arg, Arg, FileName) :- 
	print_msg('warning', 'rfuzzy_trans_sent: ERROR: Input: ', Arg),
	print_msg('warning', 'rfuzzy_trans_sent: ERROR: FileName: ', FileName),
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
	save_predicate_definition('rfuzzy_id_type', 1, _Pred_Type1, [], 'no'),
	save_predicate_definition('rfuzzy_truth_value_type', 1, _Pred_Type2, [], 'no'),
	save_predicate_definition('rfuzzy_credibility_value_type', 1, _Pred_Type2, [], 'no'),
	save_predicate_definition('rfuzzy_number_type', 1, _Pred_Type2, [], 'no'),
	save_predicate_definition('rfuzzy_predicate_type', 1, _Pred_Type2, [], 'no'),
	save_predicate_definition('fnot', 2, ['rfuzzy_predicate_type', 'rfuzzy_truth_value_type'], [], 'no').

rfuzzy_trans_sent_aux((:-activate_rfuzzy_debug), []) :- !,
	activate_rfuzzy_debug.
rfuzzy_trans_sent_aux((:-Whatever), [(:-Whatever)]) :- !.
rfuzzy_trans_sent_aux(Sentence, Translation) :-
	translate(Sentence, Translation).

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

% Unconditional default
translate((rfuzzy_default_value_for(Pred_Name/Pred_Arity, Fixed_Truth_Value)), (Pred_Functor :- Truth_Value_Functor)) :- 
	!, % If patter matching, backtracking forbiden.
	nonvar(Fixed_Truth_Value), number(Fixed_Truth_Value), nonvar(Pred_Arity), 
	number(Pred_Arity), nonvar(Pred_Name), 

	Pred_Class = 'fuzzy_rule_default_without_cond',
	% translate_predicate(Pred_Name, Pred_Arity, Pred_Class, New_Pred_Name, New_Pred_Arity).
	translate_predicate(Pred_Name, Pred_Arity, Pred_Class, New_Pred_Name, New_Pred_Arity),
	% predicate_to_functor(Pred_Name, Pred_Arity, Pred_Class, Pred_Functor, Truth_Value).
	predicate_to_functor(New_Pred_Name, New_Pred_Arity, Pred_Class, Pred_Functor, Truth_Value),

	generate_truth_value_equal_to_functor(Fixed_Truth_Value, Truth_Value, Truth_Value_Functor),
	!. % Backtracking forbidden.

% Conditional default
translate((rfuzzy_default_value_for(Pred_Name/Pred_Arity, Fixed_Truth_Value) if Pred2_Name/Pred2_Arity), (Pred_Functor :- (Pred2_Functor, Truth_Value_Functor))) :-
	print_msg('debug', 'translate', 'if detected'),
	!, % If patter matching, backtracking forbiden.
	nonvar(Fixed_Truth_Value), number(Fixed_Truth_Value), nonvar(Pred_Arity), number(Pred_Arity),
	nonvar(Pred2_Arity), number(Pred2_Arity), nonvar(Pred_Name), nonvar(Pred2_Name), 
	Pred_Arity = Pred2_Arity,

	Pred_Class = 'fuzzy_rule_default_with_cond',
	% translate_predicate(Pred_Name, Pred_Arity, Pred_Class, New_Pred_Name, New_Pred_Arity).
	translate_predicate(Pred_Name, Pred_Arity, Pred_Class, New_Pred_Name, New_Pred_Arity),
	% predicate_to_functor(Pred_Name, Pred_Arity, Pred_Class, Pred_Functor, Truth_Value).
	predicate_to_functor(New_Pred_Name, New_Pred_Arity, Pred_Class, Pred_Functor, Truth_Value),

	functor(Pred2_Functor, Pred2_Name, Pred2_Arity),
	copy_args(Pred_Arity, Pred_Functor, Pred2_Functor),    % Copy args from main functor.
	generate_truth_value_equal_to_functor(Fixed_Truth_Value, Truth_Value, Truth_Value_Functor),
	!.

translate((rfuzzy_default_value_for(Pred_Name/Pred_Arity, Fixed_Truth_Value) if thershold(Pred2_Name/Pred2_Arity, Cond, Thershold_Truth_Value)), (Pred_Functor :- ((Pred2_Functor, Pred3_Functor), Truth_Value_Functor))) :-
	print_msg('debug', 'translate :: (rfuzzy_default_value_for(Pred_Name/Pred_Arity, Truth_Value) if thershold(Pred2_Name/Pred2_Arity, Cond, Thershold_Truth_Value))', (rfuzzy_default_value_for(Pred_Name/Pred_Arity, Fixed_Truth_Value) if thershold(Pred2_Name/Pred2_Arity, Cond, Thershold_Truth_Value))),
	!, % If patter matching, backtracking forbiden.
	Pred_Arity = Pred2_Arity,
	nonvar(Fixed_Truth_Value), number(Fixed_Truth_Value), number(Pred_Arity), 
	number(Pred2_Arity), nonvar(Pred_Name), nonvar(Pred2_Name), 
	
	Pred_Class = 'fuzzy_rule_default_with_cond',
	% translate_predicate(Pred_Name, Pred_Arity, Pred_Class, New_Pred_Name, New_Pred_Arity).
	translate_predicate(Pred_Name, Pred_Arity, Pred_Class, New_Pred_Name, New_Pred_Arity),
	% predicate_to_functor(Pred_Name, Pred_Arity, Pred_Class, Pred_Functor, Truth_Value).
	predicate_to_functor(New_Pred_Name, New_Pred_Arity, Pred_Class, Pred_Functor, Truth_Value),

	Pred2_Class = 'fuzzy_rule',
	% translate_predicate(Pred_Name, Pred_Arity, Pred_Class, New_Pred_Name, New_Pred_Arity).
	translate_predicate(Pred2_Name, Pred2_Arity, Pred2_Class, New_Pred2_Name, New_Pred2_Arity),
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
	% save_fuzzy_rule_predicate_definition(Pred_Name, Pred_Arity, Pred_Type, Pred_Class)
	save_fuzzy_rule_predicate_definition(New_Pred_Name, New_Pred_Arity, _Pred_Type, Pred_Class), !.

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
translate((rfuzzy_aggregator(Aggregator_Name/Aggregator_Arity)), []) :-
	!, % If patter matching, backtracking forbiden.
	nonvar(Aggregator_Name), number(Aggregator_Arity), Aggregator_Arity = 3,

	Aggregator_Type = ['rfuzzy_truth_value_type', 'rfuzzy_truth_value_type', 'rfuzzy_truth_value_type'],
	% save_predicate_definition(Pred_Name, Pred_Arity, Pred_Type, More_Info, Needs_Head_Building)
	save_predicate_definition(Aggregator_Name, Aggregator_Arity, Aggregator_Type, [], 'no'),
	!.

% function definition.
translate((Head :# List), (Pred_Functor :- (Body, print_msg('debug', 'function_call', Pred_Functor)))) :-
	!, % If patter matching, backtracking forbiden.
	nonvar(Head), nonvar(List),
	% list(Lista),
	print_msg('debug', '(Head :# List) ', (Head :# List)),

	functor(Head, Pred_Name, 0),
	Pred_Arity = 1,
	Pred_Class = 'function',
	% translate_predicate(Pred_Name, Pred_Arity, Pred_Class, New_Pred_Name, New_Pred_Arity).
	translate_predicate(Pred_Name, Pred_Arity, Pred_Class, New_Pred_Name, New_Pred_Arity),
	% predicate_to_functor(Pred_Name, Pred_Arity, Pred_Class, Pred_Functor, Truth_Value).
	predicate_to_functor(New_Pred_Name, New_Pred_Arity, Pred_Class, Pred_Functor, Truth_Value),

	arg(1, Pred_Functor, X),
	build_straight_lines(X, Truth_Value, List, Body),

	Pred_Type = ['rfuzzy_number_type', 'rfuzzy_truth_value_type'],
	% save_predicate_definition(Pred_Name, Pred_Arity, Pred_Type, More_Info, Needs_Head_Building)
	save_predicate_definition(New_Pred_Name, New_Pred_Arity, Pred_Type, [], 'false').

translate(rfuzzy_db_value_for(Pred_Name, Database_Pred_Name, Position), Cls):-
	print_msg('debug', 'rfuzzy_db_value_for(Pred_Name, Database_Pred_Name, Position)', rfuzzy_db_value_for(Pred_Name, Database_Pred_Name, Position)),
	nonvar(Pred_Name), nonvar(Database_Pred_Name), nonvar(Position), 
	% retrieve_predicate_info(Pred_Name, Pred_Arity, Pred_Type, More_Info, Needs_Head_Building, Show_Error)
	retrieve_predicate_info(Database_Pred_Name, Database_Pred_Arity, Database_Pred_Type, _List, _NHB, 'true'), !,
	print_msg('debug', 'rfuzzy_db_value_for(Database_Pred_Arity, Database_Pred_Type)', rfuzzy_db_value_for(Database_Pred_Arity, Database_Pred_Type)),	
	(
	    (
		nonvar(Database_Pred_Arity), nonvar(Database_Pred_Type)
	    )
	;
	    (
		print_msg('error', 'You must define the database type before. Database', Database_Pred_Name), !, fail
	    )
	),
	Position < Database_Pred_Arity, 
	get_nth_element_from_list(Position, Database_Pred_Type, Type),

	functor(DB_Pred_Functor, Database_Pred_Name, Database_Pred_Arity),
	arg(Position, DB_Pred_Functor, Value),

	(
	    (
		nonvar(Type), Type = 'rfuzzy_truth_value_type', !,
		Pred_Class = 'fuzzy_rule_db_value',
		% translate_predicate(Pred_Name, Pred_Arity, Pred_Class, New_Pred_Name, New_Pred_Arity).
		translate_predicate(Pred_Name, Pred_Arity, Pred_Class, New_Pred_Name, New_Pred_Arity),
		% predicate_to_functor(Pred_Name, Pred_Arity, Pred_Class, Pred_Functor, Truth_Value).
		predicate_to_functor(New_Pred_Name, New_Pred_Arity, Pred_Class, Pred_Functor, Truth_Value),
		arg(1, Pred_Functor, Input),
		functor(Mapping, '=', 2), arg(1, Mapping, Input), arg(2, Mapping, DB_Pred_Functor),
		functor(Test, '\==', 2), arg(1, Test, Value), arg(2, Test, 'null'),
		functor(Convert, '.=.', 2), arg(1, Convert, Value), arg(2, Convert, Truth_Value),
		Cls = (Pred_Functor :- (((Mapping, DB_Pred_Functor), Test), Convert)),
		Pred_Type = [Database_Pred_Name, Type],
		% save_fuzzy_rule_predicate_definition(Pred_Name, Pred_Arity, Pred_Type, Pred_Class)
		save_fuzzy_rule_predicate_definition(New_Pred_Name, New_Pred_Arity, Pred_Type, Pred_Class)
	    )
	;
	    (
		nonvar(Type), Type = 'rfuzzy_number_type', !, Pred_Arity = 2,
		functor(Pred_Functor, Pred_Name, Pred_Arity),
		functor(Test, '\==', 2), arg(1, Test, Value), arg(2, Test, 'null'),
		arg(1, Pred_Functor, Input), arg(2, Pred_Functor, Value),
		functor(Mapping, '=', 2), arg(1, Mapping, Input), arg(2, Mapping, DB_Pred_Functor),
		Cls = (Pred_Functor :- ((Mapping, DB_Pred_Functor), Test)),
		Pred_Type = [Database_Pred_Name, Type],
		% save_predicate_definition(Pred_Name, Pred_Arity, Pred_Type, More_Info, Needs_Head_Building)
		save_predicate_definition(Pred_Name, Pred_Arity, Pred_Type, [], 'false')
	    )
	),
	print_msg('debug', 'rfuzzy_db_value_for', (Pred_Name, Cls)).

% Predicate type(s) definition (Class = database).
translate(rfuzzy_type_for(Class, Pred_Name/Pred_Arity_In, Types_In), Cls):-
	nonvar(Class), 
	Class = 'database',
	translate(rfuzzy_type_for('crisp_rule', Pred_Name/Pred_Arity_In, Types_In), Cls).

% Predicate type(s) definition (Class \== database).
translate(rfuzzy_type_for(Pseudo_Class, Pred_Name/Pred_Arity_In, Pred_Type_In),(Pred_Functor :- Cls)):-
	Pseudo_Class \== 'database',
	!, % If patter matching, backtracking forbiden.
	print_msg('debug', 'rfuzzy_type_for(Class, Pred_Name/Pred_Arity, Pred_Type)', (Pseudo_Class, Pred_Name/Pred_Arity_In, Pred_Type_In)),
	nonvar(Pseudo_Class), nonvar(Pred_Name), number(Pred_Arity_In), nonvar(Pred_Type_In), 
	(
	    (Pseudo_Class = 'fuzzy_rule', Pred_Class = 'fuzzy_rule_type', Needs_Head_Building='true',
	    append_local(Pred_Type_In, ['rfuzzy_credibility_value_type', 'rfuzzy_truth_value_type'], Pred_Type),
	    Pred_Arity is Pred_Arity_In + 1
	    )
	;
	    (Pseudo_Class = 'crisp_rule', Pred_Class = 'crisp_rule_type', Needs_Head_Building='false',
	     Pred_Type = Pred_Type_In, Pred_Arity = Pred_Arity_In
	    )
	),
	print_msg('debug', 'rfuzzy_type_for :: (Pred_Name, Pred_Arity, Pred_Class)', (Pred_Name, Pred_Arity, Pred_Class)),
	
	% translate_predicate(Pred_Name, Pred_Arity, Pred_Class, New_Pred_Name, New_Pred_Arity).
	translate_predicate(Pred_Name, Pred_Arity, Pred_Class, New_Pred_Name, New_Pred_Arity),
	% predicate_to_functor(Pred_Name, Pred_Arity, Pred_Class, Pred_Functor, Truth_Value).
	predicate_to_functor(New_Pred_Name, New_Pred_Arity, Pred_Class, Pred_Functor, _Truth_Value),

	fix_functor_type(Pred_Functor, New_Pred_Arity, 1, Pred_Type, Cls),
	
	% save_predicate_definition(Pred_Name, Pred_Arity, Pred_Type, More_Info, Needs_Head_Building)
	save_predicate_definition(New_Pred_Name, New_Pred_Arity, Pred_Type, [], Needs_Head_Building),
	
	!. % Backtracking forbidden.

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

translate(rfuzzy_synonym(Pred2_Name/Pred_Arity, Pred_Name/Pred_Arity, Cred_Op, Cred), Cls):-
	!,
	print_msg('debug', 'translate(rfuzzy_synonym(Pred2_Name/Pred_Arity, Pred_Name/Pred_Arity, Cred_Op, Cred))) ', rfuzzy_synonym(Pred2_Name/Pred_Arity, Pred_Name/Pred_Arity, Cred_Op, Cred)),
	nonvar(Pred2_Name), nonvar(Pred_Name), nonvar(Cred_Op), nonvar(Cred), number(Cred), nonvar(Arity), number(Arity),
	test_aggregator_is_defined(Cred_Op, 'true'),

	Pred_Class = 'fuzzy_rule_synonym',
	% translate_predicate(Pred_Name, Pred_Arity, Pred_Class, New_Pred_Name, New_Pred_Arity).
	translate_predicate(Pred_Name, Pred_Arity, Pred_Class, New_Pred_Name, New_Pred_Arity),
	% predicate_to_functor(Pred_Name, Pred_Arity, Pred_Class, Pred_Functor, Truth_Value).
	predicate_to_functor(New_Pred_Name, New_Pred_Arity, Pred_Class, Pred_Functor, Truth_Value_Out),

	functor(Credibility_Functor, Cred_Op, 3), 
	Credibility_Functor=..[Cred_Op, Truth_Value_In, Cred, Truth_Value_Out],

	Arity_plus_1 is Arity + 1,
	Arity_plus_2 is Arity_plus_1 + 1,

	% retrieve_predicate_info(Pred_Name, Pred_Arity, Pred_Type, More_Info, Needs_Head_Building, Show_Error),
	retrieve_predicate_info(Pred2_Name, Arity_plus_1, Pred2_Type, _MI_2, _NHB_2, 'true'),

	add_preffix_to_name(Pred2_Name, "rfuzzy_aux_", Pred2_Name_Aux),
	functor(Pred2_Functor_Aux, Pred2_Name_Aux, Arity_plus_2),
	copy_args(Arity_plus_1, Pred_Functor, Pred2_Functor_Aux),
	arg(Arity_plus_2, Pred2_Functor_Aux, Truth_Value_In),

	Cls = (Pred_Functor :- Pred2_Functor_Aux, Credibility_Functor, (Truth_Value_Out .>=. 0, Truth_Value_Out .=<. 1)),
	% save_fuzzy_rule_predicate_definition(Pred_Name, Pred_Arity, Pred_Type, Pred_Class)
	save_fuzzy_rule_predicate_definition(Pred_Name, Pred_Arity, Pred2_Type, Pred_Class).
	% save_predicate_definition(Pred_Name, Pred_Arity, Pred_Type, More_Info, Needs_Head_Building)
	% save_predicate_definition(New_Pred_Name, New_Pred_Arity, Pred_Type, [], 'true').

translate(rfuzzy_antonym(Pred2_Name/Pred_Arity, Pred_Name/Pred_Arity, Cred_Op, Cred), Cls):-
	!,
	print_msg('debug', 'translate(rfuzzy_antonym(Pred2_Name/Pred_Arity, Pred_Name/Pred_Arity, Cred_Op, Cred)) ', rfuzzy_antonym(Pred2_Name/Pred_Arity, Pred_Name/Pred_Arity, Cred_Op, Cred)),
	nonvar(Pred_Name), nonvar(Pred2_Name), nonvar(Cred_Op), nonvar(Cred), number(Cred), nonvar(Arity), number(Arity),
	test_aggregator_is_defined(Cred_Op, 'true'),

	Pred_Class = 'fuzzy_rule_antonym',
	% translate_predicate(Pred_Name, Pred_Arity, Pred_Class, New_Pred_Name, New_Pred_Arity).
	translate_predicate(Pred_Name, Pred_Arity, Pred_Class, New_Pred_Name, New_Pred_Arity),
	% predicate_to_functor(Pred_Name, Pred_Arity, Pred_Class, Pred_Functor, Truth_Value).
	predicate_to_functor(New_Pred_Name, New_Pred_Arity, Pred_Class, Pred_Functor, Truth_Value_Out),

	functor(Credibility_Functor, Cred_Op, 3), 
	Credibility_Functor=..[Cred_Op, Truth_Value_Aux, Cred, Truth_Value_Out],

	Arity_plus_1 is Arity + 1,
	Arity_plus_2 is Arity_plus_1 + 1,

	% retrieve_predicate_info(Pred_Name, Pred_Arity, Pred_Type, More_Info, Needs_Head_Building, Show_Error),
	retrieve_predicate_info(Pred2_Name, Arity_plus_1, Pred2_Type, _MI_2, _NHB_2, 'true'),

	add_preffix_to_name(Pred2_Name, "rfuzzy_aux_", Pred2_Name_Aux),
	functor(Pred2_Functor_Aux, Pred2_Name_Aux, Arity_plus_2),
	copy_args(Arity_plus_1, Pred_Functor, Pred2_Functor_Aux),
	arg(Arity_plus_2, Pred2_Functor_Aux, Truth_Value_In),

	Cls = (Pred_Functor :- Pred2_Functor_Aux, (Truth_Value_Aux .=. 1 - Truth_Value_In), Credibility_Functor, (Truth_Value_Out .>=. 0, Truth_Value_Out .=<. 1)),
	% save_fuzzy_rule_predicate_definition(Pred_Name, Pred_Arity, Pred_Type, Pred_Class)
	save_fuzzy_rule_predicate_definition(Pred_Name, Pred_Arity, Pred2_Type, Pred_Class).

translate(rfuzzy_quantifier(Pred_Name/Pred_Arity), []):-
	print_msg('debug', 'translate: rfuzzy_quantifier(Pred_Name/Pred_Arity)', rfuzzy_quantifier(Pred_Name/Pred_Arity)),
	!,
	nonvar(Pred_Name), nonvar(Pred_Arity), number(Pred_Arity), Pred_Arity = 2,

	Pred_Type = [rfuzzy_predicate_type, rfuzzy_truth_value_type],
	% save_predicate_definition(Pred_Name, Pred_Arity, Pred_Type, More_Info, Needs_Head_Building)
	save_predicate_definition(Pred_Name, Pred_Arity, Pred_Type, [], 'no'),

	print_msg('debug', 'translate: rfuzzy_quantifier(Pred_Name/Pred_Arity)', rfuzzy_quantifier(Pred_Name/Pred_Arity)).

% fuzzification:
translate(rfuzzy_define_fuzzification(Pred_Name, Crisp_Pred_Name, Funct_Pred_Name), Cls):-
	!, % If patter matching, backtracking forbiden.
	print_msg('debug', 'translate: rfuzzy_define_fuzzification(Pred_Name, Crisp_Pred_Name, Funct_Pred_Name)', rfuzzy_define_fuzzification(Pred_Name, Crisp_Pred_Name, Funct_Pred_Name)),
	% retrieve_predicate_info(Pred_Name, Pred_Arity, Pred_Type, More_Info, Needs_Head_Building, Show_Error) 
	retrieve_predicate_info(Crisp_Pred_Name,  2, Pred_Type_1, _MI_1, _NHB_1, 'true'),
	retrieve_predicate_info(Funct_Pred_Name, 2, Pred_Type_2, _MI_2, _NBH_2, 'true'),

	Pred_Class = 'fuzzy_rule_fuzzification', Pred_Arity = 0,
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

	Cls = (Pred_Functor :- (Crisp_Pred_Functor, Funct_Pred_Functor)),
	get_nth_element_from_list(1, Pred_Type_1, Type_1),
 	get_nth_element_from_list(1, Pred_Type_2, Type_2),
	Pred_Type = [Type_1, Type_2],
	save_fuzzy_rule_predicate_definition(Pred_Name, Pred_Arity, Pred_Type, Pred_Class),
	!.

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

	save_fuzzy_rule_predicate_definition(Pred_Name, Pred_Arity, _Pred_Type, Pred_Class),
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
	print_msg('debug', 'fix_functor_type(Functor, Arity, Actual, Type)', (Functor, Arity, Actual, Type) ),
	Actual = Arity, !, % Security conditions.
	fix_functor_type_aux(Functor, Actual, Type, Type_F),
	!. % Backtracking not allowed.

fix_functor_type(Functor, Arity, Actual, [Type | More], (Type_F, More_F)) :-
	print_msg('debug', 'fix_functor_type(Functor, Arity, Actual, Type)', (Functor, Arity, Actual, Type) ),
	Actual < Arity, !,  % Security conditions.
	fix_functor_type_aux(Functor, Actual, Type, Type_F),
	NewActual is Actual + 1, % Next values.
	!,
	fix_functor_type(Functor, Arity, NewActual, More, More_F),
	!. % Backtracking not allowed here.

fix_functor_type(Functor, Arity, Actual, Types, _Type_F) :-
	print_msg('error', 'fix_functor_type(Functor, Arity, Actual, Types)', fix_functor_type(Functor, Arity, Actual, Types)),
	!, fail.

fix_functor_type_aux(Functor, Actual, Type, (Cl1, Cl2)) :-
	print_msg('debug', 'fix_functor_type_aux(Functor, Actual, Type)', fix_functor_type_aux(Functor, Actual, Type)),
	Pred_Name = Type,
	% retrieve_predicate_info(Pred_Name, Pred_Arity, Pred_Type, More_Info, Needs_Head_Building, Show_Error),
	retrieve_predicate_info(Pred_Name, Pred_Arity, _Pred_Type, _More_Info, _Needs_Head_Building, 'true'),
	functor(Type_F, Type, Pred_Arity), % Build functor.
	arg(1, Type_F, X),       % Argument of functor is X.
	arg(Actual, Functor, X), % Unify with Argument of functor.
	functor(Cl1, '=', 2), 
	arg(1, Cl1, X), 
	arg(2, Cl1, Type_F),
	Cl2 = Type_F.

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

build_straight_lines(X, V, [(X1,V1),(X2,V2)], (Point1 ; Line ; Point2)) :-
	print_msg('debug', 'build_straight_lines', (build_straight_lines(X, V, [(X1,V1),(X2,V2)], (Point1, Line, Point2)))),
	build_point(X, V, X1, V1, Point1),
	build_line(X, V, X1, V1, X2, V2, Line),
	build_point(X, V, X2, V2, Point2).

build_straight_lines(X, V, [(X1,V1),(X2,V2)|List], (Point ; Line ; More)) :-
	print_msg('debug', 'build_straight_lines', (build_straight_lines(X, V, [(X1,V1),(X2,V2)|List], (Point, Line, More)))),
	build_point(X, V, X1, V1, Point),
	build_line(X, V, X1, V1, X2, V2, Line),
	build_straight_lines(X, V, [(X2,V2)|List], More).

build_point(X, V, X1, V1, (X .=. X1, V .=. V1)) :-
	print_msg('debug', 'build_point', build_point(X, V, X, V, (H :- (H :- X1 .=. X, V1 .=. V)))).

build_line(X, V, X1, V1, X2, V2, (X .>. X1, X .<. X2, Calculate_V)) :-
	print_msg('debug', 'build_line', (build_line(X, V, X1, V1, X2, V2, (X .>. X1, X .<. X2, Calculate_V)))),

	number(X1), number(X2),
	number(V1), number(V2),
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
	nonvar(Pred_Name), nonvar(Pred_Arity), nonvar(Pred_Class),
	print_msg('debug', 'translate_predicate(Pred_Name, Pred_Arity, Pred_Class)', (Pred_Name, Pred_Arity, Pred_Class)),
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

append_local([], N2, N2).
append_local([Elto|N1], N2, [Elto|Res]) :-
	append_local(N1, N2, Res).

memberchk_local(Element, [Element | _Tail]) :- !.
memberchk_local(Element, [_Head | Tail]) :- !,
	memberchk_local(Element, Tail).

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

build_auxiliary_clauses([], [end_of_file]) :- !.
build_auxiliary_clauses([Predicate_Def|Predicate_Defs], Clauses) :-
	print_msg_nl('debug'),
	print_msg('debug', 'build_auxiliary_clauses IN (Predicate_Def)', (Predicate_Def)),
	test_if_list_contains_non_rfuzzy_fuzzy_rule(Predicate_Def), !,
	print_msg('debug', 'build_auxiliary_clauses OUT', 'nothing'),
	build_auxiliary_clauses(Predicate_Defs, Clauses).	
build_auxiliary_clauses([Predicate_Def|Predicate_Defs], [Pred_Main, Pred_Aux | Clauses]) :-
	print_msg_nl('debug'),
	print_msg('debug', 'build_auxiliary_clauses IN (Predicate_Def)', (Predicate_Def)),
	build_auxiliary_clause(Predicate_Def, Pred_Main, Pred_Aux), !,
	print_msg('debug', 'build_auxiliary_clauses OUT (Pred_Main, Pred_Aux)', (Pred_Main, Pred_Aux)),
	build_auxiliary_clauses(Predicate_Defs, Clauses).	

build_auxiliary_clause(predicate_definition(Category, Pred_Name, Pred_Arity, _Pred_Type, List), Fuzzy_Cl_Main, Fuzzy_Cl_Aux) :-

	Category = 'fuzzy_rule',
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
	build_functors(List, 'type_for_fp',                 'true', Aux_Pred_Functor, Fuzzy_Pred_Types, [], Def_1, [], NDef_1),
	build_functors(List, 'fact',                              'fail',  Aux_Pred_Functor, Fuzzy_Pred_Fact, Def_1, Def_2, NDef_1, NDef_2),
	build_functors(List, 'fuzzification',                'fail',  Aux_Pred_Functor, Fuzzy_Pred_Fuzzification, Def_2, Def_3, NDef_2, NDef_3),
	build_functors(List, 'rule',                              'fail',  Aux_Pred_Functor, Fuzzy_Pred_Rule, Def_3, Def_4, NDef_3, NDef_4),
	build_functors(List, 'default_with_cond',      'fail',  Aux_Pred_Functor, Fuzzy_Pred_Default_With_Cond, Def_4, Def_5, NDef_4, NDef_5),
	build_functors(List, 'default_without_cond', 'fail',  Aux_Pred_Functor, Fuzzy_Pred_Default_Without_Cond, Def_5, Def_6, NDef_5, NDef_6),
	build_functors(List, 'synonym',                     'fail',  Aux_Pred_Functor, Fuzzy_Pred_Synonym, Def_6, Def_7, NDef_6, NDef_7),
	build_functors(List, 'antonym',                      'fail',  Aux_Pred_Functor, Fuzzy_Pred_Antonym, Def_7, Def, NDef_7, NDef),
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
							    Fuzzy_Pred_Antonym 
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
build_functors_notify_missing_facilities(_Pred_Name, Def, _NDef) :- 
	(
	    memberchk_local('synonym', Def)
	;
	    memberchk_local('antonym', Def)
	), !. % No errors.

build_functors_notify_missing_facilities(Pred_Name, _Def, NDef_In) :-
	% Not an error if the missing information belongs to the following categories:
	lists_substraction(NDef_In, ['fact', 'function', 'fuzzification', 'rule', 'default_with_cond', 'synonym', 'antonym'], NDef_Out),
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

test_if_list_contains_non_rfuzzy_fuzzy_rule(predicate_definition(_Category, _Pred_Name, _Pred_Arity, _Pred_Type, List)) :-
	test_if_list_contains_non_rfuzzy_fuzzy_rule_aux(List).

test_if_list_contains_non_rfuzzy_fuzzy_rule_aux([('non_rfuzzy_fuzzy_rule', _Sub_Pred_Name, _Sub_Pred_Arity)|_List]) :- !.
test_if_list_contains_non_rfuzzy_fuzzy_rule_aux([(_Category, _Sub_Pred_Name, _Sub_Pred_Arity)|List]) :-
	test_if_list_contains_non_rfuzzy_fuzzy_rule_aux(List).

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

generate_introspection_predicate_real(predicate_definition(Category, Pred_Name, Pred_Arity, Pred_Type, _List), rfuzzy_introspection(Category, Pred_Name, Pred_Arity, Pred_Type)).
% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

add_auxiliar_code(Fuzzy_Rules_In, Fuzzy_Rules_Out) :-
	code_for_quantifier_fnot(Fuzzy_Rules_In, Fuzzy_Rules_Aux_1), 
	code_for_getting_attribute_values(Fuzzy_Rules_Aux_1, Fuzzy_Rules_Aux_2), 
	code_for_predefined_types(Fuzzy_Rules_Aux_2, Fuzzy_Rules_Out).

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

code_for_predefined_types(In, [Type_1, Type_2, Type_3|In]) :-
	Type_1 = (rfuzzy_predicate_type(_Any)), 
	Type_2 = (rfuzzy_truth_value_type(_Any)), 
	Type_3 = (rfuzzy_number_type(_Any)), 
	!.

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------
