:- module(rfuzzy_tr,[rfuzzy_trans_sentence/3, rfuzzy_trans_clause/3],[]).

:- use_module(library(aggregates), [findall/3]).
:- use_module(library(terms),[copy_args/3]).
:- use_module(library('rfuzzy/rfuzzy_rt')).
:- include(library('clpqr-common/ops')).
:- include(library('rfuzzy/rfuzzy_ops')).

% Important info to be saved.
:- data predicate_definition/4.
:- data aggregators/1.
:- data sentences/2.

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

% translation_info(Category, Sub_Category_Of, Add_Args, Fix_Priority, Priority, Preffix_String)

translation_info('aggregator',         '', 0, 'no', 0, "").
translation_info('function',              '', 0, 'no', 0, "").
translation_info('defuzzification',   '', 0, 'no', 0, "").
translation_info('quantifier',           '', 1, 'no', 0, "").
translation_info('crisp',                   '', 0, 'no', 0, "").
translation_info('fuzzy_rule',         '', 1, 'no', 0, "").
% translation_info('auxiliar',           '', 2, 'no', 0, "rfuzzy_aux_").
% translation_info('normal',           '', 1, 'yes', 0, "").

translation_info('type',                             'fuzzy_rule', 2, 'no', 0,          "rfuzzy_type_").
translation_info('default_without_cond', 'fuzzy_rule', 2, 'yes', 0,        "rfuzzy_default_without_cond_").
translation_info('default_with_cond',      'fuzzy_rule', 2, 'yes', 0.25,   "rfuzzy_default_with_cond_"). 
translation_info('rule',                              'fuzzy_rule', 2, 'yes', 0.5,     "rfuzzy_rule_").
translation_info('fuzzification',                 'fuzzy_rule', 2, 'yes', 0.75,  "rfuzzy_fuzzification_").
translation_info('fact',                               'fuzzy_rule', 2, 'yes', 1,       "rfuzzy_fact_").
translation_info('sinonym',                        'fuzzy_rule', 2, 'no', 0,         "rfuzzy_sinonym_").
translation_info('antonym',                       'fuzzy_rule', 2, 'no', 0,         "rfuzzy_antonym_").

% This produces unexpected results.
% translation_info(_X,                             _Y,               0, 0, 'no', 0,          "rfuzzy_error_error_error_").

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

save_predicate_definition(Category, Pred_Name, Pred_Arity, Sub_Category, Sub_Pred_Name, Sub_Pred_Arity) :-
	print_msg('debug', 'save_predicate_definition(Category, Pred_Name, Pred_Arity, Sub_Category, Sub_Pred_Name, Sub_Pred_Arity)', save_predicate_definition(Category, Pred_Name, Pred_Arity, Sub_Category, Sub_Pred_Name, Sub_Pred_Arity)),
	save_predicate_definition_test_categories(Category, Sub_Category), % Only valid values, please.
	
	(
	    (	 
		retract_fact(predicate_definition(Category, Pred_Name, Pred_Arity, List)), !, % Retract last
		assertz_fact(predicate_definition(Category, Pred_Name, Pred_Arity, [(Category, Sub_Pred_Name, Sub_Pred_Arity)|List]))
	    )
	;
	    (
		retract_fact(predicate_definition(_Category_Aux, Pred_Name, Pred_Arity, List)), !, % Retract last
		print_msg('error', 'It is not allowed to define again the predicate', (Pred_Name/Pred_Arity)),
		fail
	    )
	;
	    (
		assertz_fact(predicate_definition(Category, Pred_Name, Pred_Arity, [(Category, SubPred_Name, SubPred_Arity)]))
	    )
	), 
	print_msg('debug', 'saved', save_predicate_definition(Category, Pred_Name, Pred_Arity, Category, SubPred_Name, SubPred_Arity)),
	!.		 

retrieve_predicate_info(Category, Name, Arity, List, Show_Error) :-
	print_msg('debug', 'retrieve_predicate_info(Category, Name, Arity, List, Show_Error)', retrieve_predicate_info(Category, Name, Arity, List, Show_Error)),
	save_predicate_definition_test_categories(_AnyValidCategory, Category),
	(
	    (   
		predicate_definition(Category, Name, Arity, List), !   
	    )
	;
	    (  
		translation_info(Category, '', _Add_Args, _Fix_Priority, _Priority, _Preffix_String),
		Show_Error = 'yes', !,
		print_msg('error', 'Predicate must be defined before use. Predicate ', Name/Arity), !, 
		fail
	    )
	;
	    (
		print_msg('error', 'Requested category does not exist. Category ', Category), !, 
		fail
	    )
	).

retrieve_all_predicate_info_with_category(Category, Retrieved) :-
	findall((predicate_definition(Category, Name, Arity, List)),
	(retract_fact(predicate_definition(Category, Name, Arity, List))), Retrieved),
	 !.
	
save_predicate_definition_test_categories(Category, Sub_Category) :-
	(
	    save_predicate_definition_test_categories_aux(Category, Sub_Category)
	;
	    (
		print_msg('error', 'Unknown Category or Sub_Category. (Category, Sub_Category)', (Category, Sub_Category)),
		!, fail
	    )
	), !.

save_predicate_definition_test_categories_aux(Category, Sub_Category) :-
	(
	    (	var(Category), var(Sub_Category), !, fail  )
	;
	    (   var(Category), nonvar(Sub_Category), !,
		save_predicate_definition_test_categories_aux(Sub_Category, Sub_Category)
	    )
	;
	    (   nonvar(Category), var(Sub_Category), !, 
		save_predicate_definition_test_categories_aux(Category, Category)
	    )
	).

save_predicate_definition_test_categories_aux(Category, Sub_Category) :-
	nonvar(Category), nonvar(Sub_Category),
	Category = Sub_Category, % This case is impossible because it is an infinite loop.
	(
	    translation_info(Sub_Category, _Any_Category, _Add_Args, _Fix_Priority, _Priority, _Preffix_String), !
	;
	    translation_info(_Any_Sub_Category, Category, _Add_Args, _Fix_Priority, _Priority, _Preffix_String), !
	), !.

save_predicate_definition_test_categories_aux(Category, Sub_Category) :-
	nonvar(Category), nonvar(Sub_Category),
	% translation_info(Category, Sub_Category_Of, Add_Args, Fix_Priority, Priority, Preffix_String)
	translation_info(Sub_Category, Category, _Add_Args, _Fix_Priority, _Priority, _Preffix_String), !.

%remove_predicate_info(Category, Name, Arity, Fuzzy_Name, Fuzzy_Arity) :-
%	predicate_definition_contents(Pred_Info, Category, Name, Arity, Fuzzy_Name, Fuzzy_Arity),
%	retract_fact(Pred_Info), !. % Retract 


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
	print_info(Arg), 
	print_msg_nl('debug'),
	print_msg_nl('debug').

print_info(Sentence) :-
	print_msg('info', 'print_info', Sentence),
	functor(Sentence, Name, Arity),
	print_msg('info', 'print_info: (Name, Arity)', (Name, Arity)),
	Sentence=..[Name|Args],
	print_list_info(Args).
print_list_info([]) :- !,
	print_msg('info', 'print_list_info', 'empty list').
print_list_info([Arg | Args]) :- !,
	print_info(Arg),
	print_list_info(Args).

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

% We need to evaluate the whole program at the same time.
% Note that eat_2 uses info supplied by eat_1 and 
% eat_3 uses info supplied by eat_1 and eat_2.	
rfuzzy_trans_sent_aux(end_of_file, Fuzzy_Rules_Built):-
	!,
	retrieve_all_predicate_info_with_category('fuzzy_rule', Fuzzy_Rules_To_Build),
	print_msg('debug', 'fuzzy rules to build', Fuzzy_Rules_To_Build),
	build_auxiliary_clauses(Fuzzy_Rules_To_Build, Fuzzy_Rules_Built).

rfuzzy_trans_sent_aux(0, []) :- !, nl, nl, nl.
rfuzzy_trans_sent_aux((:-Whatever), [(:-Whatever)]) :- !.
rfuzzy_trans_sent_aux(Sentence, Translation) :-
	translate(Sentence, Translation).

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

% Unconditional default
translate((rfuzzy_default_value_for(Pred_Name/Pred_Arity, Value)), (Fuzzy_H)) :- 
	!, % If patter matching, backtracking forbiden.
	number(Value), % Value must be a number.
	number(Pred_Arity), % A must be a number.
	nonvar(Pred_Name), % Name can not be a variable.

	functor(H, Pred_Name, Pred_Arity),
	% translate_functor(Functor, Category, Save_Predicate, Fuzzy_Functor, Truth_Value)
	translate_functor(H, 'default_without_cond', 'yes', Fuzzy_H, Value),
	!. % Backtracking forbidden.

% Conditional default
translate((rfuzzy_default_value_for(Pred_Name/Pred_Arity, Value) if Pred2_Name/Pred2_Arity), (Fuzzy_H :- H_Cond)) :-
%	functor(Conditioned_Default, 'if', 2), 
	print_msg('debug', 'translate', 'if detected'),
%	Conditioned_
	
	!, % If patter matching, backtracking forbiden.
	Pred_Arity = Pred2_Arity,
	number(Value), % Value must be a number.
	number(Pred_Arity), % Pred_Arity must be a number.
	number(Pred2_Arity), % Pred2_Arity must be a number.
	nonvar(Pred_Name), % Pred_Name cannot be a variable.
	nonvar(Pred2_Name), % Pred2_Name cannot be a variable.
	
	functor(H, Pred_Name, Pred_Arity),
	% translate_functor(Functor, Category, Save_Predicate, Fuzzy_Functor, Truth_Value)
	translate_functor(H, 'default_with_cond', 'yes', Fuzzy_H, Value),
	
	functor(H_Cond, Pred2_Name, Pred2_Arity),
	copy_args(Pred_Arity, H_Cond, H), !.   % Copy args from main functor.

translate((rfuzzy_default_value_for(Pred_Name/Pred_Arity, Value) if thershold(Pred2_Name/Pred2_Arity, Cond, Thershold_Value)), (H_Pred :- H_Pred2, H_Pred3)) :-
	print_msg('debug', 'translate :: (rfuzzy_default_value_for(Pred_Name/Pred_Arity, Value) if thershold(Pred2_Name/Pred2_Arity, Cond, Thershold_Value))', (rfuzzy_default_value_for(Pred_Name/Pred_Arity, Value) if thershold(Pred2_Name/Pred2_Arity, Cond, Thershold_Value))),
	!, % If patter matching, backtracking forbiden.
	Pred_Arity = Pred2_Arity,
	number(Value), number(Pred_Arity), number(Pred2_Arity), % They must be numbers.
	nonvar(Pred_Name), nonvar(Pred2_Name), % They cannot be variables.
	
	functor(H_Pred_Tmp, Pred_Name, Pred_Arity),
	% translate_functor(Functor, Category, Save_Predicate, Fuzzy_Functor, Truth_Value)
	translate_functor(H_Pred_Tmp, 'default_with_cond', 'yes', H_Pred, Value),
	functor(H_Pred2_Tmp, Pred2_Name, Pred2_Arity),
	copy_args(Pred2_Arity, H_Pred_Tmp, H_Pred2_Tmp),

	% translate_functor(Functor, Category, Save_Predicate, Fuzzy_Functor, Truth_Value)
	translate_functor(H_Pred2_Tmp, 'normal', 'yes', H_Pred2, Truth_Value_For_Thershold),
	print_msg('debug', 'translate', 'condition (over | under)'),
	(
	    (
		Cond = 'over',
		functor(H_Pred3, '.>.', 2),
		H_Pred3=..['.>.', Truth_Value_For_Thershold, Thershold_Value]
	    )
	;
	    (
		Cond = 'under',
		functor(H_Pred3, '.<.', 2),
		H_Pred3=..['.<.', Truth_Value_For_Thershold, Thershold_Value]
	    )
	), !.

% Fuzzy facts.
translate((Head value Value), Fuzzy_Head):-
	!, % If patter matching, backtracking forbiden.
	print_msg('debug', 'fact conversion :: IN ',(Head value Value)),
	number(Value),                    % Value must be a number.

	% translate_functor(Functor, Category, Save_Predicate, Fuzzy_Functor, Truth_Value)
	translate_functor(Head, 'fact', 'yes', Fuzzy_Head, Value),
	print_msg('debug', 'fact conversion :: OUT ',(Fuzzy_Head)),
	!. % Backtracking forbidden.

% Not needed: aggregators are just crisp predicates of arity 3.
%translate((rfuzzy_aggregator(Aggregator_Name, Aggregator_Arity)), []) :-
%	!,
%	save_aggregator(Aggregator_Name, Aggregator_Arity), !.

% function definition.
translate((Head :# List), (Fuzzy_H :- Body)) :-
	!, % If patter matching, backtracking forbiden.
	% list(Lista),
	print_msg('debug', '(Head :# List) ', (Head :# List)),

	functor(Head, Name, 0),
	functor(H, Name, 1),
	% translate_functor(Functor, Category, Save_Predicate, Fuzzy_Functor, Truth_Value)
	translate_functor(H, 'function', 'yes', Fuzzy_H, Value),

	arg(1, Fuzzy_H, X),
	build_straight_lines(X, Value, List, Body).

% Predicate's type(s) definition.
translate(rfuzzy_type_for(Pred_Name/Pred_Arity, Types),(Fuzzy_H :- Cls)):-
	!, % If patter matching, backtracking forbiden.
	print_msg('debug', 'rfuzzy_type_for(Pred_Name/Pred_Arity, Types)', rfuzzy_type_for(Pred_Name/Pred_Arity, Types)),
	(
	    (   % Types has the form [restaurant/1]
		number(Pred_Arity), % A must be a number.
		nonvar(Pred_Name), % Can not be a variable.
		nonvar(Types),
		
		functor(H, Pred_Name, Pred_Arity),
		% translate_functor(Functor, Category, Save_Predicate, Fuzzy_Functor, Truth_Value)
		translate_functor(H, 'type', 'yes', Fuzzy_H, _Unused_Truth_Value),
		translate_each_type(Fuzzy_H, Pred_Arity, 1, Types, Cls)
	    )
	;
	    print_msg('error', 'translate :: Syntax Error in type definition', rfuzzy_type_for(Pred_Name/Pred_Arity, Types))
	),
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

% fuzzification:
translate(rfuzzy_define_fuzzification(Pred/2, Crisp_P/2, Funct_P/2), (Fuzzy_H :- (Crisp_P_F, Funct_P_F))):-
	!, % If patter matching, backtracking forbiden.
	% retrieve_predicate_info(Category, Name, Arity, List, Show_Error)
	retrieve_predicate_info('crisp', Crisp_P, 2, _List_1, 'yes'),
	retrieve_predicate_info('function', Funct_P, 2, _List_2, 'yes'),
	functor(Pred_F, Pred, 1),
	% translate_functor(Functor, Category, Save_Predicate, Fuzzy_Functor, Truth_Value)
	translate_functor(Pred_F, 'fuzzification', 'yes', Fuzzy_H, Truth_Value),

	% We need to do here as in other translations, so 
	% it generates aux and main predicates for fuzzifications too.
	functor(Crisp_P_F, Crisp_P, 2),
	functor(Funct_P_F, Funct_P, 2),

	arg(1, Fuzzy_H, Input),
	arg(1, Crisp_P_F, Input),
	arg(2, Crisp_P_F, Crisp_Value),
	arg(1, Auxiliar_Funct_P_F, Crisp_Value),
	arg(2, Auxiliar_Funct_P_F, Truth_Value),
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
		functor(Arg_1, Name, Arity)
	    )
	;
	    (
		functor(Other, Name, Arity), 
		Name \== ':-',
		Name \== ':~',
		Name \== ':#',
		Name \== 'value',
		Name \== 'fuzzify'
	    )
	),
	% save_predicate_definition(Category, Pred_Name, Pred_Arity, Sub_Category, Sub_Pred_Name, Sub_Pred_Arity)
	save_predicate_definition('crisp', Name, Arity, 'crisp', Name, Arity).

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

translate_rule(Head, Cred_Op, Cred, Body, (Fuzzy_Head :- Fuzzy_Body)) :-
	print_msg('debug', 'translate_rule(Head, Cred_Op, Cred, Body) ', (translate_rule(Head, Cred_Op, Cred, Body))),
	nonvar(Head), nonvar(Cred_Op), nonvar(Body), number(Cred),

	% Change head's name.
	% translate_functor(Functor, Category, Save_Predicate, Fuzzy_Functor, Truth_Value)
	translate_functor(Head, 'rule', 'yes', Fuzzy_Head, Truth_Value),

	% Translate all predicates in the body.
	extract_aggregator(Body, TV_Aggregator, Tmp_Body),

	translate_rule_body(Tmp_Body, TV_Aggregator, Truth_Value, Fuzzy_Body).

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

extract_aggregator(Body, Aggregator_Op, Tmp_Body) :-
	nonvar(Body),
	functor(Body, Aggregator_Op, 1),
	nonvar(Aggregator_Op),
	defined_aggregators(Aggregators),
	memberchk_local(Aggregator_Op, Aggregators),
	arg(1, Body, Tmp_Body), !.
extract_aggregator(Body, Aggregator_Op, Tmp_Body) :-
	nonvar(Body),
	functor(Body, Aggregator_Op, _Unknown_Arity),
	nonvar(Aggregator_Op),
	% retrieve_predicate_info(Category, Name, Arity, List, Show_Error)
	retrieve_predicate_info('crisp', Aggregator_Op, 3, _List, 'yes'),
	arg(1, Body, Tmp_Body), !.
extract_aggregator(Body, 'null', Body) :- !.

%retrieve_aggregator_info(Op, Operator) :-
%	faggr(Op1, _IAny,  Operator, _FAny),
%	nonvar(Op),
%	aggregators(Aggregators),
%	print_msg('debug', 'retrieve_aggregator_info :: Op', Op),
%	print_msg('debug', 'retrieve_aggregator_info :: Aggregators', Aggregators),
%	member(faggr(Op, _IAny,  Operator, _FAny), Aggregators), !.
%retrieve_aggregator_info(Op, Op) :-
%	rfuzzy_rt:defined_aggregators(Aggregators), !,
%	member(Op, Aggregators).
%retrieve_aggregator_info(Op, 'id') :-
%	print_msg('error', 'retrieve_aggregator_info :: not a valid agregator operator :: ', Op).

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

% Conjunction.
translate_rule_body((Tmp_Body_1, Tmp_Body_2), TV_Aggregator, Truth_Value, (FB_1, FB_2, Aggr_F)) :- !,
	print_msg('debug', 'translate_rule_body(Body, TV_Aggregator, Truth_Value)',((Tmp_Body_1, Tmp_Body_2), TV_Aggregator, Truth_Value)),
	translate_rule_body(Tmp_Body_1, TV_Aggregator, TV_1, FB_1),
	translate_rule_body(Tmp_Body_2, TV_Aggregator, TV_2, FB_2),
	functor(Aggr_F, TV_Aggregator, 3),
	arg(1, Aggr_F, TV_1), 
	arg(2, Aggr_F, TV_2), 
	arg(3, Aggr_F, Truth_Value), !.

% Quantifier.
translate_rule_body(Body_F, _TV_Aggregator, Truth_Value, (Fuzzy_F, (Truth_Value .>=. 0, Truth_Value .=<. 1))) :-
	print_msg('debug', 'translate_rule_body(Body, Truth_Value) - with quantifier',(Body_F, Truth_Value)),
	functor(Body_F, Pred_Name, 1),
	Body_F=..[Pred_Name|Pred_Args],
	% retrieve_predicate_info(Category, Name, Arity, List, Show_Error)
	retrieve_predicate_info('quantifier', Pred_Name, 1, _List, 'yes'), !,
	functor(Fuzzy_F, Pred_Name, 2),
	arg(2, Fuzzy_F, Truth_Value),
	translate_rule_body_subcall(Pred_Args, _Truth_Value_Aux, SubCall),
	arg(1, Fuzzy_F, SubCall).

% Normal.
translate_rule_body(Body_F, _TV_Aggregator, Truth_Value, (Fuzzy_F, (Truth_Value .>=. 0, Truth_Value .=<. 1))) :-
	print_msg('debug', 'translate_rule_body(Body, Truth_Value) - without quantifier',(Body_F, Truth_Value)),
	translate_rule_body_subcall(Body_F, Truth_Value, Fuzzy_F).

translate_rule_body_subcall(Body_F, Truth_Value, Fuzzy_Functor) :-
	print_msg('debug', 'translate_rule_body_subcall(Body, Truth_Value)',(Body_F, Truth_Value)),
	% translate_functor(Functor, Category, Save_Predicate, Fuzzy_Functor, Truth_Value)
	translate_functor(Body_F, 'fuzzy_rule', 'no', Fuzzy_Functor, Truth_Value).

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

translate_each_type(H, Arity, Actual, [Type/1], Type_F) :-
	print_msg('debug', 'translate_each_type(H, Arity, Actual, Type)', (H, Arity, Actual, Type) ),
	Actual = Arity, !, % Security conditions.
	translate_each_type_aux(H, Actual, Type, Type_F),
	!. % Backtracking not allowed.

translate_each_type(H, Arity, Actual, [Type/1 | More], (Type_F, More_F)) :-
	print_msg('debug', 'translate_each_type(H, Arity, Actual, Type)', (H, Arity, Actual, Type) ),
	Actual < Arity, !,  % Security conditions.
	translate_each_type_aux(H, Actual, Type, Type_F),
	NewActual is Actual + 1, % Next values.
	!,
	translate_each_type(H, Arity, NewActual, More, More_F),
	!. % Backtracking not allowed here.

translate_each_type_aux(H, Actual, Type, Type_F) :-
	print_msg('debug', 'translate_each_type_aux(H, Actual, Type)', translate_each_type_aux(H, Actual, Type)),
	% retrieve_predicate_info(Category, Name, Arity, List, Show_Error)
	retrieve_predicate_info('crisp', Type, 1, _List, 'yes'), !,	
	functor(Type_F, Type, 1), % Build functor.
	arg(1, Type_F, X),       % Argument of functor is X.
	arg(Actual, H, X). % Unify with Argument of functor.

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


translate_functor(Functor, Category, Save_Predicate, Fuzzy_Functor, Truth_Value) :-
	print_msg('debug', 'translate_functor(Functor, Category)', translate_functor(Functor, Category)),
	functor(Functor, Name, Arity),
	translation_info(Category, Sub_Category_Of, Add_Args, Fix_Priority, Priority, Preffix_String),
	number(Add_Args),
	Fuzzy_Arity is Arity + Add_Args, % Fuzzy Args.
	add_preffix_to_name(Name, Preffix_String, Fuzzy_Name), % Change name
	functor(Fuzzy_Functor, Fuzzy_Name, Fuzzy_Arity),
	copy_args(Arity, Functor, Fuzzy_Functor), !,
	translate_functor_add_args(Fuzzy_Functor, Fuzzy_Arity, Add_Args, Fix_Priority, Priority, Truth_Value),
	arg(Fuzzy_Arity, Fuzzy_Functor, Truth_Value),
	print_msg('debug', 'translate_functor :: Fuzzy_Functor', Fuzzy_Functor),
	translate_functor_save_predicate_def(Save_Predicate, Sub_Category_Of, Name, Arity, Category, Fuzzy_Name, Fuzzy_Arity).


translate_functor_add_args(Fuzzy_Functor, Fuzzy_Arity, Add_Args, Fix_Priority, Priority, Truth_Value) :-
	print_msg('debug', 'translate_functor_1(Fuzzy_Functor, Fuzzy_Arity, Add_Args)', (Fuzzy_Functor, Fuzzy_Arity, Add_Args, Fix_Priority, Priority)),
	number(Add_Args),
	(
	    (	Add_Args = 0, Truth_Value = 'no_truth_value_argument_added', !  )
	;
	    (	Add_Args = 1, arg(Fuzzy_Arity, Fuzzy_Functor, Truth_Value), !  )
	;
	    (	Add_Args = 2,
		% print_msg('debug', 'translate_functor_1 :: Fuzzy_Arity_Aux', Fuzzy_Arity_Aux),
		arg(Fuzzy_Arity, Fuzzy_Functor, Truth_Value),
		(
		    (	Fix_Priority = 'no'    )
		;
		    (
			Fix_Priority = 'yes',
			Fuzzy_Arity_Aux is Fuzzy_Arity - 1,
			arg(Fuzzy_Arity_Aux, Fuzzy_Functor, Priority)
		    )
		)
	    )
	), !.

% translate_functor_save_predicate(Save_Predicate, Sub_Category_Of, Category, Name, Arity).
translate_functor_save_predicate_def('no', _Sub_Category_Of, _Pred_Name, _Pred_Arity, _Category, _Fuzzy_Name, _Fuzzy_Arity) :- !.
translate_functor_save_predicate_def('yes', '', Pred_Name, Pred_Arity_In, Category, Fuzzy_Name, Fuzzy_Arity) :- !,
	translation_info(Category, '', Add_Args, _Fix_Priority, _Priority, _Preffix_String),
	Pred_Arity is Pred_Arity_In + Add_Args,
	save_predicate_definition(Category, Pred_Name, Pred_Arity, Category, Fuzzy_Name, Fuzzy_Arity), !.
translate_functor_save_predicate_def('yes', Sub_Category_Of, Pred_Name, Pred_Arity_In, Category, Fuzzy_Name, Fuzzy_Arity) :- !,
	translation_info(Sub_Category_Of, '', Add_Args, _Fix_Priority, _Priority, _Preffix_String),
	Pred_Arity is Pred_Arity_In + Add_Args,
	save_predicate_definition(Sub_Category_Of, Pred_Name, Pred_Arity, Category, Fuzzy_Name, Fuzzy_Arity), !.

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

build_auxiliary_clauses([], []) :- !.
build_auxiliary_clauses([Predicate_Def|Predicate_Defs], [Pred_Main, Pred_Aux | Clauses]) :-
	print_msg('debug', 'build_auxiliary_clauses IN (Predicate_Def)', (Predicate_Def)),
	build_auxiliary_clause(Predicate_Def, Pred_Main, Pred_Aux),
	print_msg('debug', 'build_auxiliary_clauses OUT (Pred_Main, Pred_Aux)', (Pred_Main, Pred_Aux)),
	build_auxiliary_clauses(Predicate_Defs, Clauses).

build_auxiliary_clause(predicate_definition(Category, Pred_Name, Pred_Arity, List), Fuzzy_Cl_Main, Fuzzy_Cl_Aux) :-

	Category = 'fuzzy_rule',
	% Build MAIN functor.
	functor(Pred_Functor, Pred_Name, Pred_Arity),
	% Build AUXILIAR functor
	add_preffix_to_name(Pred_Name, 'auxiliar', Aux_Pred_Name),
	Aux_Pred_Arity is Pred_Arity + 1,
	functor(Aux_Pred_Functor, Aux_Pred_Name, Aux_Pred_Arity),
	arg(Aux_Pred_Arity, Aux_Pred_Functor, Aux_Pred_Truth_Value_Arg),
	arg(Pred_Arity,         Aux_Pred_Functor, Aux_Pred_Priority_Arg),
	% Unify crisp args of MAIN and AUXILIAR functors.
	Pred_Crisp_Arity is Pred_Arity - 1,
	copy_args(Pred_Crisp_Arity, Pred_Functor, Aux_Pred_Functor),
	
	build_functors(List, 'type',                             'true', Pred_Name, Aux_Pred_Functor, Fuzzy_Pred_Types),
	build_functors(List, 'fact',                              'fail',  Pred_Name, Aux_Pred_Functor, Fuzzy_Pred_Fact),
%	build_functors(List, 'function',                       'fail',  Pred_Name, Aux_Pred_Functor, Fuzzy_Pred_Function),
	build_functors(List, 'fuzzification',                'fail',  Pred_Name, Aux_Pred_Functor, Fuzzy_Pred_Fuzzification),
	build_functors(List, 'rule',                              'fail',  Pred_Name, Aux_Pred_Functor, Fuzzy_Pred_Rule),
	build_functors(List, 'default_with_cond',      'fail',  Pred_Name, Aux_Pred_Functor, Fuzzy_Pred_Default_With_Cond),
	build_functors(List, 'default_without_cond', 'fail',  Pred_Name, Aux_Pred_Functor, Fuzzy_Pred_Default_Without_Cond),

	(Fuzzy_Cl_Main = (
			     (
				 Pred_Functor :- (
							findall(Aux_Pred_Functor, Aux_Pred_Functor, Results), 
							supreme(Results, Result),
							copy_args(Pred_Arity, Result, Pred_Functor)
						    )		     
			     ) % Main Fuzzy Pred
			 )
	),
	(Fuzzy_Cl_Aux = ( 
			    ( 
				Aux_Pred_Functor :- ( 
						      Fuzzy_Pred_Types,
						      (   Fuzzy_Pred_Fact ; 
%							  Fuzzy_Pred_Function ;
							  Fuzzy_Pred_Fuzzification ;
							  Fuzzy_Pred_Rule ;
							  Fuzzy_Pred_Default_With_Cond ; 
							  Fuzzy_Pred_Default_Without_Cond
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
	
% build_functors(List, Category On_Error, Functor_In, Functor).
build_functors([], Category, On_Error, Pred_Name, _Functor_In, On_Error) :-
	build_functor_show_error_if_necessary(Category, Pred_Name).
	
build_functors([(Category, Sub_Pred_Name, Sub_Pred_Arity)|_List], Category, _On_Error, _Pred_Name, Functor_In, Functor) :-

	!, % Backtracking not allowed.
	functor(Functor, Sub_Pred_Name, Sub_Pred_Arity),  % Create functor
	copy_args(Sub_Pred_Arity, Functor_In, Functor).              % Unify args with the auxiliar one.

build_functors([(_Other_Category, _Sub_Pred_Name, _Sub_Pred_Arity)|List], Category, On_Error, Pred_Name, Functor_In, Functor) :-
	build_functors(List, Category, On_Error, Pred_Name, Functor_In, Functor).

build_functor_show_error_if_necessary(Category, Pred_Name) :-
	(
	    (   % Do not show warnings if the following are missing.
		Category == 'fact' ;
		Category == 'function' ;
		Category == 'fuzzification' ;
		Category == 'rule' ;
		Category == 'default_with_cond'
	    )
	;   
	    (
		print_msg('info', 'You have not defined the facility for the predicate :: (Facility, Predicate_Name) ', (Category, Pred_Name))
	    )
	), !.

%build_fail_functor(H) :-
%	functor(H, 'fail', 0).    % Create functor


% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------
