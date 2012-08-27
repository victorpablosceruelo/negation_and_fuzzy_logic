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

% translation_info(To_What, Preffix_String, Add_Args, Fix_Priority, Priority)

translation_info('type', "rfuzzy_type_", 2, 'no', 0).                  %---|
translation_info('function', "rfuzzy_function_", 0, 'no', 0).       %    |   This are just utils. 
translation_info('auxiliar', "rfuzzy_aux_", 2, 'no', 0).               %    |
translation_info('aggregator', "", 0, 'no', 0).                             %    |
translation_info('quantifier', "", 1, 'noprio', 0).                         %    |   
translation_info('defuzzification', "", 0, 'noprio', 0).                 %---|

translation_info('sinonym', "rfuzzy_sinonym_", 2, 'no', 0).                  %---|   And this ones rely on the priority
translation_info('antonym', "rfuzzy_antonym_", 2, 'no', 0).                  %---|   of the defined predicates.

translation_info('default_without_cond', "rfuzzy_default_without_cond_", 2, 'yes', 0). % Lowest priority
translation_info('default_with_cond', "rfuzzy_default_with_cond_", 2, 'yes', 0.25). 
translation_info('rule', "rfuzzy_rule_", 2, 'yes', 0.5).
translation_info('fuzzification', "rfuzzy_fuzzification_", 2, 'yes', 0.75).
translation_info('fact', "rfuzzy_fact_", 2, 'yes', 1).

translation_info('normal', "", 1, 'yes', 0).
translation_info(_X, "rfuzzy_error_error_error_", 0, 'no', 0).

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
rfuzzy_trans_sent_aux(end_of_file, Fuzzy_Aux_Predicates):-
	!,
	retrieve_list_of_defined_predicates(To_Build_Fuzzy_Aux_Predicates),
	print_msg('debug', 'To_Build_Fuzzy', To_Build_Fuzzy_Aux_Predicates),
	build_auxiliary_clauses(To_Build_Fuzzy_Aux_Predicates, Fuzzy_Aux_Predicates).

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
	translate_functor(H, 'default_without_cond', Fuzzy_H, Fuzzy_Args),
	get_truth_value_arg(Fuzzy_Args, Value),
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
	translate_functor(H, 'default_with_cond', Fuzzy_H, Fuzzy_Args),
	get_truth_value_arg(Fuzzy_Args, Value),
	
	functor(H_Cond, Pred2_Name, Pred2_Arity),
	copy_args(Pred_Arity, H_Cond, H), !.   % Copy args from main functor.

translate((rfuzzy_default_value_for(Pred_Name/Pred_Arity, Value) if thershold(Pred2_Name/Pred2_Arity, Cond, Thershold_Value)), (H_Pred :- H_Pred2, H_Pred3)) :-
	print_msg('debug', 'translate', 'if thershold detected'),
	!, % If patter matching, backtracking forbiden.
	Pred_Arity = Pred2_Arity,
	number(Value), number(Pred_Arity), number(Pred2_Arity), % They must be numbers.
	nonvar(Pred_Name), nonvar(Pred2_Name), % They cannot be variables.
	
	functor(H_Pred_Tmp, Pred_Name, Pred_Arity),
	translate_functor(H, 'default_with_cond', H_Pred, H_Fuzzy_Args1),
	get_truth_value_arg(H_Fuzzy_Args1, Value),

	functor(H_Pred2_Tmp, Pred2_Name, Pred2_Arity),
	copy_args(Pred2_Arity, H_Pred_Tmp, H_Pred2_Tmp),

	translate_functor(H, 'normal', H_Pred2, H_Fuzzy_Args2),
	get_truth_value_arg(H_Fuzzy_Args2, Truth_Value_For_Thershold),
	(
	    (
		Cond = 'over',
		functor(H_Pred3, '.>.', [Truth_Value_For_Thershold, Thershold_Value]) 
	    )
	;
	    (
		Cond = 'under',
		functor(H_Pred3, '.<.', [Truth_Value_For_Thershold, Thershold_Value])
	    )
	), !.

% Fuzzy facts.
translate((Head value Value), Fuzzy_Head):-
	!, % If patter matching, backtracking forbiden.
	print_msg('debug', 'fact conversion :: IN ',(Head value Value)),
	number(Value),                    % Value must be a number.

	translate_functor(Head, 'fact', Fuzzy_Head, Fuzzy_Args),
	get_truth_value_arg(Fuzzy_Args, Value),
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
	translate_functor(H, 'function', Fuzzy_H, Fuzzy_Args),
	get_truth_value_arg(Fuzzy_Args, Value),

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
		list(Types),
		
		functor(H, Pred_Name, Pred_Arity),
		translate_functor(H, 'type', Fuzzy_H, _Fuzzy_Args),
		translate_each_type(Fuzzy_H, Pred_Arity, 1, Types, Cls)
	    )
	;
	    print_msg('error', 'translate_types :: Syntax Error in', rfuzzy_type_for(Pred_Name/Pred_Arity, Types))
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
	test_predicate_has_been_defined('crisp', Crisp_P, 2),
	test_predicate_has_been_defined('function', Funct_P, 2),
	functor(Pred_F, Pred, 1),
	translate_functor(Pred_F, 'fuzzification', Fuzzy_H, Fuzzy_Args),

	% We need to do here as in other translations, so 
	% it generates aux and main predicates for fuzzifications too.
	functor(Crisp_P_F, Crisp_P, 2),
	functor(Funct_P_F, Funct_P, 2),

	arg(1, Fuzzy_H, Input),
	arg(1, Crisp_P_F, Input),
	arg(2, Crisp_P_F, Crisp_Value),
	arg(1, Auxiliar_Funct_P_F, Crisp_Value),
	arg(2, Auxiliar_Funct_P_F, Truth_Value),
	get_truth_value_arg(Fuzzy_Args, Truth_Value),
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
	save_predicate_definition('crisp', Name, Arity, Name, Arity).

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

translate_rule(Head, Cred_Op, Cred, Body, (Fuzzy_Head :- Fuzzy_Body)) :-
	print_msg('debug', 'translate_rule(Head, Cred_Op, Cred, Body) ', (translate_rule(Head, Cred_Op, Cred, Body))),
	nonvar(Head), nonvar(Cred_Op), nonvar(Body), number(Cred),

	% Change head's name.
	translate_functor(Head, 'rule', Fuzzy_Head, Fuzzy_Args),
	get_truth_value_arg(Fuzzy_Args, Truth_Value),

	% Translate all predicates in the body.
	extract_aggregator(Body, TV_Aggregator, Tmp_Body),

	print_msg('debug', 'translate_rule_body(Tmp_Body)',Tmp_Body),
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
	retrieve_predicate_info('crisp', Aggregator_Op, 3, _Fuzzy_Name, _Fuzzy_Arity),
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

translate_rule_body((Tmp_Body_1, Tmp_Body_2), TV_Aggregator, Truth_Value, (FB_1, FB_2, Aggr_F)) :- !,
	translate_rule_body(Tmp_Body_1, TV_Aggregator, TV_1, FB_1),
	translate_rule_body(Tmp_Body_2, TV_Aggregator, TV_2, FB_2),
	functor(Aggr_F, TV_Aggregator, 3),
	arg(1, Aggr_F, TV_1), 
	arg(2, Aggr_F, TV_2), 
	arg(3, Aggr_F, Truth_Value), !.

% translate_rule_body(H, Argument, Ret_Cls)
translate_rule_body(Body_F, _TV_Aggregator, Truth_Value, (Fuzzy_F, (Truth_Value .>=. 0, Truth_Value .=<. 1))) :-
	functor(Body_F, Pred_Name, Pred_Arity),
	retrieve_predicate_info('defined', Pred_Name, Pred_Arity, Fuzzy_Pred_Name, Fuzzy_Pred_Arity), !,
	functor(Fuzzy_F, Fuzzy_Pred_Name, Fuzzy_Pred_Arity),
	copy_args(Pred_Arity, Fuzzy_F, Body_F),
	Fuzzy_F=..[ Fuzzy_Pred_Name | Args ],
	get_truth_value_arg(Args, Truth_Value).

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

translate_each_type(H, Arity, Actual, [Type/1], Type_F) :-
	print_msg('debug', 'translate_each_type(H, Arity, Actual)', (H, Arity, Actual) ),
	Actual = Arity, !, % Security conditions.
	translate_each_type_aux(H, Actual, Type, Type_F),
	!. % Backtracking not allowed.

translate_each_type(H, Arity, Actual, [Type/1 | More], (Type_F, More_F)) :-
	Actual < Arity, !,  % Security conditions.
	print_msg('debug', 'translate_each_type(H, Arity, Actual)', (H, Arity, Actual) ),
	translate_each_type_aux(H, Actual, Type, Type_F),
	NewActual is Actual + 1, % Next values.
	!,
	translate_each_type(H, Arity, NewActual, More, More_F),
	!. % Backtracking not allowed here.

translate_each_type_aux(H, Actual, Type, Type_F) :-

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

test_predicate_has_been_defined(Kind, Name, FuzzyArity) :-
	% retrieve_predicate_info(Kind, Name, Arity, Fuzzy_Name, Fuzzy_Arity)
	retrieve_predicate_info(Kind, Name, _Arity, _FuzzyName, FuzzyArity), 
	% predicate_definition_contents(Pred_Info, 'defined', Name, Arity, Name, Arity),
	!.
test_predicate_has_been_defined(Kind, Name, FuzzyArity) :-
	print_msg('info', 'fuzzify: Cannot find predicate defined by (Kind, Name, FuzzyArity) :: ', (Kind, Name, FuzzyArity)),
	retrieve_predicate_info(Kind_A, Name_A, Arity_A, _FuzzyName_A, FuzzyArity_A), 
	print_msg('debug', 'retrieve_predicate_info(Kind, Name, _Arity, _FuzzyName, FuzzyArity)', 
	retrieve_predicate_info(Kind_A, Name_A, Arity_A, _FuzzyName_A, FuzzyArity_A)), 
	fail, !.

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

predicate_definition_contents(predicate_definition(Kind, Name, Arity, Fuzzy_Name, Fuzzy_Arity), 
	Kind, Name, Arity, Fuzzy_Name, Fuzzy_Arity) :- !.

save_predicate_definition(Kind, Name, Arity, Fuzzy_Name, Fuzzy_Arity) :-
	% translation_info(To_What, Preffix_String, Add_Args, Fix_Priority, Priority)
	translation_info(Kind, _Preffix_String, _Add_Args, _Fix_Priority, _Priority), !,
	nonvar(Kind), !,
	predicate_definition_contents(Pred_Info, Kind, Name, Arity, Fuzzy_Name, Fuzzy_Arity),
	save_predicate_definition_aux(Pred_Info),
	% Now keep a list without repetitions of defined predicates.
	predicate_definition_contents(Pred_Info2, 'defined', Name, Arity, Fuzzy_Name, Fuzzy_Arity),
	save_predicate_definition_aux(Pred_Info2).

save_predicate_definition_aux(Pred_Info) :-
	retract_fact(Pred_Info), !, % Retract last
	assertz_fact(Pred_Info).
save_predicate_definition_aux(Pred_Info) :-
	assertz_fact(Pred_Info).

retrieve_predicate_info(Kind, Name, Arity, Fuzzy_Name, Fuzzy_Arity) :-
	predicate_definition(Kind, Name, Arity, Fuzzy_Name, Fuzzy_Arity).

retrieve_list_of_defined_predicates(Retrieved) :-
	Kind = 'defined',
	findall((predicate_definition(Kind, Name, Arity, Fuzzy_Name, Fuzzy_Arity)),
	(retract_fact(predicate_definition(Kind, Name, Arity, Fuzzy_Name, Fuzzy_Arity))), Retrieved),
	 !.

%remove_predicate_info(Kind, Name, Arity, Fuzzy_Name, Fuzzy_Arity) :-
%	predicate_definition_contents(Pred_Info, Kind, Name, Arity, Fuzzy_Name, Fuzzy_Arity),
%	retract_fact(Pred_Info), !. % Retract 


% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

translate_functor(H, Preffix_Term, Fuzzy_H, Fuzzy_Args) :-
	functor(H, Name, Arity),
	translation_info(Preffix_Term, Prefix_String, Add_Args, Fix_Priority, Priority),
	number(Add_Args),
	Fuzzy_Arity is Arity + Add_Args, % Fuzzy Args.
	add_preffix_to_name(Name, Prefix_String, Fuzzy_Name), % Change name
	save_predicate_definition(Preffix_Term, Name, Arity, Fuzzy_Name, Fuzzy_Arity),
	functor(Fuzzy_H, Fuzzy_Name, Fuzzy_Arity),
	copy_args(Arity, Fuzzy_H, H), !,
	translate_functor_aux_1(Fuzzy_H, Fuzzy_Arity, Add_Args, Fuzzy_Args),
	translate_functor_aux_2(Add_Args, Fuzzy_Args, Fix_Priority, Priority).

translate_functor_aux_1(Fuzzy_H, Fuzzy_Arity, Add_Args, Fuzzy_Args) :-
	number(Add_Args),
	(
	    (
		Add_Args = 0, 
		Fuzzy_Args = [], !
	    )
	;
	    (
		Add_Args = 1,
		arg(Fuzzy_Arity, Fuzzy_H, Fuzzy_Arg1),
		Fuzzy_Args = [Fuzzy_Arg1], !
	    )
	;
	    (
		Add_Args = 2,
		Fuzzy_Arity_Aux = Fuzzy_Arity - 1,
		arg(Fuzzy_Arity_Aux, Fuzzy_H, Fuzzy_Arg1),
		arg(Fuzzy_Arity, Fuzzy_H, Fuzzy_Arg2),
		Fuzzy_Args = [Fuzzy_Arg1, Fuzzy_Arg2], !
	    )
	), !.

translate_functor_aux_2(Add_Args, Fuzzy_Args, Fix_Priority, Priority) :-
	number(Add_Args),
	(
	    (
		Add_Args \== 2
	    )
	;
	    (
		Fix_Priority = 'no'
	    )
	;
	    (
		Fix_Priority = 'yes',
		get_priority_arg(Fuzzy_Args, Priority) 
	    )
	), !.

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

% The truth value is almost always there, 
% so it must be in last place.
get_truth_value_arg([Truth_Value], Truth_Value) :- !.
get_truth_value_arg([_Head | Tail], Truth_Value) :-
	get_truth_value_arg(Tail, Truth_Value).

% The priority value is not always there, but when 
% it is there there is always a truth value.
get_priority_arg([Priority, _Truth_Value], Priority) :- !.
get_priority_arg([_Head | Tail], Priority) :-
	get_priority_arg(Tail, Priority).

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
build_auxiliary_clauses([Def_Pred|Def_Preds], [Pred_Main, Pred_Aux | Clauses]) :-
	print_msg('debug', 'build_auxiliary_clause(Def_Pred, Pred_Main, Pred_Aux)', (Def_Pred, Pred_Main, Pred_Aux)),
	build_auxiliary_clause(Def_Pred, Pred_Main, Pred_Aux),
	build_auxiliary_clauses(Def_Preds, Clauses).

build_auxiliary_clause(Pred_Info, Fuzzy_Cl_Main, Fuzzy_Cl_Aux) :-
	predicate_definition_contents(Pred_Info, 'defined', Name, Arity, Name, Arity),

	% Build MAIN functors.
	Main_Pred_Fuzzy_Arity is Arity + 1,
	Aux_Pred_Fuzzy_Arity is Arity +2,
	functor(Fuzzy_Pred_Main, Name, Main_Pred_Fuzzy_Arity),

	add_preffix_to_name(Name, 'auxiliar', Fuzzy_Pred_Aux_Name),
	functor(Fuzzy_Pred_Aux, Fuzzy_Pred_Aux_Name, Aux_Pred_Fuzzy_Arity),
	copy_args(Arity, Fuzzy_Pred_Main, Fuzzy_Pred_Aux),
	
	build_functors(Name, Arity, 'type', 'true', Fuzzy_Pred_Aux, Fuzzy_Pred_Types),
	build_functors(Name, Arity, 'fact', 'fail', Fuzzy_Pred_Aux, Fuzzy_Pred_Fact),
	build_functors(Name, Arity, 'function', 'fail', Fuzzy_Pred_Aux, Fuzzy_Pred_Function),
	build_functors(Name, Arity, 'fuzzification', 'fail', Fuzzy_Pred_Aux, Fuzzy_Pred_Fuzzification),
	build_functors(Name, Arity, 'rule', 'fail', Fuzzy_Pred_Aux, Fuzzy_Pred_Rule),
	build_functors(Name, Arity, 'default_with_cond', 'fail', Fuzzy_Pred_Aux, Fuzzy_Pred_Default_With_Cond),
	build_functors(Name, Arity, 'default_without_cond', 'fail', Fuzzy_Pred_Aux, Fuzzy_Pred_Default_Without_Cond),

	% Argument's places and values.
	Arity_Fuzzy_Arg_1 is Arity + 1,
	% Arity_Fuzzy_Arg_2 is Arity + 2,
	arg(Arity_Fuzzy_Arg_1, Fuzzy_Pred_Main, Fuzzy_Arg_1),
	% arg(Arity_Fuzzy_Arg_2, Fuzzy_Pred_Main, Fuzzy_Arg_2),

	(Fuzzy_Cl_Main = (
			     (
				 Fuzzy_Pred_Main :- (
							findall(Fuzzy_Pred_Aux, Fuzzy_Pred_Aux, Results), 
							supreme(Results, Result),
							copy_args(Arity, Fuzzy_Pred_Main, Result),
							arg(Arity_Fuzzy_Arg_1, Result, Fuzzy_Arg_1_Tmp),
							Fuzzy_Arg_1 .>=. Fuzzy_Arg_1_Tmp
						    )		     
			     ) % Main Fuzzy Pred
			 )
	),
	(Fuzzy_Cl_Aux = ( 
			    ( 
				Fuzzy_Pred_Aux :- ( 
						      Fuzzy_Pred_Types,
						      (   Fuzzy_Pred_Fact ; 
							  Fuzzy_Pred_Function ;
							  Fuzzy_Pred_Fuzzification ;
							  Fuzzy_Pred_Rule ;
							  Fuzzy_Pred_Default_With_Cond ; 
							  Fuzzy_Pred_Default_Without_Cond
						      )
						  )
			    )
			)
	).

build_auxiliary_clause(Pred_Info, (true), (true)) :-
	print_msg('info', 'it is impossible to build the auxiliary clause for predicate ', Pred_Info).
	
build_functors(Name, Arity, Kind, _On_Error, Functor_In, Functor) :-

	% Do we have saved facts ??
	retrieve_predicate_info(Kind, Name, Arity, Fuzzy_Name, Fuzzy_Arity),
	!, % Backtracking not allowed.

	% Main functor.
	functor(Functor, Fuzzy_Name, Fuzzy_Arity),           % Create functor
	copy_args(Fuzzy_Arity, Functor_In, Functor).  % Copy arguments.

build_functors(_Name, _Arity, Kind, On_Error, _Functor_In, On_Error) :- 
	(   % Do not show warnings if the following are missing.
	    Kind == 'fact' ;
	    Kind == 'function' ;
	    Kind == 'fuzzification' ;
	    Kind == 'rule' ;
	    Kind == 'default_with_cond'
	), !.

build_functors(Name, _Arity, Kind, On_Error, _Functor_In, On_Error) :- !,
	print_msg('info', 'The predicate has not been defined :: (Name, Kind) ', (Name, Kind)).

%build_fail_functor(H) :-
%	functor(H, 'fail', 0).    % Create functor


% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------
