:- module(rfuzzy_tr,[rfuzzy_trans_sentence/3, rfuzzy_trans_clause/3],[]).

:- use_module(library(aggregates), [findall/3]).
:- use_module(library(terms),[copy_args/3]).
:- use_module(library('rfuzzy/rfuzzy_rt')).
:- include(library('clpqr-common/ops')).
:- include(library('rfuzzy/rfuzzy_ops')).

% Important info to be saved.
:- data rfuzzy_predicate_info/5.
:- data aggregators/1.
:- data sentence/2.

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

rfuzzy_trans_clause(Arg1, Arg1, _Arg3) :- 
	print_msg('debug', 'trans_fuzzy_clause: arg1', Arg1).

rfuzzy_trans_sentence(Arg1, Arg2, Arg3) :- 
	print_msg('debug', 'rfuzzy_trans_sent: arg1', Arg1),
	rfuzzy_trans_sent_aux(Arg1, Arg2, Arg3), !,
	print_msg('debug', 'rfuzzy_trans_sent: arg2', Arg2),
	print_msg('debug', 'rfuzzy_trans_sent: arg3', Arg3),
	print_msg_nl('debug').

rfuzzy_trans_sentence(Arg, Arg, FileName) :- 
	print_msg('debug', 'rfuzzy_trans_sent: ERROR: Input: ', Arg),
	print_msg('debug', 'rfuzzy_trans_sent: ERROR: FileName: ', FileName),
	print_msg_nl('debug').

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

% We need to evaluate the whole program at the same time.
% Note that eat_2 uses info supplied by eat_1 and 
% eat_3 uses info supplied by eat_1 and eat_2.	
rfuzzy_trans_sent_aux(end_of_file, Result, FileName):-
	!,
	findall(Cl,(retract_fact(sentence(Cl, FileName))), Sentences_In),
	eat_lists(1, Sentences_In, Sentences_In_1, Converted_1),
	eat_lists(2, Sentences_In_1, Sentences_In_2, Converted_2),
	eat_lists(3, Sentences_In_2, Sentences_In_3, Converted_3),
	eat_lists(4, Sentences_In_3, Sentences_In_4, Converted_4),
	eat_lists(5, Sentences_In_4, Sentences_In_5, Converted_5),

	print_msg_nl('debug'),
	print_msg_nl('debug'),
	print_msg('debug', 'Sentences_In_5', Sentences_In_5),
	print_msg_nl('debug'),
	print_msg_nl('debug'),

	retrieve_all_predicate_info('defined', To_Build_Fuzzy_Aux_Predicates),
	print_msg('debug', 'To_Build_Fuzzy', To_Build_Fuzzy_Aux_Predicates),
	build_auxiliary_clauses(To_Build_Fuzzy_Aux_Predicates, Fuzzy_Aux_Predicates),  

	append_local(Sentences_In_5, Fuzzy_Aux_Predicates, Result_1),
	append_local(Result_1, Converted_5, Result_2),
	append_local(Result_2, Converted_4, Result_3),
	append_local(Result_3, Converted_3, Result_4),
	append_local(Result_4, Converted_2, Result_5),
	append_local(Result_5, Converted_1, Result_6),
	append_local(Result_6, [(end_of_file)], Result),

	print_msg_nl('debug'),
	print_msg_nl('debug'),
	print_msg_nl('debug'),
	print_msg('debug', 'OUT', Result),
	print_msg_nl('debug'),
	print_msg_nl('debug'),
	print_msg_nl('debug').

rfuzzy_trans_sent_aux(0, [], _FileName) :- !, nl, nl, nl.

%rfuzzy_trans_sent_aux(Sentence, [Sentence], _FileName) :-
%	deprecated_syntax(Sentence), !.

rfuzzy_trans_sent_aux((:-Whatever), [(:-Whatever)], _FileName) :- !.

rfuzzy_trans_sent_aux(Sentence, [], FileName) :-
%	print_info(Sentence),
	assertz_fact(sentence(Sentence, FileName)).

%print_info(Sentence) :-
%	print_msg('info', 'print_info', Sentence),
%	functor(Sentence, Name, Arity),
%	print_msg('info', 'print_info: (Name, Arity)', (Name, Arity)),
%	Sentence=..[Name|Args],
%	print_list_info(Args).
%print_list_info([]) :- !,
%	print_msg('info', 'print_list_info', 'empty list').
%print_list_info([Arg | Args]) :- !,
%	print_info(Arg),
%	print_list_info(Args).

% Deprecated syntax (we just warn about it).
%deprecated_syntax((:- default(_Name/_Arity,_X))) :-
%	print_msg('warning', 'deprecated syntax', ':- default(Name/Arity,X)'), !.
% deprecated_syntax((:- default(_Name/_Arity,_X) => _H_Cond_Name/_Arity)) :-
%	print_msg('warning', 'deprecated syntax', '(:- default(Name/Arity,X) => H_Cond_Name/Arity)'), !.
%deprecated_syntax((:- set_prop _Name/_Arity => _Properties_Decl)) :-
%	print_msg('warning', 'deprecated syntax', '(:- set_prop Name/Arity => Properties_Decl)'), !.
%deprecated_syntax((:- aggr _Whatever)) :-
%	print_msg('warning', 'deprecated syntax', '(:- aggr Whatever)'), !.

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

% Extract and Transform (Lists).
eat_lists(_Index, [], [], []) :- !.	
eat_lists(Index, [Sent_In | Sents_In], Invalid, [ Sent_Converted | Converted]) :-
	print_msg('debug', 'eat(Index, Sent_In)', (Index, Sent_In)),
	eat(Index, Sent_In, Sent_Converted), !,
	print_msg('debug', 'eat(Index, Sent_In, Sent_Converted)', (Index, Sent_In, Sent_Converted)),
	print_msg_nl('debug'),
	eat_lists(Index, Sents_In, Invalid, Converted).
eat_lists(Index, [Sent_In | Sents_In], [Sent_In | Invalid], Converted) :-
	eat_lists(Index, Sents_In, Invalid, Converted).

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

% Unconditional default
eat(1, (rfuzzy_default_value_for(Pred_Name/Pred_Arity, Value)), (Fuzzy_H)) :- 
	!, % If patter matching, backtracking forbiden.
	number(Value), % Value must be a number.
	number(Pred_Arity), % A must be a number.
	nonvar(Pred_Name), % Name can not be a variable.

	functor(H, Pred_Name, Pred_Arity),
	fuzzify_functor(H, 'default_without_cond', Fuzzy_H, Fuzzy_Arg_1, Fuzzy_Arg_2),
	Fuzzy_Arg_1 is Value, 
	Fuzzy_Arg_2 is 0,
	!. % Backtracking forbidden.

% Conditional default
eat(1, (rfuzzy_default_value_for(Pred_Name/Pred_Arity, Value) if Pred2_Name/Pred2_Arity), (Fuzzy_H :- H_Cond)) :-
	!, % If patter matching, backtracking forbiden.
	Pred_Arity = Pred2_Arity,
	number(Value), % Value must be a number.
	number(Pred_Arity), % Pred_Arity must be a number.
	number(Pred2_Arity), % Pred2_Arity must be a number.
	nonvar(Pred_Name), % Pred_Name cannot be a variable.
	nonvar(Pred2_Name), % Pred2_Name cannot be a variable.

	functor(H, Pred_Name, Pred_Arity),
	fuzzify_functor(H, 'default_with_cond', Fuzzy_H, Fuzzy_Arg_1, Fuzzy_Arg_2),
	Fuzzy_Arg_1 is Value, 
	Fuzzy_Arg_2 is 0,

	functor(H_Cond, Pred2_Name, Pred2_Arity),
	copy_args(Pred_Arity, H_Cond, H),    % Copy args from main functor.
	!. % Backtracking forbidden.

% Fuzzy facts.
eat(1, (Head value X), Fuzzy_Head):-
	!, % If patter matching, backtracking forbiden.
	print_msg('debug', 'fact conversion :: IN ',(Head value X)),
	number(X),                    % X must be a number.

	fuzzify_functor(Head, 'fact', Fuzzy_Head, Fuzzy_Arg_1, Fuzzy_Arg_2),
	Fuzzy_Arg_1 is X,
	Fuzzy_Arg_2 is 1,
	print_msg('debug', 'fact conversion :: OUT ',(Fuzzy_Head)),
	!. % Backtracking forbidden.

eat(1, (rfuzzy_aggregator(Aggregator_Name, Aggregator_Arity)), true) :-
	!,
	save_aggregator(Aggregator_Name, Aggregator_Arity), !.

% function definition.
eat(1, (Head :# List), (Fuzzy_H :- Body)) :-
	!, % If patter matching, backtracking forbiden.
	% list(Lista),
	print_msg('debug', '(Head :# List) ', (Head :# List)),

	functor(Head, Name, 0),
	functor(H, Name, 1),
	fuzzify_functor(H, 'function', Fuzzy_H, Fuzzy_Arg_1, Fuzzy_Arg_2),
	Fuzzy_Arg_2 is 1,

	arg(1, Fuzzy_H, X),
	build_straight_lines(X, Fuzzy_Arg_1, List, Body).

% Predicate's type(s) definition.
eat(1, rfuzzy_type_for(Pred_Name/Pred_Arity, Types),(Fuzzy_H :- Cls)):-
	!, % If patter matching, backtracking forbiden.
	print_msg('debug', 'rfuzzy_type_for(Pred_Name/Pred_Arity, Types)', rfuzzy_type_for(Pred_Name/Pred_Arity, Types)),
	(
	    (   % Types has the form [restaurant/1]
		number(Pred_Arity), % A must be a number.
		nonvar(Pred_Name), % Can not be a variable.
		nonvar(Types),
		list(Types),
		
		functor(H, Pred_Name, Pred_Arity),
		fuzzify_functor(H, 'type', Fuzzy_H, _Fuzzy_Arg_1, _Fuzzy_Arg_2),
		trans_each_type(Fuzzy_H, Pred_Arity, 1, Types, Cls)
	    )
	;
	    print_msg('error', 'translate_types :: Syntax Error in', rfuzzy_type_for(Pred_Name/Pred_Arity, Types))
	),
	!. % Backtracking forbidden.


% crisp predicates (non-facts).
eat(2, Other, Other) :-
	print_msg('debug', 'Test-1 if crisp pred', Other),
	nonvar(Other), 
	functor(Other, ':-', 2), !,
	arg(1, Other, Arg_1), 
	nonvar(Arg_1), 
	functor(Arg_1, Name, Arity),
	save_predicate_info('crisp', Name, Arity, Name, Arity).

% crisp facts.
eat(2, Other, Other) :-
	print_msg('debug', 'Test-2 if crisp fact', Other),
	nonvar(Other), 
	functor(Other, Name, Arity), 
	Name \== ':-',
	Name \== ':~',
	Name \== ':#',
	Name \== 'value',
	Name \== 'fuzzify',
	save_predicate_info('crisp', Name, Arity, Name, Arity).

% rules:
eat(3, (Head :~ Body), (Fuzzy_H :- Fuzzy_Body)):-
	!, % If patter matching, backtracking forbiden.
	print_msg_nl('debug'),
	print_msg('debug', 'eat(Head :~ Body) ', (Head :~ Body)),
	(
	    trans_rule(Head, Body, Fuzzy_H, Fuzzy_Body)
	;
	    (
		print_msg('error', 'translate_rule_syntax :: Error in', (Head :~ Body)),
		print_msg('error', 'translate_rule_syntax :: Syntax for rules is', '(predicate cred(Op1, Number) :~ Op2(( Body ))'),
		print_msg('error', 'translate_rule_syntax :: Please note there are 2 parenthesis around the variable Body', ' '),
		!, fail
	    )
	).

% fuzzification:
eat(3, rfuzzy_define_fuzzification(Pred/Arity, Crisp_P/Crisp_P_Arity, Funct_P/Func_P_Arity), (Fuzzy_H :- Fuzzy_Body)):-
	!, % If patter matching, backtracking forbiden.
	(
	    trans_fuzzification(Pred, Arity, Crisp_P, Crisp_P_Arity, Funct_P, Func_P_Arity, Fuzzy_H, Fuzzy_Body)
	;
	    (
		print_msg('error', 'Syntax Error in', rfuzzy_define_fuzzification(Pred/Arity, Crisp_P/Crisp_P_Arity, Funct_P/Func_P_Arity)),
		!, fail
	    )
	).

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

trans_rule(Head, Body, Fuzzy_Head, (Fuzzy_Body, Fuzzy_Operations)) :-
	extract_credibility(Head, H, Op1, Credibility),
	print_msg_nl('debug'),
	print_msg('debug', 'trans_rule(H, Op1, Credibility, Body) ', (trans_rule(H, Op1, Credibility, Body))),
	nonvar(H), nonvar(Credibility), nonvar(Body),

	% Change head's name.
	fuzzify_functor(H, 'rule', Fuzzy_Head, Fuzzy_Arg_1, Fuzzy_Arg_2),

	% Translate all predicates in the body.
	extract_op2(Body, Op2, Tmp_Body),

	print_msg('debug', 'add_auxiliar_parameters(Tmp_Body)',Tmp_Body),
	add_auxiliar_parameters(Tmp_Body, Fuzzy_Body, [], Fuzzy_Vars_1, [], Fuzzy_Vars_2),

	% Add the capability to compute the rule's truth value.
	retrieve_aggregator_info(Op1, Operator_1),
	retrieve_aggregator_info(Op2, Operator_2),
	Fuzzy_Operations = (
			       inject(Fuzzy_Vars_1,  Operator_2, Aggregated_V),
			       Mu .>=. 0, Mu .=<. 1,
			       inject([Aggregated_V, Credibility], Operator_1, Fuzzy_Arg_1),
			       Fuzzy_Arg_1  .>=. 0, Fuzzy_Arg_1  .=<. 1,
			       inject(Fuzzy_Vars_2, 'mean', Fuzzy_Arg_2)
	      ).

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

save_aggregator(Aggregator_Name, 3) :-
        retract_fact(aggregators(List)), !,
	assertz_fact(aggregators([Aggregator_Name|List])).
save_aggregator(Aggregator_Name, 3) :-
	assertz_fact(aggregators([Aggregator_Name])).

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

% extract credibility
extract_credibility((H cred (Op, D)), H, Op, D) :- !.
extract_credibility(H, H, 'prod', 1) :- !.

extract_op2(Body, Op2, Tmp_Body) :-
	functor(Body, Op2, 1),
	arg(1, Body, Tmp_Body).
extract_op2(Body, _Op2, 'fail') :-
	print_msg('debug', 'Cannot understand syntax for', (Body)).	

retrieve_aggregator_info(Op, Operator) :-
%	faggr(Op1, _IAny,  Operator, _FAny),
	nonvar(Op),
	aggregators(Aggregators),
	print_msg('debug', 'retrieve_aggregator_info :: Op', Op),
	print_msg('debug', 'retrieve_aggregator_info :: Aggregators', Aggregators),
	member(faggr(Op, _IAny,  Operator, _FAny), Aggregators), !.
retrieve_aggregator_info(Op, Op) :-
	rfuzzy_rt:defined_aggregators(Aggregators), !,
	member(Op, Aggregators).
retrieve_aggregator_info(Op, 'id') :-
	print_msg('error', 'retrieve_aggregator_info :: not a valid agregator operator :: ', Op).


add_auxiliar_parameters(Body, (Fuzzy_Body_1, Fuzzy_Body_2), Vars_1_In, Vars_1_Out, Vars_2_In, Vars_2_Out) :-
	functor(Body, ',', 2), !,
	arg(1, Body, Body_1),
	arg(2, Body, Body_2),
	add_auxiliar_parameters(Body_1, Fuzzy_Body_1, Vars_1_In, Vars_1_Tmp, Vars_2_In, Vars_2_Tmp),
	add_auxiliar_parameters(Body_2, Fuzzy_Body_2, Vars_1_Tmp, Vars_1_Out, Vars_2_Tmp, Vars_2_Out).

% add_auxiliar_parameters(H, Argument, Ret_Cls)
add_auxiliar_parameters(Body, Fuzzy_Body, Vars_1, [V|Vars_1], Vars_2, [C|Vars_2]) :-
	functor(Body, Body_Name, Arity),
	real_name_and_arity(Body_Name, Arity, Fuzzy_Body_Name),
	V_Arity is Arity +1,
	C_Arity is Arity +2,
	functor(Fuzzy_Body, Fuzzy_Body_Name, C_Arity),
	copy_args(Arity, Fuzzy_Body, Body),
	arg(V_Arity, Fuzzy_Body, V),
	arg(C_Arity, Fuzzy_Body, C).

add_auxiliar_parameters(Body, Body, Vars_1, Vars_1, Vars_2, Vars_2) :-
	print_msg('debug', 'ERROR: add_auxiliar_parameters(Body, Cls, Vars) ', (add_auxiliar_parameters(Body,
	Vars_1, Vars_2))),
	!, fail.

real_name_and_arity(Body_Name, Arity, Body_Name) :-
	retrieve_predicate_info('crisp', Body_Name, Arity, Body_Name, _Fuzzy_Arity), !.

real_name_and_arity(Body_Name, _Arity, Fuzzy_Body_Name) :-
	change_name('auxiliar', Body_Name, Fuzzy_Body_Name).

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

trans_each_type(H, Arity, Actual, [Type/1], Type_F) :-
	print_msg('debug', 'trans_each_type(H, Arity, Actual)', (H, Arity, Actual) ),
	Actual = Arity, !, % Security conditions.
	trans_each_type_aux(H, Actual, Type, Type_F),
	!. % Backtracking not allowed.

trans_each_type(H, Arity, Actual, [Type/1 | More], (Type_F, More_F)) :-
	Actual < Arity, !,  % Security conditions.
	print_msg('debug', 'trans_each_type(H, Arity, Actual)', (H, Arity, Actual) ),
	trans_each_type_aux(H, Actual, Type, Type_F),
	NewActual is Actual + 1, % Next values.
	!,
	trans_each_type(H, Arity, NewActual, More, More_F),
	!. % Backtracking not allowed here.

trans_each_type_aux(H, Actual, Type, Type_F) :-

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

trans_fuzzification(Pred, 2, Crisp_P, 2, Funct_P, 2, Fuzzy_Pred_F, (Crisp_P_F, Auxiliar_Funct_P_F)) :-

	test_predicate_has_been_defined('crisp', Crisp_P, 2),
	test_predicate_has_been_defined('function', Funct_P, 3), % Here arity is 3
	functor(Pred_F, Pred, 1),
	fuzzify_functor(Pred_F, 'fuzzification', Fuzzy_Pred_F, Fuzzy_Arg_1, Fuzzy_Arg_2),

	% We need to do here as in other translations, so 
	% it generates aux and main predicates for fuzzifications too.
	functor(Crisp_P_F, Crisp_P, 2),
	change_name('auxiliar', Funct_P, Auxiliar_Funct_P),
	functor(Auxiliar_Funct_P_F, Auxiliar_Funct_P, 3),

	arg(1, Fuzzy_Pred_F, Input),
	arg(1, Crisp_P_F, Input),
	arg(2, Crisp_P_F, Crisp_Value),
	arg(1, Auxiliar_Funct_P_F, Crisp_Value),
	arg(2, Auxiliar_Funct_P_F, Fuzzy_Arg_1),
	arg(3, Auxiliar_Funct_P_F, Fuzzy_Arg_2),

	!.

test_predicate_has_been_defined(Kind, Name, FuzzyArity) :-
	% retrieve_predicate_info(Kind, Name, Arity, Fuzzy_Name, Fuzzy_Arity)
	retrieve_predicate_info(Kind, Name, _Arity, _FuzzyName, FuzzyArity), 
	% rfuzzy_predicate_info_contents(Pred_Info, 'defined', Name, Arity, Name, Arity),
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

save_predicate_info(Kind, Name, Arity, Fuzzy_Name, Fuzzy_Arity) :-
	save_predicate_info_aux(Kind, Name, Arity, Fuzzy_Name, Fuzzy_Arity),
	save_predicate_info_aux('defined', Name, Arity, Name, Arity), % Predicate MUST be defined.
	print_msg('debug', 'save_predicate_info out', save_predicate_info(Kind, Name, Arity, Fuzzy_Name, Fuzzy_Arity)).

save_predicate_info_aux(Kind, Name, Arity, Fuzzy_Name, Fuzzy_Arity) :-
	rfuzzy_predicate_info_contents(Pred_Info, Kind, Name, Arity, Fuzzy_Name, Fuzzy_Arity),
	retract_fact(Pred_Info), !, % Retract last
	assertz_fact(Pred_Info).
save_predicate_info_aux(Kind, Name, Arity, Fuzzy_Name, Fuzzy_Arity) :-
	rfuzzy_predicate_info_contents(Pred_Info, Kind, Name, Arity, Fuzzy_Name, Fuzzy_Arity),
	assertz_fact(Pred_Info).

retrieve_predicate_info(Kind, Name, Arity, Fuzzy_Name, Fuzzy_Arity) :-
	rfuzzy_predicate_info(Kind, Name, Arity, Fuzzy_Name, Fuzzy_Arity).

retrieve_all_predicate_info(Kind, Retrieved) :-
	findall((rfuzzy_predicate_info(Kind, Name, Arity, Fuzzy_Name, Fuzzy_Arity)),
	(retract_fact(rfuzzy_predicate_info(Kind, Name, Arity, Fuzzy_Name, Fuzzy_Arity))), Retrieved),
	 !.

remove_predicate_info(Kind, Name, Arity, Fuzzy_Name, Fuzzy_Arity) :-
	rfuzzy_predicate_info_contents(Pred_Info, Kind, Name, Arity, Fuzzy_Name, Fuzzy_Arity),
	retract_fact(Pred_Info), !. % Retract 

rfuzzy_predicate_info_contents(rfuzzy_predicate_info(Kind, Name, Arity, Fuzzy_Name, Fuzzy_Arity), 
	Kind, Name, Arity, Fuzzy_Name, Fuzzy_Arity) :- !.

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

fuzzify_functor(H, Preffix, Fuzzy_H, Fuzzy_Arg_1, Fuzzy_Arg_2) :-
	functor(H, Name, Arity),
	Fuzzy_Arity_1 is Arity + 1, % For truth value.
	Fuzzy_Arity_2 is Fuzzy_Arity_1 + 1, % For preference.
	change_name(Preffix, Name, Fuzzy_Name), % Change name
	save_predicate_info(Preffix, Name, Arity, Fuzzy_Name, Fuzzy_Arity_2),
	functor(Fuzzy_H, Fuzzy_Name, Fuzzy_Arity_2),
	copy_args(Arity, Fuzzy_H, H),
	arg(Fuzzy_Arity_1, Fuzzy_H, Fuzzy_Arg_1),
	arg(Fuzzy_Arity_2, Fuzzy_H, Fuzzy_Arg_2).

change_name(Prefix, Input, Output) :-
	atom(Input),
	translate_prefix(Prefix, Real_Prefix),
	atom_codes(Input, Input_Chars),
	append_local(Real_Prefix, Input_Chars, Output_Chars),
	atom_codes(Output, Output_Chars), 
	atom(Output), !.
%	print_msg('debug', 'change_name', change_name(Prefix, Input, Output)).

append_local([], N2, N2).
append_local([Elto|N1], N2, [Elto|Res]) :-
	append_local(N1, N2, Res).

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

translate_prefix('type', "rfuzzy_type_").
translate_prefix('default_without_cond', "rfuzzy_default_without_cond_").
translate_prefix('default_with_cond', "rfuzzy_default_with_cond_").
translate_prefix('fact', "rfuzzy_fact_").
translate_prefix('rule', "rfuzzy_rule_").
translate_prefix('function', "rfuzzy_function_").
translate_prefix('fuzzification', "rfuzzy_fuzzification_").
translate_prefix('auxiliar', "rfuzzy_aux_").
translate_prefix(_X, "rfuzzy_error_error_error_").

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

build_auxiliary_clauses([], []) :- !.
build_auxiliary_clauses([Def_Pred|Def_Preds], [Pred_Main, Pred_Aux | Clauses]) :-
	print_msg('debug', 'build_auxiliary_clause(Def_Pred, Pred_Main, Pred_Aux)', (Def_Pred, Pred_Main, Pred_Aux)),
	build_auxiliary_clause(Def_Pred, Pred_Main, Pred_Aux),
	build_auxiliary_clauses(Def_Preds, Clauses).

% For crisp predicates we only generate Aux :- Crisp.
build_auxiliary_clause(Pred_Info, Fuzzy_Cl_Main, Fuzzy_Cl_Aux) :-
	rfuzzy_predicate_info_contents(Pred_Info, 'defined', Name, Arity, Name, Arity),
	retrieve_predicate_info('crisp', Name, Arity, Name, Arity), !, 

	change_name('auxiliar', Name, Fuzzy_Pred_Aux_Name),
	functor(Fuzzy_Pred_Aux, Fuzzy_Pred_Aux_Name, Arity),
        functor(Pred_Main, Name, Arity),
	copy_args(Arity, Pred_Main, Fuzzy_Pred_Aux),

	Fuzzy_Cl_Main = (Fuzzy_Pred_Aux :- Pred_Main),
	Fuzzy_Cl_Aux = (Fuzzy_Pred_Aux :- fail).

build_auxiliary_clause(Pred_Info, Fuzzy_Cl_Main, Fuzzy_Cl_Aux) :-
	rfuzzy_predicate_info_contents(Pred_Info, 'defined', Name, Arity, Name, Arity),

	% Build MAIN functors.
	Main_Pred_Fuzzy_Arity is Arity + 1,
	Aux_Pred_Fuzzy_Arity is Arity +2,
	functor(Fuzzy_Pred_Main, Name, Main_Pred_Fuzzy_Arity),

	change_name('auxiliar', Name, Fuzzy_Pred_Aux_Name),
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
						      (   Fuzzy_Pred_Fact ; 
							  Fuzzy_Pred_Function ;
							  Fuzzy_Pred_Fuzzification ;
							  Fuzzy_Pred_Rule ;
							  Fuzzy_Pred_Default_With_Cond ; 
							  Fuzzy_Pred_Default_Without_Cond
						      ),
						      Fuzzy_Pred_Types
						  )
			    )
			)
	).

build_auxiliary_clause(Pred_Info, (true), (true)) :-
	print_msg('info', 'it is impossible to build the auxiliary clause for predicate ', Pred_Info).
	
build_functors(Name, Arity, Kind, _On_Error, Functor_In, Functor) :-

	% Do we have saved facts ??
	remove_predicate_info(Kind, Name, Arity, Fuzzy_Name, Fuzzy_Arity),
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
