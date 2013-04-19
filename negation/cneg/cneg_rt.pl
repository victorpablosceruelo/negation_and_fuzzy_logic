
:- module(cneg_rt, [cneg_rt/6], [assertions]).

:- comment(title, "Contructive Negation Runtime Library").
:- comment(author, "V@'{i}ctor Pablos Ceruelo").
:- comment(summary, "This module implements negation predicates for runtime evaluation.").

:- use_module(library(aggregates),[setof/3]).
:- use_module(library('cneg/cneg_aux'), _).
:- use_module(library('cneg/cneg_diseq'), 
	[
	    get_disequalities_from_constraints_and_remove_them/2,
	    prepare_attributes_for_printing/2, 
	    cneg_diseq_echo/5
	]).

% To access pre-frontiers from anywhere.
:- multifile cneg_pre_frontier/6.
% To evaluate predicates only from the top package.
:- multifile call_to_predicate/1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cneg_rt(UQV, GoalVars, Goal, Proposal, Depth_Level, Trace) :-

	% Save trace (for debugging and tabling usage)
	CN_Call = (cneg_rt(UQV, GoalVars, Goal, Proposal, Depth_Level)), 
	add_predicate_to_trace(evaluating(CN_Call), Trace, NewTrace),
	echo_msg(2, 'trace', 'cneg_rt', 'call to cneg_rt/6 with (updated) trace', NewTrace),
	echo_msg(2, 'nl', 'calls_trace', '', ''),
	echo_msg(2, '', 'calls_trace', 'cneg_rt', evaluating(CN_Call)),

% cneg_rt_dynamic(UQV_In, GoalVars_In, Goal, Proposal, Result_List) :-
% cneg_rt_Aux(Goal, GoalVars, Proposal, Result_List) :-
	echo_msg(2, 'separation', 'cneg_rt', '', ''),
	echo_msg(2, 'nl', 'cneg_rt', '', ''),
	echo_msg(2, '', 'cneg_rt', 'cneg_rt_dynamic :: Proposal', Proposal),
	echo_msg(2, '', 'cneg_rt', 'cneg_rt_dynamic :: (UQV_In, GoalVars_In)', (UQV_In, GoalVars_In)),
	echo_msg(2, '', 'cneg_rt', 'cneg_rt_dynamic :: Goal', Goal),
	echo_msg(2, 'nl', 'cneg_rt', '', ''),
%	echo_msg(2, '', 'cneg_rt', 'cneg_rt_dynamic :: (Goal, GoalVars, Proposal)', (Goal, GoalVars, Proposal)),
	varsbag(GoalVars_In, [], [], GoalVars), % Clean up non-vars in GoalVars.
	varsbag_clean_up(UQV_In, UQV), % Clean up non-vars in UQV (subterms are not in UQV).
	echo_msg(2, '', 'cneg_rt', 'cneg_rt_dynamic :: (UQV, GoalVars)', (UQV, GoalVars)),
	cneg_diseq_echo(2, '', 'cneg_rt', 'cneg_rt_dynamic :: Goal with attrs', Goal),
	!, % Reduce the stack's memory by forbidding backtracking.
	compute_frontier(UQV, GoalVars, Goal, Proposal, Frontier),
	!, % Reduce the stack's memory by forbidding backtracking.
	echo_msg(2, 'nl', 'cneg_rt', '', ''),
	echo_msg(2, 'list', 'cneg_rt', 'cneg_rt_dynamic :: Frontier', Frontier),
	!,
	negate_frontier(Frontier, GoalVars, Proposal, Result_List),
	!, % Reduce the stack's memory by forbidding backtracking.
	echo_msg(2, 'separation', 'cneg_rt', '', ''),
	echo_msg(2, 'nl', 'cneg_rt', '', ''),
	echo_msg(2, '', 'cneg_rt', 'cneg_rt_dynamic :: Summary for Proposal', Proposal),
	echo_msg(2, '', 'cneg_rt', 'cneg_rt_dynamic :: Goal', Goal),
	echo_msg(2, '', 'cneg_rt', 'cneg_rt_dynamic :: (UQV, GoalVars)', (UQV, GoalVars)),
	echo_msg(2, 'nl', 'cneg_rt', '', ''),
	echo_msg(2, 'list', 'cneg_rt', 'cneg_rt_dynamic :: Frontier', Frontier),
	echo_msg(2, 'nl', 'cneg_rt', '', ''),
	echo_msg(2, 'list', 'cneg_rt', 'cneg_rt_dynamic :: Result (conj)', Result_List),
	echo_msg(2, 'separation', 'cneg_rt', '', ''),
	echo_msg(2, 'nl', 'cneg_rt', '', ''),

%	!, % Reduce the stack's memory by forbidding backtracking.
%	call_to_all_negated_subfrontiers(Result_List, Level, Trace_2, CN_Call).

	    !, % Backtracking forbidden.
	    call_to_conjunction_list(Result_List, Depth_Level, NewTrace, CN_Call).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% negate_frontier_list(Frontier, GoalVars, Proposal, Result_List),
% returns in Result_List a list with the negation of each subfrontier in Frontier.

negate_frontier([], _GoalVars, _Proposal, [true]) :- !. % Optimization.
negate_frontier(Frontier, GoalVars, Proposal, Negated_Frontier) :- 
	Frontier \== [], !,
	negate_each_subfrontier(Frontier, GoalVars, Proposal, Negated_Frontier),
	!.

negate_each_subfrontier([], _GoalVars, _Proposal, []) :- !.
negate_each_subfrontier([Frontier | More_Frontiers], GoalVars, Proposal, [Result_Frontier | Result_More_Frontiers]) :-
	echo_msg(2, '', 'cneg_rt', 'negate_subfrontier: (Frontier, GoalVars)', (Frontier, GoalVars)),
	negate_subfrontier_chan(Frontier, GoalVars, Proposal, Result_Frontier),
	echo_msg(2, '', 'cneg_rt', 'negate_subfrontier: Result_Frontier', Result_Frontier),
	!, % Reduce the stack's memory by forbidding backtracking.
	negate_each_subfrontier(More_Frontiers, GoalVars, Proposal, Result_More_Frontiers).
%	combine_negated_frontiers(Result_Frontier, Result_More_Frontiers, Result), 
%	!. % Reduce the stack's memory by forbidding backtracking.

negate_each_subfrontier([Frontier | More_Frontiers], GoalVars, Proposal, Result_More_Frontiers) :-
	echo_msg(1, '', 'cneg_rt', 'negate_each_subfrontier :: ERROR negating Frontier. Frontier', Frontier), !,
	negate_each_subfrontier(More_Frontiers, GoalVars, Proposal, Result_More_Frontiers).

% combine_negated_subfrontiers(Result_Subfr, Result_More_Subfr, Result_Tmp),
%combine_negated_frontiers(fail, _Result_More_Subfr, fail) :- !.
%combine_negated_frontiers(_Result_Subfr, fail, fail) :- !.
%combine_negated_frontiers(true, Result_More_Subfr, Result_More_Subfr) :- !.
%combine_negated_frontiers(Result_Subfr, true, Result_Subfr) :- !.
%combine_negated_frontiers(Result_Subfr, Result_More_Subfr, (Result_Subfr, Result_More_Subfr)) :- !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Please note this mechanism wastes less memory and cpu, 
% since it goes one by one, but allows backtracking.
call_to_conjunction_list([], _Level, Trace, CN_Call) :- 
	add_predicate_to_trace(ended_results_for(CN_Call), Trace, Trace_1),
	prepare_attributes_for_printing(CN_Call, Attributes_For_Printing_Conj),
	add_predicate_to_trace(current_attributes(Attributes_For_Printing_Conj), Trace_1, Trace_2),
	add_predicate_to_trace('-----------------------', Trace_2, Trace_3),
	end_trace(Trace_3),
	echo_msg(2, '', 'cneg_rt', 'call_to_conjunction_list ', 'EMPTY LIST'),
	echo_msg(2, '', 'calls_trace', 'call_to_conjunction_list ', 'EMPTY LIST'),
	echo_msg(2, 'nl', 'calls_trace', '', ''),
	echo_msg(2, 'nl', 'calls_trace', '', '').

call_to_conjunction_list([Result | Result_List], Level, Trace, CN_Call) :-
	add_predicate_to_trace('-----------------------', Trace, Trace_1),
	add_predicate_to_trace(result_for(CN_Call), Trace_1, Trace_2),
	add_predicate_to_trace(subgoal(Result), Trace_2, Trace_3),
	prepare_attributes_for_printing(Result, Attributes_For_Printing_Conj),
	add_predicate_to_trace(with_attributes(Attributes_For_Printing_Conj), Trace_3, Trace_4),
	echo_msg(2, '', 'cneg_rt', 'call_to_conjunction_list :: goal', CN_Call),
	cneg_diseq_echo(2, '', 'cneg_rt', 'call_to_conjunction_list :: result', Result),
	generate_traces_for_conjunction(Trace_4, Trace_5, Trace_6),
	local_call_to(Result, Level, Trace_5),
	call_to_conjunction_list(Result_List, Level, Trace_6, CN_Call).

%generate_disjunction_from_list([], fail) :- !.
%generate_disjunction_from_list([Goal], Goal) :- !.
%generate_disjunction_from_list([Goal | Goals], (Goal ; Disj_Goals)) :-
%	Goals \== [],
%	generate_disjunction_from_list(Goals, Disj_Goals).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

local_call_to(Predicate, Level_In, Trace) :- 
	Level is Level_In + 1,
	echo_msg(2, 'statistics', 'cneg_rt', '', (local_call_to(Predicate, Level))),
	echo_msg(2, 'nl', 'cneg_rt', '', ''),
	echo_msg(2, 'nl', 'calls_trace', '', ''), 
	echo_msg(2, 'nl', 'calls_trace', '', ''), 
	echo_msg_3pm(2, '', 'calls_trace', 'call_to (L', Level, ') :: Predicate', Predicate), 
	cneg_diseq_echo(2, '', 'calls_trace', 'Predicate with attrs', Predicate),
	get_trace_status_list(Trace, Trace_Status_List),
	echo_msg(2, 'list', 'calls_trace', 'call_to :: TRACE ', Trace_Status_List),
	echo_msg(2, 'nl', 'calls_trace', '', ''), 
	local_call_to_aux(Predicate, Level, Trace).

local_call_to(Predicate, Level_In, _Trace) :- 
	Level is Level_In + 1,
	echo_msg_3pm(2, '', 'calls_trace', 'call_to (L', Level, ') :: FAILED Predicate call. Predicate', Predicate), 
%	echo_msg(2, 'nl', 'calls_trace', '', ''), 
	!, fail. 

local_call_to_aux(Predicate, Level_In, Trace) :-
	goal_is_disjunction(Predicate, G1, G2), !,
	Level is Level_In + 1,
	(
	    (
		(       echo_msg_3pm(2, '', 'calls_trace', 'call_to (L', Level, ') :: G1 \\/ G2 :: G1', G1), 
			local_call_to_aux(G1, Level, Trace),
			echo_msg_3pm(2, '', 'calls_trace', 'call_to (L', Level, ') :: G1 \\/ G2 :: G1', 'OK'),
			echo_msg_3pm(2, '', 'calls_trace', 'call_to (L', Level, ') :: G1 \\/ G2', 'OK')
		)
	    ;
		(       echo_msg_3pm(2, '', 'calls_trace', 'call_to (L', Level, ') :: G1 \\/ G2 :: G1', 'FAIL'),
			echo_msg(2, 'nl', 'calls_trace', '', ''), % Differentiate paths.
			echo_msg_3pm(2, '', 'calls_trace', 'call_to (L', Level, ') :: G1 \\/ G2 :: G2', G2), 
			local_call_to_aux(G2, Level, Trace),
			echo_msg_3pm(2, '', 'calls_trace', 'call_to (L', Level, ') :: G1 \\/ G2 :: G2', 'OK'),
			echo_msg_3pm(2, '', 'calls_trace', 'call_to (L', Level, ') :: G1 \\/ G2', 'OK')
		)
	    )
	;
	    (           echo_msg_3pm(2, '', 'calls_trace', 'call_to (L', Level, ') :: G1 \\/ G2 :: G2', 'FAIL'),
			echo_msg_3pm(2, '', 'calls_trace', 'call_to (L', Level, ') :: G1 \\/ G2', 'FAIL'), 
			fail 
	    )
	).

local_call_to_aux(Predicate, Level_In, Trace) :-
	goal_is_conjunction(Predicate, G1, G2), !,
	Level is Level_In + 1,
	generate_traces_for_conjunction(Trace, Trace_G1, Trace_G2),
	(   % 1st conjunct
	    (	echo_msg_3pm(2, '', 'calls_trace', 'call_to (L', Level, ') :: G1 /\\ G2 :: G1', G1),
		local_call_to_aux(G1, Level, Trace_G1),
		echo_msg_3pm(2, '', 'calls_trace', 'call_to (L', Level, ') :: G1 /\\ G2 :: G1', 'OK')
	    )
	;
	    (	echo_msg_3pm(2, '', 'calls_trace', 'call_to (L', Level, ') :: G1 /\\ G2 :: G1', 'FAIL'),
		echo_msg_3pm(2, '', 'calls_trace', 'call_to (L', Level, ') :: G1 /\\ G2', 'FAIL'),
		fail 
	    )
	),
	(   % 2nd conjunct
	    (   echo_msg_3pm(2, '', 'calls_trace', 'call_to (L', Level, ') :: G1 /\\ G2 :: G2', G2),
		local_call_to_aux(G2, Level, Trace_G2),
		echo_msg_3pm(2, '', 'calls_trace', 'call_to (L', Level, ') :: G1 /\\ G2 :: G2', 'OK'),
		echo_msg_3pm(2, '', 'calls_trace', 'call_to (L', Level, ') :: G1 /\\ G2', 'OK')
	    )
	;
	    (   echo_msg_3pm(2, '', 'calls_trace', 'call_to (L', Level, ') :: G1 /\\ G2 :: G2', 'FAIL'),
		echo_msg_3pm(2, '', 'calls_trace', 'call_to (L', Level, ') :: G1 /\\ G2', 'FAIL'),
		fail   
	    )
	).

local_call_to_aux(Predicate, Level_In, Trace) :-
	Level is Level_In + 1,
	goal_is_cneg_rt(Predicate, UQV, GoalVars, Goal, Proposal),
	echo_msg_3pm(2, '', 'calls_trace', 'call_to (L', Level, ') :: Predicate', Predicate),
	cneg_rt(UQV, GoalVars, Goal, Proposal, Level, Trace),
	echo_msg_3pm(2, '', 'calls_trace', 'call_to (L', Level, ') :: SUCCEED Predicate call. Predicate', Predicate).

local_call_to_aux(Predicate, Level_In, Trace) :-
	Level is Level_In + 1,
	goal_is_not_negation(Predicate),
	echo_msg_3pm(2, '', 'calls_trace', 'call_to (L', Level, ') :: Predicate', Predicate),
	add_predicate_to_trace(Predicate, Trace, NewTrace),
	end_trace(NewTrace),
	call_to_predicate(Predicate),
	echo_msg_3pm(2, '', 'calls_trace', 'call_to (L', Level, ') :: SUCCEED Predicate call. Predicate', Predicate).


%local_call_to_aux(Predicate, Level_In) :- 
%	Level is Level_In + 1,
%	echo_msg_3pm(2, '', 'calls_trace', 'call_to (L', Level, ') :: Predicate - FAILED -', Predicate),
%	!, fail. 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% negate_subfrontier(SubFrontier, GoalVars, Proposal, Result_Frontier)
% returns in Result_Frontier the negation of the subfrontier in SubFrontier

% negate_subfrontier_chan(SubFrontier_In, GoalVars_In, Proposal, (Result)) :-
negate_subfrontier_chan(SubFrontier_In, GoalVars_In, 'cneg_rt_Chan', (Result)) :-
	echo_msg(2, 'nl', 'cneg_rt', '', ''),
	echo_msg(2, '', 'cneg_rt', 'negate_subfrontier :: SubFrontier_In', (SubFrontier_In)),
	split_subfrontier_into_E_IE_NIE(SubFrontier_In, SubFrontier_Aux_1),
	echo_msg(2, '', 'cneg_rt', 'negate_subfrontier :: SubFrontier', (SubFrontier_Aux_1)),
	!, % Reduce the stack's memory by forbidding backtracking.
	varsbag(GoalVars_In, [], [], GoalVars),
	normalize_E_IE_NIE(SubFrontier_Aux_1, GoalVars, SubFrontier_Aux_2),
	subfrontier_E_IE_NIE_contents(SubFrontier_Aux_2, E, _IE, NIE),
	varsbag(E, [], GoalVars, ImpVars),
	varsbag(NIE, ImpVars, [], ExpVars),
	echo_msg(2, '', 'cneg_rt', 'negate_subfrontier :: ExpVars', ExpVars),
	!, % Reduce the stack's memory by forbidding backtracking.
	echo_msg(2, '', 'cneg_rt', 'negate_subfrontier', SubFrontier_Aux_2),
	split_IE_NIE_between_imp_and_exp(SubFrontier_Aux_2, ExpVars, SubFrontier_Aux_3),
	echo_msg(2, '', 'cneg_rt', 'negate_subfrontier', SubFrontier_Aux_3),
	!, % Reduce the stack's memory by forbidding backtracking.
	negate_formula(SubFrontier_Aux_3, GoalVars, Result),
	echo_msg(2, '', 'cneg_rt', 'negate_subfrontier :: (Result)', (Result)),
	!. % Reduce the stack's memory by forbidding backtracking.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	

% normalize_I_D_R(I,D,R,GoalVars, In,Dn,Rn,ImpVars) 
% returns In and Dn that are the equalities of I and the disequalities
% of D but after normalizating them.
normalize_E_IE_NIE(Formula_In, GoalVars, Formula_Out) :-
	echo_msg(2, '', 'cneg_rt', 'normalize_E_IE_NIE :: Formula_In', Formula_In),
	remove_from_E_redundant_eqs_and_vars(Formula_In, GoalVars, Formula_Aux),  
	echo_msg(2, '', 'cneg_rt', 'normalize_E_IE_NIE :: Formula_Aux', Formula_Aux),
	remove_from_IE_irrelevant_disequalities(Formula_Aux, GoalVars, Formula_Out),
	echo_msg(2, '', 'cneg_rt', 'normalize_E_IE_NIE :: Formula_Out', Formula_Out), 
	!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% removes from E redundnant equalities and variables
remove_from_E_redundant_eqs_and_vars(Formula_In, GoalVars, Formula_Out) :- 
	echo_msg(2, '', 'cneg_rt', 'remove_from_E_redundant_eqs_and_vars :: Formula_In', Formula_In),
	echo_msg(2, '', 'cneg_rt', 'remove_from_E_redundant_eqs_and_vars :: GoalVars', GoalVars),
	subfrontier_E_IE_NIE_contents(Formula_In, E_In, IE_In, NIE_In), 
	remove_from_E_redundant_vars(E_In, GoalVars, 'fail', Changes),
	remove_from_E_redundant_eqs(E_In, [], _Visited, E_Aux),
	subfrontier_E_IE_NIE_contents(Formula_Aux, E_Aux, IE_In, NIE_In), 
	(
	    (   % There are changes. We need to iterate again.
		(
		    Changes == 'true' % There was redundant vars.
		;
		    E_In \== E_Aux % There was redundant eqs.
		), !,
		remove_from_E_redundant_eqs_and_vars(Formula_Aux, GoalVars, Formula_Out)	 
	    )
	;
	    (   % If there are no changes then we have a fixed point.
		Changes \== 'true', % No redundant vars.
		E_In == E_Aux, !, % No redundant eqs.
		Formula_Out = Formula_Aux
	    )
	).

remove_from_E_redundant_eqs([], Visited, Visited, []) :- !. % Empty E
remove_from_E_redundant_eqs(E_In, Visited_In, Visited_Out, E_Out) :- 
	goal_is_conjunction(E_In, E_In_Left, E_In_Right), !,
	remove_from_E_redundant_eqs(E_In_Left, Visited_In, Visited_Aux, E_Out_Left),
	remove_from_E_redundant_eqs(E_In_Right, Visited_Aux, Visited_Out, E_Out_Right),
	(
	    ( E_Out_Left == [], E_Out_Right == [], !, E_Out = [] )
	;
	    ( E_Out_Left == [], !, E_Out = E_Out_Right )
	;
	    ( E_Out_Right == [], !, E_Out = E_Out_Left )
	;
	    ( E_Out = (E_Out_Left, E_Out_Right) )
	).

remove_from_E_redundant_eqs(E_In, Visited_In, Visited_In, []) :- 
	goal_is_equality(E_In, _Value_1, _Value_2, _GV, _EQV, _UQV),
	memberchk(E_In, Visited_In), !, % Eq has been seen before. Redundant.
	echo_msg(2, '', 'cneg_rt', 'remove_from_E_redundant_eqs :: redundant (seen before)', E_In).

remove_from_E_redundant_eqs(E_In, Visited_In, Visited_In, []) :- 
	goal_is_equality(E_In, Value_1, Value_2, GV, EQV, UQV),
	goal_is_equality(E_Tmp, Value_2, Value_1, GV, EQV, UQV), % Args interchanged.
	memberchk(E_Tmp, Visited_In), !, % Eq has been seen before. Redundant.
	echo_msg(2, '', 'cneg_rt', 'remove_from_E_redundant_eqs :: redundant (seen before)', E_In).

remove_from_E_redundant_eqs(E_In, Visited_In, Visited_In, []) :- 
	goal_is_equality(E_In, Value_1, Value_2, _GV, _EQV, _UQV),
	Value_1 == Value_2, !, % Equality between same terms.
	echo_msg(2, '', 'cneg_rt', 'remove_from_E_redundant_eqs :: redundant (eq between identical terms)', E_In).

remove_from_E_redundant_eqs(E_In, Visited_In, [ E_In | Visited_In ], E_In) :- 
	goal_is_equality(E_In, _Value_1, _Value_2, _GV, _EQV, _UQV).

remove_from_E_redundant_vars([], _GoalVars, Changes, Changes) :- !. % Empty E
remove_from_E_redundant_vars(E_In, GoalVars, Changes_In, Changes_Out) :- 
	goal_is_conjunction(E_In, E_In_Left, E_In_Right), !,
	remove_from_E_redundant_vars(E_In_Left, GoalVars, Changes_In, Changes_Aux),
	remove_from_E_redundant_vars(E_In_Right, GoalVars, Changes_Aux, Changes_Out),
	!.

remove_from_E_redundant_vars(E_In, GoalVars, Changes_In, Changes_Out) :- 
	goal_is_equality(E_In, Value_1, Value_2, _GV, _EQV, _UQV),
	remove_from_E_redundant_vars_aux(Value_1, Value_2, GoalVars, Changes_In, Changes_Out).

remove_from_E_redundant_vars_aux(Value_1, Value_2, _GoalVars, Changes_In, Changes_In) :-
	var(Value_1), 
	var(Value_2),
	Value_1 == Value_2, % Same var. This can be a redundant eq.
	!.

remove_from_E_redundant_vars_aux(Value_1, Value_2, GoalVars, _Changes_In, 'true') :-
	(   var(Value_1)  ;  var(Value_2)   ), % One of them needs to be a variable.
	varsbag(GoalVars, [], [], Real_GoalVars), % Sometimes we have non vars in GoalVars.
	varsbag((Value_1, Value_2), Real_GoalVars, [], Non_GoalVars),
	(
	    (   % Value_1 is a var in Non_GoalVars
		var(Value_1),
		memberchk(Value_1, Non_GoalVars), !,
		echo_msg(2, '', 'cneg_rt', 'remove_from_E_redundant_vars :: redundant (1st var not in GoalVars)', (Value_1, Value_2)),
		echo_msg(2, '', 'cneg_rt', 'remove_from_E_redundant_vars :: GoalVars', GoalVars),
		Value_1 = Value_2
	    )
	;
	    (   % Value_2 is a var in Non_GoalVars
		var(Value_2),
		memberchk(Value_2, Non_GoalVars), !,
		echo_msg(2, '', 'cneg_rt', 'remove_from_E_redundant_vars :: redundant (2nd var not in GoalVars)', (Value_1, Value_2)),
		echo_msg(2, '', 'cneg_rt', 'remove_from_E_redundant_vars :: GoalVars', GoalVars),
		Value_2 = Value_1
	    )
	).

remove_from_E_redundant_vars_aux(Value_1, Value_2, _GoalVars, Changes_In, Changes_In) :-
	(   var(Value_1) ; var(Value_2)   ),
	!. 

remove_from_E_redundant_vars_aux(Value_1, Value_2, GoalVars, Changes_In, Changes_Out) :-
	functor_local(Value_1, Name, Arity, Args1),
	functor_local(Value_2, Name, Arity, Args2), !,
	echo_msg(2, '', 'cneg_rt', 'remove_from_E_redundant_vars :: eq between functors :: erroneous frontier :: terms', (Value_1, Value_2)),
	echo_msg(2, '', 'cneg_rt', 'remove_from_E_redundant_vars', 'ERROR computing the frontier.'),
	remove_from_E_redundant_vars_aux_list(Args1, Args2, GoalVars, Changes_In, Changes_Out).

remove_from_E_redundant_vars_aux(Value_1, Value_2, _GoalVars, Changes_In, Changes_In) :-
	functor(Value_1, Name1, Arity1),
	functor(Value_2, Name2, Arity2), 
	(
	    ( Name1 \== Name2) ;
	    ( Arity1 \== Arity2)
	),
	echo_msg(2, '', 'cneg_rt', 'remove_from_E_redundant_vars :: eq between different terms :: failed frontier :: terms', (Value_1, Value_2)).

remove_from_E_redundant_vars_aux_list([], [], _GoalVars, Changes_In, Changes_In) :- !.
remove_from_E_redundant_vars_aux_list([Arg1|Args1], [Arg2|Args2], GoalVars, Changes_In, Changes_Out) :-
	remove_from_E_redundant_vars_aux(Arg1, Arg2, GoalVars, Changes_In, Changes_Aux), 
	remove_from_E_redundant_vars_aux_list(Args1, Args2, GoalVars, Changes_Aux, Changes_Out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% remove_from_IE_irrelevant_disequalities(Formula_In, Vars_Info, Formula_Out) :-
% returns IE_Out that is IE_In but without disequalities 
% whose variables are not in ImpVars nor in RelVars (So they are in Dumb_Vars).
% In practice we unify each var in Dumb_Vars to avoid a result after its negation.

remove_from_IE_irrelevant_disequalities(Formula_In, GoalVars_In, Formula_Out) :-
	subfrontier_E_IE_NIE_contents(Formula_In, E_In, IE_In, NIE_In),

	varsbag(GoalVars_In, [], [], GoalVars), % Sometimes we have non vars in GoalVars.
	varsbag(NIE_In, GoalVars, [], RelVars), % RelVars = vars(NIE) - GoalVars
	varsbag(E_In, GoalVars, [], Vars_E), % Vars equality part
	varsbag_union(GoalVars, RelVars, Valid_Vars_Tmp), % GoalVars + RelVars
	varsbag_union(Valid_Vars_Tmp, Vars_E, Valid_Vars), % GoalVars + RelVars + vars(E)
	varsbag(IE_In, Valid_Vars, [], Irrelevant_Vars), % vars(IE) - (GoalVars + RelVars + vars(E)).

	remove_from_IE_irrelevant_disequalities_aux(IE_In, Irrelevant_Vars, IE_Out),
	subfrontier_E_IE_NIE_contents(Formula_Out, E_In, IE_Out, NIE_In). 

remove_from_IE_irrelevant_disequalities_aux(IE, [], IE) :- !. % Optimization.
remove_from_IE_irrelevant_disequalities_aux([], _Irrelevant_Vars, []) :- !.
remove_from_IE_irrelevant_disequalities_aux(IE_In, Irrelevant_Vars, IE_Out) :-
	goal_is_conjunction(IE_In, IE_In_1, IE_In_2), !,
	remove_from_IE_irrelevant_disequalities_aux(IE_In_1, Irrelevant_Vars, IE_Out_1),
	remove_from_IE_irrelevant_disequalities_aux(IE_In_2, Irrelevant_Vars, IE_Out_2),
	rebuild_conjunction_of_goals(IE_Out_1, IE_Out_2, IE_Out).

remove_from_IE_irrelevant_disequalities_aux(IE_In, Irrelevant_Vars, IE_Out):-
	goal_is_disequality(IE_In, _Term1, _Term2, _GV_IE_In, _EQV_IE_In, _UQV_IE_In), !,

	varsbag(IE_In, [], [], Vars_IE_In),
	varsbag_intersection(Vars_IE_In, Irrelevant_Vars, Intersection),
	(
	    (   % No intersection with irrelevant vars. We keep it.
		Intersection == [], !,
		IE_Out = IE_In 
	    )
	;
	    (   % There is intersection with irrelevant vars. We remove it.
		Intersection \== [], !,
		IE_Out = [] 
	    )
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% negate_formula(Frontier, Proposal, GoalVars, UQV, ImpVars, ExpVars, UQ_to_EQ_Vars, Dumb_Vars, Result)
% returns Result that is the result from negating the frontier.
negate_formula(Frontier, _GoalVars, fail) :- 
	% frontier_E_IE_NIE_ied_contents(frontier, E, IE_Imp, IE_Exp, IE_Dumb, NIE_Imp, NIE_Exp, NIE_Dumb).
	subfrontier_E_IE_NIE_ie_contents(Frontier, [], [], [], [], []), % Equivalent to true, nothing to negate.
	!. % Optimization

negate_formula(Frontier, GoalVars, Neg_E_IE_NIE) :-
	% frontier_E_IE_NIE_ied_contents(frontier, E, IE_Imp, IE_Exp, IE_Dumb, NIE_Imp, NIE_Exp, NIE_Dumb).
	subfrontier_E_IE_NIE_ie_contents(Frontier, E, IE_Imp, IE_Exp, NIE_Imp, NIE_Exp),

	rebuild_conjunction_of_goals(IE_Exp, NIE_Exp, IE_NIE_Exp), 
	rebuild_conjunction_of_goals(E, IE_Imp, E_IE_Imp),
	rebuild_conjunction_of_goals(E_IE_Imp, NIE_Imp, E_IE_NIE_Imp),

	echo_msg(2, '', 'cneg_rt', 'negate_formula :: E_IE_NIE_Imp', E_IE_NIE_Imp),
	echo_msg(2, '', 'cneg_rt', 'negate_formula :: IE_NIE_Exp', IE_NIE_Exp),
 	negate_IE_NIE_exp(IE_NIE_Exp, GoalVars, Neg_IE_NIE_Exp),
	negate_imp_form(E_IE_NIE_Imp, GoalVars, Neg_IE_NIE_Exp, Neg_E_IE_NIE),
	!. % Backtracking forbidden.

% negate_Dexp_Rexp(DRexp,ImpVars,ExpVars,SolC) obtain in
% SolC a solution of negating Dexp y Rexp juntos.
negate_IE_NIE_exp([], _GoalVars, []):- !.
negate_IE_NIE_exp(IE_NIE_exp, GoalVars_In, Neg_IE_NIE_exp) :-
	IE_NIE_exp \== [],
	varsbag(GoalVars_In, [], [], GoalVars),
	varsbag(IE_NIE_exp, GoalVars, [], UQV),
	functor_local(Neg_IE_NIE_exp, 'cneg_rt', 4, [ UQV |[ GoalVars |[ IE_NIE_exp |[ 'cneg_rt_Chan' ]]]]), !.

negate_imp_form([], _GoalVars, [], []) :- !. % Optimization.
negate_imp_form(Formula, _GoalVars, _Next_Formula, _Neg_Formula) :-
	goal_is_disjunction(Formula, _Formula_1, _Formula_2), !,
	echo_msg(1, '', 'cneg_rt', 'ERROR: negate_imp_form can not deal with disjunctions. Formula', Formula),
	fail.

negate_imp_form(Formula, GoalVars, Next_Formula, Neg_Formula) :-
	goal_is_conjunction(Formula, Formula_1, Formula_2), !,
 	negate_imp_form(Formula_2, GoalVars, Next_Formula, Next_Formula_Aux),
 	negate_imp_form(Formula_1, GoalVars, Next_Formula_Aux, Neg_Formula).

negate_imp_form(Formula, GoalVars, Next_Formula, Neg_Formula) :-
	negate_imp_atom(Formula, GoalVars, Neg_Atom, Keep_Atom),
	combine_negated(Neg_Atom, Keep_Atom, Next_Formula, Neg_Formula), 
	!.

combine_negated([], [], Neg_Formula, Neg_Formula) :- !.
combine_negated(Neg_Formula, _Keep_Formula, [], Neg_Formula) :- !.
combine_negated(Neg_Formula_1, Keep_Formula_1, Neg_Formula_2, Neg_Formula) :- 
	Neg_Formula_1 \== [],
	Keep_Formula_1 \== [],
	Neg_Formula_2 \== [],
	!,
	Neg_Formula = (Neg_Formula_1 ; (Keep_Formula_1, Neg_Formula_2)).

% negate_I(I,ImpVars,Sol) obtains in Sol a solution of negating I
negate_imp_atom([], _GoalVars, [], []) :- !. % Obvious.
negate_imp_atom(true, _GoalVars, fail, true):- !. % Trivial predicates
negate_imp_atom(fail, _GoalVars, true, fail):- !. % Trivial predicates

% Negation of equalities. We need to take care of converting UQV to EQV and vice-versa.
% Since EQV are not used in equality basic predicates, we assume that EQV variables
% are the result from applying double negation to a disequality. 
negate_imp_atom(Formula, GoalVars, Neg_Atom, Keep_Atom) :-
	goal_is_equality(Formula, T1, T2, _Eq_GV_In, Eq_EQV_In, Eq_UQV_In), !,

	varsbag((T1, T2), [], [], Vars_Eq), % Just variables
	varsbag_clean_up(Eq_EQV_In, Eq_EQV), % Just variables
	varsbag_clean_up(Eq_UQV_In, Eq_UQV), % Just variables
	varsbag_difference(Vars_Eq, GoalVars, Diseq_UQV_Tmp), % GoalVars are not free vars.
	varsbag_difference(Diseq_UQV_Tmp, Eq_UQV, Diseq_UQV), % Previous UQV are not free vars.

	(   (   Eq_UQV = [], !   )
	;
	    (
		echo_msg(1, '', 'cneg_rt', 'WARNING: Chans proposal can not deal with the equality below, but ours can.', ''),
		echo_msg(1, '', 'cneg_rt', 'WARNING: eq_geuqv(T1, T2, GoalVars, EQV, UQV)', Formula)
	    )
	),

	Neg_Atom = (diseq_geuqv(T1, T2, GoalVars, Eq_UQV, Diseq_UQV)),
	Keep_Atom = (eq_geuqv(T1, T2, GoalVars, Eq_EQV, Eq_UQV)),

	(   (   Diseq_UQV = [], !   )
	;
	    (
		echo_msg(1, '', 'cneg_rt', 'WARNING: Chans proposal can not deal with the disequality below, but ours can.', ''),
		echo_msg(1, '', 'cneg_rt', 'WARNING: diseq_geuqv(T1, T2, GoalVars, EQV, UQV)', Neg_Atom)
	    )
	).


% Idem for disequalities.
negate_imp_atom(Formula, GoalVars, Neg_Atom, Keep_Atom) :-
	goal_is_disequality(Formula, T1_In, T2_In, _Diseq_GV_In, Diseq_EQV_In, Diseq_UQV_In), !,

	varsbag((T1_In, T2_In), [], [], Vars_Diseq), % Just variables
	varsbag_clean_up(Diseq_EQV_In, Diseq_EQV_Aux), % Just variables
	varsbag_clean_up(Diseq_UQV_In, Diseq_UQV_Aux), % Just variables
	varsbag_difference(Vars_Diseq, GoalVars, Eq_UQV_Tmp), % GoalVars are not free vars.
	varsbag_difference(Eq_UQV_Tmp, Diseq_UQV_Aux, Eq_UQV_Aux), % Previous UQV are not free vars.

	(   (   Diseq_UQV_Aux = [], !   )
	;
	    (
		echo_msg(1, '', 'cneg_rt', 'WARNING: Chans proposal can not deal with the disequality', ''),
		echo_msg(1, '', 'cneg_rt', 'WARNING: diseq_geuqv(T1, T2, GoalVars, EQV, UQV)', Formula),
		echo_msg(1, '', 'cneg_rt', 'WARNING: but the current implementation can.', '')
	    )
	),

	% Need to replace UQV by new fresh variables.
	% This is our real improvement to Chan's version.
	copy_term(Eq_UQV_Aux, Eq_UQV),
	replace_in_term_vars_by_values(T1_In, Eq_UQV_Aux, Eq_UQV, T1),
	replace_in_term_vars_by_values(T2_In, Eq_UQV_Aux, Eq_UQV, T2),
	replace_in_term_vars_by_values(Diseq_EQV_Aux, Eq_UQV_Aux, Eq_UQV, Diseq_EQV),
	replace_in_term_vars_by_values(Diseq_UQV_Aux, Eq_UQV_Aux, Eq_UQV, Diseq_UQV),
	!, % Clean up backtracking stack.

	Neg_Atom = (eq_geuqv(T1,T2, GoalVars, Diseq_UQV, Eq_UQV)),
	Keep_Atom = (diseq_geuqv(T1,T2, GoalVars, Diseq_EQV, Diseq_UQV)),

	(   (   Eq_UQV = [], !   )
	;
	    (
		echo_msg(1, '', 'cneg_rt', 'WARNING: Chans proposal can not deal with the resultant equality', ''),
		echo_msg(1, '', 'cneg_rt', 'WARNING: eq_geuqv(T1, T2, GoalVars, EQV, UQV)', Neg_Atom),
		echo_msg(1, '', 'cneg_rt', 'WARNING: but the current implementation can.', '')
	    )
	).


negate_imp_atom(Formula, GoalVars_In, Neg_Atom, Keep_Atom) :-
	varsbag(GoalVars_In, [], [], GoalVars),
	varsbag(Formula, GoalVars, [], UQV),
	functor_local(Neg_Atom, 'cneg_rt', 4, [ UQV |[ GoalVars |[ Formula |[ 'cneg_rt_Chan' ]]]]),
	Keep_Atom = (Formula). 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Structures to manage all the info about the subfrontier in an easy way.
subfrontier_contents(subfrontier(Goal, Head, Body, FrontierTest), Goal, Head, Body, FrontierTest).
subfrontier_E_IE_NIE_contents(subfrontier_E_IE_NIE(E, IE, NIE), E, IE, NIE).
subfrontier_E_IE_NIE_ie_contents(subfrontier_E_IE_NIE_ie(E, IE_Imp, IE_Exp, NIE_Imp, NIE_Exp), E, IE_Imp, IE_Exp, NIE_Imp, NIE_Exp).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% compute_set_of_frontiers(Goal, Real_GoalVars, Proposal, Frontier)
compute_frontier(UQV_In, GoalVars_In, Goal, Proposal, Frontier) :-
	varsbag(GoalVars_In, [], [], GoalVars), % Clean up non-vars in GoalVars.
	varsbag_clean_up(UQV_In, UQV), % Clean up non-vars in UQV (subterms are not in UQV).
	echo_msg(2, '', 'cneg_rt', 'compute_set_of_frontiers :: (UQV, GoalVars, Goal, Proposal)', (UQV, GoalVars, Goal, Proposal)),
	split_goal_with_disjunctions_into_goals(Goal, Proposal, Goals),
	!,
	echo_msg(2, 'list', 'cneg_rt', 'compute_set_of_frontiers :: Goals', Goals),
	compute_frontier_aux(Goals, UQV, GoalVars, Proposal, [], Frontier),
	!.

compute_frontier_aux([], _UQV, _GoalVars, _Proposal, Frontier_Out, Frontier_Out) :- !.
compute_frontier_aux([Goal | More_Goals], UQV, GoalVars, Proposal, Frontier_In, Frontier_Out) :-
	compute_goal_frontier(Goal, Proposal, Goal_Frontier), !,
	adequate_frontier(Goal_Frontier, UQV, GoalVars, Proposal, [], Frontier_Tmp), !,
	append(Frontier_In, Frontier_Tmp, Frontier_Aux), !,
	compute_frontier_aux(More_Goals, UQV, GoalVars, Proposal, Frontier_Aux, Frontier_Out).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

adequate_frontier(Goal_Frontier, UQV, GoalVars, Proposal, Frontier_In, Frontier_Out) :-
	echo_msg(2, 'list', 'cneg_rt', 'adequate_frontier :: Frontier_In', Frontier_In),
	adequate_frontier_aux(Goal_Frontier, UQV, GoalVars, Proposal, Frontier_In, Frontier_Out), !,
	echo_msg(2, 'list', 'cneg_rt', 'adequate_frontier :: Frontier_Out', Frontier_Out).

adequate_frontier(Goal_Frontier, UQV, GoalVars, _Proposal, Frontier_In, _Frontier_Out) :-
	echo_msg(1, 'nl', 'cneg_rt', '', ''),
	echo_msg(1, '', 'cneg_rt', 'ERROR: adequate_frontier :: (UQV, GoalVars)', (UQV, GoalVars)),
	echo_msg(1, 'nl', 'cneg_rt', '', ''),
	echo_msg(1, 'list', 'cneg_rt', 'ERROR: adequate_frontier :: Goal_Frontier', Goal_Frontier),
	echo_msg(1, 'nl', 'cneg_rt', '', ''),
	echo_msg(1, 'list', 'cneg_rt', 'ERROR: adequate_frontier :: Frontier_In', Frontier_In),
	echo_msg(1, 'nl', 'cneg_rt', '', ''),
	!, fail.

adequate_frontier_aux([], _UQV, _GoalVars, _Proposal, Frontier_N_Out, Frontier_N_Out) :- !.
adequate_frontier_aux([G_F_Pre_Node | G_F_Pre_Nodes], UQV, GoalVars, Proposal, Frontier_N_In, Frontier_N_Out) :-
	convert_frontier_prenode_to_nodes(G_F_Pre_Node, UQV, GoalVars, Proposal, [], Frontier_Nodes), !,
	echo_msg(2, '', 'cneg_rt', 'adequate_frontier_aux :: Frontier_Nodes', Frontier_Nodes),
	append(Frontier_N_In, Frontier_Nodes, Frontier_N_Aux), !,
	adequate_frontier_aux(G_F_Pre_Nodes, UQV, GoalVars, Proposal, Frontier_N_Aux, Frontier_N_Out).

convert_frontier_prenode_to_nodes(G_F_Pre_Node, UQV, GoalVars, _Proposal, Frontier_N_In, Frontier_N_Out) :-
	echo_msg(2, '', 'cneg_rt', 'convert_frontier_prenode_to_nodes :: G_F_Pre_Node', G_F_Pre_Node),
%	subfrontier_contents(Frontier, Goal, Head, Body, FrontierTest).
	subfrontier_contents(G_F_Pre_Node, Real_Goal, Head, Body, _F_Test),
	split_body_between_E_IE_and_NIE(Body, [], E_IE_Body, [], NIE_Body),
	echo_msg(2, '', 'cneg_rt', 'convert_frontier_prenode_to_nodes :: Body', Body),
	echo_msg(2, '', 'cneg_rt', 'convert_frontier_prenode_to_nodes :: E_IE_Body', E_IE_Body),
	echo_msg(2, '', 'cneg_rt', 'convert_frontier_prenode_to_nodes :: NIE_Body', NIE_Body),

	% Link the query and the pre-node (only goalvars, not uqv).
	copy_term((Real_Goal, UQV, GoalVars), (Real_Goal_Copy, _UQV_Copy, GoalVars_Copy)),
	GoalVars_Copy = GoalVars, % Unify Goalvars_Copy, but not UQV_Copy.
	Real_Goal_Copy = Head, % Unify head and goal, but not UQV.
	!, % Backtracking is forbidden.

	% Proposal == 'cneg_rt_Chan',
	varsbag((Head, Body), GoalVars, [], Pre_Node_UQV), % Identify UQV in Pre_Node
	eval_frontier_prenode_to_get_nodes(E_IE_Body, NIE_Body, Pre_Node_UQV, GoalVars, Frontier_Nodes),
	echo_msg(2, '', 'cneg_rt', 'convert_frontier_prenode_to_nodes :: Frontier_Nodes', Frontier_Nodes),
	append(Frontier_N_In, Frontier_Nodes, Frontier_N_Out). % Is order really relevant ???

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

eval_frontier_prenode_to_get_nodes([], [], _UQV, _GoalVars, []) :- !.
eval_frontier_prenode_to_get_nodes([], NIE_Body, _UQV, _GoalVars, Frontier_Nodes) :- !,
	generate_conjunction_from_list(NIE_Body, Frontier_Nodes).
eval_frontier_prenode_to_get_nodes(E_IE_Body, NIE_Body, UQV, GoalVars, Frontier_Nodes) :-
	generate_conjunction_from_list(E_IE_Body, Conj_E_IE_Body),
	echo_msg(2, '', 'cneg_rt', 'eval_frontier_prenode_to_get_nodes :: Conj_E_IE_Body', Conj_E_IE_Body),
	eval_frontier_prenode_to_get_nodes_aux(UQV, GoalVars, NIE_Body, Conj_E_IE_Body, Pre_Node_Answers),
	
	get_eqs_and_diseqs_from_answers(Pre_Node_Answers, UQV, GoalVars, [], Frontier_Nodes), !,
	echo_msg(2, 'list', 'cneg_rt', 'eval_frontier_prenode_to_get_nodes :: Frontier_Nodes', Frontier_Nodes).

%:- push_prolog_flag(unused_pred_warnings, no).
%:- meta_predicate eval_frontier_prenode_to_get_nodes_aux(?, ?, ?, goal, ?).
eval_frontier_prenode_to_get_nodes_aux(UQV, GoalVars, NIE_Body, Conj_E_IE_Body, Pre_Node_Answers) :-
	echo_msg(2, '', 'cneg_rt', 'eval_frontier_prenode_to_get_nodes :: setof :: (UQV, GoalVars)', (UQV, GoalVars)),
	echo_msg(2, '', 'cneg_rt', 'eval_frontier_prenode_to_get_nodes', 'setof((UQV, GoalVars, NIE_Body), Conj_E_IE_Body, [(UQV, GoalVars, NIE_Body)])'),
	setof((UQV, GoalVars, NIE_Body), Conj_E_IE_Body, Pre_Node_Answers), !,
	cneg_diseq_echo(2, 'list', 'cneg_rt', 'setof :: [(UQV, GoalVars, NIE_Body)]', Pre_Node_Answers),
	echo_msg(2, '', 'cneg_rt', 'eval_frontier_prenode_to_get_nodes :: setof :: (UQV, GoalVars)', (UQV, GoalVars)),
	!.

eval_frontier_prenode_to_get_nodes_aux(UQV, GoalVars, NIE_Body, Conj_E_IE_Body, _Pre_Node_Answers) :-
	call(Conj_E_IE_Body), !, % If this succeeds there is an error.
	echo_msg(1, 'nl', 'cneg_rt', '', ''),
	echo_msg(1, '', 'cneg_rt', 'ERROR in eval_frontier_prenode_to_get_nodes_aux', 'setof/3 has failed'),
	echo_msg(1, 'nl', 'cneg_rt', '', ''),
	echo_msg(1, '', 'cneg_rt', 'eval_frontier_prenode_to_get_nodes', 'setof((UQV, GoalVars, NIE_Body), Conj_E_IE_Body, Pre_Node_Answers)'),
	echo_msg(1, '', 'cneg_rt', 'eval_frontier_prenode_to_get_nodes :: setof :: UQV', UQV),
	echo_msg(1, '', 'cneg_rt', 'eval_frontier_prenode_to_get_nodes :: setof :: GoalVars', GoalVars),
	echo_msg(1, '', 'cneg_rt', 'eval_frontier_prenode_to_get_nodes :: setof :: NIE_Body', NIE_Body),
	echo_msg(1, '', 'cneg_rt', 'eval_frontier_prenode_to_get_nodes :: setof :: Conj_E_IE_Body', Conj_E_IE_Body),
 	echo_msg(1, 'nl', 'cneg_rt', '', ''),
	!, fail.

eval_frontier_prenode_to_get_nodes_aux(_UQV, _GoalVars, _NIE_Body, Conj_E_IE_Body, []) :-
	echo_msg(2, '', 'cneg_rt', 'eval_frontier_prenode_to_get_nodes :: not satisfiable :: Conj_E_IE_Body', Conj_E_IE_Body), !.
%:- pop_prolog_flag(unused_pred_warnings).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_eqs_and_diseqs_from_answers([], _UQV, _GoalVars, Frontier_Nodes, Frontier_Nodes).
get_eqs_and_diseqs_from_answers([(Answer_UQV, Answer_GoalVars, NIE_Body) | Answers], UQV, GoalVars, FN_In, FN_Out) :- !,
	get_eqs_and_diseqs_from_one_answer(Answer_UQV, Answer_GoalVars, NIE_Body, UQV, GoalVars, Frontier_Node_Body), !,
	get_eqs_and_diseqs_from_answers(Answers, UQV, GoalVars, [Frontier_Node_Body | FN_In], FN_Out).

get_eqs_and_diseqs_from_answers(Answers, UQV, GoalVars, FN_In, _FN_Out) :- !,
	echo_msg(1, '', 'cneg_rt', 'ERROR in get_eqs_and_diseqs_from_answers. Answers', Answers),
	echo_msg(1, '', 'cneg_rt', 'ERROR in get_eqs_and_diseqs_from_answers. UQV', UQV),
	echo_msg(1, '', 'cneg_rt', 'ERROR in get_eqs_and_diseqs_from_answers. GoalVars', GoalVars),
	echo_msg(1, '', 'cneg_rt', 'ERROR in get_eqs_and_diseqs_from_answers. FN_In', FN_In),
	!, fail.

get_eqs_and_diseqs_from_one_answer([], [], [], _UQV, _GoalVars, true) :- !.
get_eqs_and_diseqs_from_one_answer(Answer_UQV, Answer_GoalVars, NIE_Body, UQV, GoalVars, Frontier_Node_Body) :-

	cneg_diseq_echo(2, '', 'cneg_rt', 'get_eqs_and_diseqs_from_one_answer :: (Answer_UQV, Answer_GoalVars)', (Answer_UQV, Answer_GoalVars)),
	cneg_diseq_echo(2, '', 'cneg_rt', 'get_eqs_and_diseqs_from_one_answer :: (UQV, GoalVars)', (UQV, GoalVars)),

	varsbag((Answer_UQV, Answer_GoalVars, NIE_Body), [], [], Answer_Vars),
	cneg_diseq_echo(2, '', 'cneg_rt', 'get_eqs_and_diseqs_from_one_answer :: Answer_Vars', Answer_Vars),
	% There is a bug here: a variable can have attributes coming from a higher level, 
	% and this ones are NOT part of the current frontier.
	get_disequalities_from_constraints_and_remove_them(Answer_Vars, Disequalities),
	cneg_diseq_echo(2, '', 'cneg_rt', 'get_eqs_and_diseqs_from_one_answer :: Disequalities', Disequalities),

	% Ojo q al cambiar las UQV hemos de modificar las viejas por las nuevas en las desigualdades ... !!!
	copy_term(UQV, Fresh_UQV),
	get_equalities_list_from_lists(Fresh_UQV, Answer_UQV, Disequalities, Eqs_and_Diseqs_Tmp), 
	get_equalities_list_from_lists(GoalVars, Answer_GoalVars, Eqs_and_Diseqs_Tmp, Eqs_and_Diseqs), 
	cneg_diseq_echo(2, '', 'cneg_rt', 'get_eqs_and_diseqs_from_one_answer :: Eqs_and_Diseqs', Eqs_and_Diseqs),

	append(Eqs_and_Diseqs, NIE_Body, Frontier_Node_Body_List),
	cneg_diseq_echo(2, '', 'cneg_rt', 'get_eqs_and_diseqs_from_one_answer :: Frontier_Node_Body_List', Frontier_Node_Body_List),
	generate_conjunction_from_list(Frontier_Node_Body_List, Frontier_Node_Body),
	!.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	 

get_equalities_list_from_lists([], [], List_In, List_In) :- !.
get_equalities_list_from_lists([Elto_1], [Elto_2], List_In, [(Elto_1 = Elto_2) | List_In]) :- !.
get_equalities_list_from_lists([Elto_1 | List_1], [Elto_2 | List_2], List_In, List_Out) :- !,
	get_equalities_list_from_lists(List_1, List_2, [(Elto_1 = Elto_2) | List_In], List_Out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	 

split_body_between_E_IE_and_NIE(Body, E_IE_Body_In, E_IE_Body_Out, NIE_Body_In, NIE_Body_Out) :-
	goal_is_conjunction(Body, Body_1, Body_2),
	% The order is reversed to keep the order of the literals.
	split_body_between_E_IE_and_NIE(Body_2, E_IE_Body_In, E_IE_Body_Aux, NIE_Body_In, NIE_Body_Aux),
	split_body_between_E_IE_and_NIE(Body_1, E_IE_Body_Aux, E_IE_Body_Out, NIE_Body_Aux, NIE_Body_Out).

split_body_between_E_IE_and_NIE(Body, E_IE_Body_In, [Body | E_IE_Body_In], NIE_Body_In, NIE_Body_In) :-
	goal_is_equality(Body, _Arg1, _Arg2, _GV, _EQV, _UQV), !.
split_body_between_E_IE_and_NIE(Body, E_IE_Body_In, [Body | E_IE_Body_In], NIE_Body_In, NIE_Body_In) :-
	goal_is_disequality(Body, _Arg1, _Arg2, _GV, _EQV, _UQV), !.
split_body_between_E_IE_and_NIE(Body, E_IE_Body_In, E_IE_Body_In, NIE_Body_In, [Body | NIE_Body_In]) :- !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Just to debug.
%compute_goal_frontier(Goal, _Proposal, _Frontier) :-
%	echo_msg(2, '', 'cneg_rt', '--------------------------------------------------------------------------------------------------------------', ' '),
%	echo_msg(2, '', 'cneg_rt', 'compute_goal_frontier :: (Goal)', (Goal)),	
%	fail. % Just debug and use backtracking to continue.

% First remove $ and qualification from the goal's name.
compute_goal_frontier(Goal, Proposal, Frontier) :-
	goal_clean_up(Goal, Tmp_Goal), !,
	compute_goal_frontier(Tmp_Goal, Proposal, Frontier).

% Manage true and fail ...
compute_goal_frontier('true', _Proposal, [F_Out]) :- !,
%	subfrontier_contents(Frontier, Goal, Head, Body, FrontierTest).
	subfrontier_contents(F_Out, 'true', 'true', 'true', 'true').
compute_goal_frontier('fail', _Proposal, [F_Out]) :- !,
%	subfrontier_contents(Frontier, Goal, Head, Body, FrontierTest).
	subfrontier_contents(F_Out, 'fail', 'fail', 'fail', 'fail').

% Now go for the disjunctions.
% The frontiers need to evaluated one at a time. 
compute_goal_frontier(Goal, _Proposal, _Frontier_Out):- 
	goal_is_disjunction(Goal, _G1, _G2), !,
	echo_msg(1, '', 'cneg_rt', 'ERROR: Not possible computing the frontier for a disjunction', Goal), 
	echo_msg(1, 'nl', 'cneg_rt', '', ''), !, % Backtracking is forbidden.
	fail.

% Now go for the conjunctions.
compute_goal_frontier(Goal, Proposal, Frontier):- 
	goal_is_conjunction(Goal, G1, G2), !,
	compute_goal_frontier(G1, Proposal, Frontier_G1),
	compute_goal_frontier(G2, Proposal, Frontier_G2),
	!,
	combine_frontiers_from_conjunction(Frontier_G1, Frontier_G2, Frontier),
	!.

% Now go for the functors for equality and disequality.
% None of them is managed yet, so just bypass them.
compute_goal_frontier(Goal, _Proposal, [F_Out]) :- 
	goal_is_disequality(Goal, T1, T2, GV, EQV, UQV), !,
	functor_local(Real_Goal, 'diseq_geuqv', 5, [ T1 |[ T2 |[ GV |[ EQV |[ UQV ]]]]]),
%	subfrontier_contents(Frontier, Goal, Head, Body, FrontierTest).
	subfrontier_contents(F_Out, Real_Goal, Real_Goal, Real_Goal, Real_Goal),
	!.

compute_goal_frontier(Goal, _Proposal, [F_Out]) :- 
	goal_is_equality(Goal, T1, T2, GV, EQV, UQV), !,
	functor_local(Real_Goal, 'eq_geuqv', 5, [ T1 |[ T2 |[ GV |[ EQV |[ UQV ]]]]]),
%	subfrontier_contents(Frontier, Goal, Head, Body, FrontierTest).
	subfrontier_contents(F_Out, Real_Goal, Real_Goal, Real_Goal, Real_Goal), 
	!.

% Double negation is not managed yet. Bypass it.
%compute_goal_frontier(Goal, Proposal, Real_Goal, [F_Out]) :- 
compute_goal_frontier(Goal, Proposal, Frontier) :- 
	goal_is_cneg_rt(Goal, UQV, GoalVars, SubGoal, _Unused_Negation_Proposal), !,
	echo_msg(2, '', 'cneg_rt', 'compute_goal_frontier :: dn :: double negation for (Proposal, UQV, GoalVars, SubGoal)', (Proposal, UQV, GoalVars, SubGoal)),
%     cneg_rt(UQV, GoalVars, Goal, Proposal, Depth_Level, Trace) :-
	cneg_rt(UQV, GoalVars, SubGoal, Proposal, 0, Conj_List_Result), !,
	generate_conjunction_from_list(Conj_List_Result, Conj_Of_Disjs_Frontier), !,
	echo_msg(2, 'list', 'cneg_rt', 'compute_goal_frontier :: dn :: Conj_Of_Disjs_Frontier', Conj_Of_Disjs_Frontier),
	split_goal_with_disjunctions_into_goals(Conj_Of_Disjs_Frontier, 'cneg_rt_gv', List_Of_Conjs_Frontier), !,
	echo_msg(2, 'list', 'cneg_rt', 'compute_goal_frontier :: dn :: List_Of_Conjs_Frontier', List_Of_Conjs_Frontier),
	build_a_frontier_from_each_result(Goal, List_Of_Conjs_Frontier, Frontier), !,
	echo_msg(2, 'list', 'cneg_rt', 'double neg :: Frontier', Frontier),
	echo_msg(2, 'separation', 'cneg_rt', '', ''),
	echo_msg(2, 'nl', 'cneg_rt', '', ''),
	!.

% Now go for other functors stored in our database.
compute_goal_frontier(Goal, _Proposal, Frontier_Out) :-
	goal_is_not_conj_disj_eq_diseq_dneg(Goal),
	echo_msg(2, '', 'cneg_rt', 'compute_goal_frontier :: Goal', Goal),
	look_for_the_relevant_clauses(Goal, Frontier_Tmp_1),
%	echo_msg(0, '', 'cneg_rt', 'compute_neg_frontier :: format', '(Head, Body, FrontierTest)'),
%	echo_msg(2, 'list', 'cneg_rt', 'Frontier_Tmp_1', Frontier_Tmp_1),
	simplify_frontier(Frontier_Tmp_1, Goal, [], Frontier_Out),
	echo_msg(2, 'list', 'cneg_rt', 'compute_goal_frontier :: Frontier_Out', Frontier_Out), 
	echo_msg(2, 'nl', 'cneg_rt', '', ''),
	!. % Backtracking is forbidden.

% And at last report an error if it was impossible to found a valid entry.
compute_goal_frontier(Goal, _Proposal, []) :-
	echo_msg(1, '', 'cneg_rt', 'ERROR: Not found frontier for Goal', Goal), 
	echo_msg(1, 'nl', 'cneg_rt', '', ''), !. % Backtracking is forbidden.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

generate_conjunction_from_list([], fail) :- !,
	echo_msg(1, '', 'cneg_rt', 'ERROR: generate_conjunction_from_list can not generate a frontier from an empty list.', ''), 
	echo_msg(1, 'nl', 'cneg_rt', '', ''), !, fail. % Backtracking is forbidden.
generate_conjunction_from_list([Goal], Goal) :- !.
generate_conjunction_from_list([Goal | Goals], (Goal , Disj_Goals)) :-
	Goals \== [],
	generate_conjunction_from_list(Goals, Disj_Goals).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

build_a_frontier_from_each_result(_Real_Goal, [], []) :- !.
build_a_frontier_from_each_result(Real_Goal, [Result | Results], [Frontier | Frontiers]) :-
%	subfrontier_contents(Frontier, Goal, Head, Body, FrontierTest).
	subfrontier_contents(Frontier, Real_Goal, Real_Goal, Result, Real_Goal),
	build_a_frontier_from_each_result(Real_Goal, Results, Frontiers).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

look_for_the_relevant_clauses(Goal, Frontier) :-
	functor(Goal, Name, Arity),  % Security
	Name \== ',', Name \== ';',    % Issues
	!, % Backtracking forbiden.
	cneg_pre_frontier(Name, Arity, _SourceFileName, _Head_Aux, _Body_Aux, _FrontierTest_Aux), 
%	debug_clause('look_for_the_relevant_clauses :: (Name, Arity, SourceFileName)', (Name, Arity, SourceFileName)),
	setof(subfrontier(_Real_Goal, Head, Body, FrontierTest), 
	cneg_pre_frontier(Name, Arity, _SourceFileName, Head, Body, FrontierTest), Frontier).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% simplify_frontier(Front,Frontier) simplifies the frontier Front.
% Since the frontiers retrieved are in an inverted order, 
% we must reorder them to keep procedural semantics unchanged.
simplify_frontier([], _Goal, Frontier_Acc, Frontier_Acc) :- !.
%	echo_msg(2, 'nl', 'cneg_rt', '', '').
simplify_frontier([F_In | Frontier_In], Goal, Frontier_Acc, Frontier_Out) :-
	test_frontier_is_valid(F_In, Goal), !,
%	echo_msg(2, '', 'cneg_rt', 'simplify_frontier :: valid :: F_In', F_In),
	simplify_frontier(Frontier_In, Goal, [F_In | Frontier_Acc], Frontier_Out).
simplify_frontier([_F_In | Frontier_In], Goal, Frontier_Acc, Frontier_Out) :-
%	echo_msg(2, '', 'cneg_rt', 'simplify_frontier :: not valid :: F_In', F_In),
	simplify_frontier(Frontier_In, Goal, Frontier_Acc, Frontier_Out).

% simplify_frontier_unifying_variables(H, Body_In, G, Body_Out) 
% returns in Body_Out the elements of Body whose head unifies with G.
test_frontier_is_valid(F_In, Goal) :-
%	subfrontier_contents(Frontier, Goal, Head, Body, FrontierTest).
	subfrontier_contents(F_In, Goal, _Head, _Body, F_Test),
	copy_term((Goal, F_Test), (Goal_Tmp, F_Test_Tmp)),
	F_Test_Tmp = Goal_Tmp, % Test that test and goal can be unified. 
	cneg_diseq_echo(2, '', 'cneg_rt', 'test_frontier_is_valid :: VALID :: Goal', Goal),
	cneg_diseq_echo(2, '', 'cneg_rt', 'test_frontier_is_valid :: VALID :: F_In', F_In),
	!. % Backtracking forbidden. 
test_frontier_is_valid(F_In, Goal) :-
	cneg_diseq_echo(2, '', 'cneg_rt', 'test_frontier_is_valid :: NOT VALID :: Goal', Goal),
	cneg_diseq_echo(2, '', 'cneg_rt', 'test_frontier_is_valid :: NOT VALID :: F_In', F_In),
	!, fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% combine_frontiers(F1,F2,F3) returns F3 that is the resulting frontier
% from combining the frontiers F1 and F2 in all possible ways.
combine_frontiers_from_conjunction([],_F2,[]) :- !, % Optimization
	echo_msg(2, 'nl', 'cneg_rt', '', ''),
	echo_msg(2, '', 'cneg_rt', 'combine_frontiers_from_conjunction', 'empty F1 -> empty F3'),
	echo_msg(2, 'nl', 'cneg_rt', '', '').
combine_frontiers_from_conjunction(_F1,[],[]) :- !, % Optimization
	echo_msg(2, 'nl', 'cneg_rt', '', ''),
	echo_msg(2, '', 'cneg_rt', 'combine_frontiers_from_conjunction', 'empty F2 -> empty F3'),
	echo_msg(2, 'nl', 'cneg_rt', '', '').
combine_frontiers_from_conjunction(F1,F2,F3) :-
	F1 \== [], F2 \== [],
	echo_msg(2, 'nl', 'cneg_rt', '', ''),
	echo_msg(2, '', 'cneg_rt', 'combine_frontiers_from_conjunction', 'F1 /\\ F2 -> F3 '),
	combine_frontiers_from_conjunction_aux_1(F1,F2,F3),
	echo_msg(2, '', 'cneg_rt', 'combine_frontiers_from_conjunction', 'F3'),
	echo_msg(2, 'list', 'cneg_rt', '', F3),
	echo_msg(2, 'nl', 'cneg_rt', '', '').
	
combine_frontiers_from_conjunction_aux_1([],_F2,[]) :- !.
combine_frontiers_from_conjunction_aux_1([F1_1 | More_F1], F2, F3):-
        combine_frontiers_from_conjunction_aux_2(F1_1, F2, F3_1),
        combine_frontiers_from_conjunction_aux_1(More_F1, F2, More_F3),
        cneg_aux:append(F3_1, More_F3, F3).

% combine_frontiers_aux_1(F1_1,F2, F3) returns F3 that is 
% the result of combining F1_1 with each element of F2.
combine_frontiers_from_conjunction_aux_2(_F1_1, [], []).
combine_frontiers_from_conjunction_aux_2(F1_1, [F2_1 | More_F2], [F3 | More_F3]) :-
%	subfrontier_contents(Frontier, Goal, Head, Body, FrontierTest).
	subfrontier_contents(F1_1, F1_1_Real_Goal, F1_1_Head, F1_1_Body, F1_1_F_Test),
	subfrontier_contents(F2_1, F2_1_Real_Goal, F2_1_Head, F2_1_Body, F2_1_F_Test),
	F3_Real_Goal = ((F1_1_Real_Goal), (F2_1_Real_Goal)),
	F3_Head = ((F1_1_Head), (F2_1_Head)),
	F3_Body = ((F1_1_Body), (F2_1_Body)),
	F3_F_Test = ((F1_1_F_Test), (F2_1_F_Test)),
	subfrontier_contents(F3, F3_Real_Goal, F3_Head, F3_Body, F3_F_Test),
	test_frontier_is_valid(F3, F3_Real_Goal), !, 
        combine_frontiers_from_conjunction_aux_2(F1_1, More_F2, More_F3).
combine_frontiers_from_conjunction_aux_2(F1_1, [_F2_1 | More_F2], More_F3) :-
	combine_frontiers_from_conjunction_aux_2(F1_1, More_F2, More_F3).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

split_subfrontier_into_E_IE_NIE(Frontier_In, _Frontier_Out) :-
	goal_is_disjunction(Frontier_In, _G1, _G2), !, 
	echo_msg(1, '', 'cneg_rt', 'ERROR: split_subfrontier_into_E_IE_NIE can not deal with disjunctions. Frontier_In', Frontier_In),
	fail.

split_subfrontier_into_E_IE_NIE(Frontier_In, Frontier_Out) :-
	goal_is_conjunction(Frontier_In, G1, G2), !,
	split_subfrontier_into_E_IE_NIE(G1, Frontier_G1),
	split_subfrontier_into_E_IE_NIE(G2, Frontier_G2),
	subfrontier_E_IE_NIE_contents(Frontier_G1, E_G1, IE_G1, NIE_G1),
	subfrontier_E_IE_NIE_contents(Frontier_G2, E_G2, IE_G2, NIE_G2),
	rebuild_conjunction_of_goals(E_G1, E_G2, E_Out),
	rebuild_conjunction_of_goals(IE_G1, IE_G2, IE_Out),
	rebuild_conjunction_of_goals(NIE_G1, NIE_G2, NIE_Out),
	subfrontier_E_IE_NIE_contents(Frontier_Out, E_Out, IE_Out, NIE_Out).

split_subfrontier_into_E_IE_NIE(Frontier_In, Frontier_Out) :- 
	goal_is_equality(Frontier_In, _Term1, _Term2, _GV, _EQV, _UQV), !,
	subfrontier_E_IE_NIE_contents(Frontier_Out, Frontier_In, [], []).

split_subfrontier_into_E_IE_NIE(Frontier_In, Frontier_Out) :- 
	goal_is_disequality(Frontier_In, _Term1, _Term2, _GV, _EQV, _UQV), !,
	subfrontier_E_IE_NIE_contents(Frontier_Out, [], Frontier_In, []).

% This leads to infinite loops because double negation 
% sould be managed when generating the subfrontier.
% The way to fix this is remove cneg(cneg(...))
% when evaluating the subfrontier. To be done.
split_subfrontier_into_E_IE_NIE(Frontier_In, Frontier_Out) :- 
	goal_is_negation(Frontier_In, _UQV, _GoalVars, _SubGoal, _Negation_Proposal), !,
	subfrontier_E_IE_NIE_contents(Frontier_Out, [], [], Frontier_In).

split_subfrontier_into_E_IE_NIE(Frontier_In, Frontier_Out) :- 
	goal_is_not_conj_disj_eq_diseq_dneg(Frontier_In), !,
	subfrontier_E_IE_NIE_contents(Frontier_Out, [], [], Frontier_In).

split_subfrontier_into_E_IE_NIE(Frontier_In, _Frontier_Out) :- 
	echo_msg(1, '', 'cneg_rt', 'ERROR: split_subfrontier_into_E_IE_NIE can not deal with subfrontier. Frontier_In', Frontier_In),
	fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

rebuild_conjunction_of_goals([], [], []) :- !. % Empty elements when re-joining
rebuild_conjunction_of_goals(Goals, [], Goals) :- !. % Empty element when re-joining
rebuild_conjunction_of_goals([], Goals, Goals) :- !. % Empty element when re-joining
rebuild_conjunction_of_goals(Goals_1, Goals_2, (Goals_1, Goals_2)) :- % Non-empty elements.
	Goals_1 \== [], 
	Goals_2 \== [], !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% split_IE_NIE_between_imp_and_exp(Frontier_In, ImpVars, ExpVars, UQ_to_EQ_Vars, Dumb_Vars, Frontier_Out)
% returns Frontier_Out that is the frontier divided betwen 
% ImpVars, ExpVars and UQ_Vars.
split_IE_NIE_between_imp_and_exp(Frontier_In, ExpVars, Frontier_Out):-
	subfrontier_E_IE_NIE_contents(Frontier_In, E, IE, NIE),
	echo_msg(2, '', 'cneg_rt', 'split_IE_NIE_between_imp_and_exp :: (E, IE, NIE)', (E, IE, NIE)),
	echo_msg(2, '', 'cneg_rt', 'split_IE_NIE_between_imp_and_exp :: ExpVars', ExpVars),

	split_ie_or_nie_between_imp_and_exp(IE, ExpVars, IE_Imp, IE_Exp),
	echo_msg(2, '', 'cneg_rt', 'split_ie_or_nie_between_imp_and_exp :: (IE_Imp, IE_Exp)', (IE_Imp, IE_Exp)),

	split_ie_or_nie_between_imp_and_exp(NIE, ExpVars, NIE_Imp, NIE_Exp),
	echo_msg(2, '', 'cneg_rt', 'split_ie_or_nie_between_imp_and_exp :: (NIE_Imp, NIE_Exp)', (NIE_Imp, NIE_Exp)),

	% frontier_E_IE_NIE_ied_contents(frontier, E, IE_Imp, IE_Exp, IE_Dumb, NIE_Imp, NIE_Exp, NIE_Dumb).
	subfrontier_E_IE_NIE_ie_contents(Frontier_Out, E, IE_Imp, IE_Exp, NIE_Imp, NIE_Exp).

% split_formula_between_imp_and_exp(F,ExpVars,Fimp,Fexp) divide F between Fimp and Fexp.
% In Fexp are the elements of F with any variables of ExpVars and
% the rest of elements of F will be in Fimp
split_ie_or_nie_between_imp_and_exp([], _ExpVars, [], []) :- !. % Optimization.
split_ie_or_nie_between_imp_and_exp(Form, ExpVars, Form_imp, Form_exp) :-
	goal_is_conjunction(Form, Form_1, Form_2), !,
	split_ie_or_nie_between_imp_and_exp(Form_1, ExpVars, Form_imp_1, Form_exp_1),
	split_ie_or_nie_between_imp_and_exp(Form_2, ExpVars, Form_imp_2, Form_exp_2),
	rebuild_conjunction_of_goals(Form_imp_1, Form_imp_2, Form_imp),
	rebuild_conjunction_of_goals(Form_exp_1, Form_exp_2, Form_exp).

split_ie_or_nie_between_imp_and_exp(Form, _ExpVars, _Form_imp, _Form_exp) :-
	goal_is_disjunction(Form, _Form_1, _Form_2), !,
	echo_msg(1, '', 'cneg_rt', 'ERROR: split_ie_or_nie_between_imp_and_exp can not deal with disjunctions. Form', Form),
	fail.

split_ie_or_nie_between_imp_and_exp(Form, ExpVars, Form_imp, Form_exp) :-
	varsbag(Form, [], [], Form_Vars), 
	varsbag_intersection(Form_Vars, ExpVars, Intersection),
	(
	    (   % Form has some ExpVars
		Intersection \== [], !,
		Form_exp = Form, Form_imp = []
	    )
	;
	    (   % Form has no ExpVars 
		Intersection == [], !,
		Form_exp = [], Form_imp = Form
	    )
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
