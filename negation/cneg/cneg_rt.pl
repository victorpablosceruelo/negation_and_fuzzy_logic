
:- module(cneg_rt, [cneg_rt/4, cneg_rt_aux/3], [assertions]).

:- comment(title, "Contructive Negation Runtime Library").
:- comment(author, "V@'{i}ctor Pablos Ceruelo").
:- comment(summary, "This module implements negation predicates for runtime evaluation.").

:- use_module(library('cneg/cneg_aux')).
:- use_module(library('cneg/cneg_diseq')).
:- use_module(library('cneg/cneg_frontier')).
:- use_module(library('cneg/cneg_negate_frontier')).

% To access pre-frontiers from anywhere.
:- multifile cneg_pre_frontier/9.
% To evaluate predicates only from the top package.
:- multifile call_to_predicate/1.
% Debugging or not ... that is the question.
:- multifile file_debug_is_activated/1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cneg_rt(Goal, GoalVars_In, Depth_Level, Trace) :-

	% Save trace (for debugging and tabling usage)
	CN_Call = (cneg_rt(Goal, GoalVars_In, Depth_Level)), 
	add_predicate_to_trace(evaluating(CN_Call), Trace, NewTrace),
	echo_msg(2, 'trace', 'cneg_rt', 'call to cneg_rt/4 with (updated) trace', NewTrace),

	cneg_rt_aux(Goal, GoalVars_In, Negated_Frontier),
	!, % Backtracking forbidden.
	evaluate_negated_frontier(Negated_Frontier, Depth_Level, NewTrace, CN_Call).

cneg_rt_aux(Goal, GoalVars_In, Negated_Frontier) :-
	echo_msg(2, 'nl', 'cneg_rt', '', ''),
	echo_msg(2, '', 'cneg_rt', 'cneg_rt_aux :: Goal', Goal),
	echo_msg(2, 'separation', 'cneg_rt', '', ''),
	echo_msg(2, 'nl', 'cneg_rt', '', ''),

	varsbag(GoalVars_In, [], [], GoalVars), % Clean up non-vars in GoalVars.
	echo_msg(2, '', 'cneg_rt', 'cneg_rt :: (GoalVars)', GoalVars),
	cneg_diseq_echo(2, '', 'cneg_rt', 'cneg_rt :: Goal with attrs', Goal),
	!, % Reduce the stack's memory by forbidding backtracking.
	compute_frontier(Goal, GoalVars, Frontier),
	!, % Reduce the stack's memory by forbidding backtracking.
	echo_msg(2, 'nl', 'cneg_rt', '', ''),
	echo_msg(2, 'list', 'cneg_rt', 'cneg_rt :: Frontier', Frontier),
	!,
	negate_frontier(Frontier, GoalVars, Negated_Frontier),
	!, % Reduce the stack's memory by forbidding backtracking.
	echo_msg(2, 'separation', 'cneg_rt', '', ''),
	echo_msg(2, 'nl', 'cneg_rt', '', ''),
	echo_msg(2, '', 'cneg_rt', 'cneg_rt :: (Goal, GoalVars)', (Goal, GoalVars)),
	echo_msg(2, 'nl', 'cneg_rt', '', ''),
	echo_msg(2, 'list', 'cneg_rt', 'cneg_rt :: Frontier', Frontier),
	echo_msg(2, 'nl', 'cneg_rt', '', ''),
	echo_msg(2, 'list', 'cneg_rt', 'cneg_rt :: Result (conj)', Negated_Frontier),
	echo_msg(2, 'separation', 'cneg_rt', '', ''),
	echo_msg(2, 'nl', 'cneg_rt', '', '').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Please note this mechanism wastes less memory and cpu, 
% since it goes one by one, but allows backtracking.
evaluate_negated_frontier([], _Level, Trace, CN_Call) :- 
	add_predicate_to_trace(ended_results_for(CN_Call), Trace, Trace_1),
	prepare_attributes_for_printing(CN_Call, Attributes_For_Printing_Conj),
	add_predicate_to_trace(current_attributes(Attributes_For_Printing_Conj), Trace_1, Trace_2),
	add_predicate_to_trace('-----------------------', Trace_2, Trace_3),
	end_trace(Trace_3),
	echo_msg(2, '', 'cneg_rt', 'evaluate_negated_frontier ', 'EMPTY LIST'),
	echo_msg(2, '', 'calls_trace', 'evaluate_negated_frontier ', 'EMPTY LIST'),
	echo_msg(2, 'nl', 'calls_trace', '', ''),
	echo_msg(2, 'nl', 'calls_trace', '', '').

evaluate_negated_frontier([Result | Result_List], Level, Trace, CN_Call) :-
	add_predicate_to_trace('-----------------------', Trace, Trace_1),
	add_predicate_to_trace(result_for(CN_Call), Trace_1, Trace_2),
	add_predicate_to_trace(subgoal(Result), Trace_2, Trace_3),
	prepare_attributes_for_printing(Result, Attributes_For_Printing_Conj),
	add_predicate_to_trace(with_attributes(Attributes_For_Printing_Conj), Trace_3, Trace_4),
	echo_msg(2, '', 'cneg_rt', 'evaluate_negated_frontier :: goal', CN_Call),
	cneg_diseq_echo(2, '', 'cneg_rt', 'evaluate_negated_frontier :: result', Result),
	generate_traces_for_conjunction(Trace_4, Trace_5, Trace_6),
	local_call_to(Result, Level, Trace_5),
	evaluate_negated_frontier(Result_List, Level, Trace_6, CN_Call).

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
	goal_is_negation(Predicate, GoalVars, Goal),
	echo_msg_3pm(2, '', 'calls_trace', 'call_to (L', Level, ') :: Predicate', Predicate),
	cneg_rt(Goal, GoalVars, Level, Trace),
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
