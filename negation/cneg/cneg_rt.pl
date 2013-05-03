
:- module(cneg_rt, [cneg_rt/4, cneg_rt_aux/3, test_execution/5], [assertions]).

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
	CN_Call = (cneg_rt(Goal, GoalVars_In, Depth_Level, 'trace')), 
	add_predicate_to_trace(evaluating(CN_Call), Trace, NewTrace),
	print_msg(3, 3, 'trace', 'call to cneg_rt/4 with (updated) trace', NewTrace),

	cneg_rt_aux(Goal, GoalVars_In, Negated_Frontier),
	!, % Backtracking forbidden.
	evaluate_negated_frontier(Negated_Frontier, Depth_Level, NewTrace, CN_Call).

cneg_rt_aux(Goal, GoalVars_In, Negated_Frontier) :-
	print_msg(3, 3, 'nl', '', ''),
	print_msg(3, 3, '', 'cneg_rt_aux :: Goal', Goal),
	print_msg(3, 3, 'separation', '', ''),
	print_msg(3, 3, 'nl', '', ''),

	varsbag(GoalVars_In, [], [], GoalVars), % Clean up non-vars in GoalVars.
	print_msg(3, 3, '', 'cneg_rt :: (GoalVars)', GoalVars),
	print_msg(3, 3, 'aux', 'cneg_rt :: Goal', Goal),
	print_vars_diseqs(3, '', Goal),
	print_msg(3, 3, 'nl', '', ''),
	!, % Reduce the stack's memory by forbidding backtracking.
	compute_frontier(Goal, GoalVars, Frontier),
	!, % Reduce the stack's memory by forbidding backtracking.
	print_msg(3, 3, 'nl', '', ''),
	print_msg(3, 3, 'list', 'cneg_rt :: Frontier', Frontier),
	!,
	negate_frontier(Frontier, GoalVars, Negated_Frontier),
	!, % Reduce the stack's memory by forbidding backtracking.
	print_msg(3, 3, 'separation', '', ''),
	print_msg(3, 3, 'nl', '', ''),
	print_msg(3, 3, '', 'cneg_rt :: (Goal, GoalVars)', (Goal, GoalVars)),
	print_msg(3, 3, 'nl', '', ''),
	print_msg(3, 3, 'list', 'cneg_rt :: Frontier', Frontier),
	print_msg(3, 3, 'nl', '', ''),
	print_msg(3, 3, 'list', 'cneg_rt :: Result (conj)', Negated_Frontier),
	print_msg(3, 3, 'separation', '', ''),
	print_msg(3, 3, 'nl', '', '').

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
	print_msg(3, 3, '', 'evaluate_negated_frontier ', 'EMPTY LIST'),
	print_msg(3, 3, '', 'evaluate_negated_frontier ', 'EMPTY LIST'),
	print_msg(3, 3, 'nl', '', ''),
	print_msg(3, 3, 'nl', '', '').

evaluate_negated_frontier([Result | Result_List], Level, Trace, CN_Call) :-
	add_predicate_to_trace('-----------------------', Trace, Trace_1),
	add_predicate_to_trace(result_for(CN_Call), Trace_1, Trace_2),
	add_predicate_to_trace(subgoal(Result), Trace_2, Trace_3),
	prepare_attributes_for_printing(Result, Attributes_For_Printing_Conj),
	add_predicate_to_trace(with_attributes(Attributes_For_Printing_Conj), Trace_3, Trace_4),
	print_msg(3, 3, '', 'evaluate_negated_frontier :: goal', CN_Call),
	print_msg(3, 3, 'aux', 'evaluate_negated_frontier :: result', Result),
	print_vars_diseqs(3, '', Result), 
	print_msg(3, 3, 'nl', '', ''),
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
	print_msg(3, 3, 'statistics', '', (local_call_to(Predicate, Level))),
	print_msg(3, 3, 'nl', '', ''),
	print_msg(3, 3, 'nl', '', ''), 
	print_msg(3, 3, 'nl', '', ''), 
	print_msg(3, 3, '', ['call_to (L' |[ Level |[ ') :: Predicate']]], Predicate), 
	print_msg(3, 3, 'aux', 'Predicate with attrs', Predicate),
	print_vars_diseqs(3, '', Predicate), 
	print_msg(3, 3, 'nl', '', ''), 
	get_trace_status_list(Trace, Trace_Status_List),
	print_msg(3, 3, 'list', 'call_to :: TRACE ', Trace_Status_List),
	print_msg(3, 3, 'nl', '', ''), 
	local_call_to_aux(Predicate, Level, Trace).

local_call_to(Predicate, Level_In, _Trace) :- 
	Level is Level_In + 1,
	print_msg(3, 3, '', [ 'call_to (L' |[ Level |[ ') :: FAILED Predicate call. Predicate']]], Predicate), 
%	print_msg(3, 3, 'nl', '', ''), 
	!, fail. 

local_call_to_aux(Predicate, Level_In, Trace) :-
	goal_is_disjunction(Predicate, G1, G2), !,
	Level is Level_In + 1,
	(
	    (
		(       print_msg(3, 3, '', ['call_to (L' |[ Level |[ ') :: G1 \\/ G2 :: G1']]], G1), 
			local_call_to_aux(G1, Level, Trace),
			print_msg(3, 3, '', ['call_to (L' |[ Level |[ ') :: G1 \\/ G2 :: G1']]], 'OK'),
			print_msg(3, 3, '', ['call_to (L' |[ Level |[ ') :: G1 \\/ G2']]], 'OK')
		)
	    ;
		(       print_msg(3, 3, '', ['call_to (L' |[ Level |[ ') :: G1 \\/ G2 :: G1']]], 'FAIL'),
			print_msg(3, 3, 'nl', '', ''), % Differentiate paths.
			print_msg(3, 3, '', ['call_to (L' |[ Level |[ ') :: G1 \\/ G2 :: G2']]], G2), 
			local_call_to_aux(G2, Level, Trace),
			print_msg(3, 3, '', ['call_to (L' |[ Level |[ ') :: G1 \\/ G2 :: G2']]], 'OK'),
			print_msg(3, 3, '', ['call_to (L' |[ Level |[ ') :: G1 \\/ G2']]], 'OK')
		)
	    )
	;
	    (           print_msg(3, 3, '', ['call_to (L' |[ Level |[ ') :: G1 \\/ G2 :: G2']]], 'FAIL'),
			print_msg(3, 3, '', ['call_to (L' |[ Level |[ ') :: G1 \\/ G2']]], 'FAIL'), 
			fail 
	    )
	).

local_call_to_aux(Predicate, Level_In, Trace) :-
	goal_is_conjunction(Predicate, G1, G2), !,
	Level is Level_In + 1,
	generate_traces_for_conjunction(Trace, Trace_G1, Trace_G2),
	(   % 1st conjunct
	    (	print_msg(3, 3, '', ['call_to (L' |[ Level |[ ') :: G1 /\\ G2 :: G1']]], G1),
		local_call_to_aux(G1, Level, Trace_G1),
		print_msg(3, 3, '', ['call_to (L' |[ Level |[ ') :: G1 /\\ G2 :: G1']]], 'OK')
	    )
	;
	    (	print_msg(3, 3, '', ['call_to (L' |[ Level |[ ') :: G1 /\\ G2 :: G1']]], 'FAIL'),
		print_msg(3, 3, '', ['call_to (L' |[ Level |[ ') :: G1 /\\ G2']]], 'FAIL'),
		fail 
	    )
	),
	(   % 2nd conjunct
	    (   print_msg(3, 3, '', ['call_to (L' |[ Level |[ ') :: G1 /\\ G2 :: G2']]], G2),
		local_call_to_aux(G2, Level, Trace_G2),
		print_msg(3, 3, '', ['call_to (L' |[ Level |[ ') :: G1 /\\ G2 :: G2']]], 'OK'),
		print_msg(3, 3, '', ['call_to (L' |[ Level |[ ') :: G1 /\\ G2']]], 'OK')
	    )
	;
	    (   print_msg(3, 3, '', ['call_to (L' |[ Level |[ ') :: G1 /\\ G2 :: G2']]], 'FAIL'),
		print_msg(3, 3, '', ['call_to (L' |[ Level |[ ') :: G1 /\\ G2']]], 'FAIL'),
		fail   
	    )
	).

local_call_to_aux(Predicate, Level_In, Trace) :-
	Level is Level_In + 1,
	goal_is_negation(Predicate, GoalVars, Goal),
	print_msg(3, 3, '', ['call_to (L' |[ Level |[ ') :: Predicate']]], Predicate),
	cneg_rt(Goal, GoalVars, Level, Trace),
	print_msg(3, 3, '', ['call_to (L' |[ Level |[ ') :: SUCCEED Predicate call. Predicate']]], Predicate).

local_call_to_aux(Predicate, Level_In, Trace) :-
	Level is Level_In + 1,
	goal_is_not_negation(Predicate),
	print_msg(3, 3, '', ['call_to (L' |[ Level |[ ') :: Predicate']]], Predicate),
	add_predicate_to_trace(Predicate, Trace, NewTrace),
	end_trace(NewTrace),
	call_to_predicate(Predicate),
	print_msg(3, 3, '', ['call_to (L' |[ Level |[ ') :: SUCCEED Predicate call. Predicate']]], Predicate).


%local_call_to_aux(Predicate, Level_In) :- 
%	Level is Level_In + 1,
%	print_msg(3, 3, '', 'call_to (L', Level, ') :: Predicate - FAILED -', Predicate),
%	!, fail. 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test_execution(Logo, Vars, First_Part, Second_Part, Should_What) :-
	(
	    (
		print_msg(1, 3, 'nl', '', ''),
		print_msg(1, 3, 'aux', 'Test: ', Logo),
		print_msg(3, 3, 'nl', '', ''),
		print_msg(3, 3, 'aux', '', '(vars with attrs) '),
		print_vars_diseqs(3, '', Vars), 
		print_msg(3, 3, 'nl', '', ''),
		print_msg(3, 3, 'aux', '1st: ', First_Part), 
		call_to_predicate(First_Part),
		print_msg(3, 3, 'aux', '', ' --> (vars with attrs) '),
		print_vars_diseqs(3, '', Vars), 
		print_msg(3, 3, 'nl', '', ''),
		print_msg(3, 3, 'aux', '2nd: ', Second_Part), 
		call_to_predicate(Second_Part),
		print_msg(3, 3, 'aux', ' --> (vars with attrs) ', ''), 
		print_vars_diseqs(3, '', Vars), 
		print_msg(3, 3, 'nl', '', ''), !,
		
		(
		    (
			Should_What = 'should_succeed',
			print_msg(1, 3, 'aux', ' -> PASS', ''),
			print_msg(3, 3, 'nl', '', '')
		    )
		;
		    (
			Should_What = 'should_fail',
			print_msg(1, 3, 'aux', ' -> ERROR', ''),
			print_msg(3, 3, 'nl', '', '')
		    )
		),
		!
	    )
	;
	    (
		(
		    (
			Should_What = 'should_succeed',
			print_msg(1, 3, 'aux', ' -> ERROR', ''),
			print_msg(3, 3, 'nl', '', '')
		    )
		;
		    (
			Should_What = 'should_fail',
			print_msg(1, 3, 'aux', ' -> PASS', ''),
			print_msg(3, 3, 'nl', '', '')
		    )
		)
	    )
	), !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
