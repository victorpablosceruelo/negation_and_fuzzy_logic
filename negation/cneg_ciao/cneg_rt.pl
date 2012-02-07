
:- module(cneg_rt, [cneg_rt_Chan/2, cneg_rt_New/2, cneg_rt_uqv/3, cneg_rt_gv/5, cneg_rt_Aux/4], [assertions]).

:- comment(title, "Contructive Negation Runtime Library - Chan's Proposal").

:- comment(author, "V@'{i}ctor Pablos Ceruelo").

:- comment(summary, "This module implements negation predicates for runtime evaluation.").

:- use_module(cneg_aux, _).
:- use_module(cneg_rt_aux_frontiers, _).
:- use_module(cneg_diseq, _).
:- use_module(cneg_rt_Chan, _).
:- use_module(cneg_rt_Stuckey, _).

% To access pre-frontiers from anywhere.
:- multifile cneg_pre_frontier/6.
:- multifile call_to/3.

cneg_rt_Chan(UQV, Goal) :-
	Proposal = 'cneg_rt_Chan',
	cneg_rt_uqv(Goal, UQV, Proposal).

cneg_rt_New(UQV, Goal) :-
	Proposal = 'cneg_rt_New',
	cneg_rt_uqv(Goal, UQV, Proposal).

cneg_rt_uqv(Goal, UQV, Proposal) :-
	generate_empty_trace(Trace),
	varsbag(Goal, UQV, [], GoalVars), % We prefer to play with goalvars
	cneg_rt_gv(Goal, GoalVars, Proposal, 0, Trace).

cneg_rt_gv(Goal, GoalVars, Proposal, Level, Trace) :-
	% Save trace information (basic for debugging).
	CN_Call = (cneg_rt_gv(Goal, GoalVars, Proposal, Level)),
	generate_conjunction_trace(Trace, Trace_1, Trace_2),
	add_predicate_to_trace(CN_Call, Trace_1),

	echo_msg(2, 'separation', 'cneg_rt', '', ''),
	echo_msg(2, 'nl', 'cneg_rt', '', ''),
	get_trace_status_list(Trace_2, Trace_Status_List),
	echo_msg(2, 'list', 'cneg_rt', 'TRACE: ', Trace_Status_List),
	!,
	cneg_rt_Aux(Goal, GoalVars, Proposal, Result_List),
	!, % Reduce the stack's memory by forbidding backtracking.
	call_to_all_negated_subfrontiers(Result_List, Level, Trace_2, CN_Call).

% Please note this mechanism wastes less memory and cpu, 
% since it goes one by one, but allows backtracking.
call_to_all_negated_subfrontiers([], _Level, Trace, CN_Call) :- 
	generate_conjunction_trace(Trace, Trace_Info, Trace_End),
	generate_conjunction_trace(Trace_Info, Trace_Info_1, Trace_Info_2),
	add_predicate_to_trace(ended_subfrontiers_for(CN_Call), Trace_Info_1),
  	get_attributes_in_term_vars(CN_Call, Vars_With_Attrs, _Vars_Without_Attrs), 
	add_predicate_to_trace(attributes(Vars_With_Attrs), Trace_Info_2),
	add_predicate_to_trace('-----------------------', Trace_End),
	get_trace_final_status_list(Trace, Status_List),
	echo_msg(2, 'list', 'cneg_rt', 'TRACE: ', Status_List),
	echo_msg(2, 'nl', 'cneg_rt', '', ''),
	echo_msg(2, 'list', 'trace', 'TRACE: ', Status_List),
	echo_msg(2, 'nl', 'trace', '', '').

call_to_all_negated_subfrontiers([Result | Result_List], Level, Trace, CN_Call) :-
	generate_conjunction_trace(Trace, Trace_Current_Goal, Trace_Next_Goal),
	generate_conjunction_trace(Trace_Current_Goal, Trace_Info, Trace_Result),
	generate_conjunction_trace(Trace_Info, Trace_Info_1, Trace_Info_2),
	generate_conjunction_trace(Trace_Info_2, Trace_Info_3, Trace_Info_4),
	generate_conjunction_trace(Trace_Info_4, Trace_Info_5, Trace_Info_6),
	add_predicate_to_trace('-----------------------', Trace_Info_1),
	add_predicate_to_trace(subfrontier_for(CN_Call), Trace_Info_3),
	add_predicate_to_trace(Result, Trace_Info_5),
  	get_attributes_in_term_vars(Result, Vars_With_Attrs, _Vars_Without_Attrs), 
	add_predicate_to_trace(attributes(Vars_With_Attrs), Trace_Info_6),
	echo_msg(2, '', 'cneg_rt', 'SUBFRONTIER for goal', CN_Call),
	echo_msg(2, '', 'cneg_rt', '', Result),
	portray_attributes_in_term_vars(2, 'cneg_rt', Result),
	call_to(Result, Level, Trace_Result),
	call_to_all_negated_subfrontiers(Result_List, Level, Trace_Next_Goal, CN_Call).

%generate_disjunction_from_list([], fail) :- !.
%generate_disjunction_from_list([Goal], Goal) :- !.
%generate_disjunction_from_list([Goal | Goals], (Goal ; Disj_Goals)) :-
%	Goals \== [],
%	generate_disjunction_from_list(Goals, Disj_Goals).

cneg_rt_Aux(Goal, GoalVars, Proposal, Result_List) :-
	echo_msg(2, 'separation', 'cneg_rt', '', ''),
	echo_msg(2, 'nl', 'cneg_rt', '', ''),
	echo_msg(2, '', 'cneg_rt', 'cneg_rt_Aux :: Proposal', Proposal),
	echo_msg(2, '', 'cneg_rt', 'cneg_rt_Aux :: GoalVars', GoalVars),
	echo_msg(2, '', 'cneg_rt', 'cneg_rt_Aux :: Goal', Goal),
	echo_msg(2, 'nl', 'cneg_rt', '', ''),
	echo_msg(2, 'statistics', 'statistics', '', (cneg_rt_Aux(Goal, GoalVars, Proposal))),
	echo_msg(2, 'nl', 'cneg_rt', '', ''),
	echo_msg(2, '', 'cneg_rt', 'cneg_rt_Aux :: (Goal, GoalVars, Proposal)', (Goal, GoalVars, Proposal)),
	varsbag(GoalVars, [], [], Real_GoalVars), % Clean up non-vars
	echo_msg(2, '', 'cneg_rt', 'cneg_rt_Aux :: Real_GoalVars', Real_GoalVars),
	portray_attributes_in_term_vars(2, 'cneg_rt', Goal),
	!, % Reduce the stack's memory by forbidding backtracking.
	compute_frontier(Goal, Real_GoalVars, Proposal, Frontier),
	!, % Reduce the stack's memory by forbidding backtracking.
	echo_msg(2, 'nl', 'cneg_rt', '', ''),
	echo_msg(2, 'list', 'cneg_rt', 'cneg_rt_Aux :: Frontier', Frontier),
	!,
	negate_frontier_list(Frontier, GoalVars, Proposal, Result_List),
	!, % Reduce the stack's memory by forbidding backtracking.
	echo_msg(2, 'separation', 'cneg_rt', '', ''),
	echo_msg(2, 'nl', 'cneg_rt', '', ''),
	echo_msg(2, '', 'cneg_rt', 'cneg_rt_Aux :: Summary for Proposal', Proposal),
	echo_msg(2, '', 'cneg_rt', 'cneg_rt_Aux :: Goal', Goal),
	echo_msg(2, '', 'cneg_rt', 'cneg_rt_Aux :: Real_GoalVars', Real_GoalVars),
	echo_msg(2, 'nl', 'cneg_rt', '', ''),
	echo_msg(2, 'list', 'cneg_rt', 'cneg_rt_Aux :: Frontier', Frontier),
	echo_msg(2, 'nl', 'cneg_rt', '', ''),
	echo_msg(2, 'list', 'cneg_rt', 'cneg_rt_Aux :: Result (conj)', Result_List),
	echo_msg(2, 'separation', 'cneg_rt', '', ''),
	echo_msg(2, 'nl', 'cneg_rt', '', '').

%by_pass_universallity_of_variables(UQV_In, UQV_Aux) :-
%	varsbag(UQV_In, [], [], UQV_Aux). % All vars in UQV_In are now UQV.
