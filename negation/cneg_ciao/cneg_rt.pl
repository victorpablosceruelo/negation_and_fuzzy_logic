
:- module(cneg_rt, [cneg_rt/3, cneg_rt_gv/5], [assertions]).

:- comment(title, "Contructive Negation Runtime Library").

:- comment(author, "V@'{i}ctor Pablos Ceruelo").

:- comment(summary, "This module implements negation predicates for runtime evaluation.").

:- use_module(cneg_aux, _).
:- use_module(cneg_diseq, [prepare_attributes_for_printing/2, cneg_diseq_echo/4]).
:- use_module(cneg_rt_aux, [cneg_rt_Aux/4]).
:- use_module(cneg_tr_hybrid, [cneg_tr_hybrid_negate_literal/4]).

% To access pre-frontiers from anywhere.
:- multifile cneg_pre_frontier/6.
% To evaluate predicates only from the top package.
:- multifile call_to/3.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cneg_rt(UQV, Goal, Proposal) :-
	    Proposal == 'cneg_intneg', !,
	    echo_msg(1, '', 'cneg_rt', 'Proposal not working yet', Proposal),
	    echo_msg(1, '', 'cneg_rt', 'Query', cneg_rt(UQV, Goal, Proposal)).

cneg_rt(UQV, Goal, Proposal) :-
	Proposal == 'cneg_hybrid', !,
	cneg_rt_hybrid(UQV, Goal).

cneg_rt(UQV, Goal, Proposal) :-
	validate_proposal(Proposal),
	generate_empty_trace(Trace),
	varsbag(Goal, UQV, [], GoalVars), % We prefer to play with goalvars
	cneg_rt_gv(Goal, GoalVars, Proposal, 0, Trace).

validate_proposal(Proposal) :-
	(
	    Proposal == 'cneg_rt_New'
	;
	    Proposal == 'cneg_rt_Chan'
	;
	    Proposal == 'cneg_rt_Stuckey'
	), !.
validate_proposal(Proposal) :-
	echo_msg(1, '', 'cneg_rt', 'Invalid Proposal for negation', Proposal),
	!, fail.		

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Please note this mechanism wastes less memory and cpu, 
% since it goes one by one, but allows backtracking.
call_to_all_negated_subfrontiers([], _Level, Trace, CN_Call) :- 
	generate_conjunction_trace(Trace, Trace_Info, Trace_End),
	generate_conjunction_trace(Trace_Info, Trace_Info_1, Trace_Info_2),
	add_predicate_to_trace(ended_subfrontiers_for(CN_Call), Trace_Info_1),
	prepare_attributes_for_printing(CN_Call, Attributes_For_Printing_Conj),
	add_predicate_to_trace(attributes(Attributes_For_Printing_Conj), Trace_Info_2),
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
	prepare_attributes_for_printing(Result, Attributes_For_Printing_Conj),
	add_predicate_to_trace(attributes(Attributes_For_Printing_Conj), Trace_Info_6),
	echo_msg(2, '', 'cneg_rt', 'SUBFRONTIER for goal', CN_Call),
	echo_msg(2, '', 'cneg_rt', '', Result),
	cneg_diseq_echo(2, '', 'cneg_rt', Result),
	call_to(Result, Level, Trace_Result),
	call_to_all_negated_subfrontiers(Result_List, Level, Trace_Next_Goal, CN_Call).

%generate_disjunction_from_list([], fail) :- !.
%generate_disjunction_from_list([Goal], Goal) :- !.
%generate_disjunction_from_list([Goal | Goals], (Goal ; Disj_Goals)) :-
%	Goals \== [],
%	generate_disjunction_from_list(Goals, Disj_Goals).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cneg_rt_hybrid(UQV, Body) :- 
	test_if_cneg_rt_needed(GoalVars, true, Body, Result),
	(
	    (   Result == 'true', !   )
	;
	    (   Result == 'fail',  !,
		varsbag(Body, UQV, [], GoalVars),
		cneg_tr_hybrid_negate_literal(Body, GoalVars, 'true', Neg_Body),
		echo_msg(1, '', 'cneg_hybrid', 'cneg_hybrid :: call', Neg_Body),
		generate_empty_trace(Trace),
		call_to(Neg_Body, 0, Trace)
	    )
	).

test_if_cneg_rt_needed(GoalVars, Body_First_Unification, Body, Result) :-
	echo_msg(1, '', 'cneg_hybrid', 'test_if_cneg_rt_needed :: GoalVars', GoalVars),
	echo_msg(1, '', 'cneg_hybrid', 'test_if_cneg_rt_needed :: Body_First_Unification', Body_First_Unification),
	echo_msg(1, '', 'cneg_hybrid', 'test_if_cneg_rt_needed :: Body', Body), 
	varsbag(GoalVars, [], [], Real_GoalVars),
	varsbag(Body_First_Unification, [], Real_GoalVars, Non_Problematic_Vars),
	varsbag(Body, Non_Problematic_Vars, [], Problematic_Vars),
	varsbag(Body, Real_GoalVars, [], UQV),
	!, % No backtracking allowed.
	(
	    (   % If no problematic vars, use cneg_hybrid (INTNEG).
		Problematic_Vars == [],
		Result = 'fail'
	    )
	;
	    (   % If problematic vars, use cneg_rt (Chan's approach).
		Problematic_Vars \== [],
		Result = 'true',
		cneg_rt(UQV, Body, 'cneg_hybrid')
	    )
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

