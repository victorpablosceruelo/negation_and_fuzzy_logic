
:- module(cneg_rt, [cneg_rt/6, cneg_rt_test/5], [assertions]).

:- comment(title, "Contructive Negation Runtime Library").

:- comment(author, "V@'{i}ctor Pablos Ceruelo").

:- comment(summary, "This module implements negation predicates for runtime evaluation.").

:- use_module(cneg_aux, _).
:- use_module(cneg_diseq, [prepare_attributes_for_printing/2, cneg_diseq_echo/4]).
:- use_module(cneg_rt_dynamic, [cneg_rt_dynamic/5]).
:- use_module(cneg_tr_hybrid, [cneg_tr_hybrid_negate_literal/4]).

% To access pre-frontiers from anywhere.
:- multifile cneg_pre_frontier/6.
% To evaluate predicates only from the top package.
:- multifile call_to/3.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cneg_rt(UQV, GoalVars, Goal, Proposal, Depth_Level, Trace) :-
	% Test the proposal is a valid one.
	validate_proposal(Proposal),

	% Save trace (for debugging and tabling usage)
	CN_Call = (cneg_rt(UQV, GoalVars, Goal, Proposal, Depth_Level)), 
	add_predicate_to_trace(evaluating(CN_Call), Trace, NewTrace),
	echo_msg(2, 'trace', 'cneg_rt', 'call to cneg_rt/6 with (updated) trace', NewTrace),

	% Now select the adequate code for each negation.
	(
	    (
		Proposal == 'cneg_rt_intneg', !
	    )
	;
	    (
		Proposal == 'cneg_hybrid', !,
		cneg_rt_hybrid(UQV, GoalVars, Goal, Proposal, Result)
	    )
	;
	    (
		(
		    Proposal == 'cneg_rt_New', !
		;
		    Proposal == 'cneg_rt_Chan', !
		;
		    Proposal == 'cneg_rt_Stuckey', !
		),
		cneg_rt_dynamic(UQV, GoalVars, Goal, Proposal, Result)
	    ),
	    call_to_conjunction_list(Result, Depth_Level, NewTrace, CN_Call)
	).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

validate_proposal(Proposal) :-
	(
	    Proposal == 'cneg_rt_New'
	;
	    Proposal == 'cneg_rt_Chan'
	;
	    (
		Proposal == 'cneg_rt_Stuckey',
		echo_msg(1, '', 'cneg_rt', 'Proposal is not fully working yet', Proposal)
	    )
	;
	    (
		Proposal == 'cneg_rt_intneg', 
 		echo_msg(1, '', 'cneg_rt', 'Proposal is not fully working yet', Proposal)
	    )
	;
	    (
		Proposal == 'cneg_hybrid', 
 		echo_msg(1, '', 'cneg_rt', 'Proposal is not fully working yet', Proposal)
	    )
	), !.
validate_proposal(Proposal) :-
	echo_msg(1, '', 'cneg_rt', 'Invalid Proposal for negation', Proposal),
	!, fail.		

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cneg_rt_hybrid(UQV, GoalVars, Goal, _Proposal, [Answer]) :- 
	% varsbag(Body, UQV, [], GoalVars),
	cneg_tr_hybrid_negate_literal(Goal, GoalVars, 'true', Neg_Goal),
	cneg_rt_test(UQV, GoalVars, true, Neg_Goal, Answer).

cneg_rt_test(UQV, GoalVars, Body_First_Unification, Body, Result) :-
	echo_msg(1, '', 'cneg_hybrid', 'test_if_cneg_rt_needed :: GoalVars', GoalVars),
	echo_msg(1, '', 'cneg_hybrid', 'test_if_cneg_rt_needed :: UQV', UQV),
	echo_msg(1, '', 'cneg_hybrid', 'test_if_cneg_rt_needed :: Body_First_Unification', Body_First_Unification),
	echo_msg(1, '', 'cneg_hybrid', 'test_if_cneg_rt_needed :: Body', Body), 
	    
	varsbag(GoalVars, [], [], Real_GoalVars),
	varsbag(Body_First_Unification, [], Real_GoalVars, Non_Problematic_Vars),
	varsbag(Body, Non_Problematic_Vars, [], Problematic_Vars),
	!, % No backtracking allowed.
	(
	    (   % If no problematic vars, use an static approach (intneg).
		Problematic_Vars == [],
		UQV == [],
		Result = Body % Just run the body.
	    )
	;
	    (   % If problematic vars, use a dynamic approach (chan, stuckey, new).
		(
		    Problematic_Vars \== []
		;
		    UQV \== []
		),
		Result = cneg_rt(UQV, GoalVars, Body, 'cneg_rt_New') % Just run the body.
	    )
	).

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
	echo_msg(2, 'trace', 'cneg_rt', 'call_to_conjunction_list: ', Trace_3),
	echo_msg(2, 'trace', 'trace', 'call_to_conjunction_list: ', Trace_3).

call_to_conjunction_list([Result | Result_List], Level, Trace, CN_Call) :-
	add_predicate_to_trace('-----------------------', Trace, Trace_1),
	add_predicate_to_trace(result_for(CN_Call), Trace_1, Trace_2),
	add_predicate_to_trace(subgoal(Result), Trace_2, Trace_3),
	prepare_attributes_for_printing(Result, Attributes_For_Printing_Conj),
	add_predicate_to_trace(with_attributes(Attributes_For_Printing_Conj), Trace_3, Trace_4),
	echo_msg(2, '', 'cneg_rt', 'call_to_conjunction_list :: goal', CN_Call),
	echo_msg(2, 'aux', 'cneg_rt', 'call_to_conjunction_list :: result :: ', ''),
	cneg_diseq_echo(2, '', 'cneg_rt', Result),
	generate_traces_for_conjunction(Trace_4, Trace_5, Trace_6),
	call_to(Result, Level, Trace_5),
	call_to_conjunction_list(Result_List, Level, Trace_6, CN_Call).

%generate_disjunction_from_list([], fail) :- !.
%generate_disjunction_from_list([Goal], Goal) :- !.
%generate_disjunction_from_list([Goal | Goals], (Goal ; Disj_Goals)) :-
%	Goals \== [],
%	generate_disjunction_from_list(Goals, Disj_Goals).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
