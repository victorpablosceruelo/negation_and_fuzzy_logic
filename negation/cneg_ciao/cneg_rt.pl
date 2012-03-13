
:- module(cneg_rt, [cneg_rt/6, cneg_rt_test/5], [assertions]).

:- comment(title, "Contructive Negation Runtime Library").

:- comment(author, "V@'{i}ctor Pablos Ceruelo").

:- comment(summary, "This module implements negation predicates for runtime evaluation.").

:- use_module(cneg_aux, _).
:- use_module(cneg_diseq, [prepare_attributes_for_printing/2, cneg_diseq_echo/5]).
:- use_module(cneg_rt_dynamic, [cneg_rt_dynamic/5]).
:- use_module(cneg_tr_hybrid, [cneg_tr_hybrid_negate_literal/4]).

% To access pre-frontiers from anywhere.
:- multifile cneg_pre_frontier/6.
% To evaluate predicates only from the top package.
:- multifile call_to_predicate/1.

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
	echo_msg(2, 'nl', 'calls_trace', '', ''),
	echo_msg(2, '', 'calls_trace', 'cneg_rt', evaluating(CN_Call)),

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
	    !, % Backtracking forbidden.
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
