:- package(cneg).
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Package  CNEG is part of the constructive negation implementation.
%
% Original by Susana Munoz Hernandez.
% Modified by Victor Pablos Ceruelo
%
% Distributed without any warranty.
% Works on Ciao Prolog r11293.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%:- module(cneg,_).
% To be able to call Pred from cneg

% Needed to be able to compile the modules.
:- use_module(cneg_aux).
% , [varsbag/4, varsbag_addition/3, append/3, goal_is_conjunction/3, goal_is_disjunction/3, functor_local/4, echo_msg/3]). 
:- use_module(cneg_diseq, [equality/3, disequality/3, 
	diseq_geuqv/5, eq_geuqv/5,
	diseq_geuqv_adv/6, eq_geuqv_adv/6,
	portray_term_with_attributes/2,
	portray_attributes_in_term_vars/3]).
:- use_module(cneg_tr).
:- use_module(cneg_rt, [cneg_rt/3, cneg_rt_gv/5]).
%:- use_module(cneg_rt_Stuckey, [cneg_rt_Stuckey/2]).

% Re-export predicates to use them in console.
:- reexport(cneg_aux, [varsbag/4, varsbag_addition/3]).    
:- reexport(cneg_diseq, [equality/3, disequality/3,
	diseq_geuqv/5, eq_geuqv/5,
	diseq_geuqv_adv/6, eq_geuqv_adv/6,
	portray_term_with_attributes/2,
	portray_attributes_in_term_vars/3]).
:- reexport(cneg_rt, [cneg_rt_gv/5]).
%:- reexport(cneg_rt_Stuckey, [cneg_rt_Stuckey/2]).

% To access pre-frontiers from anywhere.
:- multifile cneg_pre_frontier/6.
:- multifile call_to/3.
:- meta_predicate call_to(?). % /3.
%:- export(call_to/3).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cneg(UQV, Predicate) :-
	(
	    (   cneg_choosen_negation(Negation_Mechanism), !   )
	;
	    (   
		echo_msg(1, '', 'cneg_rt', 'You can choose the negation mechanism by defining the predicate', 'cneg_choosen_negation/1'),
		echo_msg(1, '', 'cneg_rt', 'Examples: .', 'cneg_choosen_negation(cneg_rt_Chan)'),
		Negation_Mechanism = 'cneg_rt_Chan', !
	    )
	),
	cneg_rt(UQV, Predicate, Negation_Mechanism).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

goalvars(Term, GoalVars) :- varsbag(Term, [], [], GoalVars).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

call_to(Predicate, Level_In, Trace) :- 
	Level is Level_In + 1,
	echo_msg(2, 'nl', 'trace', '', ''), 
	get_trace_status_list(Trace, Trace_Status_List),
	echo_msg(2, 'list', 'trace', 'call_to :: TRACE ', Trace_Status_List),
	portray_attributes_in_term_vars(2, 'cneg_diseq', Predicate),
	echo_msg(2, 'nl', 'trace', '', ''), 
	echo_msg_3pm(2, '', 'calls', 'call_to (L', Level, ') :: Predicate', Predicate), 
	call_to_aux(Predicate, Level, Trace).

call_to(Predicate, Level_In, _Trace) :- 
	Level is Level_In + 1,
	echo_msg_3pm(2, '', 'calls', 'call_to (L', Level, ') :: Predicate', 'FAILED'), 
	echo_msg(2, 'nl', 'calls', '', ''), 
	echo_msg_3pm(2, '', 'calls', 'call_to (L', Level, ') :: Predicate FAILED was', Predicate), 
	echo_msg(2, 'nl', 'calls', '', ''), 
	!, fail. 

call_to_aux(Predicate, Level_In, Trace) :-
	goal_is_disjunction(Predicate, G1, G2), !,
	Level is Level_In + 1,
	(
	    (
		(       echo_msg_3pm(2, '', 'calls', 'call_to (L', Level, ') :: G1 \\/ G2 :: G1', G1), 
			call_to_aux(G1, Level, Trace),
			echo_msg_3pm(2, '', 'calls', 'call_to (L', Level, ') :: G1 \\/ G2 :: G1', 'OK'),
			echo_msg_3pm(2, '', 'calls', 'call_to (L', Level, ') :: G1 \\/ G2', 'OK')
		)
	    ;
		(       echo_msg_3pm(2, '', 'calls', 'call_to (L', Level, ') :: G1 \\/ G2 :: G1', 'FAIL'),
			echo_msg(2, 'nl', 'calls', '', ''), % Differentiate paths.
			echo_msg_3pm(2, '', 'calls', 'call_to (L', Level, ') :: G1 \\/ G2 :: G2', G2), 
			call_to_aux(G2, Level, Trace),
			echo_msg_3pm(2, '', 'calls', 'call_to (L', Level, ') :: G1 \\/ G2 :: G2', 'OK'),
			echo_msg_3pm(2, '', 'calls', 'call_to (L', Level, ') :: G1 \\/ G2', 'OK')
		)
	    )
	;
	    (           echo_msg_3pm(2, '', 'calls', 'call_to (L', Level, ') :: G1 \\/ G2 :: G2', 'FAIL'),
			echo_msg_3pm(2, '', 'calls', 'call_to (L', Level, ') :: G1 \\/ G2', 'FAIL'), 
			fail 
	    )
	).

call_to_aux(Predicate, Level_In, Trace) :-
	goal_is_conjunction(Predicate, G1, G2), !,
	Level is Level_In + 1,
	generate_conjunction_trace(Trace, Trace_G1, Trace_G2),
	(   % 1st conjunct
	    (	echo_msg_3pm(2, '', 'calls', 'call_to (L', Level, ') :: G1 /\\ G2 :: G1', G1),
		call_to_aux(G1, Level, Trace_G1),
		echo_msg_3pm(2, '', 'calls', 'call_to (L', Level, ') :: G1 /\\ G2 :: G1', 'OK')
	    )
	;
	    (	echo_msg_3pm(2, '', 'calls', 'call_to (L', Level, ') :: G1 /\\ G2 :: G1', 'FAIL'),
		echo_msg_3pm(2, '', 'calls', 'call_to (L', Level, ') :: G1 /\\ G2', 'FAIL'),
		fail 
	    )
	),
	(   % 2nd conjunct
	    (   echo_msg_3pm(2, '', 'calls', 'call_to (L', Level, ') :: G1 /\\ G2 :: G2', G2),
		call_to_aux(G2, Level, Trace_G2),
		echo_msg_3pm(2, '', 'calls', 'call_to (L', Level, ') :: G1 /\\ G2 :: G2', 'OK'),
		echo_msg_3pm(2, '', 'calls', 'call_to (L', Level, ') :: G1 /\\ G2', 'OK')
	    )
	;
	    (   echo_msg_3pm(2, '', 'calls', 'call_to (L', Level, ') :: G1 /\\ G2 :: G2', 'FAIL'),
		echo_msg_3pm(2, '', 'calls', 'call_to (L', Level, ') :: G1 /\\ G2', 'FAIL'),
		fail   
	    )
	).

call_to_aux(Predicate, Level_In, Trace) :-
	Level is Level_In + 1,
	goal_is_negation_gv(Predicate, GoalVars, Goal, Proposal),
	echo_msg_3pm(2, '', 'calls', 'call_to (L', Level, ') :: Predicate', Predicate),
	cneg_rt_gv(Goal, GoalVars, Proposal, Level, Trace).

call_to_aux(Predicate, Level_In, Trace) :-
	Level is Level_In + 1,
	goal_is_not_negation(Predicate),
	echo_msg_3pm(2, '', 'calls', 'call_to (L', Level, ') :: Predicate', Predicate),
	add_predicate_to_trace(Predicate, Trace),
	call(Predicate).


%call_to_aux(Predicate, Level_In) :- 
%	Level is Level_In + 1,
%	echo_msg_3pm(2, '', 'calls', 'call_to (L', Level, ') :: Predicate - FAILED -', Predicate),
%	!, fail. 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% cneg_tr contains the code transformation needed by Constructive Negation.
% This includes all the all the transformations needed by each one of the proposals included,
% i.e. Chan's proposal, Barbuti's proposal, the New proposal and the hybrid proposal.
%:- load_compilation_module(library('cneg/cneg_tr')). CUANDO SEA LIBRERIA
:- load_compilation_module(.('cneg_tr')).

% trans_sent/3 makes the transformation.
:- add_sentence_trans(trans_sent/3, 1). % TODO: Right priority?
% :- add_clause_trans(trans_clause/3). % Only for debug !!!
