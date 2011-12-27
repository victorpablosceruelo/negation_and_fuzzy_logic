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
	diseq_uqv/3, eq_uqv/3, diseq_eqv/3, eq_eqv/3, 
	diseq_euqv/4, eq_euqv/4,
	diseq_euqv_adv/5, eq_euqv_adv/5,
	portray_attributes_in_term/2]).
:- use_module(cneg_tr).
:- use_module(cneg_rt_Chan, [cneg_rt_Chan/2, cneg_rt_New/2]).
:- use_module(cneg_rt_Stuckey, [cneg_rt_Stuckey/2]).

% Re-export predicates to use them in console.
:- reexport(cneg_aux, [varsbag/4, varsbag_addition/3]).    
:- reexport(cneg_diseq, [equality/3, disequality/3,
	diseq_uqv/3, eq_uqv/3, diseq_eqv/3, eq_eqv/3, 
	diseq_euqv/4, eq_euqv/4,
	diseq_euqv_adv/5, eq_euqv_adv/5]).
:- reexport(cneg_rt_Chan, [cneg_rt_Chan/2, cneg_rt_New/2]).
:- reexport(cneg_rt_Stuckey, [cneg_rt_Stuckey/2]).

% To access pre-frontiers from anywhere.
:- multifile cneg_pre_frontier/6.
:- multifile call_to/1.
:- meta_predicate call_to(?). % /1.
%:- export(call_to/1).

goalvars(Term, GoalVars) :- varsbag(Term, [], [], GoalVars).

cneg_tr(UQV, Functor) :- cneg_tr_aux(Functor, UQV, [], _FV_Out, 'fail', 'true').

cneg_tr_aux(Functor, FV_Cneg, FV_In, FV_Out, _Allowed_To_Fail, Result) :-
	goal_is_conjunction(Functor, _Conj_1, _Conj_2), !,
	echo_msg(1, 'cneg :: Not implemented conjunction. Error processing ', cneg_tr_aux(Functor, FV_Cneg, FV_In, FV_Out, _Allowed_To_Fail, Result)),
	!, fail.

cneg_tr_aux(Functor, FV_Cneg, FV_In, FV_Out, _Allowed_To_Fail, Result) :-
	goal_is_disjunction(Functor, _Conj_1, _Conj_2), !,
	echo_msg(1, 'cneg :: Not implemented disjunction. Error processing ', cneg_tr_aux(Functor, FV_Cneg, FV_In, FV_Out, _Allowed_To_Fail, Result)),
	!, fail.

cneg_tr_aux(Functor, FV_Cneg, FV_In, FV_Out, Allowed_To_Fail, Result) :-
	functor_local(Functor, Name, Arity, Args),
	New_Arity is Arity + 4,
	cneg_main_and_aux_cl_names(Name, _Main_Cl_Name, Aux_Cl_Name),
	cneg_aux:append(FV_Cneg, FV_In, FV_Aux),
	cneg_aux:append(Args, [FV_Aux |[FV_Out |[Allowed_To_Fail |[Result]]]], New_Args),
	functor_local(New_Functor, Aux_Cl_Name, New_Arity, New_Args),
	echo_msg(1, 'cneg_tr_aux :: call', New_Functor),
	call(New_Functor).

test_if_cneg_rt_needed(GoalVars, Body_First_Unification, Body, Result) :-
	echo_msg(1, 'test_if_cneg_rt_needed :: GoalVars', GoalVars),
	echo_msg(1, 'test_if_cneg_rt_needed :: Body_First_Unification', Body_First_Unification),
	echo_msg(1, 'test_if_cneg_rt_needed :: Body', Body), 
	varsbag(GoalVars, [], [], Real_GoalVars),
	varsbag(Body_First_Unification, [], Real_GoalVars, Non_Problematic_Vars),
	varsbag(Body, Non_Problematic_Vars, [], Problematic_Vars),
	varsbag(Body, Real_GoalVars, [], UQV),
	!, % No backtracking allowed.
	(
	    (   % If no problematic vars, use cneg_tr (INTNEG).
		Problematic_Vars == [],
		Result = 'fail'
	    )
	;
	    (   % If problematic vars, use cneg_rt (Chan's approach).
		Problematic_Vars \== [],
		cneg_rt(UQV, Body)
	    )
	).

cneg_rt(UQV, Predicate) :- cneg_rt_New(UQV, Predicate).
call_to(Predicate) :- 
	echo_msg_nl(2), 
	echo_msg_nl(2), 
	echo_msg(2, 'call_to :: Predicate', Predicate), 
	echo_msg_nl(2),
	portray_attributes_in_term(2, Predicate),
	echo_msg_nl(2), 
	call_to_aux(Predicate, 0).
call_to(Predicate) :- 
	echo_msg(2, 'call_to (L0) :: Predicate - FAILED - :: Predicate', Predicate),
	echo_msg_nl(2), !, fail. 

call_to_aux(Predicate, Level_In) :-
	goal_is_disjunction(Predicate, G1, G2), !,
	Level is Level_In + 1,
	(
	    (
		(       echo_msg_for_call(2, Level, 'G1 \\/ G2 :: G1', G1), 
			call_to_aux(G1, Level)
		)
	    ;
		(       echo_msg_nl(2), % Differentiate paths.
			echo_msg_for_call(2, Level, 'G1 \\/ G2 :: G2', G2), 
			call_to_aux(G2, Level)
		)
	    )
	;
	    (           echo_msg_for_call(2, Level, 'G1 \\/ G2 - FAILED -', Predicate), 
			!, fail 
	    )
	).

call_to_aux(Predicate, Level_In) :-
	goal_is_conjunction(Predicate, G1, G2), !,
	Level is Level_In + 1,
	(
	    (	echo_msg_for_call(2, Level, 'G1 /\\ G2 :: G1', G1),
		call_to_aux(G1, Level),
		echo_msg_for_call(2, Level, 'G1 /\\ G2 :: G2', G2),
		call_to_aux(G2, Level)
	    )
	;
	    (   echo_msg_for_call(2, Level, 'G1 /\\ G2 - FAILED -', Predicate),
		!, fail 
	    )
	).

call_to_aux(Predicate, Level_In) :-
	Level is Level_In + 1,
	echo_msg_for_call(2, Level, 'Predicate', Predicate),
	call(Predicate).

call_to_aux(Predicate, Level_In) :- 
	Level is Level_In + 1,
	echo_msg_for_call(2, Level, 'Predicate - FAILED -', Predicate),
	!, fail. 

% cneg_tr contains the code transformation needed by cneg_lib
%:- load_compilation_module(library('cneg/cneg_tr')). CUANDO SEA LIBRERIA
:- load_compilation_module(.('cneg_tr')).

% trans_sent/3 makes the transformation.
:- add_sentence_trans(trans_sent/3).
% :- add_clause_trans(trans_clause/3). % Only for debug !!!
