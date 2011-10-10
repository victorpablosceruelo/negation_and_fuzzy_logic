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
% , [varsbag/4, varsbag_addition/3, append/3, goal_is_conjunction/3, goal_is_disjunction/3, functor_local/4, debug_msg/3]). 
:- use_module(cneg_diseq, [equality/3, disequality/3, 
	diseq_uqv/3, eq_uqv/3, diseq_eqv/3, eq_eqv/3, 
	cneg_diseq_eqv_uqv/5, cneg_eq_eqv_uqv/5]).
:- use_module(cneg_tr).
:- use_module(cneg_rt_Chan, [cneg_rt_Chan/2, cneg_rt_New/2]).
:- use_module(cneg_rt_Stuckey, [cneg_rt_Stuckey/2]).

% Re-export predicates to use them in console.
:- reexport(cneg_aux, [varsbag/4, varsbag_addition/3]).    
:- reexport(cneg_diseq, [equality/3, disequality/3,
	diseq_uqv/3, eq_uqv/3, diseq_eqv/3, eq_eqv/3, 
	cneg_diseq_eqv_uqv/5, cneg_eq_eqv_uqv/5]).
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
	debug_msg(1, 'cneg :: Not implemented conjunction. Error processing ', cneg_tr_aux(Functor, FV_Cneg, FV_In, FV_Out, _Allowed_To_Fail, Result)),
	!, fail.

cneg_tr_aux(Functor, FV_Cneg, FV_In, FV_Out, _Allowed_To_Fail, Result) :-
	goal_is_disjunction(Functor, _Conj_1, _Conj_2), !,
	debug_msg(1, 'cneg :: Not implemented disjunction. Error processing ', cneg_tr_aux(Functor, FV_Cneg, FV_In, FV_Out, _Allowed_To_Fail, Result)),
	!, fail.

cneg_tr_aux(Functor, FV_Cneg, FV_In, FV_Out, Allowed_To_Fail, Result) :-
	functor_local(Functor, Name, Arity, Args),
	New_Arity is Arity + 4,
	cneg_main_and_aux_cl_names(Name, _Main_Cl_Name, Aux_Cl_Name),
	cneg_aux:append(FV_Cneg, FV_In, FV_Aux),
	cneg_aux:append(Args, [FV_Aux |[FV_Out |[Allowed_To_Fail |[Result]]]], New_Args),
	functor_local(New_Functor, Aux_Cl_Name, New_Arity, New_Args),
	debug_msg(1, 'cneg_tr_aux :: call', New_Functor),
	call(New_Functor).

test_if_cneg_rt_needed(GoalVars, Body_First_Unification, Body, Result) :-
	debug_msg(1, 'test_if_cneg_rt_needed :: GoalVars', GoalVars),
	debug_msg(1, 'test_if_cneg_rt_needed :: Body_First_Unification', Body_First_Unification),
	debug_msg(1, 'test_if_cneg_rt_needed :: Body', Body), 
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
		cneg_rt(Body, UQV)
	    )
	).

cneg_rt(Predicate, UQV) :- cneg_rt_New(Predicate, UQV).
call_to(Predicate) :- 
	debug_msg(1, 'call_to :: Predicate', Predicate), 
	call(Predicate).

% cneg_tr contains the code transformation needed by cneg_lib
%:- load_compilation_module(library('cneg/cneg_tr')). CUANDO SEA LIBRERIA
:- load_compilation_module(.('cneg_tr')).

% trans_sent/3 makes the transformation.
:- add_sentence_trans(trans_sent/3).
% :- add_clause_trans(trans_clause/3). % Only for debug !!!


