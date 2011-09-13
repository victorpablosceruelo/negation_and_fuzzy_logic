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
:- use_module(cneg_diseq, [diseq_uqv/3, eq_uqv/3, diseq_eqv/3, eq_eqv/3, 
	cneg_diseq_uqv/5, cneg_eq_uqv/5, cneg_diseq_eqv/5, cneg_eq_eqv/5]).
:- use_module(cneg_tr).
:- use_module(cneg_rt, [cneg_rt/2]).

% Re-export predicates to use them in console.
:- reexport(cneg_diseq, [diseq_uqv/3, eq_uqv/3, diseq_eqv/3, eq_eqv/3, 
	cneg_diseq_uqv/5, cneg_eq_uqv/5, cneg_diseq_eqv/5, cneg_eq_eqv/5]).
:- reexport(cneg_rt, [cneg_rt/2]).   

% To access pre-frontiers from anywhere.
:- multifile cneg_pre_frontier/6.
:- multifile call_to/1.
:- meta_predicate call_to(?). % /1.
%:- export(call_to/1).

cneg(UQV, Functor) :- cneg_aux(Functor, UQV, [], _FV_Out, 'fail', 'true').

cneg_aux(Functor, FV_Cneg, FV_In, FV_Out, _Allowed_To_Fail, Result) :-
	goal_is_conjunction(Functor, _Conj_1, _Conj_2), !,
	debug_msg(1, 'cneg :: Not implemented conjunction. Error processing ', cneg_aux(Functor, FV_Cneg, FV_In, FV_Out, _Allowed_To_Fail, Result)),
	!, fail.

cneg_aux(Functor, FV_Cneg, FV_In, FV_Out, _Allowed_To_Fail, Result) :-
	goal_is_disjunction(Functor, _Conj_1, _Conj_2), !,
	debug_msg(1, 'cneg :: Not implemented disjunction. Error processing ', cneg_aux(Functor, FV_Cneg, FV_In, FV_Out, _Allowed_To_Fail, Result)),
	!, fail.

cneg_aux(Functor, FV_Cneg, FV_In, FV_Out, Allowed_To_Fail, Result) :-
	functor_local(Functor, Name, Arity, Args),
	New_Arity is Arity + 4,
	cneg_main_and_aux_cl_names(Name, _Main_Cl_Name, Aux_Cl_Name),
	cneg_aux:append(FV_Cneg, FV_In, FV_Aux),
	cneg_aux:append(Args, [FV_Aux |[FV_Out |[Allowed_To_Fail |[Result]]]], New_Args),
	functor_local(New_Functor, Aux_Cl_Name, New_Arity, New_Args),
	debug_msg(1, 'cneg_aux :: call', New_Functor),
	call(New_Functor).

%cneg_initialize([], _UQV_Out, 'true', _Cont_Out).
%cneg_test_for_true(UQV_Out, UQV_Out, 'true', 'true').
%cneg_test_for_fail(UQV_Out, UQV_Out, 'fail', 'fail').

call_to(Predicate) :- call(Predicate).

% cneg_tr contains the code transformation needed by cneg_lib
%:- load_compilation_module(library('cneg/cneg_tr')). CUANDO SEA LIBRERIA
:- load_compilation_module(.('cneg_tr')).

% trans_sent/3 makes the transformation.
:- add_sentence_trans(trans_sent/3).
% :- add_clause_trans(trans_clause/3). % Only for debug !!!


