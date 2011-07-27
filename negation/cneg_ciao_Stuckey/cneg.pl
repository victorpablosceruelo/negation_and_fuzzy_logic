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
:- use_module(cneg_diseq, [diseq/3, cneg_diseq/7, cneg_eq/7]).
:- use_module(cneg_tr).

% Re-export predicates to use them in console.
:- reexport(cneg_diseq, [diseq/3, cneg_diseq/7, cneg_eq/7]).   

cneg(UQV, Functor) :- cneg_aux(Functor, UQV, _FV_Out, 'fail', 'fail', 'fail').

cneg_aux(Functor, FV_In, FV_Out, _Allowed_To_Fail, Cont_In, Cont_Out) :-
	goal_is_conjunction(Functor, _Conj_1, _Conj_2), !,
	debug_msg(1, 'cneg :: Not implemented conjunction. Error processing ', cneg_aux(Functor, FV_In, FV_Out, Cont_In, Cont_Out)),
	!, fail.

cneg_aux(Functor, FV_In, FV_Out, _Allowed_To_Fail, Cont_In, Cont_Out) :-
	goal_is_disjunction(Functor, _Conj_1, _Conj_2), !,
	debug_msg(1, 'cneg :: Not implemented disjunction. Error processing ', cneg_aux(Functor, FV_In, FV_Out, Cont_In, Cont_Out)),
	!, fail.

cneg_aux(Functor, FV_In, FV_Out, Allowed_To_Fail, Cont_In, Cont_Out) :-
	functor_local(Functor, Name, Arity, Args),
	New_Arity is Arity + 5,
	cneg_main_and_aux_cl_names(Name, _Main_Cl_Name, Aux_Cl_Name),
	append(Args, [FV_In |[FV_Out |[Allowed_To_Fail |[Cont_In |[Cont_Out]]]]], New_Args),
	functor_local(New_Functor, Aux_Cl_Name, New_Arity, New_Args),
	call(New_Functor).

%cneg_initialize([], _UQV_Out, 'true', _Cont_Out).
%cneg_test_for_true(UQV_Out, UQV_Out, 'true', 'true').
%cneg_test_for_fail(UQV_Out, UQV_Out, 'fail', 'fail').


% cneg_tr contains the code transformation needed by cneg_lib
%:- load_compilation_module(library('cneg/cneg_tr')). CUANDO SEA LIBRERIA
:- load_compilation_module(.('cneg_tr')).

% trans_sent/3 makes the transformation.
:- add_sentence_trans(trans_sent/3).
% :- add_clause_trans(trans_clause/3). % Only for debug !!!


