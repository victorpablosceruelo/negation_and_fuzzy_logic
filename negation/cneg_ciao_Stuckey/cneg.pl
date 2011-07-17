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

% To access predicates from anywhere.
%:- multifile cneg_processed_pred/4.
%:- multifile cneg_dynamic_cl/6.
%:- multifile cneg_static_cl/3.

:- use_module(cneg_aux).    
:- use_module(cneg_lib, [cneg_lib_aux/3, negate_subfrontier/4]).
:- use_module(cneg_diseq, 
	[
	    cneg_diseq/3, cneg_eq/2,
	    remove_universal_quantification/2,
	    keep_universal_quantification/3,
	    put_universal_quantification/1
	]).

% Re-export predicates to use them in console.
:- reexport(cneg_diseq,
	[
	    cneg_diseq/3, cneg_eq/2,
	    remove_universal_quantification/2,
	    keep_universal_quantification/3,
	    put_universal_quantification/1
	]).   
:- reexport(cneg_lib, [ cneg_lib_aux/3 ]).   

:- use_module(cneg_tr).

cneg(Functor) :-
	goal_is_conjunction(Functor, _Conj_1, _Conj_2),
	debug_msg(1, 'cneg :: Not implemented conjunction. Error processing ', Functor),
	!, fail.

cneg(Functor) :-
	goal_is_disjunction(Functor, _Conj_1, _Conj_2),
	debug_msg(1, 'cneg :: Not implemented disjunction. Error processing ', Functor),
	!, fail.

cneg(Functor) :-
	functor_local(Functor, Name, Arity, Args),
	cneg_main_cl_name(Name, Main_Cl_Name),
	functor_local(New_Functor, Main_Cl_Name, Arity, Args),
	call(New_Functor).

cneg_initialize([], true).
cneg_test(_Any, true).


% cneg_tr contains the code transformation needed by cneg_lib
%:- load_compilation_module(library('cneg/cneg_tr')). CUANDO SEA LIBRERIA
:- load_compilation_module(.('cneg_tr')).

% trans_sent/3 makes the transformation.
:- add_sentence_trans(trans_sent/3).
:- add_clause_trans(trans_clause/3). % Only for debug !!!


