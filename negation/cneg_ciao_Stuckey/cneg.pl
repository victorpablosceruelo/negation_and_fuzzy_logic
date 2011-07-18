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
:- use_module(cneg_diseq, [cneg_diseq/6, cneg_eq/6]).
:- use_module(cneg_tr).

% Re-export predicates to use them in console.
:- reexport(cneg_diseq, [cneg_diseq/6, cneg_eq/6]).   

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
	cneg_main_and_aux_cl_names(Name, Main_Cl_Name, _Aux_Cl_Name),
	functor_local(New_Functor, Main_Cl_Name, Arity, Args),
	call(New_Functor).

cneg_initialize([], true).
cneg_test(_Any, true).


% cneg_tr contains the code transformation needed by cneg_lib
%:- load_compilation_module(library('cneg/cneg_tr')). CUANDO SEA LIBRERIA
:- load_compilation_module(.('cneg_tr')).

% trans_sent/3 makes the transformation.
:- add_sentence_trans(trans_sent/3).
% :- add_clause_trans(trans_clause/3). % Only for debug !!!


