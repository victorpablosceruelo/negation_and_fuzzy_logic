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
:- multifile cneg_processed_pred/4.
:- multifile cneg_dynamic_cl/6.
:- multifile cneg_static_cl/3.

:- use_module(cneg_aux).    
:- use_module(cneg_lib, [cneg_lib_aux/3, negate_subfrontier/4]).
:- use_module(cneg_diseq, 
	[
	    cneg_diseq/3, cneg_eq/2,
	    put_universal_quantification/1,
	    remove_universal_quantification/2,
	    keep_universal_quantification/1,
	    quantify_universally_new_vars/2
	]).

% Re-export predicates to use them in console.
:- reexport(cneg_diseq,
	[
	    cneg_diseq/3, cneg_eq/2,
	    put_universal_quantification/1,
	    remove_universal_quantification/2,
	    keep_universal_quantification/1
	]).   
:- reexport(cneg_lib, [ cneg_lib_aux/3 ]).   

cneg(Predicate) :- cneg_aux(Predicate, []).

cneg_aux(Predicate, Universal_Vars) :- 
	cneg_lib_aux(Predicate, Universal_Vars, Result), 
	!, call(Result).

cneg_static_predicate_call(_Goal, _SourceFileName, 0).
cneg_static_predicate_call(Goal, SourceFileName, Occurences) :-
	Occurences \== 0,
%	debug_msg('cneg_static_predicate_call :: IN', cneg_static_pred(Goal, SourceFileName, Occurences)),
	cneg_static_cl(Goal, SourceFileName, Occurences), 
	NewOccurences is Occurences -1,
	cneg_static_predicate_call(Goal, SourceFileName, NewOccurences).



% cneg_tr contains the code transformation needed by cneg_lib
%:- load_compilation_module(library('cneg/cneg_tr')). CUANDO SEA LIBRERIA
:- load_compilation_module(.('cneg_tr')).

% trans_sent/3 makes the transformation.
:- add_sentence_trans(trans_sent/3).
% :- add_clause_trans(trans_clause/3). % Only for debug !!!


