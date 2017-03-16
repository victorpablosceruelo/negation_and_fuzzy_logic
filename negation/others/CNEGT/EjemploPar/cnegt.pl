%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Package CNEGT (complete Constructive NEGation with  %
%  Typed arguments) for negating a predicate whose     %
%  arguments can be typed                              %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- meta_predicate cnegt(goal).
%  for a goal which has to be module expanded

:- use_package(.(cneg_types)).
%  CNEGT uses package CNEG_TYPES

:- use_module(engine(internals)).
%  for the predicate term_to_meta/2

:- use_module(library(terms_vars),[varset/2,varsbag/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% change_name(OriginalName,TransfName). TransfName is
% the same as OriginalName but concatenated to the 
% sufix '_check_types'
change_name(NamePred,Name_inst) :-
	atom_concat(NamePred,'_check_types',Name_inst).

% cnegt(Predicate) instantiates the resulting vars of
% cneg/1 by calling to Predicate_check_types, with the
% types of the args as predicates in its body
cnegt(Pred):-
       % cneg(Pred),
	term_to_meta(NoExpanded,Pred),
	NoExpanded=..[Name|Args],
	change_name(Name,Name_inst),
        PredInst=..[Name_inst|Args],
        term_to_meta(PredInst,Expanded),
        call(Expanded),
	cneg(Pred).


% compilation module that contains the predicate
% same_pred/3
:- load_compilation_module(.(cnegt_tr)).

% same_pred/3 is a predicate that lets the code as
% the original; it means that does a neutral expansion
:- add_sentence_trans(same_pred/3).
