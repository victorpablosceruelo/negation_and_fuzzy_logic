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

:- use_module(library('cneg/cneg_tr')).
:- use_module(library('cneg/cneg_aux')).
:- use_module(library('cneg/cneg_diseq')).
:- use_module(library('cneg/cneg_rt')).

:- reexport(library('cneg/cneg_diseq')).
:- reexport(library('cneg/cneg_rt'), [cneg_rt/4]).

% To access pre-frontiers from anywhere.
:- multifile cneg_choosen_negation/1.
:- multifile cneg_pre_frontier/6.
:- multifile call_to_predicate/1.
:- meta_predicate call_to_predicate(?). % /1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cneg(Predicate) :-

	echo_msg(2, 'statistics', 'cneg_rt', '', (cneg(Predicate))),
	echo_msg(2, 'nl', 'cneg_rt', '', ''),

	varsbag(Predicate, [], [], GoalVars), % Compute goalvars. It is used by some methods.
	generate_empty_trace(Trace), % This is for debugging.
	cneg_aux(Predicate, GoalVars).

cneg_aux(Predicate, GoalVars) :-
	cneg_rt(Predicate, GoalVars, 0, Trace). % 0 is the depth level.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

goalvars(Term, GoalVars) :- varsbag(Term, [], [], GoalVars).
call_to_predicate(Predicate) :- call(Predicate).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% cneg_tr contains the code transformations needed by Constructive Negation.
% trans_sent/3 makes the transformation.
:- load_compilation_module(library('cneg/cneg_tr')).
:- add_sentence_trans(trans_sent/3, 740). % TODO: Right priority?
% :- add_clause_trans(trans_clause/3). % Only for debug !!!
