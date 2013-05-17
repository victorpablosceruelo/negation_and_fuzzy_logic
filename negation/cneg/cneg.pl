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

:- use_module(library('cneg/cneg_msgs'), [print_msg/5]).
:- use_module(library('cneg/cneg_aux')).
:- use_module(library('cneg/cneg_diseq')).
:- use_module(library('cneg/cneg_rt')).
:- use_module(library('cneg/cneg_tr')).

:- reexport(library('cneg/cneg_msgs'), [print_msg/5]).
:- reexport(library('cneg/cneg_aux')).
:- reexport(library('cneg/cneg_diseq')).
:- use_module(library('cneg/cneg_rt'), [test_execution/6]).
%:- reexport(library('cneg/cneg_rt'), [cneg_rt/4]).

% To access pre-frontiers from anywhere.
:- multifile cneg_choosen_negation/1.
:- multifile cneg_pre_frontier/10.
:- multifile call_to_predicate/1.
:- meta_predicate call_to_predicate(?). % /1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cneg(Predicate) :-

	print_msg(3, 3, 'statistics', '', (cneg(Predicate))),
	print_msg(3, 3, 'nl', '', ''),

	varsbag(Predicate, [], [], GoalVars), % Compute goalvars. It is used by some methods.
	cneg_rt(Predicate, GoalVars).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

goalvars(Term, GoalVars) :- varsbag(Term, [], [], GoalVars).
call_to_predicate(Predicate) :- call(Predicate).
examine_cneg_pre_frontier(V1, V2, V3, V4, V5, V6, V7, V8, V9) :-
	cneg_pre_frontier(V1, V2, V3, V4, V5, V6, V7, V8, V9).
testing_call_to(Predicate) :-
	call_to_predicate(Predicate).
forget_it(_Whatever).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% cneg_tr contains the code transformations needed by Constructive Negation.
% trans_sent/3 makes the transformation.
:- load_compilation_module(library('cneg/cneg_tr')).
:- add_sentence_trans(cneg_tr:trans_sent/3, 740). % TODO: Right priority?
% :- add_clause_trans(trans_clause/3). % Only for debug !!!
