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

% To avoid problems when test is not defined.
:- multifile test/5.
% To access pre-frontiers from anywhere.
:- multifile cneg_pre_frontier/10.
% To be able to call predicates in the main file.
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
call_to_predicate(Predicate) :- 
	print_msg(3, 3, '', 'call(Predicate)', (call(Predicate))),
	call(Predicate).
examine_cneg_pre_frontier(V1, V2, V3, V4, V5, V6, V7, V8, V9, V10) :-
	cneg_pre_frontier(V1, V2, V3, V4, V5, V6, V7, V8, V9, V10).
testing_call_to(Predicate) :-
	call_to_predicate(Predicate).
fail_and_forget_it(_Whatever) :- fail.

maximum_value(0, _Max).
maximum_value(s(X), s(Y)) :- maximum_value(X, Y).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tests :- 
	print_msg(1, 3, 'nl', '', ''),
	print_msg(1, 3, 'nl', '', ''),
	test(Logo, Part_1, Part_1_Should_What, Part_2, Part_2_Should_What), 
	test_execution(Logo, Part_1, Part_1_Should_What, Part_2, Part_2_Should_What, Result),
	(
	    (
		Result = true,
		fail % Go for more tests
	    )
	;
	    (
		Result = fail,
		!, fail % Stop testing.
	    )
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% cneg_tr contains the code transformations needed by Constructive Negation.
% trans_sent/3 makes the transformation.
:- load_compilation_module(library('cneg/cneg_tr')).
:- add_sentence_trans(cneg_tr:trans_sent/3, 740). % TODO: Right priority?
% :- add_clause_trans(trans_clause/3). % Only for debug !!!
