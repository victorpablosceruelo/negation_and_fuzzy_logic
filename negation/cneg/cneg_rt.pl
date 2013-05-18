
:- module(cneg_rt, [cneg_rt/2, cneg_rt_aux/3, test_execution/6], [assertions]).

:- comment(title, "Contructive Negation Runtime Library").
:- comment(author, "V@'{i}ctor Pablos Ceruelo").
:- comment(summary, "This module implements negation predicates for runtime evaluation.").

:- use_module(library(engine(data_facts)),[retractall_fact/1]).
:- use_module(library('cneg/cneg_aux')).
:- use_module(library('cneg/cneg_diseq')).
:- use_module(library('cneg/cneg_frontier')).
:- use_module(library('cneg/cneg_negate_frontier')).

% To avoid problems when test is not defined.
:- multifile test/5.
% To access pre-frontiers from anywhere.
:- multifile cneg_pre_frontier/10.
% To evaluate predicates only from the top package.
:- multifile call_to_predicate/1.
% Debugging or not ... that is the question.
:- multifile file_debug_is_activated/1.
% To determine if a predicate passed or not
:- dynamic test/1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cneg_rt(Goal, GoalVars_In) :-
	print_msg(3, 6, '', 'trace', 'evaluating negation for cneg_rt(Goal, GoalVars_In)'),
	print_msg_with_diseqs(3, 6, 'trace', cneg_rt(Goal, GoalVars_In)),

	cneg_rt_aux(Goal, GoalVars_In, Negated_Frontier),
	!, % Backtracking forbidden.
	generate_conjunction_from_list(Negated_Frontier, Conjunction),
	print_msg(3, 6, '', 'trace', 'evaluating result of cneg_rt(Goal, GoalVars_In)'),
	print_msg_with_diseqs(3, 6, 'trace', cneg_rt(Goal, GoalVars_In)),
	print_msg(3, 6, '', 'trace', 'Result to be evaluated: '),
	print_msg_with_diseqs(3, 6, 'trace', Conjunction),
	print_msg(3, 6, 'separation', '', ''),
	call_to_predicate(Conjunction).	

cneg_rt_aux(Goal, GoalVars_In, Negated_Frontier) :-
	print_msg(3, 3, 'nl', '', ''),
	print_msg(3, 3, '', 'cneg_rt_aux :: Goal', Goal),
	print_msg(3, 3, 'separation', '', ''),
	print_msg(3, 3, 'nl', '', ''),

	varsbag(GoalVars_In, [], [], GoalVars), % Clean up non-vars in GoalVars.
	print_msg(3, 3, '', 'cneg_rt :: (GoalVars)', GoalVars),
	print_msg_with_diseqs(3, 3, 'cneg_rt :: Goal', Goal),
	!, % Reduce the stack's memory by forbidding backtracking.
	compute_frontier(Goal, GoalVars, Frontier),
	!, % Reduce the stack's memory by forbidding backtracking.
	print_msg(3, 3, 'nl', '', ''),
	print_msg(3, 3, 'list', 'cneg_rt :: Frontier', Frontier),
	!,
	negate_frontier(Frontier, GoalVars, Negated_Frontier),
	!, % Reduce the stack's memory by forbidding backtracking.
	print_msg(3, 3, 'separation', '', ''),
	print_msg(3, 3, 'nl', '', ''),
	print_msg(3, 3, '', 'cneg_rt :: (Goal, GoalVars)', (Goal, GoalVars)),
	print_msg(3, 3, 'nl', '', ''),
	print_msg(3, 3, 'list', 'cneg_rt :: Frontier', Frontier),
	print_msg(3, 3, 'nl', '', ''),
	print_msg(3, 3, 'list', 'cneg_rt :: Result (conj)', Negated_Frontier),
	print_msg(3, 3, 'separation', '', ''),
	print_msg(3, 3, 'nl', '', '').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test(_Logo, _Part_1, _Part_1_Should_What, _Part_2, _Part_2_Should_What) :- fail.

test_execution(Logo, Part_1, Part_1_Should_What, Part_2, Part_2_Should_What, Result) :-
	print_msg(1, 3, 'nl', '', ''),
	print_msg(1, 3, 'aux', '', 'Test: '),
	print_msg_with_diseqs(1, 3, Logo, (Part_1, Part_2)),
	print_msg(1, 3, 'nl', '', ''),
	
	test_execution_by_part('1st part: ', Part_1, Part_1_Should_What, Result_Part_1),
	(
	    (
		Result_Part_1 = true,
		test_execution_by_part('2nd part: ', Part_2, Part_2_Should_What, Result_Part_2)
	    )
	;
	    (
		Result_Part_1 = fail, Result_Part_2 = fail, 
		print_msg(1, 3, 'aux', '', 'Not evaluating the 2nd part.')
	    )
	),
	goals_join_by_conjunction(Result_Part_1, Result_Part_2, Result).


test_execution_by_part(Logo, Part, Should_What, Result) :-
	print_msg(1, 3, 'nl', '', ''),
	print_msg_with_diseqs(1, 3, Logo, Part),
	retractall_fact(test(Logo)),
	(
	    (
		call_to_predicate(Part),
		assertz_fact(test(Logo)),
		print_msg_with_diseqs(1, 3, '', Part), 
		(
		    (
			Should_What = 'should_succeed',
			print_msg(1, 3, 'aux', ' -> PASS (', Should_What),
			print_msg(1, 3, '', '', ')'),
			Result = true
		    )
		;
		    (
			Should_What = 'should_fail',
			print_msg(1, 3, 'aux', ' -> ERROR (', Should_What),
			print_msg(1, 3, '', '', ')'),
			print_msg_with_diseqs(1, 3, '', (Part)),
			Result = fail
		    )
		)
	    )
	;
	    (
		(
		    Should_What = 'should_fail',
		    (
			(
			    test(Logo), !, 
			    print_msg(1, 3, 'aux', ' -> ERROR (', Should_What),
			    print_msg(1, 3, '', '', ')'),
			    Result = fail, !, fail
			)
		    ;
			(
			    print_msg(1, 3, 'aux', ' -> PASS (', Should_What),
			    print_msg(1, 3, '', '', ')'),
			    Result = true
			)
		    )
		)
	    ;
		(
		    Should_What = 'should_succeed',
		    (
			(
			    test(_Whatever_passed), !,
			    Result = true, !, fail
			)
		    ;
			(
			    print_msg(1, 3, 'aux', ' -> ERROR (', Should_What),
			    print_msg(1, 3, '', '', ')'),
			    Result = fail, !, fail
			)
		    )
		)
	    )
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
