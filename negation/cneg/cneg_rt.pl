
:- module(cneg_rt, [cneg_rt/2, cneg_rt_aux/3, test_execution/5], [assertions]).

:- comment(title, "Contructive Negation Runtime Library").
:- comment(author, "V@'{i}ctor Pablos Ceruelo").
:- comment(summary, "This module implements negation predicates for runtime evaluation.").

:- use_module(library(engine(data_facts)),[retractall_fact/1]).
:- use_module(library('cneg/cneg_aux')).
:- use_module(library('cneg/cneg_diseq')).
:- use_module(library('cneg/cneg_frontier')).
:- use_module(library('cneg/cneg_negate_frontier')).

% To access pre-frontiers from anywhere.
:- multifile cneg_pre_frontier/9.
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
	% print_msg(3, 3, '', 'cneg_rt', cneg_rt(Goal, GoalVars_In)),
	print_msg(3, 6, '', 'cneg_rt :: trace', cneg_rt(Goal, GoalVars_In)),

	cneg_rt_aux(Goal, GoalVars_In, Negated_Frontier),
	!, % Backtracking forbidden.
	generate_conjunction_from_list(Negated_Frontier, Conjunction),
	print_msg(3, 6, '', 'trace', 'evaluating result of cneg_rt(Goal, GoalVars_In)'),
	print_msg(3, 6, '', 'trace', cneg_rt(Goal, GoalVars_In)),
	print_msg(3, 6, '', 'trace', Conjunction),
	call_to_predicate(Conjunction).	

cneg_rt_aux(Goal, GoalVars_In, Negated_Frontier) :-
	print_msg(3, 3, 'nl', '', ''),
	print_msg(3, 3, '', 'cneg_rt_aux :: Goal', Goal),
	print_msg(3, 3, 'separation', '', ''),
	print_msg(3, 3, 'nl', '', ''),

	varsbag(GoalVars_In, [], [], GoalVars), % Clean up non-vars in GoalVars.
	print_msg(3, 3, '', 'cneg_rt :: (GoalVars)', GoalVars),
	print_msg(3, 3, 'aux', 'cneg_rt :: Goal', Goal),
	get_list_of_disequalities_in_vars(Goal, Goal_Diseqs_For_Printing),
	print_msg(1, 3, 'one-line', ' (diseqs) ', Goal_Diseqs_For_Printing),
	print_msg(3, 3, 'nl', '', ''),
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

test_execution(Logo, Part_1, Part_1_Should_What, Part_2, Part_2_Should_What) :-
	print_msg(1, 3, 'nl', '', ''),
	print_msg(1, 3, 'aux', '', 'Test: '),
	print_msg_with_diseqs(1, 3, Logo, (Part_1, Part_2)),
	print_msg(1, 3, 'nl', '', ''),
	
	test_execution_by_part('1st part: ', Part_1, Part_1_Should_What),
	test_execution_by_part('2nd part: ', Part_2, Part_2_Should_What).


test_execution_by_part(Logo, Part, Should_What) :-
	print_msg(1, 3, 'nl', '', ''),
	print_msg_with_diseqs(1, 3, Logo, Part),
	retractall_fact(test(_Whatever)),
	(
	    (
		call_to_predicate(Part),
		assertz_fact(test('passed')),
		print_msg_with_diseqs(1, 3, '', Part), 
		(
		    (
			Should_What = 'should_succeed',
			print_msg(1, 3, '', ' -> PASS', '')
		    )
		;
		    (
			Should_What = 'should_fail',
			print_msg(1, 3, '', ' -> ERROR', ''),
			print_msg_with_diseqs(1, 3, '', (Part))
		    )
		)
	    )
	;
	    (
		(
		    Should_What = 'should_fail',
		    (
			(
			    test(_Whatever_passed), !, 
			    print_msg(1, 3, '', ' -> ERROR', '')
			)
		    ;
			(
			    print_msg(1, 3, '', ' -> PASS', '')
			)
		    )
		)
	    ;
		(
		    Should_What = 'should_succeed',
		    (
			(
			    test(_Whatever_passed), !
			)
		    ;
			(
			    print_msg(1, 3, '', ' -> ERROR', '')
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
