:- module(ex_diseqs,_,[cneg]).
% :- module(ex_diseqs,_,[.(cneg), .(debugger_pkg)]).

cneg_ignores_preds([tests/0, logo/1, tests_fail/2, tests_succeed/2]).

tests :- 
	print_msg(1, 3, 'nl', '', ''),
	print_msg(1, 3, 'nl', 'Tests that should fail:', ''), 
	print_msg(1, 3, 'nl', '', ''),
	tests_fail(Logo, Vars, First_Part, Second_Part), 
	test_execution(Logo, Vars, First_Part, Second_Part, 'should_fail'),
	fail.

tests :- 
	print_msg(1, 3, 'nl', '', ''),
	print_msg(1, 3, 'nl', '', ''),
	print_msg(1, 3, 'nl', 'Tests that should succeed:', ''),
	print_msg(1, 3, 'nl', '', ''),
	tests_succeed(Logo, Vars, First_Part, Second_Part), 
	test_execution(Logo, Vars, First_Part, Second_Part, 'should_succeed'),
	fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test_execution(Logo, Vars, First_Part, Second_Part, Should_What) :-
	(
	    (
		print_msg(1, 3, 'nl', '', ''),
		print_msg(1, 3, '', 'Test', Logo),
		print_msg(3, 3, 'aux', '', '(vars with attrs) '),
		print_vars_diseqs(3, '', Vars), 
		print_msg(3, 3, 'nl', '', ''),
		print_msg(3, 3, 'aux', '1st: ', First_Part), 
		call(First_Part),
		print_msg(3, 3, 'aux', '', ' --> (vars with attrs) '),
		print_vars_diseqs(3, '', Vars), 
		print_msg(3, 3, 'nl', '', ''),
		print_msg(3, 3, 'aux', '2nd: ', Second_Part), 
		call(Second_Part),
		print_msg(3, 3, 'aux', ' --> (vars with attrs) ', ''), 
		print_vars_diseqs(3, '', Vars), 
		print_msg(3, 3, 'nl', '', ''), !,
		
		(
		    (
			Should_What = 'should_succeed',
			print_msg(1, 3, '', 'PASS', '')
		    )
		;
		    (
			Should_What = 'should_fail',
			print_msg(1, 3, '', 'ERROR', '')
		    )
		),
		!
	    )
	;
	    (
		(
		    (
			Should_What = 'should_succeed',
			print_msg(1, 3, '', 'ERROR', '')
		    )
		;
		    (
			Should_What = 'should_fail',
			print_msg(1, 3, '', 'PASS', '')
		    )
		)
	    )
	), !.

	    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tests_fail('f01', [T1 |[ T2]], (T1 = a, T2 = b), equality(T1, T2, [])).
tests_fail('f02', [T1 |[ T2]], (T1 = a, T2 = a), disequality(T1, T2, [])).
tests_fail('f03', [T1 |[ T2]], equality(T1, T2, []), disequality(T1, T2, [])).
tests_fail('f04', [T1 |[ T2]], disequality(T1, T2, []), equality(T1, T2, [])).
tests_fail('f05', [T1 |[ T2]], equality(T2, s(T1), []), equality(T1, T2, [])).
tests_fail('f06', [T1 |[ T2]], equality(T1, T2, [T2]), true).
tests_fail('f07', [T1 |[ T2]], equality(T1, s(T3), []), equality(T1, T2, [T3])).
tests_fail('f08', [T1 |[ T2]], equality(T1, s(a), []), equality(T1, T2, [T2])).
tests_fail('f09', [T1 |[ T2]], equality(T1, s(_T3), []), equality(T1, T2, [T2])).
tests_fail('f10', [T1 |[ T2]], disequality(T1, T2, [T2]), true).

tests_succeed('s01', [T1 |[ T2]], (T1 = a, T2 = a), equality(T1, T2, [])).
tests_succeed('s02', [T1 |[ T2]], equality(T1, T2, []), equality(T1, T2, [])).
tests_succeed('s03', [T1 |[ T2]], disequality(T1, T2, []), disequality(T1, T2, [])).
tests_succeed('s04', [T1 |[ T2]], equality(T1, T2, []), equality(T1, T2, [T1])).
% tests_succeed('s05', [T1 |[ T2]], equality(T1, s(T3), []), disequality(T1, T2, [T3])).
tests_succeed('s06', [T1 |[ T2]], (disequality(T1, s(T3), [T3]), disequality(T1, s(T2), [T2])), equality(T1, T2, [])).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
