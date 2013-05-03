:- module(ex_diseqs,_,[cneg]).
% :- module(ex_diseqs,_,[.(cneg), .(debugger_pkg)]).

cneg_ignores_preds([tests/0, logo/1, tests_fail/2, tests_succeed/2]).

tests :- 
	print_msg(1, 3, 'nl', '', ''),
	print_msg(1, 3, '', 'Tests that should fail', ''), 
	tests_fail(Logo, Vars, First_Part, Second_Part), 
	test_execution(Logo, Vars, First_Part, Second_Part, 'should_fail'),
	fail.

tests :- 
	print_msg(1, 3, 'nl', '', ''),
	print_msg(1, 3, 'nl', '', ''),
	print_msg(1, 3, '', 'Tests that should succeed', ''),
	tests_succeed(Logo, Vars, First_Part, Second_Part), 
	test_execution(Logo, Vars, First_Part, Second_Part, 'should_succeed'),
	fail.

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
tests_fail('f20', [], disequality(f/1, f/1, []), true).
tests_fail('f21', [], disequality(T1, f/1, []), equality(T1, f/1, [])).
tests_fail('f22', [], disequality(T1, f/1, []), equality(T1, f(a), [])).
tests_fail('f23', [], equality(T1, f/1, []), disequality(T1, f/1, [])).
tests_fail('f24', [], equality(T1, f(a), []), disequality(T1, f/1, [])).


tests_succeed('s01', [T1 |[ T2]], (T1 = a, T2 = a), equality(T1, T2, [])).
tests_succeed('s02', [T1 |[ T2]], equality(T1, T2, []), equality(T1, T2, [])).
tests_succeed('s03', [T1 |[ T2]], disequality(T1, T2, []), disequality(T1, T2, [])).
tests_succeed('s04', [T1 |[ T2]], equality(T1, T2, []), equality(T1, T2, [T1])).
% tests_succeed('s05', [T1 |[ T2]], equality(T1, s(T3), []), disequality(T1, T2, [T3])).
tests_succeed('s06', [T1 |[ T2]], (disequality(T1, s(T3), [T3]), disequality(T1, s(T2), [T2])), equality(T1, T2, [])).
tests_succeed('s07', [X], true, (disequality(X, a, []), disequality(X, b, []), disequality(X, c, []))).
tests_succeed('s20', [], disequality(a, f/1, []), true).
tests_succeed('s21', [T1], disequality(T1, f/1, []), true).
tests_succeed('s22', [], disequality(g/1, f/1, []), true).
tests_succeed('s23', [T1], disequality(T1, f/1, []), disequality(T1, g/1, [])).
tests_succeed('s24', [T1], disequality(T1, f/1, []), equality(T1, f(a, b, c), [])).
tests_succeed('s25', [T1], equality(T1, f(a, b, c), []), disequality(T1, f/1, [])).
tests_succeed('s30', [X], disequality(X, f(a, b, c), []), (disequality(X, f/1, []), disequality(X, f/3, []))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
