:- module(ex_diseqs,_,[.(cneg)]).
% :- module(ex_diseqs,_,[.(cneg), .(debugger_pkg)]).

cneg_ignores_preds([tests/0, logo/1, tests_fail/2, tests_succeed/2]).

tests :- 
	cneg_diseq_echo(1, 'nl', 'ex_diseqs', ''),
	cneg_diseq_echo(1, '', 'ex_diseqs', 'Tests that should fail:'), 
	tests_fail(T1, T2), 
	cneg_diseq_echo(1, '', 'ex_diseqs', test_result(T1, T2)), 
	cneg_diseq_echo(1, '', 'ex_diseqs', 'ERROR').

tests :- 
	cneg_diseq_echo(1, 'nl', 'ex_diseqs', ''),
	cneg_diseq_echo(1, 'nl', 'ex_diseqs', ''),
	cneg_diseq_echo(1, '', 'ex_diseqs', 'Tests that should succeed:'),
	tests_succeed(T1, T2), 
	cneg_diseq_echo(1, '', 'ex_diseqs', test_result(T1, T2)), 
	fail.
tests :- 	
	cneg_diseq_echo(1, 'nl', 'ex_diseqs', ''),
	cneg_diseq_echo(1, '', 'ex_diseqs', 'if test_result not shown there is an ERROR.').

logo(Text) :- cneg_diseq_echo(1, 'aux', 'ex_diseqs', Text).
just_nl :- 	cneg_diseq_echo(1, 'nl', 'ex_diseqs', '').

tests_fail(T1, T2) :- logo('f01'), T1 = a, T2 = b, equality(T1, T2, []).
tests_fail(T1, T2) :- logo('f02'), T1 = a, T2 = a, disequality(T1, T2, []).
tests_fail(T1, T2) :- logo('f03'), equality(T1, T2, []), disequality(T1, T2, []).
tests_fail(T1, T2) :- logo('f04'), disequality(T1, T2, []), just_nl, cneg_diseq_echo(1, '', 'ex_diseqs', f04_intermediate(T1, T2)), nl, equality(T1, T2, []).
tests_fail(T1, T2) :- logo('f05'), equality(T2, s(T1), []), equality(T1, T2, []).
tests_fail(T1, T2) :- logo('f06'), equality(T1, T2, [T2]).
tests_fail(T1, T2) :- logo('f07'), equality(T1, s(T3), []), equality(T1, T2, [T3]).
tests_fail(T1, T2) :- logo('f08'), equality(T1, s(a), []), equality(T1, T2, [T2]).
tests_fail(T1, T2) :- logo('f09'), equality(T1, s(_T3), []), equality(T1, T2, [T2]).
tests_fail(T1, T2) :- logo('f10'), disequality(T1, T2, [T2]).

tests_succeed(T1, T2) :- logo('s01'), T1 = a, T2 = a, equality(T1, T2, []).
tests_succeed(T1, T2) :- logo('s02'), equality(T1, T2, []), equality(T1, T2, []).
tests_succeed(T1, T2) :- logo('s03'), disequality(T1, T2, []), disequality(T1, T2, []).
tests_succeed(T1, T2) :- logo('s04'), equality(T1, T2, []), equality(T1, T2, [T1]).
tests_succeed(T1, T2) :- logo('s05'), equality(T1, s(T3), []), disequality(T1, T2, [T3]).

