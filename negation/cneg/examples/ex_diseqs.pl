:- module(ex_diseqs,_,[cneg]).
% :- module(ex_diseqs,_,[.(cneg), .(debugger_pkg)]).

cneg_ignores_preds([tests/0, logo/1, tests_fail/2, tests_succeed/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test('f01', (T1 = a, T2 = b), 'should_succeed', equality(T1, T2, []), 'should_fail').
test('f02', (T1 = a, T2 = a), 'should_succeed', disequality(T1, T2, []), 'should_fail').
test('f03', equality(T1, T2, []), 'should_succeed', disequality(T1, T2, []), 'should_fail').
test('f04', disequality(T1, T2, []), 'should_succeed', equality(T1, T2, []), 'should_fail').
test('f05', equality(T2, s(T1), []), 'should_succeed', equality(T1, T2, []), 'should_fail').
test('f06', equality(T1, T2, [T2]), 'should_succeed', fail_and_forget_it(T1), 'should_fail').
test('f07', (equality(T1, s(T3), []), equality(T1, T2, [T3])), 'should_fail', fail_and_forget_it(T2), 'should_fail').
test('f08', equality(T1, s(a), []), 'should_succeed', equality(T1, T2, [T2]), 'should_fail').
test('f09', equality(T1, s(_T3), []), 'should_succeed', equality(T1, T2, [T2]), 'should_fail').
test('f10', disequality(T1, T2, [T2]), 'should_succeed', fail_and_forget_it(T1), 'should_fail').
test('f20', disequality(f/1, f/1, []), 'should_succeed', true, 'should_fail').
test('f21', disequality(T1, f/1, []), 'should_succeed', equality(T1, f/1, []), 'should_fail').
test('f22', disequality(T1, f/1, []), 'should_succeed', equality(T1, f(a), []), 'should_fail').
test('f23', equality(T1, f/1, []), 'should_succeed', disequality(T1, f/1, []), 'should_fail').
test('f24', equality(T1, f(a), []), 'should_succeed', disequality(T1, f/1, []), 'should_fail').


test('s01', (T1 = a, T2 = a), 'should_succeed', equality(T1, T2, []), 'should_succeed').
test('s02', equality(T1, T2, []), 'should_succeed', equality(T1, T2, []), 'should_succeed').
test('s03', disequality(T1, T2, []), 'should_succeed', disequality(T1, T2, []), 'should_succeed').
test('s04', equality(T1, T2, []), 'should_succeed', equality(T1, T2, [T1]), 'should_succeed').
% test('s05', equality(T1, s(T3), []), 'should_succeed', disequality(T1, T2, [T3]), 'should_succeed').
test('s06', disequality(T1, s(T3), [T3]), 'should_succeed', (disequality(T1, s(T2), [T2]), equality(T1, T2, [])), 'should_succeed').
test('s07', (disequality(X, a, []), disequality(X, b, []), disequality(X, c, [])), 'should_succeed', fail_and_forget_it(a), 'should_fail').
test('s20', disequality(a, f/1, []), 'should_succeed', fail_and_forget_it(a), 'should_fail').
test('s21', disequality(T1, f/1, []), 'should_succeed', fail_and_forget_it(T1), 'should_fail').
test('s22', disequality(g/1, f/1, []), 'should_succeed', fail_and_forget_it(a), 'should_fail').
test('s23', disequality(T1, f/1, []), 'should_succeed', disequality(T1, g/1, []), 'should_succeed').
test('s24', disequality(T1, f/1, []), 'should_succeed', equality(T1, f(a, b, c), []), 'should_succeed').
test('s25', equality(T1, f(a, b, c), []), 'should_succeed', disequality(T1, f/1, []), 'should_succeed').
test('s30', disequality(X, f(a, b, c), []), 'should_succeed', (disequality(X, f/1, []), disequality(X, f/3, [])), 'should_succeed').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
