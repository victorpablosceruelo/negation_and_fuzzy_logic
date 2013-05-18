:- module(ex_with_errors,_,[cneg]).
% :- module(ex_with_errors,_,[.(cneg), .(debugger_pkg)]).

cneg_ignores_preds([tests/0, test_chan/0, test_stuckey/0, echo/1, echo_error/0]).
cneg_choosen_negation(cneg_rt_Chan).
% cneg_choosen_negation(cneg_rt_Stuckey).

test('test_chan', (cneg(r(Y))), 'should_fail', fail_and_forget_it(Y), 'should_fail').
test('test_stuckey', (cneg(s(Z))), 'should_succeed', s(Z), 'should_fail').


s(X) :- disequality(X, Z, []), equality(Z, 3, []). 

q(V, W) :- disequality((Z, W), (3, W), []), equality(V, Z, []).
r(V) :- q(V, _W).

p(X) :- disequality(struct(X, _Y), struct(a, b), []).
t(X) :- (disequality(X, Z, []), equality(Z, 3, []), t(Z)) ; (disequality(X, W, []), equality(W, 5, []), t(W)). 

test_occur_check :- m(X, X).
m(Y, f(Y)).