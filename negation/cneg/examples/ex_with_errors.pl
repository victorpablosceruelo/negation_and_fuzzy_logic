:- module(ex_with_errors,_,[cneg]).
% :- module(ex_with_errors,_,[.(cneg), .(debugger_pkg)]).

cneg_ignores_preds([tests/0, test_chan/0, test_stuckey/0, echo/1, echo_error/0]).
cneg_choosen_negation(cneg_rt_Chan).
% cneg_choosen_negation(cneg_rt_Stuckey).

tests :- test_chan ; test_stuckey.

test_chan :- cneg([], r(Y)), echo(r(Y)), r(Y), echo_error.
test_stuckey :- cneg([], s(X)), echo(s(X)), s(X), echo_error.

echo(Term) :- 
	cneg_diseq_echo(1, 'aux', 'ex_ancestor_and_grandparent', 'testing ', ''),
	cneg_diseq_echo(1, '', 'ex_ancestor_and_grandparent', Term, '').
echo_error :- cneg_diseq_echo(1, '', 'ex_ancestor_and_grandparent', 'ERROR: test has failed.', '').


s(X) :- disequality(X, Z, []), equality(Z, 3, []). 

q(V, W) :- disequality((Z, W), (3, W), []), equality(V, Z, []).
r(V) :- q(V, _W).

p(X) :- disequality(struct(X, _Y), struct(a, b), []).

test_occur_check :- m(X, X).
m(Y, f(Y)).