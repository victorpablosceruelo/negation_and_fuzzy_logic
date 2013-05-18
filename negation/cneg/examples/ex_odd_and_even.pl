:- module(ex_odd_and_even,_,[cneg]).
%:- module(ex_odd_and_even,_,[.(cneg), .(debugger_pkg)]).

test('test', even(X), 'should_succeed', odd(X), 'should_fail').
test('test', odd(X), 'should_succeed', even(X), 'should_fail').

even(0).
even(s(s(X))):- even(X).

odd(X):- cneg(even(X)).

