:- module(ex_boole_list,_,[cneg]).
% :- module(ex_boole_list,_,[.(cneg), .(debugger_pkg)]).

cneg_ignores_preds([tests/2, test_parent/2, test_grandparent/2, test_ancestor/2, echo/1]).

maxValue(N) :- maximum_value(N, s(s(s(s(s(s(s(s(s(s(0))))))))))).

test('basic1', (maxValue(N), binary_list(N, L)), 'should_succeed', cneg(binary_list(N, L)), 'should_fail').
test('basic2', (maxValue(N), cneg(binary_list(N, L))), 'should_succeed', binary_list(N, L), 'should_fail').

%%%%%%%%%%%%%%%%%%%%

boole(0).
boole(1).

binary_list(0, []).
binary_list(s(N), [X|L]):-
 	boole(X),  
 	binary_list(N, L).

no_binary_list(N, L) :- cneg(binary_list(N, L)).


