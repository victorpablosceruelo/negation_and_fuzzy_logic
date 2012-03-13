:- module(ex_boole_list,_,[.(cneg)]).
% :- module(ex_boole_list,_,[.(cneg), .(debugger_pkg)]).

cneg_ignores_preds([tests/2, test_parent/2, test_grandparent/2, test_ancestor/2, echo/1]).
cneg_choosen_negation(cneg_rt_Chan).

tests :- binary_list(L), echo(binary_list(L)), cneg([], binary_list(L)), echo_error.
tests :- cneg([], binary_list(L)), echo(binary_list(L)), binary_list(L), echo_error.

echo(Term) :- 
	cneg_diseq_echo(1, 'aux', 'ex_boole_list', 'testing '),
	cneg_diseq_echo(1, '', 'ex_boole_list', Term).
echo_error :- cneg_diseq_echo(1, '', 'ex_boole_list', 'ERROR: test has failed.').


boole(0).
boole(1).

binary_list([]).
binary_list([Y|L]):-
 	boole(Y),  
 	binary_list(L).

no_binary_list(X) :- cneg([], binary_list(X)).