% :- module(ex_ancestor_and_grandparent,_,[.(cneg), .(debugger_pkg)]).
:- module(ex_ancestor_and_grandparent,_,[.(cneg)]).

cneg_ignores_preds([tests/2, test_parent/2, test_grandparent/2, test_ancestor/2, echo/1, echo_error/0, echo_nl/0]).
cneg_choosen_negation(cneg_rt_Chan).

tests(X, Y) :- test_parent(X, Y).
tests(X, Y) :- test_grandparent(X, Y).
tests(X, Y) :- test_ancestor(X, Y).

echo_nl :- cneg_diseq_echo(1, 'nl', 'ex_peano_queens', '', '').
echo(Term) :- 
	cneg_diseq_echo(1, 'aux', 'ex_ancestor_and_grandparent', 'testing ', Term),
	echo_nl.

echo_error :- cneg_diseq_echo(1, '', 'ex_ancestor_and_grandparent', 'ERROR: test has failed.', '').

test_parent(X, Y) :- cneg([], parent(X, Y)), echo(parent(X, Y)), parent(X, Y), echo_error.
test_parent(X, Y) :- parent(X, Y), echo(parent(X, Y)), cneg([], parent(X, Y)), echo_error.
test_grandparent(X, Y) :- cneg([], grandparent(X, Y)), echo(grandparent(X, Y)), grandparent(X, Y), echo_error.
test_grandparent(X, Y) :- grandparent(X, Y), echo(grandparent(X, Y)), cneg([], grandparent(X, Y)), echo_error.
test_ancestor(X, Y) :- cneg([], ancestor(X, Y)), echo(ancestor(X, Y)), ancestor(X, Y), echo_error.
test_ancestor(X, Y) :- ancestor(X, Y), echo(ancestor(X, Y)), cneg([], ancestor(X, Y)), echo_error.

parent(marcus, daniel).

parent(bob,oscar).
parent(oscar,joan).

parent(tom, john).
parent(john, mary).
parent(john, peter).
parent(mary, joe).
parent(peter, susan).

ancestor(X, Y):-
 	parent(X, Y).
ancestor(X, Y):-
	parent(X, Z),
	ancestor(Z, Y).

no_ancestor(X, Y) :- cneg([], ancestor(X, Y)).

grandparent(X,Y):- 
    parent(X,Z),
    parent(Z,Y).

no_grandparent(X,Y):- cneg([], grandparent(X, Y)).

parent1(john, mary).
%parent1(john, peter).
parent1(mary, joe).
%parent1(peter, susan).

ancestor1(X, Y):-
 	parent1(X, Y).
ancestor1(X, Y):-
	parent1(X, Z),
	ancestor1(Z, Y).

grandparent1(X,Y):- 
    parent1(X,Z),
    parent1(Z,Y).
