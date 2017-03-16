% :- module(ex_ancestor_and_grandparent,_,[.(cneg), .(debugger_pkg)]).
:- module(ex_ancestor_and_grandparent,_,[cneg]).

cneg_ignores_preds([tests/2, test_parent/2, test_grandparent/2, test_ancestor/2, echo/1, echo_error/0, echo_nl/0]).

test(parent, cneg(parent(X, Y)), 'should_succeed', parent(X, Y), 'should_fail').
test(parent, parent(X, Y), 'should_succeed', cneg(parent(X, Y)), 'should_fail').
test(grandparent, cneg(grandparent(X, Y)), 'should_succeed', grandparent(X, Y), 'should_fail').
test(grandparent, grandparent(X, Y), 'should_succeed', cneg(grandparent(X, Y)), 'should_fail').
test(ancestor, cneg(ancestor(X, Y)), 'should_succeed', ancestor(X, Y), 'should_fail').
test(ancestor, ancestor(X, Y), 'should_succeed', cneg(ancestor(X, Y)), 'should_fail').

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

no_ancestor(X, Y) :- cneg(ancestor(X, Y)).

grandparent(X,Y):- 
    parent(X,Z),
    parent(Z,Y).

no_grandparent(X,Y):- cneg(grandparent(X, Y)).

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
