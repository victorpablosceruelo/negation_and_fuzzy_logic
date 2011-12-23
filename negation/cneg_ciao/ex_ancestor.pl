:- module(ex_ancestor_grandparent,_,[.(cneg), debugger_pkg]).
% :- module(ex_ancestor,_,[.(cneg)]).

test_parent(X, Y) :- cneg_rt([], parent(X, Y)), parent(X, Y).
test_ancestor(X, Y) :- cneg_rt([], ancestor(X, Y)), ancestor(X, Y).
test_grandparent(X, Y) :- cneg_rt([], parent(X, Y)), parent(X, Y).

parent(marcus, daniel).

parent2(bob,oscar).
parent2(oscar,joan).

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

no_ancestor(X, Y) :- cneg_tr([], ancestor(X, Y)).

grandparent(X,Y):- 
    parent(X,Z),
    parent(Z,Y).

no_grandparent(X,Y):- cneg_tr([], grandparent(X, Y)).
