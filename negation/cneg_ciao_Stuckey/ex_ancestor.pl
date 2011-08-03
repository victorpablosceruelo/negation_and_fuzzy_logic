:- module(ex_ancestor,_,[.(cneg), .(debugger_pkg)]).
% :- module(ex_ancestor,_,[.(cneg)]).


parent1(john, mary).
parent1(john, peter).
parent1(mary, joe).
parent1(peter, susan).

ancestor(X, Y):-
 	parent1(X, Y).
ancestor(X, Y):-
	parent1(X, Z),
	ancestor(Z, Y).
