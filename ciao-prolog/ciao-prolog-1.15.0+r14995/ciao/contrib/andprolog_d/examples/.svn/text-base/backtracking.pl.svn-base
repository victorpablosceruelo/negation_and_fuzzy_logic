:- module(backtracking, _, [andprolog_d]).

:- use_module(library(format), [format/2]).

p(a).
p(b).
q(c).
q(b).

% with &/2
test1(X, Y) :-
	goal_thread,
	p(X) & q(Y),
	format("~w = ~w~n", [X,Y]),
	X = Y.

% with &>/2 and <&/1
test2(X, Y) :-
	goal_thread,
	q(Y) &> L,
	p(X),
	L <&,
	format("~w = ~w~n", [X,Y]),
	X = Y.

% with &/2, and dependency
test3(X) :-
	goal_thread,
	p(X) & q(X).

% with &>/2 and <&/1, and dependency
test4(X) :-
	goal_thread,
	q(X) &> L,
	p(X),
	L <& .

main :-
	test1(_,_).


