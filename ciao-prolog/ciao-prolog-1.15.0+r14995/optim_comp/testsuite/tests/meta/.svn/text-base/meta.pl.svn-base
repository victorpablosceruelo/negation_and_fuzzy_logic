% A test for correctness of meta expansions, metacalls and hiord.
% --Jose F. Morales

% TODO: move testsuite/meta as a benchmark for hiord and metacalls

:- module(_, [], []).

:- use_package(hiord).
:- '$pragma'(allow_runtime_expansions).

:- export(main/0).
main :-
	( test ->
	    true
	; display('meta test failed!'), nl
	).

test :-
	test1,
	test2.

% This test checks that builtins with meta-arguments are
% correctly annotated with the 'hardrtexp' property. That
% property indicates that runtime expansion must expand the
% arguments.
test1 :-
	X = call(Y),
	Y = myunify(Z),
	% This call will execute call(myunify(Z)) and then execute
	% myunify(Z).
	call(X),
	Z == 1.

myunify(1).

% Checks that meta-expansions are done only if necessary, and that the
% result is always callable.
test2 :-
	myterm(Z, G),
	'$meta_exp'(goal, G, G2),
	'$meta_exp'(goal, G2, G3),
	mycall(G),
	mycall(G2),
	mycall(G3),
	Z == 1.

myterm(Z, myunify(Z)).
	
:- meta_predicate mycall(goal).
mycall(X) :- call(X).

