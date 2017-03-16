:- module(attr_test, [], [assertions, fsyntax, attr]).

:- doc(title, "A test for attributed variables").
:- doc(author, "Jose F. Morales").

% TODO:
%  - split in smaller tests
%  - missing test for multiattributes
%  - missing performance tests for attributes

:- use_module(engine(io_basic)).
:- use_module(library(freeze)).

:- export(main/0).
main :-
 	test_attr_1,
 	test_attr_2,
 	test_attr_3,
	%
 	test_freeze_1,
 	test_freeze_2,
 	test_freeze_3,
 	test_freeze_4,
	%
	test_freeze_5,
	test_freeze_6,
	%
	test_freeze_7.

attr_unify_hook(_,_) :- fail. % not used

% Test that we can put and get attributes
test_attr_1 :-
	display('** Test attr 1'), nl,
	put_attr_local(X, myattr),
	display(test_attr(X, ~get_attr_local(X))), nl.

% Test that we can put attributes many times
test_attr_2 :-
	display('** Test attr 2'), nl,
	put_attr_local(X, myattr1),
	put_attr_local(X, myattr2),
	put_attr_local(X, myattr3),
	display(test_attr_2(X, ~get_attr_local(X))), nl.

% Test that attributes are undone on backtracking
test_attr_3 :-
	display('** Test attr 3'), nl,
	put_attr_local(X, myattr1),
	( put_attr_local(X, myattr2),
	  ( put_attr_local(X, myattr3),
	    display(test_attr_3_3(X, ~get_attr_local(X))), nl,
	    fail
	  ; true
	  ),
	  display(test_attr_3_2(X, ~get_attr_local(X))), nl,
	  fail
	; true
	),
	display(test_attr_3_1(X, ~get_attr_local(X))), nl.

% Test that freeze works when X is instantiated
test_freeze_1 :-
	display('** Test freeze 1'), nl,
	X = 1,
	freeze(X, (display(unfrozen), nl)).

% Test that freeze works when X is instantiated later
test_freeze_2 :-
	display('** Test freeze 2'), nl,
	display('X='), display(X), nl,
	freeze(X, (display(unfrozen_a), nl)),
	display('X='), display(X), nl,
	freeze(X, (display(unfrozen_b), nl)),
	display('X='), display(X), nl,
	display('unifying X='), display(2), nl,
	X = 2,
	display('X='), display(X), nl.

% Test that we can unify variables with frozen goals
test_freeze_3 :-
	display('** Test freeze 3'), nl,
	display('X='), display(X), nl,
	freeze(X, (display(unfrozen_a), nl)),
	display('X='), display(X), nl,
	display('Y='), display(Y), nl,
	freeze(Y, (display(unfrozen_b), nl)),
	display('Y='), display(Y), nl,
	display('unifying X=Y'), nl,
	X = Y,
	display('X='), display(X), nl,
	display('Y='), display(Y), nl,
%	display('X.attr='), display(~X.'$attrget'), nl,
	display('unifying X='), display(2), nl,
	X = 2,
	display('X='), display(X), nl.

% Test a frozen goal that fails
test_freeze_4 :-
	display('** Test freeze 4'), nl,
	freeze(X, (X > 10)),
        display('X must be greater than 10'), nl,
	( X = 1, wake -> % note: with early constraint wake we do not have problems with cut
	    display('Bad, X=1 did not fail'), nl
	; display('Right, X=1 failed'), nl,
	  display('X='), display(X), nl
	).

wake. % explicit call to make sure that constraints are awaken

% Test a frozen goal with other frozen goal
test_freeze_5 :-
	display('** Test freeze 5'), nl,
	K = 0,
	freeze(X,freeze(Y,test5(X,Y,K))),
	( X = 1, Y = 1, wake ->
	    display('OK (you should see attr_test.test5(1,1,0))'), nl
	; display('Failed'), nl
	).

test5(X,Y,K):-
	display(test5(X,Y,K)), nl.
%	X =\= Y,
%	X+K =\= Y,
%	X-K =\= Y.

% Test a frozen goal that fails
test_freeze_6 :-
	display('** Test freeze 6'), nl,
	K = 0,
	freeze(X,freeze(Y,test6(X,Y,K))),
	( X = 1, Y = 1, wake ->
	    display('Wrong (did not fail)'), nl
	; display('OK (X =\\= Y was false)'), nl,
	  display('Both X and Y must be unbound'), nl,
	  display('X='), display(X), nl,
	  display('Y='), display(Y), nl,
	  ( X = 2, Y = 3, wake ->
	      display('OK (X =\\= Y is true)'), nl,
	      display('X='), display(X), nl,
	      display('Y='), display(Y), nl
	  ; display('Failed'), nl
	  )
	).

test6(X,Y,K):-
	X =\= Y,
	X+K =\= Y,
	X-K =\= Y.

% Test a that attributes are correctly unset
test_freeze_7 :-
	display('** Test freeze 7'), nl,
	display('(you should see those characters in separate lines: y x x)'), nl,
	% The expected output is:
	%  x
	%  y
	%  x
	%  x
	freeze(X, (display(x), nl)),
	freeze(Y, (display(y), nl)),
	( ( X = Y % TODO: on backtracking, the attributes are not set back correctly!
	  ; true
	  ),
	  X = 1,
	  fail
	; true
	).
