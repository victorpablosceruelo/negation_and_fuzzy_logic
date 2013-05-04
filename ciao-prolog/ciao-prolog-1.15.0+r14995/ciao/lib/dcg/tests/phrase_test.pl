:- module(phrase_test, [], [dcg, 'dcg/dcg_phrase', hiord]).

:- use_module(library(strings)).

:- export(main/0).

main :-
	display('DCG phrase test'), nl,
	try_test(1),
	try_test(2),
	try_test(3),
	try_test(4).

try_test(Name) :-
	display('Test '), display(Name), display(' '),
	( runtest(Name) -> display('[OK]'), nl
	; display('[Failed]'), nl
	).

:- discontiguous runtest/1.

% ---------------------------------------------------------------------------

runtest(1) :-
	phrase(mystring, Xs, []),
	Xs = "Hello".

mystring --> "Hello".

% ---------------------------------------------------------------------------

runtest(2) :-
	X = phrase(mystring, Xs, []),
	call(X),
	Xs = "Hello".

% ---------------------------------------------------------------------------

runtest(3) :-
	phrase(myphrase(mystring), Xs, []),
	Xs = "Hello".

% Note:
%  - There is no good meta_predicate declaration for DCG goals
myphrase(X) --> X.

% ---------------------------------------------------------------------------

:- use_module(library(lists), [select/3]).

runtest(4) :-
	repeated2([1,2,3,4,4,5]),
	\+ repeated2([1,2,3,4,5]).

% The list Xs contains two repeated elements
% (combines phrase and select/3)
repeated2(Xs) :-
	phrase((select(X),select(X),!), Xs, _).

