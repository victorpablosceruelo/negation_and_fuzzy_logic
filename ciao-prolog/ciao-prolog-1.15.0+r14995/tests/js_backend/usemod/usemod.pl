:- module(usemod, [], [oo_syntax, hiord, assertions, fsyntax, mutables, string_type]).

:- doc(title, "Tests for use_module").

% TODO: Write a version with errors (condcomp?), capture errors in
%       test output.

:- export(main/0).
main :-
	test0,
	test1,
	test2.

% Check that we can call 'foo.d'
test0 :-
%	display('main'), nl, % ERROR
	foo.d,
	foo.d1.

test1 :-
	% Test that Bar and Baz are the same
	% (same predicate imported from two different modules)
	bar.d(Bar),
	baz.d(Baz),
	tests.require_equal(Bar, Baz).

test2 :-
	% Test that Bar and Bar2 are the same
	% (same predicate accessed in two different ways)
	bar.d(Bar),
	bar.d2(Bar2),
	tests.require_equal(Bar, Bar2).

% A module 'foo'
:- module foo {
%    d :- display('foo:d'), nl. % ERROR, display/1 not visible
    d :- true. % display('foo:d'), nl. % ERROR
    d1 :- X = io_basic.display('hello\n'), X. % TODO: this should give an error
}.

% A module 'bar'
:- module bar {
    :- use_module(engine(io_basic)).
    % Unify X with 'nl', which is resolved as 'io_basic.nl'.
    d(X) :- X = nl, display('bar:d'), nl.
    % Unify X with 'io_basic.nl' (should give the same result than d/1).
    d2(X) :- X = io_basic.nl, display('bar:d2'), nl.

    % d3(X) :- X = bar.io_basic.nl, display('bar:d2'), nl. % ERROR: 'io_basic' is NOT nested under 'bar'
}.

% A module 'baz'
:- module baz {
    :- use_module(engine(io_basic)).
    % Unify X with 'nl', which is resolved as 'io_basic.nl'.
    d(X) :- X = nl, display('baz:d'), nl.
}.

% % A module 'extra'
% :- module extra {
%     :- use_module(engine(io_basic)).
%     d.
% }.

% A module for tests
:- module tests {
    :- use_module(engine(io_basic)).
    require_equal(X, Y) :-
	display('a_equal_b?'), nl,
        tab, display(a(X)), nl,
        tab, display(b(Y)), nl,
	tab,
	( X = Y ->
	    display('yes (ok)'), nl
	; display('no (bad)'), nl
	).
    tab :- display('  ').
}.
