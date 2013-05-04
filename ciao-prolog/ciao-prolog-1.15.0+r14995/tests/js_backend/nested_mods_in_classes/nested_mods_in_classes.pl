:- module(nested_mods_in_classes, [], [oo_syntax, hiord, assertions, fsyntax, mutables, string_type]).

:- doc(title, "Tests for nested modules in classes").

:- use_module(engine(io_basic)).
:- use_module(library(arithpreds)).

:- export(main/0).
main :-
	foo.d,
	B = ~bar(10),
	B.d1,
	B.d2.

% A module 'foo'
:- module foo {
    d :- display('foo:d'), nl.
}.

% A class 'bar'
:- class bar {
    :- attr n.
    cons__(N) :- ~n = N.

    % :- use_module(.(use_mod_in_class_aux)).
    % Some modules nested in class 'bar'
    :- module aux1 {
        inc(N, N1) :- N1 is N + 1.
    }.
    :- module aux2 {
        inc(N, N1) :- N1 is N + 2.
    }.

    d1 :- display('bar:d1'(~aux1.inc(~n))), nl.
    d2 :- display('bar:d2'(~aux2.inc(~n))), nl.
}.
