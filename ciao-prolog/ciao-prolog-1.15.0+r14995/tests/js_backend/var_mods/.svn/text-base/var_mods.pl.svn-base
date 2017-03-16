:- module(var_mods, [], [oo_syntax, hiord, assertions, fsyntax, mutables, string_type]).

:- doc(title, "Tests for accessing functors of unknown modules (version of nested_mods)").

:- use_module(engine(io_basic)).

% The expected output is:
% 
%   <$foo>
%   foo.d
%   <$bar>
%   foo:d
%   foo:bar:d1
%   foo:bar:d2

:- export(main/0).
main :-
	% TODO: module names are functions at this point, should we
	%       change it? -> YES (they should behave like atoms)
	Foo = ~foo, display(Foo), nl,
	FooD = Foo.d, display(FooD), nl,
	FooBar = ~Foo.bar, display(FooBar), nl,
	Foo.d,
	Foo.bar_d1,
	FooBar.d2.

% A module 'foo'
:- module foo {
    d :- display('foo:d'), nl.
    bar_d1 :-
        Bar = ~bar,
        Bar.d1.

    % A module 'bar' inside 'foo'
    :- module bar {
        d1 :- display('foo:bar:d1'), nl.
        d2 :- display('foo:bar:d2'), nl.
    }.
}.
