:- module(nested_mods, [], [oo_syntax, hiord, assertions, fsyntax, mutables, string_type]).

:- doc(title, "Tests for nested modules").

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
	% TODO: foo.d is equivalent to (~foo).d
	FooD = foo.d, display(FooD), nl,
	% TODO: ~foo.bar is (~((~foo).bar)), for the same reason than ~foo
	FooBar = ~foo.bar, display(FooBar), nl,
	foo.d,
	foo.bar_d1,
	foo.bar.d2.

% A module 'foo'
:- module foo {
    d :- display('foo:d'), nl.
    bar_d1 :- bar.d1.

    % A module 'bar' inside 'foo'
    :- module bar {
        d1 :- display('foo:bar:d1'), nl.
        d2 :- display('foo:bar:d2'), nl.
    }.
}.
