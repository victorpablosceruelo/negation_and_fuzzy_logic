% A test for instantiable modules based on context variables
%
% Author: Jose F. Morales

:- module(_, [main/0], [assertions, 'compiler/compiler_object']).

:- use_module(library(write)).

main :-
	foo.

:- export(foo/0).
:- '$context'(foo/0, module).
foo :-
	'$this'(X),
	display(X), nl.
