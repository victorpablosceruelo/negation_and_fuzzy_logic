% A test for context variables
%
% Author: Jose F. Morales

:- module(_, [main/0], [compiler(complang)]).

:- use_module(library(write)).

main :-
	foo,
	push(from, 'From Value') '$ctx_on' test.

:- export(foo/0).
:- '$context'(foo/0, module).
foo :-
	'$module'(X),
	display(X), nl.

:- '$begin_context'(test).
:- '$incctx'(module).
:- '$incctx'(single(from)).
test :-
	test1,
	test2.

test1.
test2 :- test3.

test3 :-
	display(from(~from)), nl,
	display(module(~'$module')), nl.
:- '$end'.
