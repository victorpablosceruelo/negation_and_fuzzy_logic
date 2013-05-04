:- module(dcg_test, [], [oo_syntax, hiord, assertions, fsyntax, mutables, string_type, dcg]).

:- doc(title, "Test for DCGs (also tests compilation modules)").

:- use_module(engine(io_basic)).

:- export(main/0).
main :-
	dcg_test.

dcg_test :-
	elems(X, [2,1,0]),
	display(X), nl.

elems -->
	[9,8,7].

