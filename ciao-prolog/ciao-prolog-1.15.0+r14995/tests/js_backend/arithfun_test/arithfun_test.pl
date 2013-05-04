:- module(arithfun_test, [], [oo_syntax, hiord, assertions, fsyntax, mutables, string_type]).

:- doc(title, "Test for arithmetic using fsyntax and arithpreds").

:- use_module(engine(io_basic)).
:- use_module(library(arithpreds)).

:- export(main/0).
main :-
	arithfun_test.

arithfun_test :-
        M = 10 + 100,
	display(M), nl.

