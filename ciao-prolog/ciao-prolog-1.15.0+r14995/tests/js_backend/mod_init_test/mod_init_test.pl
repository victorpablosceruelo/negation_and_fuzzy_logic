:- module(mod_init_test, [], [oo_syntax, hiord, assertions, fsyntax, mutables, string_type]).

:- doc(title, "Tests for module initializations").

:- use_module(engine(io_basic)).

% The expected output is:
%
% $ ./mod_inits
% module initialized
% program start

:- export(cons__/0).
cons__ :-
	display('module initialized'), nl.

:- export(main/0).
main :-
	display('program started'), nl.

