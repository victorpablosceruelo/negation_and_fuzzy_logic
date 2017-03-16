:- module(_,[main/1],[]).

:- use_module(ciaopp(ciaopp)).
:- use_module(program(p_asr)).

:- set_prolog_flag(multi_arity_warnings,off).
main([]):-
	display('Usage: lib_gen_sources <dest-dir>'),nl,
	display('       Where <dest-dir> is the directory where the generated files'),nl,
	display('       will be created.'),nl.
main([Dir]):-
	module(lib_fake),
	p_asr:gen_lib_sources(Dir).

