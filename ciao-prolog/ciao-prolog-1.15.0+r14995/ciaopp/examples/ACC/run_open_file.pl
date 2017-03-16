:- module(_,[main/0],[]).

:- use_module(ciaopp(ciaopp)).

main:-
	set_pp_flag(widencall,off),
	module(open_file),
	analyze(eterms),
	acheck,
	set_pp_flag(dump_ai,off),
	output.
