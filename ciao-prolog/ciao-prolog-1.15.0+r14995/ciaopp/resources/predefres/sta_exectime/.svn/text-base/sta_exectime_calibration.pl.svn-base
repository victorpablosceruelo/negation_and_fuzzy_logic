:- module(_, _, [assertions, fsyntax]).

:- use_module(library(messages)).
:- use_module(library(system)).
:- use_module(sta_exectime(sta_exectime_db)).
:- use_module(library(timingmodel)).

lazy_calibration :-
	lazy_benchmark('-s bench estimate').

lazy_benchmark(Command) :-
	timing_model_file_db(AFileName),
	absolute_file_name(AFileName, FileName),
	(
	    file_exists(FileName) -> true
	;
	    show_message(warning, "Calibration file ~w does not exist." ||
		"  Running calibration to generate it", [FileName]),
	    mpsetup(Command)
	).
