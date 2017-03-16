:- module(_, _, [assertions, fsyntax]).

:- doc(author, "Edison Mera").

:- use_module(library(terms)).
:- use_module(library(timingmodel)).

:- initialization(init_timing_model_file).
:- initialization(init_conversion_unit).

:- data timing_model_assrt_db/2.
:- data wamcode_db/2.
:- data lit_wamcode_db/2.
:- data timing_model_file_db/1.
:- data conversion_unit_db/1.
:- data wamcode_params_db/3.

timing_model_file(MiniPrologDir, Arch) :=
	~atom_concat(~timing_model_dir(MiniPrologDir, Arch),
	    '/calibrator_tm.pl').

timing_model_dir(MiniPrologDir, Arch) :=
	~atom_concat([MiniPrologDir, 'estimate/', Arch, '/']).

init_timing_model_file :-
	set_timing_model_file_arch(
	    ~atom_concat(['_', ~get_platform])).

init_conversion_unit :-
	set_conversion_unit(1).

set_conversion_unit(Unit) :-
	retractall_fact(conversion_unit_db(_)),
	asserta_fact(conversion_unit_db(Unit)).

set_timing_model_file_arch(Arch) :-
	set_timing_model_file(~timing_model_file(~timingmodel_dir_db, Arch)).

set_timing_model_file(File) :-
	retractall_fact(timing_model_file_db(_)),
	asserta_fact(timing_model_file_db(File)).
