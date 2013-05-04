:- module(driver_tdg_ciao,[pre_tdg/1]).

:- use_module(library(tdg(prolog(program_loader_lp)))).
:- use_module(library(tdg(prolog(auto_pe)))).

:- use_module(example_list).


pre_tdg(N) :- 
	example(N,Name),
	load_lp_program(Name),
	atom_concat(Name,'_ir',Name_ir),
	auto_pe(Name_ir),
	atom_concat(Name,'_ef',Name_ef),
	load_ef_program(Name_ef).

