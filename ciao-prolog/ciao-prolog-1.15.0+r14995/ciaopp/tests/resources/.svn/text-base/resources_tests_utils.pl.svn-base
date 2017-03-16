:- module(resources_tests_utils, [
		save_profiling_results/0,
		save_profiling_results/1,
		load_profiling_results/0
	    ], [assertions, nativeprops, fsyntax]).

:- use_module(library(system)).
:- use_module(library(read)).
:- use_module(library(write)).
:- use_module(library(terms)).
:- use_module(library(file_utils),                       [output_to_file/2]).
:- use_module(profcost(caliresource(caliresource_cost)), [valid_resources/1]).
:- use_module(res_exectime_hlm(res_exectime_hlm_gen),
	    [compound_model_name/3, get_dep_file_name/3, selected_resources/2,
		get_res_exectime_hlm_dir/1]).

:- doc(author, "Edison Mera").

:- doc(module, "This module contains some utilities to facilitate
	maintenance of resources tests in order to allow the tests can
	be reproduced.  Mainly it preserves data that are
	platform-dependent.").

:- pred save_profiling_results/0 + (not_fails, is_det) # "Save the
	results of the profiler in the directory
	resources(saved_results).".

save_profiling_results :-
	save_profiling_results(~get_platform).

save_profiling_results(Platform) :-
	save_platform(Platform),
	save_profiling_results_llm(Platform),
	save_profiling_results_arith(Platform),
	save_profiling_results_hlm(Platform).

:- pred load_profiling_results/0 + (not_fails, is_det) # "Load the
	previous saved results of the profiler.  Note that it will
	overwrite the profiling results of a previous calibration.".

load_profiling_results :-
	load_platform(Platform),
	load_profiling_results_hlm(Platform),
	load_profiling_results_arith(Platform),
	load_profiling_results_llm(Platform).

get_saved_results_dir(TDir) :-
	absolute_file_name(resources(resources), ResourcesPl),
	atom_concat(TDir0, 'resources.pl',   ResourcesPl),
	atom_concat(TDir0, 'saved_results/', TDir).

copy_profiling_result(SDir, TDir, Platform, AllResources, Resources) :-
	compound_model_name(AllResources, Resources, Resource),
	get_dep_file_name(Resource, Platform, FileName),
	atom_concat(SDir, FileName, SFileName),
	atom_concat(TDir, FileName, TFileName),
	copy_file(SFileName, TFileName, [overwrite]).

copy_profiling_results(SDir, TDir, Platform, AllResources) :-
	selected_resources(AllResources, Resources),
	copy_profiling_result(SDir, TDir, Platform, AllResources, Resources),
	fail
    ;
	true.

calibrator_tm_dir(CalibratorTmDir) :-
	absolute_file_name(library(timingmodel), TimingModel),
	atom_concat(BaseDir, '/timingmodel.pl', TimingModel),
	atom_concat([BaseDir, '/estimate/_', ~get_platform],
	    CalibratorTmDir).

:- push_prolog_flag(multi_arity_warnings, off).

calibrator_tm(Dir) := ~atom_concat(Dir, '/calibrator_tm.pl').

calibrator_tm := ~calibrator_tm(~calibrator_tm_dir).

:- pop_prolog_flag(multi_arity_warnings).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

load_profiling_results_llm(Platform) :-
	calibrator_tm_dir(Dir),
	calibrator_tm(Dir, CalibratorTm),
	make_dirpath(Dir),
	copy_file(~atom_concat([~get_saved_results_dir, 'calibrator_',
		    Platform, '_tm.pl']), CalibratorTm, [overwrite]).

save_profiling_results_llm(Platform) :-
	copy_file(~absolute_file_name(~calibrator_tm),
	    ~atom_concat([~get_saved_results_dir, 'calibrator_', Platform,
		    '_tm.pl']), [overwrite]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

save_profiling_results_hlm(Platform) :-
	valid_resources(AllResources),
	get_res_exectime_hlm_dir(SDir),
	get_saved_results_dir(TDir),
	copy_profiling_results(SDir, TDir, Platform, AllResources).

save_platform(Platform) :-
	platform_pl(PFileName),
	output_to_file(portray_clause((:- platform(Platform))), PFileName).

load_profiling_results_hlm(Platform) :-
	get_saved_results_dir(SDir),
	get_res_exectime_hlm_dir(TDir),
	make_dirpath(TDir),
	valid_resources(AllResources),
	copy_profiling_results(SDir, TDir, Platform, AllResources).

load_platform(Platform) :-
	platform_pl(PFileName),
	open(PFileName, read, OS),
	read(OS, Term),
	close(OS),
	Term = (:- platform(Platform)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mod_arith_dep(Platform) := ~atom_concat(['res_arith_', Platform, '_dep']).
back_res_arith_dep(Platform) :=
	~atom_concat([~get_saved_results_dir, ~mod_arith_dep(Platform),
		'.pl']).

orig_res_arith_dep(Platform, ResArithDep) :-
	atom_concat(Dir, 'res_arith.pl',
	    ~absolute_file_name(predefres(res_arith))),
	atom_concat([Dir, ~mod_arith_dep(Platform), '_auto.pl'], ResArithDep).

platform_pl := ~atom_concat(~get_saved_results_dir, 'platform.pl').

save_profiling_results_arith(Platform) :-
	copy_file(~orig_res_arith_dep(Platform), ~back_res_arith_dep(Platform),
	    [overwrite]).

load_profiling_results_arith(Platform) :-
	copy_file(~back_res_arith_dep(Platform), ~orig_res_arith_dep(Platform),
	    [overwrite]).
