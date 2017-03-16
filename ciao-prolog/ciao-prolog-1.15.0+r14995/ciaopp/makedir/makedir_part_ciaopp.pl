% =============================================================================
:- module(_, _, [ciaopaths, dcg, make, hiord, fsyntax, regtypes]).
% =============================================================================
:- doc(title,  "CiaoPP Global Compilation/Installation installer").
:- doc(author, "Edison Mera").
:- doc(module, "This file is part of the CiaoDE installation system.").
% =============================================================================

:- use_module(library(lists),  [append/3]).
:- use_module(library(llists), [flatten/2]).
:- use_module(library(system)).
:- use_module(library(file_utils)).
:- use_module(library(write)).
:- use_module(library(terms), [atom_concat/2]).

% ===========================================================================
% Be careful with vers and patch: when uninstall, the patch version
% may differ with the version that we try to uninstall.
% -----------------------------------------------------------------------------

ciaopp_exe := 'ciaopp_exe.pl'.

ciaopplib_dir := ~ciaopplib_dir_(~instype).
ciaopplib_dir_(local) := ~fsR(bundle_src(ciaopp)).
ciaopplib_dir_(global) := ~fsR(bundle_inslib(ciaopp)).

% ============================================================================

:- include(library(lpdist('makedir_SHARE'))).
bundle_id(ciaopp).
bundle_dname('CiaoPP').
%
bundle_readme_dir := ~fsR(bundle_src(ciaopp) /'doc'/'readmes').
bundle_manual_dir := ~fsR(bundle_src(ciaopp) /'doc'/'internals').
bundle_manual_dir := ~fsR(bundle_src(ciaopp) /'doc'/'reference').
%
bundle_readme(as('INSTALLATION_CIAOPP', 'INSTALLATION')).
bundle_readme(as('README_CIAOPP',       'README')).
%
bundle_ins_reg. % TODO: document (register the bundle on ins?)

% ============================================================================
:- doc(section, "Compilation").
% ============================================================================

% (hook)
bundle_build <- [build_libraries, build_applications] :- true.

% (invoked from 'makedir', for each bundle)
build_libraries <- [prebuild_libraries, ilciao_java, dobuild_mods] :-
	true.

dobuild_mods <- [] :-
	cd('..'),
	set_configured_flags,
	build_mods('ciaopp', ~compiling_options(ciaopp)),
	cd('ciaopp').

build_platdep <- [build_applications] :- true.

% (invoked from 'makedir', for each bundle)
build_nolibs <- [prebuild_libraries, build_applications] :- true.

% (invoked from 'makedir', for each 'extra' bundle)
build_applications <- [] :-
	build_ciaopp,
	build_ciaoppcl.

% Creation of sh script that runs Ciao with CiaoPP loaded
build_ciaopp :-
	b_filebuild(shscript, ciaopp, 'ciaopp', CiaoPP),
	string_to_file(~ciaopp_line, CiaoPP),
	b_link(shscript, ciaopp, 'ciaopp'),
	-set_exec_perms(CiaoPP, ~perms).
%%	CiaoPP = ~atom_concat([~buildbin_dir, '/', ~bundle_name_version(ciaopp)]),
%%	bundle_name_version(ciaopp, VersionMain),
%%	string_to_file(~ciaopp_line, CiaoPP),
%%	--copy_file(VersionMain, ~atom_concat([~buildbin_dir, '/', 'ciaopp']), [overwrite, symlink]),
%%	-set_exec_perms(CiaoPP, ~perms).

ciaopp_line(L) :-
	ciaopp_line_(L, []).

ciaopp_line_(L, Tail) :-
	Lib = ~atom_codes(~ciaopplib_dir),
	Ciaosh = ~atom_codes(~binciaosh),
	CiaoPP_Exe = ~atom_codes(~ciaopp_exe),
	flatten([
		"#!/bin/sh\n"||
		"# Do not edit - automatically generated!\n\n"||
		"APPHOME="|| Lib, "\n"||
		Ciaosh,
		" -p \"ciaopp ?- \" "||
		"-u $APPHOME/"|| CiaoPP_Exe, "\n"|| Tail], L).

% TODO: use build_standalone_list/3
build_ciaoppcl <- [] :- build_ciaoppcl.
build_ciaoppcl :-
	( with_ciaoppcl(yes) ->
	    bold_message("Building CiaoPP command line program (ciaoppcl)"),
	    b_make_exec(ciaopp, bundle_src(ciaopp) /'command_line', 'ciaoppcl',
		same_process)
	; bold_message("Not building CiaoPP command line program (ciaoppcl)")
	).

% ===========================================================================

% Libraries that require customized installation
% (such as automatically generated code, foreign interfaces, etc.)
prebuild_libraries <- :-
	ppl_interface_file,
	time_analysis_auto,
	load_calibration_results.

% ===========================================================================

:- use_module(library(compiler), [use_module/1, unload/1]).

time_analysis_auto <- :- time_analysis_auto.
time_analysis_auto :-
	bold_message("Generating auto created modules"),
	set_configured_flags,
	use_module(res_wamcode(res_wamcode_gen)),
	_:res_wamcode_gen,
	unload(res_wamcode(res_wamcode_gen)).

% ===========================================================================

% TODO: Why is this module necessary? It seems that only output_to_file/2
%       is used here (and the file was reexported from library(file_utils))
:- use_module(predefres(res_common_gen)).
:- use_module(ciaopp(tests(resources(resources_tests_utils))),
	    [load_profiling_results/0, save_profiling_results/0]).

load_calibration_results <- :- load_calibration_results.

load_calibration_results :-
	bold_message(
	    "Loading previous profiling results."),
	load_profiling_results,
	do_calibrate(~gen_load_results_cmd).

save_calibration_results <- :- save_calibration_results.

save_calibration_results :-
	bold_message(
	    "Saving current profiling results."),
	save_profiling_results,
	do_calibrate(~gen_load_results_cmd).

calibrate <- :- calibrate.

calibrate :-
	bold_message("Calibration of execution time analysis"),
	do_calibrate(~gen_calibrate_cmd).

gen_calibrate_cmd := ~flatten([
		~gen_common_cmd,
		note('Platform independent step, analyzing calibrators'),
		make_po(profcost(caliresource(caliresource_indep))),
		use_module(profcost(caliresource(caliresource_indep))),
		analyze_calibration_tests,
		make_po(profcost(caliresource(caliresource_cost))),
		note('Platform dependent step, generating cost models'),
		use_module(profcost(caliresource(caliresource_dep))),
		fill_all_model_params,
		halt
	    ]).

gen_load_results_cmd := ~flatten([
		~gen_common_indep_cmd,
		use_module(profcost(caliresource(caliresource_dep_utils))),
		res_exectime_hlm_gen_all,
		halt
	    ]).

gen_data_table_cmd := ~flatten([~gen_common_cmd, halt]).

gen_common_cmd := ~append(~gen_common_indep_cmd, ~gen_common_dep_cmd).

gen_common_indep_cmd([
		note('Platform independent step, generating calibrators'),
		use_module(profcost(caliresource(caliresource_gen))),
		generate_calibration_tests,
		generate_calibration_measures(5),
		note('Platform independent step for arithmetics'),
		use_module(res_arith(res_arith_gen)),
		res_arith_gen_indep
	    ]).

gen_common_dep_cmd([
		note('Platform dependent step, measuring times'),
		use_module(profcost(caliresource(caliresource_gen_data))),
		% for papers, 250, but to save time set to 100 iterations
		generate_data_table(100, ticks),
		note('Platform dependent step, timing model for arithmetics'),
		use_module(profcost(calibuil)),
		calibrate_builtins(100, ticks),
		res_arith_gen_dep
	    ]).

do_calibrate(GenCommands) :-
	set_configured_flags,
	output_to_file(gen_calibrate(GenCommands), 'calibrate.tmp'),
	invoke_ciaosh('calibrate.tmp').
%%	do([~setlocalciao, ' ', ~ciaosh, ' -f < calibrate.tmp >> ',
%%		~install_log], ~command_option).
%	--delete_file('calibrate.tmp').

:- use_module(library(aggregates), [findall/3]).

gen_calibrate(GenCommands) :-
	findall(set_prolog_flag(Name, Value),
	    (current_prolog_flag(Name, Value), \+ ignored_flag(Name)),
	    SetPrologFlags),
	list(SetPrologFlags, portray_clause),
	list([
		set_prolog_flag(read_assertions,      no),
		set_prolog_flag(unused_pred_warnings, no),
		set_prolog_flag(runtime_checks,       no)], portray_clause),
	list(GenCommands, portray_clause).

% ===========================================================================

ilciao_java <- [] :-
	ilciao_java.

ilciao_java :-
	( with_ant(yes) ->
	    % TODO: Why 'clean'?
	    make_subdir(ant, ilciao, '', clean, ~command_option, ~install_log,
		'&1'),
	    make_subdir(ant, ilciao, '', compile, ~command_option, ~install_log,
		'&1')
	;
	    true
	).

% ============================================================================
:- doc(section, "Documentation").
% ============================================================================

docs <- [] # "Creates all the documentation files." :-
	docs_readmes,
	docs_manuals.

% ============================================================================
% REGISTER
% ============================================================================

bundle_register_hook.
bundle_unregister_hook.

% ============================================================================
:- doc(section, "Installation").
% ============================================================================

bundle_install <- [] :-
	bundle_install.
bundle_install :-
	bold_message("Installing CiaoPP"),
	install_docs,
	bundle_install_lib,
	install_bin,
	bold_message("CiaoPP installation completed").

bundle_uninstall <- :-
	bundle_uninstall.
bundle_uninstall :-
	bold_message("Uninstalling CiaoPP"),
	uninstall_docs,
	uninstall_bin,
	bundle_uninstall_lib,
	bold_message("CiaoPP uninstallation completed").

install_docs <- :- install_docs.
install_docs :-
	bold_message("Installing documentation files"),
	% TODO: warning, it is not uninstalled in uninstall_docs for ~instype=local! Is it a bug?
	bundle_install_docs(ciaopp).

uninstall_docs <- :- uninstall_docs.
uninstall_docs :- uninstall_docs_(~instype).
uninstall_docs_(local). % TODO: Why not? WRONG?
uninstall_docs_(global) :-
	bold_message("Uninstalling CiaoPP documentation"),
	bundle_uninstall_docs(ciaopp).

bundle_install_lib :- install_lib_(~instype).
install_lib_(local).
install_lib_(global) :-
	bold_message("Installing CiaoPP library"),
	b_install_dir_rec(bundle_src(ciaopp), ~fsR(bundle_inslib(ciaopp))).

bundle_uninstall_lib :- uninstall_lib_(~instype).
uninstall_lib_(local).
uninstall_lib_(global) :-
	bold_message("Uninstalling CiaoPP libraries"),
	% TODO: share with lpdoc
	b_uninstall_dir_rec(~fsR(bundle_insbaselib(ciaopp))).

install_bin :- install_bin_(~instype).
install_bin_(local).
install_bin_(global) :-
	bold_message("Installing CiaoPP executables"),
	install_ciaopp,
	( with_ciaoppcl(yes) ->
	    install_ciaoppcl
	; true
	).

uninstall_bin :- uninstall_bin_(~instype).
uninstall_bin_(local).
uninstall_bin_(global) :-
	bold_message("Uninstalling CiaoPP executables"),
	uninstall_ciaopp,
	uninstall_ciaoppcl.

install_ciaopp :-
	b_install_copy_and_link(shscript, ciaopp, 'ciaopp').

uninstall_ciaopp :-
	b_uninstall_copy_and_link(shscript, ciaopp, 'ciaopp').

install_ciaoppcl :-
	b_install_copy_and_link(plexe, ciaopp, 'ciaoppcl').

uninstall_ciaoppcl :-
	b_uninstall_copy_and_link(plexe, ciaopp, 'ciaoppcl').

% ============================================================================
% NOTE: This part needs to be considered in the distclean process:
% ============================================================================

% TODO: this should be part of uninstall... right?
delete_auto_gen <- [] # "Removal of automatically generated files" :-
	b_clean_link(plexe, 'ciaoppcl'),
	b_clean_copy(plexe, ciaopp, 'ciaoppcl').

% ============================================================================
:- doc(section, "Preparation for PPL Library").
% ============================================================================

ppl_interface_file <- [] # "Creation of the interface for the PPL library" :-
	ppl_interface_file.

ppl_interface_file := 'plai/domains/selected_ppl_interface_auto.pl'.

selected_ppl_interface_auto(P) :-
	( with_ppl(yes) ->
	    ppl_interface(P)
	; dummy_ppl_interface(P)
	).
dummy_ppl_interface := "domain(ppl_ciao_dummy)".
ppl_interface := "library(ppl)".

:- push_prolog_flag(multi_arity_warnings, off).

ppl_interface_file :-
	make_ppl_interface_file(~ppl_interface_file).

:- pop_prolog_flag(multi_arity_warnings).

~ppl_interface_file <- [] :: F :-
	make_ppl_interface_file(F).

make_ppl_interface_file(F) :-
	bold_message("Generating ~w file", [F]),
	selected_ppl_interface_auto(P),
	string_to_file(~flatten([
		    "%% Do not edit - automatically generated!\n\n"||
		    ":- module(_, _, _).\n"||
		    ":- reexport("|| P, ")."]), F).

% ===========================================================================

:- doc(section, "Tests").

:- use_module(library(unittest)).
:- use_module(library(compiler), [use_module/1, unload/1]).

runtests <- [unittests, ciaopptests] :- true.

unittests <- [] # "Run the ciaopp unit tests." :-
	bold_message("Running CiaoPP tests"),
	load_profiling_results,
	runtests_profcost,
	runtests_resources,
	runtests_infercost.

runtests_profcost :-
	absolute_file_name(profcost(profdb), ProfDbPl),
	atom_concat(ProfCostDir, '/profdb.pl', ProfDbPl),
	run_test_dir(ProfCostDir, [rtc_entry]).

runtests_resources <- [] :- runtests_resources.

runtests_resources :-
	absolute_file_name(resources(resources), ResourcesPl),
	atom_concat(ResourcesDir, '/resources.pl', ResourcesPl),
	run_test_dir(ResourcesDir,         [rtc_entry]),
	run_test_dir('tests/resources',    [rtc_entry]),
	run_test_dir('examples/resources', [rtc_entry]).

runtests_infercost :-
	absolute_file_name(infercost(infercost), InferCostPl),
	atom_concat(InferCostDir, '/infercost.pl', InferCostPl),
	run_test_dir(InferCostDir, [rtc_entry]).

ciaopptests <- [] :-
	working_directory(ThisDir, ThisDir),
	working_directory(_,       tests),
	use_module(ciaoppsrc(tests(generate_benchmarks))),
	_:generate,
	use_module(ciaoppsrc(tests(ciaopp_test_all))),
	_:test_all,
	working_directory(_, ThisDir).

runbenchmarks <- [] # "Run Benchmarks" :- true.
