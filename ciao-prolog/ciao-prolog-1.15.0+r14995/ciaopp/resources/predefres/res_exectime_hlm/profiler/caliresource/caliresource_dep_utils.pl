:- module(_, [res_exectime_hlm_gen_all/0], []).

:- use_module(library(file_utils),         [output_to_file/2]).
:- use_module(library(system),             [make_dirpath/1]).
:- use_module(profcost(caliresource(caliresource_cost)),
	    [valid_resources/1]).
:- use_module(res_exectime_hlm(res_exectime_hlm_gen)).

:- push_prolog_flag(multi_arity_warnings, off).

res_exectime_hlm_gen_all :-
	get_res_exectime_hlm_dir(Dir),
	make_dirpath(Dir),
	res_exectime_hlm_gen_all(Dir).

write_hlm_indep_all :-
	valid_resources(AllResources),
	list(AllResources, use_predefres_package),
	(
	    selected_resources(AllResources, Resources),
	    compound_model_name(AllResources, Resources, Resource),
	    atom_concat('res_', Resource, Name),
	    write_hlm_indep_2(Resources, Name),
	    fail
	;
	    true
	).

write_hlm_dep_all :-
	get_res_exectime_hlm_dir(Dir),
	write_hlm_dep(Dir, 'res_exectime_hlm_*_*_dep.pl').

res_exectime_hlm_gen_all(Dir) :-
	valid_resources(AllResources),
	(
	    selected_resources(AllResources, Resources),
	    gen_hlm_files(Dir, AllResources, Resources),
	    fail
	;
	    true
	),
	atom_concat(Dir, 'res_exectime_hlm_indep.pl', FileIndep),
	atom_concat(Dir, 'res_exectime_hlm_dep.pl',   FileDep),
	output_to_file(write_hlm_indep_all, FileIndep),
	output_to_file(write_hlm_dep_all,   FileDep).

:- pop_prolog_flag(multi_arity_warnings).
