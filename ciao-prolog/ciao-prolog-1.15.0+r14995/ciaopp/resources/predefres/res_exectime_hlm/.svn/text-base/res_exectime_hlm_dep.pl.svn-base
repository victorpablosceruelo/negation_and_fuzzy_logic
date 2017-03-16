:- module(res_exectime_hlm_dep, [
		gen_all_hlm_dep_files/2,
		gen_hlm_dep_file/4
	    ],
	    [assertions, fsyntax]).

:- use_module(library(clpr(eval_r)), [as_float/2]).
:- use_module(library(write)).
:- use_module(library(apply)).
:- use_module(library(math(vector))).
:- use_module(library(file_utils), [output_to_file/2]).
:- use_module(profcost(profdb),    [global_model_params/2]).
:- use_module(profcost(caliresource(caliresource_basic))).
:- use_module(profcost(caliresource(caliresource_params))).
:- use_module(res_exectime_hlm(res_exectime_hlm_gen)).

:- doc(author, "Edison Mera").

gen_all_hlm_dep_files(Platform, AllResources) :-
	global_model_params(Resources, Params),
	gen_hlm_dep_file(Platform, AllResources, Resources, Params),
	fail
    ;
	true.

gen_hlm_dep_file(Platform, AllResources, Resources, Params) :-
	compound_model_name(AllResources, Resources, Resource),
	get_res_exectime_hlm_dir(Dir),
	get_dep_file_name(Resource, Platform, BaseName),
	atom_concat(Dir, BaseName, FileDep),
	output_to_file(write_hlm_decl_dep(Platform, Resources, Resource,
		Params), FileDep).

write_hlm_decl_dep(Platform, Resources, Resource, Params) :-
	params_Solution(Params, K),
	params_LowerBoundSolution(Params, KLower),
	params_UpperBoundSolution(Params, KUpper),
	params_StdError(Params, StdError),
	params_TValue(Params, TValue),
	vector_constant_multiply(TValue, 100, Percent),
	params_MRSS(Params, MRSS),
	display_list([
		'/* WARNING: This file was automatically generated\n',
		'Model    = \t', Resources, '\n',
		'K        = \t', ~maplist(as_float, K), '\n',
		'lower(K) = \t', ~maplist(as_float, KLower), '\n',
		'upper(K) = \t', ~maplist(as_float, KUpper), '\n',
		'error(K) = \t', ~maplist(as_float, StdError), '\n',
		'  %      = \t', ~maplist(as_float, Percent), '\n',
		'MRSS     = \t', ~as_float(MRSS), '\n',
		'*/\n'
	    ]),
	portray_clause(( :- platform_constants(Platform, Resource, lb,
		    ~maplist(as_float, KLower)) )),
	portray_clause(( :- platform_constants(Platform, Resource, me,
	    ~maplist(as_float, K)))),
	portray_clause(( :- platform_constants(Platform, Resource, ub,
		    ~maplist(as_float, KUpper)) )).
