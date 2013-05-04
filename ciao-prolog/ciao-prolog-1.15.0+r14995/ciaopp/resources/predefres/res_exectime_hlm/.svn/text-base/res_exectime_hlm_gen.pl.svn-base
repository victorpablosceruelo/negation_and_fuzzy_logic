:- module(res_exectime_hlm_gen, [
		compound_model_name/3,
		get_dep_file_name/3,
		gen_hlm_files/3,
		selected_resources/2,
		use_predefres_package/1,
		write_hlm_dep/2,
		write_hlm_indep_2/2,
		write_hlm_indep_each/2,
		get_res_exectime_hlm_dir/1
	    ],
	    [assertions, fsyntax]).

:- use_module(library(write)).
:- use_module(library(lists)).
:- use_module(library(file_utils), [output_to_file/2]).
:- use_module(library(terms),     [atom_concat/2]).
:- use_module(library(system_extra), [ls/3]).

:- doc(author, "Edison Mera").

:- doc(gen_hlm_files/3, "Generate files of predefined resources to
	estimate the execution time using high level models.").

gen_hlm_files(Dir, AllResources, Resources) :-
	compound_model_name(AllResources, Resources, Resource),
	atom_concat('res_', Resource, Name),
	atom_concat(Name,   '_indep', NameIndep),
	atom_concat([Dir, Name,      '.pl'],      File),
	atom_concat([Dir, Name,      '_decl.pl'], FileDecl),
	atom_concat([Dir, Name,      '_assr.pl'], FileAssr),
	atom_concat([Dir, NameIndep, '.pl'],      FileIndep),
	output_to_file(write_hlm(Name),                      File),
	output_to_file(write_hlm_decl(Resource),             FileDecl),
	output_to_file(write_hlm_assr(Dir, Name, NameIndep), FileAssr),
	output_to_file(write_hlm_indep(Resources, Resource), FileIndep).

write_hlm(Name) :-
	atom_concat(Name, '_decl', NameDecl),
	atom_concat(Name, '_assr', NameAssr),
	portray_clause((:- package(Name))),
	portray_clause((:- use_package(resdefs))),
	portray_clause((:- include(res_exectime_hlm(auto(NameDecl))))),
	portray_clause((:- include(res_exectime_hlm(auto(NameAssr))))).

write_hlm_decl(Resource) :-
	portray_clause((:- resource(Resource))).

write_hlm_assr(Dir, Name, NameIndep) :-
	write_include_indep(NameIndep),
	write_include_deps(Dir, Name).

write_include_deps(Dir, Name) :-
	atom_concat(Name, '_*_dep.pl', Pattern),
	write_hlm_dep(Dir, Pattern).

write_hlm_indep_each(AllResources, Resources) :-
	compound_model_name(AllResources, Resources, Name),
	atom_concat(Name, '_indep', NameIndep),
	write_include_indep(NameIndep).

write_include_indep(NameIndep) :-
	portray_clause((:- include(res_exectime_hlm(auto(NameIndep))))).

write_hlm_dep(Dir, Pattern) :-
	ls(Dir, Pattern, Names),
	list(Names, ( ''(X) :-
		atom_concat(F, '.pl', X),
		portray_clause((:- include(res_exectime_hlm(auto(F))))) )).

write_hlm_indep(Resources, Resource) :-
	list(Resources, use_predefres_package),
	write_hlm_indep_2(Resources, Resource).

write_hlm_indep_2(Resources, Resource) :-
	portray_clause((:- compound_resource(Resource, Resources))),
	portray_clause((:- trust_default + cost(ub, Resource, 0))),
	portray_clause((:- trust_default + cost(lb, Resource, 0))).

use_predefres_package(Resource) :-
	atom_concat('res_',    Resource, Resource0),
	atom_concat(Resource0, '_assr',  Resource1),
	ResourceDir =.. [Resource0, Resource1],
	portray_clause((:- include(ResourceDir))).

get_res_exectime_hlm_dir(Dir) :-
	absolute_file_name(predefres(res_exectime_hlm), FileName),
	atom_concat(BaseDir, 'res_exectime_hlm.pl', FileName),
	atom_concat(BaseDir, 'auto/',               Dir).

selected_resources(Resources, SelectedResources) :-
	subordlist(SelectedResources, Resources),
	SelectedResources \== [].

get_dep_file_name(Resource, Platform) :=
	~atom_concat(['res_', Resource, '_', Platform, '_dep.pl']).

compound_model_name(AllResources, Resources, Name) :-
	get_subset_id(AllResources, Resources, 0, Id),
	atom_number(AId, Id),
	atom_concat('exectime_hlm_', AId, Name).

:- export(get_subset_id/4).
get_subset_id([],     _,  Id,  Id).
get_subset_id([R|Rs], S0, Id0, Id) :-
	(
	    select(R, S0, S) ->
	    Id1 is 2*Id0+1
	;
	    S = S0,
	    Id1 is 2*Id0
	),
	get_subset_id(Rs, S, Id1, Id).

