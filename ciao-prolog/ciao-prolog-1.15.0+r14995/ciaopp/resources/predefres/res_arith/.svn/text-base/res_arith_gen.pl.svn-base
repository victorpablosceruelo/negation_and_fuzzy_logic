:- module(_, [res_arith_gen/0, res_arith_gen_indep/0, res_arith_gen_dep/0],
	    [assertions, fsyntax, nortchecks]).

:- use_module(library(write)).
:- use_module(library(aggregates)).
:- use_module(library(terms)).
:- use_module(library(system_extra), [ls/3]).
:- use_module(resources(resources_db),     [arith_costs/2]).
:- use_module(predefres(res_common_gen)).

arith_operator((-) /1).
arith_operator((+) /1).
arith_operator((--) /1).
arith_operator((++) /1).
arith_operator(integer/1).
arith_operator(truncate/1).
arith_operator(float/1).
arith_operator((\) /1).
arith_operator(abs/1).
arith_operator(sign/1).
arith_operator(float_integer_part/1).
arith_operator(float_fractional_part/1).
arith_operator(floor/1).
arith_operator(round/1).
arith_operator(ceiling/1).
arith_operator((+) /2).
arith_operator((-) /2).
arith_operator(* /2).
arith_operator(/ /2).
arith_operator(// /2).
arith_operator(rem/2).
arith_operator(# /2).
arith_operator(/\ /2).
arith_operator(\/ /2).
arith_operator(<< /2).
arith_operator(>> /2).
arith_operator(mod/2).
arith_operator(** /2).
arith_operator(exp/1).
arith_operator(log/1).
arith_operator(sqrt/1).
arith_operator(cos/1).
arith_operator(atan/1).
arith_operator(gcd/2).

res_arith_gen :-
	res_arith_gen_dir(Dir),
	res_arith_gen_indep(Dir),
	res_arith_gen_dep(Dir, ~get_platform).

res_arith_gen_dir(Dir) :-
	absolute_file_name(predefres(res_arith), FileName),
	atom_concat(Dir, 'res_arith.pl', FileName).

:- push_prolog_flag(multi_arity_warnings, off).

res_arith_gen_indep :- res_arith_gen_indep(~res_arith_gen_dir).

res_arith_gen_indep(Dir) :-
	atom_concat(Dir, 'res_arith_each_decl_auto.pl', FileDecl),
	atom_concat(Dir, 'res_arith_each_assr_auto.pl', FileAssr),
	atom_concat(Dir, 'res_arith_assr_auto.pl',      FileComp),
	atom_concat(Dir, 'res_arith_auto.pl',           File),
	atom_concat(Dir, 'res_arith_res_auto.pl',       FileRes),
	output_to_file(write_res_arith_decl,      FileDecl),
	output_to_file(write_res_arith_assr,      FileAssr),
	output_to_file(write_res_arith_comp_assr, FileComp),
	output_to_file(write_res_arith_comp(Dir), File),
	output_to_file(write_res_arith_res,       FileRes).

res_arith_gen_dep :- res_arith_gen_dep(~res_arith_gen_dir, ~get_platform).

res_arith_gen_dep(Dir, Platform) :-
	atom_concat(['res_arith_', Platform, '_dep_auto'], Name),
	atom_concat([Dir,          Name,     '.pl'],       FileDep),
	output_to_file(write_res_arith_dep(Dir), FileDep).

:- pop_prolog_flag(multi_arity_warnings).

write_res_arith_comp(Dir) :-
	ls(Dir, 'res_arith_*_dep_auto.pl', NameDeclDeps),
	list(NameDeclDeps, ( ''(X) :-
		atom_concat(F, '.pl', X),
		portray_clause(
		    (:- include(res_arith(F)))) )).

write_res_arith_dep(Dir) :-
	get_os(Os),
	get_arch(Arch),
	atom_concat(Os, Arch, Platform),
	findall(Time, ( arith_operator(Operator), arith_costs(Operator,
		    [Time, _Variance]) ), Constants),
	absolute_file_name(predefres(res_arith), FileName),
	atom_concat(Dir, 'res_arith.pl', FileName),
	portray_clause((:- platform_constants(Platform, arith, Constants))).

write_res_arith_res :-
	display_autogen_message,
	arith_operator(AO),
	current_arith_resource_(AO, Resource),
	Head =.. [Resource, LitInfo, Cost],
	( Predicate = resource_arith(Resource, AO)
	; Predicate = (Head :- arith_operator(AO, LitInfo, Cost))
	),
	portray_clause(Predicate),
	fail
    ;
	true.

write_res_arith_decl :-
	display_autogen_message,
	current_arith_resource(_, Resource),
	portray_clause((:- resource(Resource))),
	fail
    ;
	true.

write_res_arith_assr :-
	display_autogen_message,
	member(Approx, [ub, lb]),
	current_arith_resource(_ArithOp, Resource),
	( Assertion = (:- literal_cost(Approx, Resource, Resource))
	; Assertion = (:- head_cost(Approx, Resource, 0))
	; Assertion = (:- trust_default + cost(Approx, Resource, 0))
	),
	portray_clause(Assertion),
	fail
    ;
	true.


write_res_arith_comp_assr :-
	display_autogen_message,
	findall(Resource, current_arith_resource(_, Resource), Resources),
	portray_clause((:- compound_resource(arith, Resources))).

current_arith_resource(ArithOp, Resource) :-
	arith_operator(ArithOp),
	current_arith_resource_(ArithOp, Resource).

current_arith_resource_(A/N, Resource) :-
	atom_number(NA, N),
	atom_concat(['arith_operator_', A, '/', NA], Resource).
