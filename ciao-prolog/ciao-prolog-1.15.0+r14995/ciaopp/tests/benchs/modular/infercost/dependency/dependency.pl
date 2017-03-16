%
%  dependency.pl		Nai-Wei Lin			November, 1991
%
%  This file contains the procedures for performing the data dependency
%  analysis for the predicates in the program in topologically sorted order.
%

:- module(dependency,
	[ dependency_analysis/7,
	  find_adg_field/4,
	  find_gvars_field/4,
	  find_ldg_field/4,
	  gen_clause_pos/2,
	  gen_literal_iopos/5,
	  gen_literal_pos/3,
	  insert_gvars_field/4,
	  insert_ldg_field/4,
	  new_lit/2,
	  new_pos/3,
	  pos_argnum/2,
	  pos_litnum/2
	],
	[]).

:- use_module('..'(determinacy)).
:- use_module('..'(diff_equation)).
:- use_module('..'(init)).
:- use_module('..'(solution)).
:- use_module('..'(top)).

:- push_prolog_flag(multi_arity_warnings,off).

:- include(adg).
:- include(build_adg).
:- include(build_ldg).
:- include(dependency_).
:- include(gvars).
:- include(ldg).
:- include(position).

:- pop_prolog_flag(multi_arity_warnings).

