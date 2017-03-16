:- module(dependency, [], []).

%
%  dependency.pl		Nai-Wei Lin			November, 1991
%
%  This file contains the procedures for performing the data dependency
%  analysis for the predicates in the program in topologically sorted order.
%

:- reexport(infercost(dependency(adg)), [find_adg_field/4]).
:- reexport(infercost(dependency(dependency_)), [dependency_analysis/7]).
:- reexport(infercost(dependency(gvars)),
	[
	    find_gvars_field/4,
	    insert_gvars_field/4
	]).
:- reexport(infercost(dependency(ldg)),
	[
	    find_ldg_field/4,
	    insert_ldg_field/4,
	    new_lit/2
	]).
:- reexport(infercost(dependency(position)),
	[
	    gen_clause_pos/2,
	    gen_literal_iopos/5,
	    gen_literal_pos/3,
	    new_pos/3,
	    pos_argnum/2,
	    pos_litnum/2
	]).
