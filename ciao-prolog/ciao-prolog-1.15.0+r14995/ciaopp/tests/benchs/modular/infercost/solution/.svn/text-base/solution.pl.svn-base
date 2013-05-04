%
%  solution.pl			Nai-Wei Lin			December, 1991
%
%  This file contains the procedures for performing the solution size
%  analysis for the predicates in the program in topologically sorted order.
%
:- module(solution,
	[ no_of_cuts/2,
	  solution_analysis/9,
% binding
	  normalize_solution_function/12,
	  pos_var/3,
	  term_var/2,
% comp_diff_equ
	  solve_complexity_equ/6,
	  solve_one_index_comp_diff_equ/10,
% relation
	  fail_body/1,
	  fail_clause/1,
	  recursive_clause/2,
	  recursive_predicate/3,
	  relation_analysis/9
        ],
	[]).

:- use_module('..'(algebraic)).
:- use_module('..'(color)).
:- use_module('..'(csp)).
:- use_module('..'(dependency)).
:- use_module('..'(diff_equation)).
:- use_module('..'(init)).
:- use_module('..'(size)).
:- use_module('..'(top)).

:- use_module('..'(database), [approximation/1]).

:- use_module(library(lists), [length/2	]).

:- push_prolog_flag(multi_arity_warnings,off).
:- push_prolog_flag(single_var_warnings,off).

:- include(binding).
:- include(comp_diff_equ).
:- include(relation).
:- include(solution_).

:- pop_prolog_flag(single_var_warnings).
:- pop_prolog_flag(multi_arity_warnings).
