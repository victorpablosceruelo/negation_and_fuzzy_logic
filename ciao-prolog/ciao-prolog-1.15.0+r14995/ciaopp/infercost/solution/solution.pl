:- module(solution, [], []).

%
%  solution.pl			Nai-Wei Lin			December, 1991
%
%  This file contains the procedures for performing the solution size
%  analysis for the predicates in the program in topologically sorted order.
%

:- reexport(infercost(solution(binding)),
	[
	    normalize_solution_function/12,
	    pos_var/3,
	    term_var/2
	]).
:- reexport(infercost(solution(comp_diff_equ)),
	[
	    solve_complexity_equ/6,
	    solve_one_index_comp_diff_equ/10
	]).
:- reexport(infercost(solution(relation)),
	[
	    fail_body/1,
	    fail_clause/1,
	    recursive_clause/2,
	    recursive_predicate/3,
	    relation_analysis/9
	]).
:- reexport(infercost(solution(solution_)),
	[
	    no_of_cuts/2,
	    solution_analysis/9
        ]).
