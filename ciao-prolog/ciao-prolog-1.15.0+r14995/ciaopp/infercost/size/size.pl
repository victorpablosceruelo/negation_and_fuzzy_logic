:- module(size, [], []).

%
%  size.pl			Nai-Wei Lin			December, 1991
%
%  This file contains the procedures for performing the argument size
%  analysis for the predicates in the program in topologically sorted order.
%


:- reexport(infercost(size(clause)),
	[
	    ith_clause_literal/3,
	    number_of_literals/3,
            ith_body_literal/3
	]).
:- reexport(infercost(size(ground_size)),
	[
	    ground_size/2,
	    ground_term_size/3
	]).
:- reexport(infercost(size(normalize_)),
	[
	    find_recursive_comp/3,
	    init_normalize_queue/3,
	    literal_output_comp/11,
	    normalize/13,
	    substitute/4
	]).
:- reexport(infercost(size(size_)),
	[
	    explicit_output_size/8,
	    implicit_output_size/8,
	    remove_recursive_comps/4,
	    size_analysis/7,
	    up_low_approximation_expr/3
	]).
:- reexport(infercost(size(size_diff_equ)),
	[
	    abnormal_equs/2,
	    adjust_pos/2,
	    boundary_condition_one_index_reducible/4,
	    boundary_condition_two_indices_reducible/4,
	    boundary_conds/2,
	    classify_equs/3,
	    construct_comp_equs/4,
	    exprs_one_index_reducible/6,
	    exprs_two_indices_reducible/5,
	    indirect_recursion/2,
	    indirect_recursion_expr/2,
	    input_sizes/3,
	    min_unifiable_term_size/5,
	    min_unifiable_term_size1/4,
	    real_pos/4,
	    reduce_two_indices_exprs/6,
	    replace_values_in_equs/4,
	    solve_comp_non_diff_equs/2,
	    solve_one_index_size_diff_equ/10,
	    solve_two_indices_diff_equs/4
	]).
