%
%  size.pl			Nai-Wei Lin			December, 1991
%
%  This file contains the procedures for performing the argument size
%  analysis for the predicates in the program in topologically sorted order.
%

:- module(size,[
        abnormal_equs/2,
        adjust_pos/2,
        boundary_condition_one_index_reducible/4,
        boundary_condition_two_indices_reducible/4,
        boundary_conds/2,
        classify_equs/3,
        construct_comp_equs/4,
        exprs_one_index_reducible/6,
        exprs_two_indices_reducible/5,
        find_recursive_comp/3,
        ground_size/2,
        indirect_recursion/2,
        indirect_recursion_expr/2,
        init_normalize_queue/3,
        input_sizes/3,
        ith_clause_literal/3,
        literal_output_comp/10,
        min_unifiable_term_size/5,
        min_unifiable_term_size1/4,
        normalize/13,
        number_of_literals/3,
        real_pos/4,
        reduce_two_indices_exprs/6,
        remove_recursive_comps/4,
        replace_values_in_equs/4,
	size_analysis/7,
        solve_comp_non_diff_equs/2,
        solve_one_index_size_diff_equ/10,
        solve_two_indices_diff_equs/4,
        substitute/4,
        up_low_approximation_expr/3
	       ],[]).

:- use_module('..'(algebraic)).
:- use_module('..'(dependency)).
:- use_module('..'(determinacy)).
:- use_module('..'(diff_equation)).
:- use_module('..'(init)).
:- use_module('..'(solution)).
:- use_module('..'(top)).

:- use_module('..'(database), [approximation/1]).

:- use_module(library(lists), [length/2	]).

:- push_prolog_flag(multi_arity_warnings,off).
:- push_prolog_flag(discontiguous_warnings,off).

:- include(clause).
:- include(ground_size).
:- include(implicit_size). % ground_term_depth/3 undefined in source
:- include(insert_size).
:- include(normalize).
:- include(size_).
:- include(size_diff_equ).
:- include(term_diff).
:- include(term_size).

:- pop_prolog_flag(discontiguous_warnings).
:- pop_prolog_flag(multi_arity_warnings).

%% Control version comment prompting for the file.
%% Local Variables: 
%% mode: CIAO
%% update-version-comments: "off"
%% End:

