
:- module(diff_equation,
	[ solve_diff_equ/7,
	  solve_typed_diff_equ/8,
% first_order
	  solve_fode/5
        ],
	[]).

:- use_module('..'(algebraic)).
:- use_module('..'(size)).
:- use_module('..'(solution)).

:- use_module('..'(database), [approximation/1]).
:- use_module('..'(undefined), [write/1]).

:- use_module(library(lists), [length/2	]).

:- push_prolog_flag(multi_arity_warnings,off).

:- include(diff_equ).
:- include(diff_equ_utility).
:- include(divide_conquer).
:- include(explicit_size).
:- include(first_order).
:- include(higher_order).
:- include(implicit_size).
:- include(list_size).
:- include(mutual_size).
:- include(product).
:- include(second_order).

:- pop_prolog_flag(multi_arity_warnings).
