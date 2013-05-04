%
%  csp.pl			Nai-Wei Lin			March 1992
%
%  This file contains the procedures for estimating the number of solutions
%  of a CSP.
%
:- module(csp,
	[ arithmetic_op/1,
	  csp/4,
	  linear_arithmetic_constraints/3,
	  test_predicate/1,
	  unfoldable/5
	],
	[]).

:- use_module('..'(algebraic)).
:- use_module('..'(init)).
:- use_module('..'(size)).
:- use_module('..'(top)).

:- use_module('..'(undefined)).

:- use_module(library(lists), [length/2	]).

:- push_prolog_flag(multi_arity_warnings,off).

:- include(constraint).
:- include(consistency).
:- include(csp_).
:- include(clique).
:- include(unfold).

:- pop_prolog_flag(multi_arity_warnings).

