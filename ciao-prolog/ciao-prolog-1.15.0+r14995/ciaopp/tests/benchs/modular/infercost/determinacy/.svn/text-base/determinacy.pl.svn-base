%
%  determinacy.pl		Nai-Wei Lin			May 1992
%
%  This file contains the procedures for performing the determinacy analysis
%  for the predicates in the program in topologically sorted order.
%
:- module(determinacy,
	[ comparison_op/1,
	  determinacy_analysis/4,
	  mutual_exclusive_classes/3
	],
	[]).

:- use_module('..'(csp)).
:- use_module('..'(dependency)).
:- use_module('..'(diff_equation)).
:- use_module('..'(init)).
:- use_module('..'(size)).
:- use_module('..'(solution)).
:- use_module('..'(top)).

:- use_module(library(lists), [length/2	]).

:- push_prolog_flag(multi_arity_warnings,off).

:- include(determinacy_).
:- include(mutual_exclusion).

:- pop_prolog_flag(multi_arity_warnings).
