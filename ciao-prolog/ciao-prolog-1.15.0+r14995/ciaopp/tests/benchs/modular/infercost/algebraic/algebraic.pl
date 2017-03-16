%
%  algebraic.pl			Nai-Wei Lin			January, 1992
%
%  This file contains the procedures for performing algebraic computations
%  in normal-form.
%
:- module(algebraic,
	[ add_expr/3,
	  divide_expr/3,
	  exp_expr/3,
	  factorial_expr/2,
	  log_expr/3,
	  multiply_expr/3,
	  subtract_expr/3,
% arithm_opers
	  max/3,
	  min/3,
% general_form
	  general_form/2,
% maxmin
	  max_expr/3,
% normal_form
	  list_simplification/2,
	  normal_form/2,
	  simplification/2,
	  userfunc/1,
	  variable/1,
% sumprod
	  prod_expr/5,
	  sum_expr/5
	],
	[]).

:- use_module('..'(size)).
:- use_module('..'(top)).

:- push_prolog_flag(multi_arity_warnings,off).

:- include(algebraic_).
:- include(arithm_opers).
:- include(general_form).
%% :- consult(math).
:- include(maxmin).
:- include(normal_form).
:- include(sumprod).

:- pop_prolog_flag(multi_arity_warnings).

%% Version comment prompting control for this file.
%% Local Variables: 
%% mode: CIAO
%% update-version-comments: "on"
%% End:

