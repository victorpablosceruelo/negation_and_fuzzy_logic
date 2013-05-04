:- module(algebraic, [], [assertions]).

%
%  algebraic.pl			Nai-Wei Lin			January, 1992
%
%  This file contains the procedures for performing algebraic computations
%  in normal-form.
%

:- reexport(infercost(algebraic(algebraic_)),
	[
	    add_expr/3,
	    time_add_expr/3,
	    divide_expr/3,
	    exp_expr/3,
	    factorial_expr/2,
	    log_expr/3,
	    time_multiply_expr/3,
	    multiply_expr/3,
	    subtract_expr/3
	]).
:- reexport(infercost(algebraic(arithm_opers)),
	[
	    max/3,
	    min/3,
	    pow/3,
	    logar/3,
	    factorial/2
	]).
:- reexport(infercost(algebraic(compare)),
	[
	    exp_greater_eq_than/2,
	    exp_greater_than/2,
	    exp_eq_than/2,
	    order_of_function/2, 
	    complexity_order_greater_than/2, 
	    complexity_order_greater_eq_than/2
	]).
:- reexport(infercost(algebraic(general_form)),
	[
	    general_form/2,
	    time_general_form/2
	]).
:- reexport(infercost(algebraic(maxmin)),
	[
	    max_expr/3
	]).
:- reexport(infercost(algebraic(normal_form)),
	[
	    time_normal_form/2,
	    normal_form/2
	]).
:- reexport(infercost(algebraic(simpl_form)),
	[
	    list_simplification/2,
	    simplification/2
	]).
:- reexport(infercost(algebraic(normal_form_basic)),
	[
	    userfunc/1,
	    variable/1
	]).
:- reexport(infercost(algebraic(sumprod)),
	[
	    prod_expr/5,
	    sum_expr/5
	]).
