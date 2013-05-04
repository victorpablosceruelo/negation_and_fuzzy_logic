:- module(diff_equ_res, [solve_diff_equ/7, diff_equ_type/7], [assertions]).

%
%  diff_equ.pl			Nai-Wei Lin			February, 1992
%
%  This file contains the procedures for solving linear difference equations.
%

:- use_module(resources(diff_equation_res(first_order_res)), 
	    [
		first_order_diff_equ/6,
		solve_fode/5
	    ]).
:- use_module(resources(diff_equation_res(second_order_res)), 
	    [
		second_order_diff_equ/6,
		solve_sode/6
	    ]).
:- use_module(resources(diff_equation_res(higher_order_res)), 
	    [
		higher_order_diff_equ/6,
		solve_hode/5
	    ]).
:- use_module(resources(diff_equation_res(divide_conquer_res)), 
	    [
		divide_conquer_diff_equ/6,
		solve_dcde/6
	    ]).
:- use_module(resources(diff_equation_res(mutual_size_res)), 
	    [
		mutual_size_diff_equ/6,
		solve_msde/5
	    ]).
:- use_module(resources(diff_equation_res(diff_equ_implicit_size_res)), 
	    [
		implicit_size_diff_equ/6,
		solve_isde/4
	    ]).
:- use_module(resources(diff_equation_res(explicit_size_res)), 
	    [
		explicit_size_diff_equ/6,
		solve_esde/4
	    ]).
:- use_module(resources(diff_equation_res(list_size_res)), 
	    [
		list_size_diff_equ/6,
		solve_lsde/4
	    ]).

:- use_module(library(messages), [debug_message/1]).

/* PBC: not used now
% Added by PLG (25-Mar-97)
	write_equs_general_form( [] ).
write_equs_general_form( [ equ( A, B, E ) |Eqs ] ) :-
	general_form( E, GE ), nl, write( A ), write( ' ' ), write( B ),
	write( ' ' ), write( GE ), nl,
	write_equs_general_form( Eqs ).
*/

%
%  Determine the type of a difference equation.
%
diff_equ_type(Equ, Var, Pred, A1n, A2n, Bn, first_order) :-
	first_order_diff_equ(Equ, Var, Pred, A1n, A2n, Bn), !,
	debug_message("Recurrence equation of first order").
diff_equ_type(Equ, Var, Pred, A1n, A2n, Bn, second_order) :-
	second_order_diff_equ(Equ, Var, Pred, A1n, A2n, Bn), !,
	debug_message("Recurrence equation of second order").
diff_equ_type(Equ, Var, Pred, A1n, A2n, Bn, higher_order) :-
	higher_order_diff_equ(Equ, Var, Pred, A1n, A2n, Bn), !,
	debug_message("Recurrence equation of higher order").
diff_equ_type(Equ, Var, Pred, A1n, A2n, Bn, divide_conquer) :-
	divide_conquer_diff_equ(Equ, Var, Pred, A1n, A2n, Bn), !,
	debug_message("Recurrence equation of divide and conquer").
diff_equ_type(Equ, Var, Pred, A1n, A2n, Bn, mutual_size) :-
	mutual_size_diff_equ(Equ, Var, Pred, A1n, A2n, Bn), !,
	debug_message("Recurrence equation of mutual size").
diff_equ_type(Equ, Var, Pred, A1n, A2n, Bn, implicit_size) :-
	implicit_size_diff_equ(Equ, Var, Pred, A1n, A2n, Bn), !,
	debug_message("Recurrence equation of implicite size").
diff_equ_type(Equ, Var, Pred, A1n, A2n, Bn, explicit_size) :-
	explicit_size_diff_equ(Equ, Var, Pred, A1n, A2n, Bn), !,
	debug_message("Recurrence equation of explicit size").
diff_equ_type(Equ, Var, Pred, A1n, A2n, Bn, list_size) :-
	list_size_diff_equ(Equ, Var, Pred, A1n, A2n, Bn), !,
	debug_message("Recurrence equation of list  size").
diff_equ_type(_, _, _, _, _, _, no_match).

% solve_diff_equ(first_order,Var,An,_,Bn,Ivalue,Sol)
% Solve a difference equation.
% Ivalue is a list
% PLG
solve_diff_equ(first_order, Var, An, _, Bn, Ivalue, Sol) :-
	solve_fode(Var, Ivalue, An, Bn, Sol).
solve_diff_equ(second_order, Var, A1n, A2n, Bn, Ivalue, Sol) :-
	solve_sode(Var, Ivalue, A1n, A2n, Bn, Sol).
solve_diff_equ(higher_order, Var, A1n, _, Bn, Ivalue, Sol) :-
	solve_hode(Var, Ivalue, A1n, Bn, Sol).
solve_diff_equ(divide_conquer, Var, A1n, A2n, Bn, Ivalue, Sol) :-
	solve_dcde(Var, Ivalue, A1n, A2n, Bn, Sol).
solve_diff_equ(mutual_size, Var, An, _, Bn, Ivalue, Sol) :-
	solve_msde(Var, Ivalue, An, Bn, Sol).
solve_diff_equ(implicit_size, Var, _, _, Bn, Ivalue, Sol) :-
	solve_isde(Var, Ivalue, Bn, Sol).
solve_diff_equ(explicit_size, Var, _, _, Bn, Ivalue, Sol) :-
	solve_esde(Var, Ivalue, Bn, Sol).
solve_diff_equ(list_size, Var, _, _, Bn, Ivalue, Sol) :-
	solve_lsde(Var, Ivalue, Bn, Sol).
% Commented out PLG
% solve_diff_equ(no_match,_,_,_,inf).
% Added by PLG

% :- push_prolog_flag(multi_arity_warnings, off).

% :- use_module(resources(resources_basic), [bound_bottom/2]).

% solve_diff_equ(no_match, Bound, _, _, _, Sol) :-
% 	bound_bottom(Bound, Sol0),
% 	normal_form(Sol0, Sol).

% :- pop_prolog_flag(multi_arity_warnings).

%End added.
