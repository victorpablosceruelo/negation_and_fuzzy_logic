:- module(normal_form_res, [normal_form/2, time_normal_form/2], []).

%
%  normal_form.pl		Nai-Wei Lin			January, 1992
%
%  This file contains the procedures for transforming general-form 
%  expressions into normal-form expressions, and performing algebraic
%  simplification.
%

:- use_module(resources(top_res(utility_res)), [compound/1]).
:- use_module(resources(algebraic_res(normal_form_basic_res)), 
	    [variable/1, userfunc/1]).
:- use_module(resources(algebraic_res(algebraic__res)), 
	    [
		add_expr/3,
		subtract_expr/3,
		multiply_expr/3,
		divide_expr/3,
		exp_expr/3,
		log_expr/3,
		factorial_expr/2
	    ]).
:- use_module(resources(algebraic_res(maxmin_res)), [max_expr/3, min_expr/3]).
:- use_module(resources(algebraic_res(sumprod_res)), 
	    [sum_expr/5, prod_expr/5]).

%
%  Transform a general-form expression into a normal-form expression.
%

time_normal_form(X, Y) :- resource_normal_form(X, Y), !.
time_normal_form(X, Y) :- normal_form(X, Y).

resource_normal_form([],     []).
resource_normal_form([X|Xs], [Y|Ys]) :-
	normal_form(X, Y),
	resource_normal_form(Xs, Ys).

normal_form(top, top) :- !.
normal_form(bot, bot) :- !.
%normal_form( minus_inf, minus_inf ) :- !.
normal_form(inf, inf) :- !.
normal_form(X,   Y) :-
	number(X),
	!,
	number_normal_form(X, Y).
normal_form(X, Y) :-
	compound(X),
	comp_normal_form(X, Y).

%
%  Transform a general-form number into a normal-form number.
%
number_normal_form(X, expr([], [factor([], X)])) :-
	X =\= 0,
	!.
number_normal_form(X, expr([], [])) :-
	X =:= 0.

%
%  Transform a general-form compound expression into a normal-form compound 
%  expression.
%
comp_normal_form(X, Y) :-
	variable(X),
	!,
	var_normal_form(X, Y).
comp_normal_form(X+Y, Z) :-
	!,
	normal_form(X, X1),
	normal_form(Y, Y1),
	add_expr(X1, Y1, Z).
comp_normal_form(X-Y, Z) :-
	!,
	normal_form(X, X1),
	normal_form(Y, Y1),
	subtract_expr(X1, Y1, Z).
comp_normal_form(X*Y, Z) :-
	!,
	normal_form(X, X1),
	normal_form(Y, Y1),
	multiply_expr(X1, Y1, Z).
comp_normal_form(X/Y, Z) :-
	!,
	normal_form(X, X1),
	normal_form(Y, Y1),
	divide_expr(X1, Y1, Z).
comp_normal_form(-X, Y) :-
	!,
	Minus1 is -1,
	normal_form(Minus1, X1),
	normal_form(X,      X2),
	multiply_expr(X1, X2, Y).
comp_normal_form(exp(X, Y), Z) :-
	!,
	normal_form(X, X1),
	normal_form(Y, Y1),
	exp_expr(X1, Y1, Z).
comp_normal_form(log(X, Y), Z) :-
	!,
	normal_form(X, X1),
	normal_form(Y, Y1),
	log_expr(X1, Y1, Z).
comp_normal_form(fact(X), Y) :-
	!,
	normal_form(X, X1),
	factorial_expr(X1, Y).
comp_normal_form(max(X, Y), Z) :-
	!,
	normal_form(X, X1),
	normal_form(Y, Y1),
	max_expr(X1, Y1, Z).
comp_normal_form(min(X, Y), Z) :-
	!,
	normal_form(X, X1),
	normal_form(Y, Y1),
	min_expr(X1, Y1, Z).
comp_normal_form(X, Y) :-
	functor(X, sum, 4),
	!,
	sum_normal_form(X, Y).
comp_normal_form(X, Y) :-
	functor(X, prod, 4),
	!,
	prod_normal_form(X, Y).
comp_normal_form(X, Y) :-
	functor(X, arg, 2),
	!,
	function_expr(X, Y).
comp_normal_form(X, Y) :-
	functor(X, arity, 1),
	!,
	function_expr(X, Y).
comp_normal_form(X, Y) :-
	functor(X, head, 1),
	!,
	function_expr(X, Y).
comp_normal_form(X, Y) :-
	functor(X, tail, 1),
	!,
	function_expr(X, Y).
comp_normal_form(X, Y) :-
	userfunc(X),
	userfunc_normal_form(X, Y).

%
%  Transform a general-form variable into a normal-form variable.
%
var_normal_form(X, expr([], [factor([X], 1)])).

sum_normal_form(X, Y) :-
	functor(X, sum, 4),
	arg(1, X, Index),
	arg(2, X, 1),
	arg(3, X, arity(Var)),
	arg(4, X, arg(Var, Index)), !,
	normal_form(Var -1, Y).
sum_normal_form(X, Y) :-
	functor(X, sum, 4),
	arg(1, X, Var),
	arg(2, X, Lower),
	normal_form(Lower, NLower),
	arg(3, X, Upper),
	normal_form(Upper, NUpper),
	arg(4, X, Expr),
	normal_form(Expr, NExpr),
	sum_expr(Var, NLower, NUpper, NExpr, Y).

prod_normal_form(X, Y) :-
	functor(X, prod, 4),
	arg(1, X, Var),
	arg(2, X, Lower),
	normal_form(Lower, NLower),
	arg(3, X, Upper),
	normal_form(Upper, NUpper),
	arg(4, X, Expr),
	normal_form(Expr, NExpr),
	prod_expr(Var, NLower, NUpper, NExpr, Y).

%
%  Transform a general-form sum or prod function into a normal-form 
%  sum or product function.
%
function_expr(X, expr([], [factor([Y], 1)])) :-
	functor(X, F, N),
	functor(Y, F, N),
	function_normal_form(N, X, Y).

function_normal_form(0, _, _) :- !.
function_normal_form(N, X, Y) :-
	N > 0,
	arg(N, X, Arg),
	normal_form(Arg, NArg),
	arg(N, Y, NArg),
	N1 is N -1,
	function_normal_form(N1, X, Y).

%
%  Transform a general-form user-defined function into a normal-form 
%  user-defined function.
%
userfunc_normal_form(X, expr([term([Y], [factor([], 1)])], [])) :-
	functor(X, F, N),
	functor(Y, F, N),
	function_normal_form(N, X, Y).
