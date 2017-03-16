:- module(compare_res,
	    [
		exp_greater_eq_than/2,
		exp_greater_than/2,
		exp_eq_than/2,
		order_of_function/2,
		complexity_order_greater_than/2,
		complexity_order_greater_eq_than/2
	    ], [assertions, isomodes]).

:- use_module(resources(algebraic_res(normal_form_res)), 
	    [normal_form/2]).
:- use_module(resources(algebraic_res(normal_form_basic_res)), 
	    [variable/1]).
:- use_module(resources(algebraic_res(maxmin_res)), 
	    [max_expr/3]).

exp_greater_than(Ex1, Ex2) :-
	normal_form(Ex1, NfEx1),
	normal_form(Ex2, NfEx2),
	!,
	normal_form_exp_greater_than(NfEx1, NfEx2).

normal_form_exp_greater_than(NfEx, NfEx) :-
	!,
	fail.
normal_form_exp_greater_than(NfEx1, NfEx2) :-
	max_expr(NfEx1, NfEx2, NfEx1).

%% normal_form_exp_greater_than(inf,E1):-
%% 	E1 \== inf.
%% normal_form_exp_greater_than(E1,E2) :-
%% 	Minus1 is -1,
%% 	normal_form(Minus1,Y1),
%% 	multiply_expr(Y1,E2,Y2),
%% 	add_expr(E1,Y2,Y),
%% 	positive_sign_expr(E1,E2,Y).
%%  
%% positive_sign_expr(_,_,Y) :-
%% 	positive_expr(Y),
%%         !.
%%  %% positive_sign_expr(_,_,Y) :-
%%  %% 	negative_expr(Y),
%%  %%         !,
%%  %%         fail.
%%  %% positive_sign_expr(E1,E2,_) :-
%%  %%         positive_term_by_term(E1,E2).

exp_greater_eq_than(Ex1, Ex2) :-
	normal_form(Ex1, NfEx1),
	normal_form(Ex2, NfEx2),
	!,
	normal_form_exp_greater_eq_than(NfEx1, NfEx2).

:- pred exp_eq_than(+Ex1, +Ex2) # "Arithmetic expressions @var{Ex1} and
@var{Ex2} are equivalent.".

exp_eq_than(Ex1, Ex2) :-
	normal_form(Ex1, NfEx1),
	normal_form(Ex2, NfEx2),
	!,
	NfEx1 = NfEx2.

% Not used
% comp_order_max(Ex1, Ex2, Max) :-
% 	normal_form(Ex1, NfEx1),
% 	normal_form(Ex2, NfEx2),
% 	max_expr(NfEx1, NfEx2, NMax),
% 	general_form(NMax, Max).

normal_form_exp_greater_eq_than(NfEx1, NfEx2) :-
	max_expr(NfEx1, NfEx2, NfEx1).

%% maximum_expre(Ex1, Ex2, Max):-
%%    comp_order_max(Ex1, Ex2, Max).

maximum_expre(Ex1, Ex2, Max) :-
	complexity_order_greater_than(Ex1, Ex2),
	!,
	Max = Ex1.
maximum_expre(_Ex1, Ex2, Ex2).

minimum_expre(Ex1, Ex2, Min) :-
	complexity_order_greater_than(Ex1, Ex2),
	!,
	Min = Ex2.
minimum_expre(Ex1, _Ex2, Ex1).

complexity_order_greater_eq_than(Ex, Ex) :-
	!.
complexity_order_greater_eq_than(Ex1, Ex2) :-
	complexity_order_greater_than(Ex1, Ex2).

complexity_order_greater_than(Ex1, Ex1) :-
	!,
	fail.
complexity_order_greater_than(Ex1, _Ex2) :-
	Ex1 == inf,
	!.
complexity_order_greater_than(Ex1, _Ex2) :-
	Ex1 == bot,
	!.
complexity_order_greater_than(exp(Exp, N1), exp(Exp, N2)) :-
	number(N1),
	number(N2),
	!,
	N1 > N2.
complexity_order_greater_than(exp(Exp, N), Exp) :-
	!,
	N > 1.
complexity_order_greater_than(Exp, exp(Exp, N)) :-
	!,
	N < 1.
complexity_order_greater_than(exp(_Exp, _N), _Exp2) :- !.
complexity_order_greater_than(Exp,           log(_B, Exp)) :- !.


order_of_function(X, X) :-
	variable(X),
	!.
order_of_function(X+Y, Z) :-
	number(X),
	!,
	order_of_function(Y, Z).
order_of_function(X+Y, Z) :-
	number(Y),
	!,
	order_of_function(X, Z).
order_of_function(X+Y, Z) :-
	!,
	order_of_function(X, X1),
	order_of_function(Y, Y1),
	maximum_expre(X1, Y1, Z).
order_of_function(X-Y, Z) :-
	number(X),
	!,
	order_of_function(Y, Z).
order_of_function(X-Y, Z) :-
	number(Y),
	!,
	order_of_function(X, Z).
order_of_function(X-Y, Z) :-
	!,
	order_of_function(X, X1),
	order_of_function(Y, Y1),
	maximum_expre(X1, Y1, Z).
order_of_function(X*Y, Z) :-
	number(X),
	!,
	order_of_function(Y, Z).
order_of_function(X*Y, Z) :-
	number(Y),
	!,
	order_of_function(X, Z).
order_of_function(X*Y, X1*Y1) :-
	order_of_function(X, X1),
	order_of_function(Y, Y1).
order_of_function(X/Y, 1 /Z) :-
	number(X),
	!,
	order_of_function(Y, Z).
order_of_function(X/Y, Z) :-
	number(Y),
	Y > 0,
	!,
	order_of_function(X, Z).
order_of_function(X/Y, Z) :- % Rewrite -PLG
	order_of_function(X, X1),
	order_of_function(Y, Y1),
	maximum_expre(X1, Y1, Z).
order_of_function(-X, Z) :-
	!,
	order_of_function(X, Z).
order_of_function(exp(X, Y), exp(X, Y)) :-
	!.
% order_of_function(X,X1).
order_of_function(log(_X, Y), log(10, Y)) :-
	!.
order_of_function(fact(X), fact(X)) :-
	!.
order_of_function(max(X, Y), Z) :-
	!,
	order_of_function(X, X1),
	order_of_function(Y, Y1),
	maximum_expre(X1, Y1, Z).
order_of_function(min(X, Y), Z) :-
	!,
	order_of_function(X, X1),
	order_of_function(Y, Y1),
	minimum_expre(X1, Y1, Z).
order_of_function(sum(_Index, _Lower, _Upper, Exp), OExp) :-
	!,
	order_of_function(Exp, OExp).
order_of_function(prod(_Index, _Lower, _Upper, Exp), OExp) :-
	!,
	order_of_function(Exp, OExp).
order_of_function(X, 0) :-
	number(X),
	X =:= 0,
	!.
order_of_function(X, 1) :-
	number(X),
	!.
order_of_function(X, bot) :-
	X == bot,
	!.
order_of_function(X, inf) :-
	X == inf,
	!.
order_of_function(F, F).

% End added
