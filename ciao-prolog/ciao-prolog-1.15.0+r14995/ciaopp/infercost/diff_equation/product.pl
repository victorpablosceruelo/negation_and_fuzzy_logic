:- module(product,
	[
	    product_diff_equ/3,
	    log_base_equs/2,
	    exp_solution/2
	], [assertions]).

%
%  product.pl 
%
%    Handle nonlinear difference equations in the form of a product.
%

:- use_module(library(lists)).

:- use_module(infercost(algebraic(algebraic_)), [log_expr/3, exp_expr/3]).
:- use_module(infercost(algebraic(normal_form)), [normal_form/2]).
:- use_module(infercost(algebraic(normal_form_basic)), [userfunc/1]).

%
%  Test if a difference equation is linear first-order.
%
product_diff_equ(Equ,Pred,NEqu) :-
	Equ = expr([term(Term,Factor)],[]),
	Factor = [factor([],_)],
	product_term(Term,Pred,NTerm),
	NEqu = expr(NTerm,Factor).

product_term(Term,Pred,NTerm) :-
	length(Term,Len), Len > 1,
	product_term(Term,Pred,0,NTerm).

:- push_prolog_flag(multi_arity_warnings,off).

product_term([],_,Count,[]) :- Count > 0.	% direct difference equation
product_term([Dvar|Term],F/N,Count,[term([Dvar],[factor([],1)])|NT]) :-
	userfunc(Dvar),
	functor(Dvar,F,1),!,		% 1-index reducible only
	Count1 is Count+1,
	product_term(Term,F/N,Count1,NT).
product_term([Dvar|Term],F/N,Count,[term([Dvar],[factor([],1)])|NT]) :-
	userfunc(Dvar),
	functor(Dvar,F1,_), F \== F1,!,
	product_term(Term,F/N,Count,NT).

:- pop_prolog_flag(multi_arity_warnings).

%
%
log_base_equs([],[]).
log_base_equs([equ(N,I,E)|BE],[equ(N,I,NE)|NBE]) :-
	normal_form(2,Two),
	log_expr(Two,E,NE),
	log_base_equs(BE,NBE).

%
%
exp_solution(Sol,NSol) :-
	normal_form(2,Two),
	exp_expr(Two,Sol,NSol).