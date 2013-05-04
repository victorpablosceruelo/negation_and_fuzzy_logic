:- module(finite_calculus,
	[
 	    simplify_sum/2 % main predicate
% 	    %debug
% 	    expand_falling_power/2,
% 	    linear_coeff/3,
% 	    simplify_expression/2,
% 	    binomial_expansion/3,
% 	    stirling_number/3,
% 	    power_to_fallingpower/2,
% 	    fallingpower_to_power/2,
% 	    eval_definite_sum/5,
% 	    delta/3,
% 	    sigma/3,
% 	    close_sum_exp/2
% 	    %/debug
	], [assertions]).

:- use_module(library(iso_misc)). %for compound/1
:- use_module(infercost('algebraic/normal_form'), [normal_form/2]).
:- use_module(infercost('algebraic/general_form'), [general_form/2]).
:- use_module(infercost('algebraic/math_polynom'), [eval_arith/3, combination/3, fact_poly/2]).


% change log:

:- doc(title, "Module for Finite Calculus").
:- doc(author, "Luthfi Darmawan").
:- doc(summary, "Module for Finite Calculus (Prototype)").
:- doc(module, "Module for Finite Calculus. This module is intended for solving
complicated summatory functions. For the obvious cases Caslog has already had
the effective solution. Therefore this module should be used only to improve
the power of caslog's summatory function evaluation code.
We introduce @emph{expf} to denote @emph{falling power} exponentiation. 

Note: The implementation is done without intensive performance consideration. 
Since we  don't use caslog's normal form, high overhead might occur.

Reference: @emph{Finite Calculus: A Tutorial} by David Gleich
").



%----------------------------------------------------------------------------------
% ------------------------- Finite Calculus -------------------------------
%----------------------------------------------------------------------------------
%examples:
% simplify_sum(sum($(i), 1,$(1),exp(2,$(1) - $(i))),R).
% simplify_sum(sum($(i), 1,$(1),$(i)*exp(2,$(1) - $(i))),R)
% delta($(i),- (2.0*exp(2,- $(i)+ $(1)-1)),L).

%----------------------------------------------------------------------------------
%-------------------------- Main Function --------------------------------
%----------------------------------------------------------------------------------
:- pred simplify_sum(sum(Var, Low, Up, Exp), ClosedExpr)#"Simplifies 
summatory function and transform it into closed expression @var{ClosedExpr}.".
%----------------------------------------------------------------------------------
simplify_sum(sum(Var, Low, Up, Exp), ClosedExpr):-
	simplify_expression(Exp, ExpSimple),
	sigma(Var, ExpSimple, PrimSigmaExp),
	simplify_expression(PrimSigmaExp, PrimSigmaExpSimple),
	eval_definite_sum(Var, Low, Up, PrimSigmaExpSimple, ClosedExp1),
	simplify_expression(ClosedExp1, ClosedExp2),
	expand_falling_power(ClosedExp2, ClosedExp3),
	simplify_expression(ClosedExp3, ClosedExpr),!.

%------------------------------------------------------------------------------
%-------------------------- Auxiliary Functions -------------------------------- 
%----------------------------------------------------------------------------------
%------------------------------------------------------------------------------
:- pred simplify_expression(Expr, Expr2)#" This operation is actually pretty 
expensive and must be considered as a hack because we don't implement the 
 finite calculus operation in caslog's normal form.".
%------------------------------------------------------------------------------
simplify_expression(Expr, Expr2):-
	normal_form(Expr, NFExpr),
	general_form(NFExpr,Expr2).

%------------------------------------------------------------------------------
:- pred numvar(I,A)#"Check whether @var{A} is a number or variable, relative
to variable @var{I}.".
%------------------------------------------------------------------------------
numvar(I, $(A)):- I \== $(A).
numvar(I,A):- number(A), A \== I.
numvar(I,ConsExp):-
	functor(ConsExp,_,2), %+,-,*,/
	arg(1, ConsExp, Arg1),
	arg(2, ConsExp, Arg2),
	numvar(I, Arg1),
	numvar(I, Arg2).

%------------------------------------------------------------------------------
:- pred linear_coeff(I, Expr, Coeff)#" Look for the coeffisien of 
expression @var{Expr} which is relatively linear to variable @var{I}.".
%------------------------------------------------------------------------------
linear_coeff(I, Expr, Coeff):-
	normal_form(Expr, expr([],LsExpr)),
	linear_coeff_elmt(I, LsExpr, Coeff).

%base
linear_coeff_elmt(_I,[],0).
linear_coeff_elmt(I,[factor([I],Coeff)|_],Coeff):-!. % variable. no need to check the rest
                                  % because the normal form has put the same linear term
                                  % together in this element
%recc
linear_coeff_elmt(I, [factor([],_Coeff)|Ls], Coeff):- % a constant
	linear_coeff_elmt(I, Ls, Coeff).
linear_coeff_elmt(I,[factor([$(A)],_Coeff)|Ls],Coeff):- % relative to I, it is a constant 
	$(A) \= I,
	linear_coeff_elmt(I,Ls,Coeff).

%------------------------------------------------------------------------------
:- pred choose_simpler(A, B, C, D)#"A heuristic to choose the simplest 
expression between two expressions. It is useful to guide the Sigma expansion 
so it converges. This predicate is very crucial for the termination of 
Sigma computation of the form Exp1 * Exp2,  therefore we plea for more
sophisticated method".
%------------------------------------------------------------------------------
choose_simpler(A, B, A, B):- numvar(dummy,A).
choose_simpler(A, B, B, A):- numvar(dummy,B).
choose_simpler(A, B, A, B).%just follow the order from simplification

%------------------------------------------------------------------------------
:-pred eval_constant(A, B)#"It is merely a heuristic to speed up the
syntactic simplification.".
%------------------------------------------------------------------------------
eval_constant(0, 0).
eval_constant(exp(_,0), 1).
eval_constant(expf(_,0), 1).

%------------------------------------------------------------------------------
:- pred expand_falling_power(Expr, ExprClean)#" Expand falling power
expression in @var{Expr} so the result  @var{ExprClean} will not have 
@emph{expf} function anymore".
%------------------------------------------------------------------------------
expand_falling_power(expf(A,B), ExprClean):-!, 
	expand_falling_power(A, CleanA),
	expand_falling_power(B, CleanB),
	fallingpower_to_power(expf(CleanA,CleanB),ExprClean).	
expand_falling_power(Expr, ExprClean):-
	functor(Expr, F, N),
	functor(ExprClean, F, N),
	expand_falling_power_elmt(N, Expr, ExprClean).
expand_falling_power(Expr, Expr):- numvar(dummy, Expr).

expand_falling_power_elmt(1, Expr, ExprClean):-!,
	arg(1, Expr, Arg1),
	expand_falling_power(Arg1, Arg1Clean),
	arg(1, ExprClean, Arg1Clean).
expand_falling_power_elmt(N, Expr, ExprClean):-
	arg(N, Expr, Arg1),
	expand_falling_power(Arg1, Arg1Clean),
	arg(N, ExprClean, Arg1Clean),
	Nm is N - 1,
	expand_falling_power_elmt(Nm, Expr, ExprClean).

%----------------------------------------------------------------------------------
%---------------------- finite calculus related function -------------------------
%----------------------------------------------------------------------------------

%------------------------------------------------------------------------------
:- pred eval_definite_sum(Var, Low, Up, ClosedExp, FullyCloseExp)#"get the 
	definite integral evaluation result of an expression @var{ClosedExp}.
        @var{Low} and @var{Up} are the value from original sum,  therefore we have
	to increment @var{Up} during this evaluation".
%------------------------------------------------------------------------------
eval_definite_sum(Var, Low, Up, SigmaExp, ClosedExp):-
	Up1 = Up + 1,
	substitute_term(Var, Low, SigmaExp, LowExp),
	substitute_term(Var, Up1, SigmaExp, UpExp),
	ClosedExp = UpExp - LowExp.

%------------------------------------------------------------------------------
:- pred stirling_number(N, K, St)#"Obtain the stirling number (N,K) of 
second-kind.".
%------------------------------------------------------------------------------
stirling_number(N, K, St):-
	fact_poly(K,KFac),
	eval_arith(sum($(i),0,K,exp(-1,$(i))*comb(K,$(i)) * exp(K - $(i),$(1))), N, L), 
	St is L/KFac.

%------------------------------------------------------------------------------
:- pred power_to_fallingpower(exp(X,R), FP)#" Computes falling power of X**R. 
        The signature of falling power is @emph{expf}".
:- doc(bug, "need simplification on the result").
%------------------------------------------------------------------------------
power_to_fallingpower(exp(X,R), FP):-
	sigma_elmt(X, 0, R, FP).

%base
sigma_elmt(X, R, R, FP):-!,
	stirling_number(R,R,St),
	FP = St * expf(X, R).
%recc
sigma_elmt(X, K, R, FP):-
	K1 is K + 1,
	sigma_elmt(X, K1, R, FP1),
	stirling_number(R, K, St),
	FP = St * expf(X, K) + FP1.

%------------------------------------------------------------------------------
:- pred fallingpower_to_power(FallingPower,Pow)#"Computes the ordinary power form
	of a falling power".
%------------------------------------------------------------------------------
fallingpower_to_power(expf(_X,0),1):-!. %warning: red cut
fallingpower_to_power(expf(0,_M),0):-!.
fallingpower_to_power(expf(X,M),Pow):-
	Mm is M - 1,
	prod_elmt(X, 0, Mm, Pow).

%base
prod_elmt(X, Mm, Mm, X-Mm):-!.
%recc
prod_elmt(X, K, Mm, Pow):-
	Kp is K + 1,
	prod_elmt(X, Kp, Mm, Pow1),
	Pow = (X-K) * Pow1.

%------------------------------------------------------------------------------
:- pred binomial_expansion(Var, exp(Var+1, N), SimpleExpr)#"expand expression 
	in the form (X+1)^N. The prerequisite of (X+1) is mandatory".
%------------------------------------------------------------------------------
binomial_expansion(Var, exp(Var+1, N), SimpleExpr):-
	sigma_binomialp1(exp, Var, 0, N, SimpleExpr).
binomial_expansion(Var, expf(Var+1, N), SimpleExpr):-
	sigma_binomialp1(expf, Var, 0, N, SimpleExpr).


%recc
sigma_binomialp1(Func, Var, K, N, C * Expr + Expr1):- 
	K \== N,
	combination(N, K, C),
	functor(Expr, Func, 2),
	arg(1, Expr, Var),
	arg(2, Expr, K),
	Kp is K + 1,
	sigma_binomialp1(Func, Var, Kp, N, Expr1).
%base
sigma_binomialp1(Func, Var, N, N, Expr):- %  combination(N,N,1) is omitted
	functor(Expr, Func, 2),
	arg(1, Expr, Var),
	arg(2, Expr, N).

%------------------------------------------------------------------------------
:- pred substitute_term(OldTerm, NewTerm, OldExp, NewExp)#"Substitutes any 
occurence of @var{OldTerm} in @var{OldExp} with @var{NewTerm}, result is @var{NewExp}".
%------------------------------------------------------------------------------
%base
substitute_term(OldExp, NewTerm, OldExp, NewTerm). %case where OldExp exactly the same as OldTerm
%
substitute_term(OldTerm, _NewTerm, OldExp, OldExp):- %constant which is not match, it stays the same
	constant(OldExp), OldExp \== OldTerm.
%recc
substitute_term(OldTerm, NewTerm, OldExp, NewExp):-
	compound(OldExp),
	functor(OldExp, F, N),
	functor(NewExp, F, N), %get exactly the same arity
	substitute_elmt(N, OldTerm, NewTerm, OldExp, NewExp).


substitute_elmt(N, OldTerm, NewTerm, OldExp, NewExp) :-
	N > 0,
	arg(N, OldExp, OldArg),
	substitute_term(OldTerm, NewTerm, OldArg, NewArg),
	arg(N, NewExp, NewArg),
	Nm is N - 1,
	substitute_elmt(Nm, OldTerm, NewTerm, OldExp, NewExp).
substitute_elmt(0, _OldTerm, _NewTerm, _OldExp, _NewExp). %do nothing

%------------------------------------------------------------------------------
% table of delta operation, analogous to differential in infinite calculus
:- pred delta(I, Expr, Result)#"computes @emph{Delta Expr / dI}. @var{I} is 
index of the summatory function. The invariant is input and output are 
simplified.".
%------------------------------------------------------------------------------
delta(I, Constant, 0):-
	numvar(I, Constant).
delta(I, - (Exp), DeltaExp):- 
	delta(I, Exp, DeltaExp1),
	simplify_expression(-1 * DeltaExp1, DeltaExp).
delta(I, I, Expr):-                    %expf(x,1)=exp(x,1)
	delta(I, expf(I,1), Expr).
delta(I, expf(I,M), DeltaExp):-
	Mm is M - 1,
	simplify_expression(M * expf(I, Mm), DeltaExp).
delta(I, expf(I+1,M), DeltaExp):-
	binomial_expansion(I, expf(I+1,M), PolynomialExp),
	simplify_expression(PolynomialExp, PolynomialExpSimple),
	delta(I, PolynomialExpSimple, DeltaExp).
delta(I, exp(I,M),DeltaExp):-
	power_to_fallingpower(exp(I,M), ExpFP),
	simplify_expression(ExpFP, ExpFPSimple),
	delta(I, ExpFPSimple, DeltaExp).
%exponential
delta(I, exp(2,I), exp(2,I)).
delta(I, exp(C,I), DeltaExp):-
	numvar(I, C),
	simplify_expression((C - 1) * exp(C,I), DeltaExp).
delta(I, exp(C,A*I+B), DeltaExp):-
	numvar(I, A), numvar(I, B), numvar(I, C),
	simplify_expression((exp(C,A) - 1) * exp(C,A*I+B), DeltaExp).
delta(I, exp(C,I+B), DeltaExp):-
	numvar(I, B), numvar(I, C),
	simplify_expression((C - 1) * exp(C,I+B), DeltaExp).
delta(I, exp(C,A*I-B), DeltaExp):-
	numvar(I, A), numvar(I, B), numvar(I, C),
	simplify_expression((exp(C,A) - 1) * exp(C,A*I-B), DeltaExp).
delta(I, exp(C,I-B), DeltaExp):-
	numvar(I, B), numvar(I, C),
	simplify_expression((C - 1) * exp(C,I-B), DeltaExp).
delta(I, exp(C,-I), DeltaExp):-
	numvar(I, C),
	simplify_expression((exp(C,-1) - 1) * exp(C,-I), DeltaExp).
delta(I, exp(C,-I+B), DeltaExp):-
	numvar(I, B),numvar(I, C),
	simplify_expression((exp(C,-1) - 1) * exp(C,-I+B), DeltaExp).
%this is the most general exponential we provide
delta(I, exp(C,LinearExpr), DeltaExp):-
	linear_coeff(I, LinearExpr, P), numvar(I, C),
	simplify_expression((exp(C,P) - 1) * exp(C,LinearExpr), DeltaExp).
%
delta(I, U + V, DeltaExp):-
	delta(I, U, DeltaU),
	delta(I, V, DeltaV),
	simplify_expression(DeltaU + DeltaV, DeltaExp).
delta(I, U - V, DeltaExp):-
	delta(I, U, DeltaU),
	delta(I, V, DeltaV),
	simplify_expression(DeltaU - DeltaV, DeltaExp).
%
delta(I, U * V, DeltaExp):-
	delta(I, U, DeltaU),
	delta(I, V, DeltaV),
	substitute_term(I, I+1, V, V1),
	simplify_expression(U * DeltaV - (V1 * DeltaU), DeltaExp).
delta(I, U / V, DeltaExp):-
	simplify_expression(U * (1 / V), SimpleExpr),
	delta(I, SimpleExpr, DeltaExp).

%------------------------------------------------------------------------------
% table of sigma operation
:- pred sigma(I, Exp, Result)#" computes indefinite form of @emph{Sigma Exp dI}.
 Analogous to integral in infinite calculus
 this function only transform sigma into primitive. Concretization to the
 value using Upper and Lower bound is not done here. 
 the invariant is input and output are simplified".
%------------------------------------------------------------------------------
sigma(I, Constant, Constant * I):-
	number(Constant).
sigma(I, - (Exp), SigmaExp):- 
	sigma(I, Exp, SigmaExp1),
	simplify_expression(-1 * SigmaExp1, SigmaExp).
sigma(I, I, Expr):-                    %expf(x,1)=exp(x,1)
	sigma(I, expf(I,1), Expr).
sigma(I, expf(I,M), SigmaExp):-
	Mp is M + 1,
	simplify_expression(M * expf(I, Mp)/Mp, SigmaExp).
sigma(I, expf(I+1,M), SigmaExp):-
	binomial_expansion(I, expf(I+1,M), PolynomialExp),
	simplify_expression(PolynomialExp, PolynomialExpSimple),
	sigma(I, PolynomialExpSimple, SigmaExp).
sigma(I, exp(I,M),SigmaExp):-
	power_to_fallingpower(exp(I,M), ExpFP),
	simplify_expression(ExpFP, ExpFPSimple),
	sigma(I, ExpFPSimple, SigmaExp).
%exponential
sigma(I, exp(2,I), exp(2,I)).
sigma(I, exp(C,I), SigmaExp):-
	numvar(I, C),
	simplify_expression(exp(C,I) / (C - 1), SigmaExp).
sigma(I, exp(C,A*I+B), SigmaExp):-
	numvar(I, A), numvar(I, B), numvar(I, C),
	simplify_expression(exp(C,A*I+B) / (exp(C,A) - 1), SigmaExp).
sigma(I, exp(C,I+B), SigmaExp):-
	numvar(I, B), numvar(I, C),
	simplify_expression(exp(C,I+B) / (C - 1), SigmaExp).
sigma(I, exp(C,A*I-B), SigmaExp):-
	numvar(I, A), numvar(I, B), numvar(I, C),
	simplify_expression(exp(C,A*I-B) / (exp(C,A) - 1), SigmaExp).
sigma(I, exp(C,I-B), SigmaExp):-
	numvar(I, B), numvar(I, C),
	simplify_expression(exp(C,I-B) / (C - 1), SigmaExp).
sigma(I, exp(C,-I), SigmaExp):-
	numvar(I, C),
	simplify_expression(exp(C,-I) / (exp(C,-1) - 1), SigmaExp).
sigma(I, exp(C,-I+B), SigmaExp):-
	numvar(I, B),numvar(I, C),
	simplify_expression(exp(C,-I+B) / (exp(C,-1) - 1), SigmaExp).
%the most general case provided for exponential
sigma(I, exp(C,LinearExpr), SigmaExp):-
	linear_coeff(I,LinearExpr, P),numvar(I, C),
	simplify_expression(exp(C,LinearExpr) / (exp(C,P) - 1), SigmaExp).
%
sigma(I, U + V, SigmaExp):-
	sigma(I, U, SigmaU),
	sigma(I, V, SigmaV),
	simplify_expression(SigmaU + SigmaV, SigmaExp).
sigma(I, U - V, SigmaExp):-
	sigma(I, U, SigmaU),
	sigma(I, V, SigmaV),
	simplify_expression(SigmaU - SigmaV, SigmaExp).
% this operation is tricky, a strategy must be applied to prevent infinite expression growth
%sigma(I, U * DeltaV, SigmaExp):-
sigma(I, A * B, SigmaExp):-
	choose_simpler(A,B,C,D), 
	U = C, % U should be the simple one that after several time of derivation it goes constant
	DeltaV = D, % the simplification puts complex one on the front
	delta(I, U, DeltaU),
	sigma(I, DeltaV, V),
	(   
	    eval_constant(DeltaU, 0) -> %a bit of strategy
	    % since one of multiplication component is 0 then the rest of related variable will also 0
	    % so SimpleVDU = 0 and then 
	    SigmaVDeltaU = 0
	
	;
	    substitute_term(I, I+1, V, V1),
	    simplify_expression(V1 * DeltaU, SimpleVDU),
	    sigma(I, SimpleVDU, SigmaVDeltaU) % this point may provoke infinite expansion
	),
	simplify_expression(U * V - SigmaVDeltaU, SigmaExp).
%
sigma(I, U / V, SigmaExp):-
	simplify_expression(U * (1/V), SimpleExpr),
	sigma(I, SimpleExpr, SigmaExp).

%----------------------------------------------------------------------------------
% ------------------------- Finite Calculus End -------------------------------
%----------------------------------------------------------------------------------

