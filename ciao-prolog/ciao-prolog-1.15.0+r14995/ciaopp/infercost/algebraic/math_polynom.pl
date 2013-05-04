
:- module(math_polynom,
	[
	    combination/3,
	    eval_arith/3,
	    power/3,
	    ln/2,
	    fact_poly/2,
	    add_polynom/3,
	    brute_eval_intervals/3,
	    add_polynom_asc/3
	], [assertions]).
:- use_module(library(lists),[reverse/2, append/3]).

% change log:
% 2011-6-27 : adding combination 
% 2011-4-25 : adding logarithm and production on eval_arith
% 2011-4-19 : adding factorial on eval_arith
% 2011-3-29 : bug fixes for handling summatory functions
% 2011-3-1  : move single_variable/1 to normal_form_polynom

:- doc(title, "Module of mathematic functions and constants for polynomial comparison").
:- doc(author, "Luthfi Darmawan").
:- doc(summary, "Module for mathematic functions and constants").
:- doc(module, "Module for mathematic functions and constants.  
Some functions were taken and modified from Ciao and CiaoPP library ").
% Define aritmetic operations by using CIAO (iso) builtins.
% These were defined in math* but now are removed.

 %% :- pred power(+Base, +Exponent, -Res)
 %% 
 %% # "Exponentiation. @var{Res} is @var{Base} raised to @var{Exponent}.". 

power(X, Y, Z):- Z is X**Y.

 %% :- pred ln(+Expr, -Log)
 %% 
 %% # "Logarithm. @var{Log} is the logarithm in base e of the arithmetic expression @var{Expr}.". 
:- doc(ln/2, "Actually @em{log} function is already in @em{e}-base, but 
we want to clearly emphasized that it is @em{e}-based logarithm instead 
of  @em{10}-based logarithm").

ln(Expr, Ln):- Ln is log(Expr).



%Factorials are listed as constants, so there are no more computation
:- doc(fact_poly/2, " Obtains factorial of integer value. There is no computation,
the result is precomputed as ordinary fact").
:- pred fact_poly(ExprInt, FactRes)#"@var{ExprInt} valid for any integer ".
fact_poly(X,Y):-
	fac(X,Y).


%P1,P2,P3 order is sorted descending X^n+...+X^0
:- pred add_polynom(P1,P2,P3)# "@var{P3= P1 + P2}, where  
@var{P1}, @var{P2}, @var{P3}, are in descending order".
add_polynom(P1,P2,P3):-
	reverse(P1,P1asc),
	reverse(P2,P2asc),
	add_polynom_asc(P1asc,P2asc,P3asc),
	reverse(P3asc,P3).

:- pred add_polynom_asc(P1,P2,P3)# "@var{P3= P1 + P2}, where  
@var{P1}, @var{P2}, @var{P3}, are in ascending order".
add_polynom_asc([],P2,P2).
add_polynom_asc(P1,[],P1).
add_polynom_asc([],[],[]).
add_polynom_asc([P1|P1s],[P2|P2s],[P3|P3s]):-
	P3 is P1+P2,
	add_polynom_asc(P1s,P2s,P3s).


:- pred combination(N,K,C)#" Computes the combination(N,K) and the result is C".
combination(N,K,C):- 
	fact_poly(N, NFac), fact_poly(K, KFac),
	NK is N - K, fact_poly(NK,NKFac),
	C is NFac / (KFac * NKFac).

%------------------------------------------------------------------------------
% arithmetic function evaluation in prolog
%------------------------------------------------------------------------------
:- doc(eval_arith/3, "Evaluate arithmetic expression on certain x value").
:- pred eval_arith(Expr, X, Res)#"value of @var{Res} might be @em{NaN} when 
the @var{Exp} is undefined on @var{X}".
%base
eval_arith(Expr, Subx, Subx):-
	functor(Expr, $, 1). %variable x

eval_arith(Expr, _Sub, F):-
	functor(Expr, F, 0). %constant

%recc	
eval_arith(Expr, Subx, Res):-
	functor(Expr, F, 1), %unary func
	arg(1, Expr, Arg1),
	eval_arith(Arg1, Subx, Res1),
	(
	    F == '-' ->
	    Res is - Res1
	;
	    F == 'fact' ->
	    fac(Res1, Res)
	).

eval_arith(Expr, Subx, Res):-
	functor(Expr, F, 2), %binary func
	arg(1, Expr, Arg1),
	arg(2, Expr, Arg2),
	eval_arith(Arg1, Subx, Res1),
	eval_arith(Arg2, Subx, Res2),
	(
	    F == '+' ->
	    Res is Res1 + Res2
	;
	    F == '-' ->
	    Res is Res1 - Res2
	;
	    F == '*' ->
	    Res is Res1 * Res2
	;
	    F == '/' ->
	    Res is Res1 / Res2
	;
	    F == '**' ->
	    Res is Res1 ** Res2
	;
	    F == 'exp' ->
	    Res is Res1 ** Res2
	;
	    F == 'log' ->
	    %log(Res1,Res2) means 'Res1' is the base
	    Res is log(Res2) / log(Res1)
	;
	    F == 'comb' ->
	    combination(Res1, Res2,  Res)
	).

eval_arith(sum(Vari, Begin, End, Expri), Subx, Res):-
	substitute_foreign_var(Expri, Vari, Subx, ExpriClean),
	substitute_foreign_var(Begin, Vari, Subx, ConcreteBegin),
	IntConcreteBegin is floor(ConcreteBegin),
	substitute_foreign_var(End, Vari, Subx, ConcreteEnd),
	IntConcreteEnd is floor(ConcreteEnd),
	sum_eval(ExpriClean, Vari, IntConcreteBegin, IntConcreteEnd, Subx, Res).

eval_arith(prod(Vari, Begin, End, Expri), Subx, Res):-
	substitute_foreign_var(Expri, Vari, Subx, ExpriClean),
	substitute_foreign_var(Begin, Vari, Subx, ConcreteBegin),
	IntConcreteBegin is floor(ConcreteBegin),
	substitute_foreign_var(End, Vari, Subx, ConcreteEnd),
	IntConcreteEnd is floor(ConcreteEnd),
	prod_eval(ExpriClean, Vari, IntConcreteBegin, IntConcreteEnd, Subx, Res).

%------------------------------------------------------------------------------
% evaluate factorial
% taken from optim_comp/testsuite/tests/factorial/factorial.pl
%------------------------------------------------------------------------------
fac(0, 1) :- !.
fac(1, 1) :- !.
fac(2, 2) :- !.
fac(3, 6) :- !.
fac(4, 24) :- !.
fac(5, 120) :- !.
fac(6, 720) :- !.
fac(7, 5040) :- !.
fac(8, 40320) :- !.
fac(9, 362880) :- !.
fac(10, 3628800) :- !.
fac(11, 39916800) :- !.
fac(12, 479001600) :- !.
fac(13, 6227020800) :- !.
fac(14, 87178291200) :- !.
fac(15, 1307674368000) :- !.
fac(16, 20922789888000) :- !.
fac(17, 355687428096000) :- !.
fac(18, 6402373705728000) :- !.
fac(19, 121645100408832000) :- !.
fac(20, 2432902008176640000) :- !.
fac(21, 51090942171709440000) :- !.
fac(22, 1124000727777607680000) :- !.
fac(23, 25852016738884976640000) :- !.
fac(24, 620448401733239439360000) :- !.
fac(25, 15511210043330985984000000) :- !.
fac(26, 403291461126605635584000000) :- !.
fac(27, 10888869450418352160768000000) :- !.
fac(28, 304888344611713860501504000000) :- !.
fac(29, 8841761993739701954543616000000) :- !.
fac(30, 265252859812191058636308480000000) :- !.
fac(31, 8222838654177922817725562880000000) :- !.
fac(32, 263130836933693530167218012160000000) :- !.
fac(33, 8683317618811886495518194401280000000) :- !.
fac(34, 295232799039604140847618609643520000000) :- !.
fac(35, 10333147966386144929666651337523200000000) :- !.
fac(36, 371993326789901217467999448150835200000000) :- !.
fac(37, 13763753091226345046315979581580902400000000) :- !.
fac(38, 523022617466601111760007224100074291200000000) :- !.
fac(39, 20397882081197443358640281739902897356800000000) :- !.
fac(40, 815915283247897734345611269596115894272000000000) :- !.

fac(X, Y) :-
% X > 0,
        X0 is X - 1,
        fac(X0, Y0),
        Y is X * Y0.

%------------------------------------------------------------------------------
% evaluate summation function
:- doc(bug, "does not cover nested sum expression").
%------------------------------------------------------------------------------
%base
sum_eval(Expri, Vari, End, End, Subx, Res):-
	sub_vari(Expri, Vari, End, Expr),
	eval_arith(Expr, Subx, Res).
%recc
sum_eval(Expri, Vari, Begin, End, Subx, Res):-
	Begin < End,
	sub_vari(Expri, Vari, Begin, Expr),
	eval_arith(Expr, Subx, Res1),
	Begin1 is Begin + 1,
	sum_eval(Expri, Vari, Begin1, End, Subx, Res2),
	Res is Res1 + Res2.

% case analysis
% sum is still defined even though Begin > End, based on the argument 
% that 0 is identity function for addition, so the result is 0.
sum_eval(_Expri, _Vari, Begin, End, _Subx, 0):-
	Begin > End.


%------------------------------------------------------------------------------
% evaluate production function
:- doc(bug, "does not cover nested prod expression, the evaluation is very naive").
%------------------------------------------------------------------------------
%base
prod_eval(Expri, Vari, End, End, Subx, Res):-
	sub_vari(Expri, Vari, End, Expr),
	eval_arith(Expr, Subx, Res).
%recc
prod_eval(Expri, Vari, Begin, End, Subx, Res):-
	Begin < End,
	sub_vari(Expri, Vari, Begin, Expr),
	eval_arith(Expr, Subx, Res1),
	Begin1 is Begin + 1,
	prod_eval(Expri, Vari, Begin1, End, Subx, Res2),
	Res is Res1 * Res2.
% case analysis
% prod is still defined even though Begin > End, based on the argument 
% that 1 is the result of empty product, so the result is 1.
prod_eval(_Expri, _Vari, Begin, End, _Subx, 1):-
	Begin > End.


%------------------------------------------------------------------------------
% substitute index with integer
%------------------------------------------------------------------------------
%base
sub_vari(Vari, Vari, Subi, Subi):-!.
sub_vari(Expri, _Vari, _Subi, Expri):-
	functor(Expri, _F, 0). %constant
sub_vari(Expri, _Vari, _Subi, Expri):-
	functor(Expri, $, 1). %variable
%recc
sub_vari(Expri, Vari, Subi, Expr):-
	N = 2,               %binary func
	functor(Expri, F, N), 
	arg(1, Expri, Arg1),
	arg(2, Expri, Arg2),
 	sub_vari(Arg1, Vari, Subi, Expr1),
	sub_vari(Arg2, Vari, Subi, Expr2),
	functor(Expr, F, N),
	arg(1, Expr, Expr1),
	arg(2, Expr, Expr2).


%------------------------------------------------------------------------------
% substitute variable which is not index variable with constant
% eg sum(i, 1, x, x+i), the index is i, so x will be substitute w/ a constant
%------------------------------------------------------------------------------
%base
substitute_foreign_var(Vari, Vari, _Subx, Vari):-!.
substitute_foreign_var(Varx, _Vari, Subx, Subx):-
	functor(Varx, $, 1). % foreign variable
substitute_foreign_var(Varx, _Vari, _Subi, Varx):-
	functor(Varx, _F, 0). %constant
%rec
substitute_foreign_var(Expr, Vari, Subx, ExprClean):-
	N = 2,               %binary func
	functor(Expr, F, N), 
	arg(1, Expr, Arg1),
	arg(2, Expr, Arg2),
	substitute_foreign_var(Arg1, Vari, Subx, Expr1),
	substitute_foreign_var(Arg2, Vari, Subx, Expr2),
	functor(ExprClean, F, N),
	arg(1, ExprClean, Expr1),
	arg(2, ExprClean, Expr2).


%------------------------------------------------------------------------------
% brute evaluation of certain intervals
%------------------------------------------------------------------------------
brute_eval_intervals(_Func, [], []).
brute_eval_intervals(Func, [i(A,B)|IvalRes], Result):-
	brute_eval_one(Func, A, B, ResultIvalAB),
	complete_intervals_bres(A, B, ResultIvalAB, ResultIval),
	brute_eval_intervals(Func, IvalRes, Result1),
	concat_intervals_bres(ResultIval, Result1, Result),!.

%------------------------------------------------------------------------------
% complete interval so it has the following form
% [-1,A, Ival, B, -1], Ival is the result of brute_eval_one
%------------------------------------------------------------------------------
complete_intervals_bres(A, B, ResIval, ResIvalComplete):-
	append([-1,A|ResIval], [B, -1], ResIvalComplete).

%------------------------------------------------------------------------------
% concatenate intervals which are the result of brute_eval_one
%------------------------------------------------------------------------------
concat_intervals_bres(Intv1, [], Intv1).
concat_intervals_bres(Intv1, [_|Intv2Res], Intervals):-
	append(Intv1, Intv2Res, Intervals).

%------------------------------------------------------------------------------
% brute evaluation in a single interval
%------------------------------------------------------------------------------
brute_eval_one(Func, B, B, [ResEvalSimple]):-
	eval_arith(Func, B, ResEval),
	simplify_eval_result(ResEval, ResEvalSimple).
brute_eval_one(Func, A, B, ResultInterval):-
	eval_arith(Func, A, ResEval),
	simplify_eval_result(ResEval, ResEvalSimple),
	A1 is A + 1,
	brute_eval_one(Func, A1, B, ResultIval1),
	ResultIval1 = [Res1|_],
	(
	    Res1 == ResEvalSimple ->
	    ResultInterval = ResultIval1
	;% different sign
	    (
		ResEvalSimple == 1 ->
		ResultInterval = [1,A|ResultIval1]
	    ;
		% move forward because check is open interval
		ResultInterval = [-1, A1|ResultIval1]
	    )
	).

%------------------------------------------------------------------------------
% simplify evaluation result into either 1 or -1
% 0 is also denoted as 1
%------------------------------------------------------------------------------
simplify_eval_result(Val, ValSimple):-
	(
	    Val >= 0 ->
	    ValSimple = 1
	;
	    ValSimple = -1
	).
