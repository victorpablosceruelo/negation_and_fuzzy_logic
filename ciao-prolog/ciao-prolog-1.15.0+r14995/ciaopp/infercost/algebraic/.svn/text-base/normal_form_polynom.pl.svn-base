:- module(normal_form_polynom,
	[
	    polynomize/2
	], [assertions]).

%:- doc(filetype, part).

:- use_module(library(format)).
:- use_module(library(lists),[insert_last/3]).
:- use_module(infercost('algebraic/normal_form'), [normal_form/2]).
:- use_module(infercost('algebraic/general_form'), [general_form/2]).
:- use_module(infercost('algebraic/normal_form_basic'), [variable/1, userfunc/1]).
:- use_module(infercost('top/utility'),	[multiply/3]).
:- use_module(math_polynom,
	[
%	    power/3,
	    ln/2,
	    fact_poly/2,
	    add_polynom/3
	]).

:- doc(title, "Normal Form Polynom").
:- doc(author, "Luthfi Darmawan").
:- doc(summary, "Module related to polynom representation").
:- doc(module, "Module related to polynom representation. 
@em{Important Note}: all polynoms
representations in this module is in descending order, following Ciao Normal
Form of arithmetic expression representation

Some of the codes were taken from algebraic normal form,
attributions belong to the original authors.").

%------------------------------------------------------------------------------
%change log:
% 2011-7-22 : optimize exponentiation "**"
% 2011-5-12 : handle a^(x+c) and a^(x-c)
% 2010-3-1  : add single_variable/1, it was in math_polynom
% 2010-2-17 : clean up commented codes
% 2011-2-10 : 1. clean up unused functions

%------------------------------------------------------------------------------


%auxiliary polynom
%  Transform a normal-form primary or item into a general-form expression.
%
item_general_form(I,I) :-
	variable(I),
	!.
item_general_form(exp(E1,E2),exp(G1,G2)) :-
	!,
	general_form(E1,G1),
	general_form(E2,G2).
item_general_form(log(E1,E2),log(G1,G2)) :-
	!,
	general_form(E1,G1),
	general_form(E2,G2).
item_general_form(fact(E),fact(G)) :-
	!,
	general_form(E,G).
item_general_form(I,Y) :-
	functor(I,sum,4),
	functor(Y,sum,4),
	!,
	function_general_form(4,I,Y).
item_general_form(I,Y) :-
	functor(I,prod,4),
	functor(Y,prod,4),
	!,
	function_general_form(4,I,Y).
item_general_form(I,Y) :-
	functor(I,arg,2),
	functor(Y,arg,2),
	!,
	function_general_form(2,I,Y).
item_general_form(I,Y) :-
	functor(I,arity,1),
	functor(Y,arity,1),
	!,
	function_general_form(1,I,Y).
item_general_form(I,Y) :-
	functor(I,head,1),
	functor(Y,head,1),
	!,
	function_general_form(1,I,Y).
item_general_form(I,Y) :-
	functor(I,tail,1),
	functor(Y,tail,1),
	!,
	function_general_form(1,I,Y).
item_general_form(I,Y) :-
	userfunc(I),
	functor(I,F,N),
	functor(Y,F,N),
	function_general_form(N,I,Y).

%
%  Transform a normal-form user-defined function into a general-form expression.
%
function_general_form(0,_,_) :- !.
function_general_form(N,I,Y) :-
	N > 0,
	arg(N,I,Arg),
	general_form(Arg,Arg1),
	arg(N,Y,Arg1),
	N1 is N-1,
	function_general_form(N1,I,Y).

%  Transform the normal-form items into a general-form expression.
%
items_general_form([],Y,Y).
items_general_form([I|Is],X,Y) :-
	item_general_form(I,Y1),
	multiply(X,Y1,Y2),
	items_general_form(Is,Y2,Y).


%------------------------------------------------------------------------------
%------------------------------------------------------------------------------
%                     From this point below it is about polynom
%------------------------------------------------------------------------------

%------------------------------------------------------------------------------
:- doc(polynomize/2, "main predicate converting ciao function expression 
to complete polynom. The definition is:
@includedef{polynomize/2}
").
%------------------------------------------------------------------------------
polynomize(Exp, Polynom):-
	normal_form(Exp,ExpNF),
	polynom_form(ExpNF,PPolynom),
	marshall_polynom(PPolynom, Polynom).

%------------------------------------------------------------------------------
:- doc(polynom_form/2,"Transforms Ciao normal form (NF) into 
e.g. [p(exp(2,$(1)),1),p(1,-1000)].
In Ciao NF, the order of polynom is sorted descending
but we need to make it ascending, X^0+...+X^n according to GSL specification
the reversal action will be done before calling gsl library").
% here is an example:
% polynom_form(expr([],[factor([exp(expr([],[factor([],2)]),expr([],[factor([...],1)]))],1),factor([],-1000)]),[p(exp(2,$(1)),1),p(1,-1000)]) ? 
%------------------------------------------------------------------------------
polynom_form(expr([],F),P):-
	factor_polynom(F,P).

factor_polynom([],[]).
factor_polynom([F|Fs],[p(E,Fact)|Pol]):-
	F=factor(Expr,Fact),
	items_general_form(Expr,1,E),
	factor_polynom(Fs,Pol).

%------------------------------------------------------------------------------
:- doc(single_variable/1, "Check if that the input is a single variable, not
a complex expression").
single_variable($(_)).

%------------------------------------------------------------------------------
% marshall_polynom:
%transforms PPol list, [p(E,Factor)|PPols], 
%e.g. [p(exp(2,$(1)),1),p(1,-1000)], into list of coefficient of polynomials.
:- doc(doinclude, marshall_polynom/2).

:- doc(marshall_polynom/2,"transforms structure like [p(exp(2,$(1)),1),p(1,-1000)]
into list of coefficient of polynomials. The polynomial result is a complete
polynomial, thus it is a list which consists of the coefficients of the polynom from order 0
to the highest order.
It is capable of transforming these function into polynomial:
@begin{itemize}
@item @bf{Exponential}, by approximation using taylor series. However currently it
is only accurate on position near x=0, and the error become very large when
it approximates a function around a big number. An experimental predicate is
available for this purpose, marshall_polynom/3, but it needs changes on the 
source code, there is not yet user interface for using this feature. 

This approximation doesn't cover arbitrary exponential expressions. When we
have exponential with the form @var{exp(Base, Order) [op Expr]} we have the following 
restrictions:
   @begin{itemize}
   @item Expression under '[' and ']' is optional.
   @item @var{Base} must be a non-negative number, for it may cause non-continous 
 function. Expression is not accepted.
   @item @var{Order} must be a variable. Expression is not accepted.
   @item @var{op} can only be multiplication ('*'). Division ('/') can be a future work.
   @item @var{Expr} can only be a variable. Support for an expression can be done
for future work by building predicates for multiplication and division of polynomial.
   @end{itemize}
@item @bf{Sum}, when sum expression is simple, it will be transformed into exponential or 
 polynomial by the normal form predicate. However when it is complex, we do
a @em{rude} simplification by cutting the least significant part of the sum
expression.
@end{itemize}
").
%------------------------------------------------------------------------------

%transforms sum, by doing _RUDE_ simplifications
% - when sum expr is simple, it will be transformed into exponential or 
%   polynomial by the normal form predicate
% - in other word, we cannot proceed with the complex sum expr
%------------------------------------------------------------------------------
marshall_polynom([p(sum(Var,Beg,End,Expr),Factor)|PPols],Polynom):-
	rude_simplify(Expr,SimplExpr),
	polynomize(sum(Var,Beg,End,SimplExpr), PolynomS), %note the output is 
	                                                  % polynom already
	(
	    Factor == 1 ->
	    Polynom1 = PolynomS
	    ;%else
	    polynom_times(PolynomS, Factor, Polynom1) %we left Factor of sum
	),
	marshall_polynom(PPols,Polynom2),
	add_polynom(Polynom1,Polynom2,Polynom).

%transforms exponential using taylor approx
%------------------------------------------------------------------------------

%a^x = e^(x ln a) = 1 + x ln a + ((x ln a)^2)/2! + ...
%a^(x+c) = a^c * a^x = a^c * (e^(x ln a)) = a^c * (1 + x ln a + ((x ln a)^2)/2! + ...)
%a^(x-c) = (a^x)/(a^c) = (e^(x ln a))/ (a^c) = (1 + x ln a + ((x ln a)^2)/2! + ...)/ (a^c)

marshall_polynom([p(exp(Term,Ord),Factor)|PPols],Polynom):-
	number(Term), 
	Term >= 0,         %Term must be a valid POSITIVE number
	( %try to handle simple expressions
	    single_variable(Ord), %only handles single variable, not a function
	    Nom = 1,
	    Denom = 1
	    ;
	    functor(Ord, Ops, 2),  % handles x-a and x+a, and the order is "var op const"
	    arg(1, Ord, Arg1),
	    arg(2, Ord, Arg2),
	    single_variable(Arg1),
	    number(Arg2),
	    (
		Ops == '-',
		Nom = 1,
		Denom is Term ** Arg2
	    ;
		Ops == '+',
		Nom is Term ** Arg2,
		Denom = 1
		
	    )
	),
	%The approximation will be order 8 taylor polynomial
	
	ln(Term,LnTerm),
	fact_poly(8,Fact8),
	fact_poly(7,Fact7),
	fact_poly(6,Fact6),
	fact_poly(5,Fact5),
	fact_poly(4,Fact4),
	fact_poly(3,Fact3),
	fact_poly(2,Fact2),
	fact_poly(1,Fact1),

% 	power(LnTerm,8,PowerLnTerm8),
% 	power(LnTerm,7,PowerLnTerm7),
% 	power(LnTerm,6,PowerLnTerm6),
% 	power(LnTerm,5,PowerLnTerm5),
% 	power(LnTerm,4,PowerLnTerm4),
% 	power(LnTerm,3,PowerLnTerm3),
% 	power(LnTerm,2,PowerLnTerm2),
% 	power(LnTerm,1,PowerLnTerm1),

	F8 is Factor*LnTerm**8*Nom/(Fact8* Denom),
	F7 is Factor*LnTerm**7*Nom/(Fact7* Denom),
	F6 is Factor*LnTerm**6*Nom/(Fact6* Denom),
	F5 is Factor*LnTerm**5*Nom/(Fact5* Denom),
	F4 is Factor*LnTerm**4*Nom/(Fact4* Denom),
	F3 is Factor*LnTerm**3*Nom/(Fact3* Denom),
	F2 is Factor*LnTerm**2*Nom/(Fact2* Denom),
	F1 is Factor*LnTerm**1*Nom/(Fact1* Denom),
	F0 is Nom/Denom,

	%Polynom1=[p(exp(Ord,8),F8),p(exp(Ord,7),F7),p(exp(Ord,6),F6),p(exp(Ord,5),F5),p(exp(Ord,4),F4),p(exp(Ord,3),F3),p(exp(Ord,2),F2),p(exp(Ord,1),F1),p(1,1)], %let it be here as an artifact of wrong analysis
	Polynom1=[F8,F7,F6,F5,F4,F3,F2,F1,F0],
	marshall_polynom(PPols,Polynom2),
	add_polynom(Polynom1,Polynom2,Polynom).


% marshall_polynom(PPol,Polynom):-
% 	PPol=[p(E,_Factor)|PPols],
% 	E=exp(Term,_Ord),
marshall_polynom([p(exp(Term,_Ord),_Factor)|PPols],Polynom):-

	number(Term),
	Term < 0,  %In case term is NEGATIVE we ignore it!! give [0] 
	           %just to make the computation continue 
	           %Why? it will be non-continuous function
	Polynom1=[0],
	marshall_polynom(PPols,Polynom2),
	add_polynom(Polynom1,Polynom2,Polynom).

%end approx taylor

%                 __complex exponential expression__
%
% e.g. exp(2, $(1)) * $(1)
% according to the format, the possible functor are '/' and '*'
% and the expression following exponential expression is polynomial (simple variable)
%------------------------------------------------------------------------------
marshall_polynom([p(ComplexExp,Factor)|PPols],Polynom):-
	exp_in_expr(ComplexExp), %there is exist exponential expression
	
	functor(ComplexExp, F, N),
	N == 2, %binary ops
	arg(1, ComplexExp, Arg1),
	arg(2, ComplexExp, Arg2),
	marshall_polynom([p(Arg1,1)], PolynomS),
	variable(Arg2),
	(
	    F == '*', %multiply polynom with a variable, ie. x^2 -> x^3
	    insert_last(PolynomS, 0, PolynomSS) %because polynom in descending order
	;
	    F == '/',
	    format(user, "marshall_polynom, complex exponential division is not yet implemented",[])
	),

	(
	    Factor == 1 ->
	    Polynom1 = PolynomSS
	    ;%else
	    polynom_times(PolynomSS, Factor, Polynom1) %we left Factor of sum
	),

	marshall_polynom(PPols,Polynom2),
	add_polynom(Polynom1,Polynom2,Polynom).

%ordinary polynom
%------------------------------------------------------------------------------
marshall_polynom([p(exp(E,Ord),Fact)|PPols],Polynom):-
	variable(E),
	integer(Ord),
	Ord1 is Ord -1,
	complete_polynom(Ord1,Polynom1), %creating complete form polynom
	                               %from the higher to lowest order
	                               %X^3= X^3+0X^2+0X^1+0X^0
	marshall_polynom(PPols,Polynom2),
	add_polynom([Fact|Polynom1],Polynom2,Polynom).


marshall_polynom([p(E,Fact)|PPols],Polynom):-
	variable(E),             %cover  non power series
	complete_polynom(0,Polynom1), %creating complete form polynom
                                     %from the higher to lowest order
                                     %X = 1X^1+0X^0
	marshall_polynom(PPols,Polynom2),
	add_polynom([Fact|Polynom1],Polynom2,Polynom).

%marshall_polynom(PPol,[Fact]):-
%	PPol=[p([],Fact)|_].%constant (1 May 2009, it is wrong and impossible)
                             %because items_general_form never returns []
                             %change into below

%constant
%------------------------------------------------------------------------------
marshall_polynom([p(exp(A,B),Fact)|PPols],Polynom):-
	number(A),
	number(B),
	Constant_eval is Fact * A ** B,
	marshall_polynom(PPols,Polynom2),
	add_polynom([Constant_eval],Polynom2,Polynom).


% marshall_polynom(PPol,[Fact]):-
% 	PPol=[p(1,Fact)|_].%constant (1 May 2009, new version of constant)

marshall_polynom([p(1,Fact)|PPols],Polynom):-
	marshall_polynom(PPols,Polynom2),
	add_polynom([Fact],Polynom2,Polynom).

%empty case base
%------------------------------------------------------------------------------
marshall_polynom([],[]). 


:- set_prolog_flag(multi_arity_warnings,off).
:- doc(doinclude, marshall_polynom/3).
:- doc(marshall_polynom/3,"marshall_polynom/3 is going to be experimental feature to 
	have approximation position on other point than zero").

marshall_polynom([],_,[]). 

marshall_polynom([p(exp(Term,Ord),Factor)|PPols],Pos, Polynom):-
	number(Term), 
	Term >= 0,         %Term must be a valid POSITIVE number
	( %try to handle simple expressions
	    single_variable(Ord), %only handles single variable, not a function
	    Nom = 1,
	    Denom = 1
	    ;
	    functor(Ord, Ops, _NbArg),  % handles x-a
	    Ops == '-',
	    arg(1, Ord, Arg1),
	    arg(2, Ord, Arg2),
	    single_variable(Arg1),
	    number(Arg2),
	    Nom = 1,
	    Denom = Arg2
	),

	%control whether position is specified
%	power(2.71828183, Pos, Position),
	Position is 2.71828183**Pos,
	%The approximation will be order 8 taylor polynomial
	
	ln(Term,LnTerm),
	fact_poly(8,Fact8),
	fact_poly(7,Fact7),
	fact_poly(6,Fact6),
	fact_poly(5,Fact5),
	fact_poly(4,Fact4),
	fact_poly(3,Fact3),
	fact_poly(2,Fact2),
	fact_poly(1,Fact1),

% 	power(LnTerm,8,PowerLnTerm8),
% 	power(LnTerm,7,PowerLnTerm7),
% 	power(LnTerm,6,PowerLnTerm6),
% 	power(LnTerm,5,PowerLnTerm5),
% 	power(LnTerm,4,PowerLnTerm4),
% 	power(LnTerm,3,PowerLnTerm3),
% 	power(LnTerm,2,PowerLnTerm2),
% 	power(LnTerm,1,PowerLnTerm1),

	F8 is Position*Factor*LnTerm**8*Nom/(Fact8* Denom),
	F7 is Position*Factor*LnTerm**7*Nom/(Fact7* Denom),
	F6 is Position*Factor*LnTerm**6*Nom/(Fact6* Denom),
	F5 is Position*Factor*LnTerm**5*Nom/(Fact5* Denom),
	F4 is Position*Factor*LnTerm**4*Nom/(Fact4* Denom),
	F3 is Position*Factor*LnTerm**3*Nom/(Fact3* Denom),
	F2 is Position*Factor*LnTerm**2*Nom/(Fact2* Denom),
	F1 is Position*Factor*LnTerm**1*Nom/(Fact1* Denom),

	%Polynom1=[p(exp(Ord,8),F8),p(exp(Ord,7),F7),p(exp(Ord,6),F6),p(exp(Ord,5),F5),p(exp(Ord,4),F4),p(exp(Ord,3),F3),p(exp(Ord,2),F2),p(exp(Ord,1),F1),p(1,1)], %let it be here as an artifact of wrong analysis
	Polynom1=[F8,F7,F6,F5,F4,F3,F2,F1,1],
	marshall_polynom(PPols,Polynom2),
	add_polynom(Polynom1,Polynom2,Polynom),!.
:- set_prolog_flag(multi_arity_warnings,on).

%------------------------------------------------------------------------------
% rudely simplify an expression in a hope finding a form that can be translate
% into polynomial form
% arg(1,..) will take the front part of expression. eg. arg(1, 1+2+3, 1+2)
%------------------------------------------------------------------------------
rude_simplify(Expr, SimplExpr):-
	arg(1, Expr, SimplExpr).

%------------------------------------------------------------------------------
% multiply each term in polynom with Factor
%------------------------------------------------------------------------------
polynom_times([], _Factor, []).
polynom_times([Term|Polynom], Factor, [Res|PolynomRes]):-
	Res is Term * Factor,
	polynom_times(Polynom, Factor, PolynomRes).

%------------------------------------------------------------------------------
% complete polynomials from highest order to 0 order
% rev 23 Nov 2010
%    - it only complete _one term_
%    - change from /3 to /2
%------------------------------------------------------------------------------
complete_polynom(Ord,[]):- Ord<0.

complete_polynom(Ord,[0|P]):-
	Ord >= 0,
	Ord1 is Ord - 1,
	complete_polynom(Ord1,P).


%------------------------------------------------------------------------------
%------------------------------------------------------------------------------
varexp($(_)).

%------------------------------------------------------------------------------
% check whether there are any variable in an expression
%------------------------------------------------------------------------------
%terrible reccursive
var_in_expr(Expr):-
	varexp(Expr).

var_in_expr(Expr):-
	functor(Expr, _, N),
	N > 0,
	arg_explore(Expr, N).

arg_explore(Expr,1):-
	arg(1, Expr, ArgExpr),
	var_in_expr(ArgExpr).

arg_explore(Expr, N):-
	N>1,
	arg(N, Expr, ArgExpr),
	(
	    var_in_expr(ArgExpr)
	;
	    N1 is N - 1,
	    arg_explore(Expr, N1)
	).


%------------------------------------------------------------------------------
% check whether there are any exponential in an expression
%------------------------------------------------------------------------------
%terrible reccursive
exp_in_expr(exp(_,Ord)):-
	var_in_expr(Ord).

exp_in_expr(Expr):-
	functor(Expr, _, N),
	N > 0,
	arg_explore_exp(Expr, N).

arg_explore_exp(Expr,1):-
	arg(1, Expr, ArgExpr),
	exp_in_expr(ArgExpr).

arg_explore_exp(Expr, N):-
	N>1,
	arg(N, Expr, ArgExpr),
	(
	    exp_in_expr(ArgExpr)
	;
	    N1 is N - 1,
	    arg_explore_exp(Expr, N1)
	).



% 
%----------------  SELF DOCUMENTATION OF THIS FILE -----------------------------
% The ADT used in this file is complicated. Therefore this run result driver
% hopefully will be useful to recall it
%------------------------------------------------------------------------------

%pp_qsort([p(exp($(1),5),1),p(exp($(1),4),1),p(exp(3,$(2)),1),p(exp(2,$(1)),1)],L).

%polynomize(0.5*exp($(1),5)+1.5* $(1)+1+1,L).
%polynomize(0.5*exp(5,$(1))+1.5* $(1)+1+1,L).
%polynomize(_,[]).%handling uncovered function \comment:uncovered function will be considered as fail

%normal_form_polynom:polynom_form(expr([],[factor([exp(expr([],[factor([],2)]),expr([],[factor([...],1)]))],1),factor([],-1000)]),[p(exp(2,$(1)),1),p(1,-1000)])
%sum sum($(j),1,length(A),exp(2,length(A)- $(j))
%sum($(j),1,length(A),exp(2,length(A)- $(j))* $(j))
%sum($(j),1,$(1),exp(2,$(1)- $(j))* $(j))

%normalize_cost(0.5*exp(5,$(1))+0.5*exp($(1),3)+1.5* $(1)+1+1,L).
%normalize_cost(sum($(j),1,$(1),exp(2,$(1)- $(j))* $(j)),C)


%marshall_polynom([p(exp(1.618033988749895,$(1)),1.447213595499958),p(exp(-0.6180339887498949,$(1)),0.5527864045000421),p(1,-1.0)],R).

%pp_qsort([p(sum($(j),1,$(2),exp(2,- $(j)+ $(2))* $(j)),1),p(exp(2,$(2)-1)* $(2),1),p(exp(2,$(2)),2.0),p(1,-1.0)],P).

%normal_form_polynom:marshall_polynom([p(exp(2,$(2)-1)* $(2),1),p(exp(2,$(2)),2.0),p(1,-1.0)],P).