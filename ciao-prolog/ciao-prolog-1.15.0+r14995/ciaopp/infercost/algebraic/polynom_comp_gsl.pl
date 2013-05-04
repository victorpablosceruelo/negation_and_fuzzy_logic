:- module(polynom_comp_gsl,
	[ 
	  difference/3,
	  polynom_root_interval/3,
	  validate_polynom/2,
	  compute_safe_intervals/4
	],[assertions,foreign_interface]).
:- use_module(infercost(algebraic(math_polynom)), [eval_arith/3]).
:- use_module(library(lists), [length/2]).
:- use_module(library(sort), [sort/2]).


:- include(library(gsl_imports(gsl_imports_auto))).


:- doc(title,"Module for polynomial function comparison").  
:- doc(author,"Luthfi Darmawan").  

:- doc(summary,"This module provides procedures for comparing
   polynomial functions or some other mathematical functions that can
   be approximated using polynomials. Using the GSL library this
   procedure will provide the intervals for which a polynomial is
   greater/less than another.").

:- doc(module,"This module provides procedures for comparing
   polynomial functions or some other mathematical functions that can
   be approximated using polynomial functions. Using the GSL library,
   this procedure will provide the intervals for which a polynomial is
   greater/less than another.  For comparing two polynomials P1 and
   P2, we create an expression of the form P1-P2, and then look for
   the roots of equation P1-P2=0.  GSL is called (using the C
   interface) to obtain the roots. Then, from these roots, we obtain
   the intervals for which a polynomial function is greater/less than
   the other.").

%Change log: 
%    - 2011 June 6: bug fix where the safe root searching move one point forward 
%                   so in the end it tends to have 'check' interval
%    - 2011 May 30: - prevent infinite safe root searching 
%    - 2011 March 28: - move the code for calling gsl into ciao/contrib/math/gsl_imports.pl
%    - 2011 Feb 23: - error code from GSL is transfered using dedicated parameter
%                     instead of implicitly inside the list of root data
%    - 2011 Feb 17: - clean up commented code
%    - 2011 Feb 15: -Drop change on 2011 Feb 11, change it using polynom instead of 
%                    arithmetic cost function
%                   -compute_safe_root is using current root value as Left, Left is removed,
%                    and Right is guaranteed not to exceed 2nd Boundary 
%    - 2011 Feb 11: fundamental change on polynom_root_interval, using original arithmetic
%                   expression for evaluating value on a certain point. DROPPED on 2010 Feb 15
%    - 2011 Feb 10: cleanup unused functions
%    - On compute_safe_intervals, there no case analysis for computing safe root
%      even though it is safe we call compute_safe_root to obtain the safe
%      integer of the the root.
%    - 21 Sept 2010 
%        - cleaning compute_safe_interval_for_polynom
%        - cleaning unused codes and variables						

% :- doc(doinclude, polynomial_root/5).
% :- true pred polynomial_root(in(LengthIn),in(LengthOut),in(X),go(Y), go(Err))::
% 	int*int*double_list * double_list * int + (foreign,size_of(X,LengthIn),size_of(Y,LengthOut)) #
%  "obtains roots of a polynomial function by calling foreign C program which will call GSL solver.  @var{Err} is
%  error code, 0 when GSL succeed, -1 otherwise".

:- doc(doinclude, polynom_root/4).
:- doc(polynom_root/4, "This is the core of polynomial comparison, by 
	searching the roots of the polynom.
Order is highest order of polynom, According to fundamental theorem of algebra
	any order n polynomial has n root as well, real or complex 
 Out is list with composite values <real,imaginary>, therefore the length will be 2*order").
polynom_root(_,[_In],[],_). %exception on gsl,gsl: zsolve.c:55: ERROR: cannot solve for only one term
polynom_root(_,[],[], _).
polynom_root(Order,In,Out, ErrCode):-
    LenIn is Order + 1,
    length(In,LenIn),%failure to ensure this condition will lead to problem
    LenOut is Order*2,
    polynomial_root(LenIn,LenOut,In,Out, ErrCode).

interval_value_p(_,[],[]).
interval_value_p(Polynom, [R1|Rs], [ResR0|Interval]):-
	R0 is R1 - 1,
	eval_polynom(Polynom, 0, R0, ResR0),
	interval_value_p_core(Polynom, [R1|Rs], Interval).

interval_value_p_core(Polynom,[R], [R,ResR1]):- %base
	R1 is R + 1,
	eval_polynom(Polynom, 0, R1, ResR1).

interval_value_p_core(Polynom, [R1,R2|Rs], [R1,ResRMid|Interval]):-
	RMid is (R1 + R2)/2,
	eval_polynom(Polynom, 0, RMid, ResRMid),
	interval_value_p_core(Polynom, [R2|Rs], Interval).

:- doc(doinclude, eval_polynom/4).
:- doc(eval_polynom/4, "Evaluate the value of polynom in certain point. 
@var{Polynom} is ordered in ascending degree").
%eval_polynom(Polynom, CurrentOrder, X, Y
eval_polynom([], _CurrDeg, _X, 0).
eval_polynom([Coef| Polynom], CurrDeg, X, Y):-
	Y1 is Coef * X ** CurrDeg,
	CurrDeg1  is CurrDeg + 1,
	eval_polynom(Polynom, CurrDeg1, X,  Y2),
	Y is Y1 + Y2.

:- doc(difference/3, "difference between two polynomials").
difference([],[],[]).
difference([A|P1],[],[A|PR1]):-
	difference(P1,[],PR1).

difference([],[B|P2],[BMin|PR1]):-
	BMin is -B,
	difference([],P2,PR1).

difference([A|P1],[B|P2],[R|PR1]):-
	R is A-B,
	difference(P1,P2,PR1).

:- doc(scrap_imaginary/2, "delete imaginary roots on the list of
root obtained by GSL. Each root is represented as <real,imaginary>,
when the imaginary part is not 0.0 it means imaginary root.
The result is real root without imaginary part").
scrap_imaginary([],[]).
scrap_imaginary([A,I|L],[A|P]):-
	I==0.0,!,
	scrap_imaginary(L,P).
scrap_imaginary([_,_|L],P):-
	scrap_imaginary(L,P).


%11 Feb 2010 adding ArithExp for evaluating values between roots
:- doc(polynom_root_interval/3, "obtains roots and  compute the 
evaluation of @var{Pol} on the value between roots. 
However this value between roots is merely a reference, because it
is computed from a polynomial approximation, instead of the real function.
Note that we reserve @var{_ArithExp} for future development when
accuracy is an important demand").

:- pred polynom_root_interval(_ArithExp, Pol,Intervals)#"@var{Pol} is a Polynomial
in ascending order".
polynom_root_interval(_ArithExp, Pol,Intervals):-
	length(Pol,LenPol),
	Order is LenPol-1,
	polynom_root(Order,Pol,Roots,ErrCode),!,
	(
	    ErrCode == -1 -> 
	    Intervals=[2]% our error code for GSL unconvergence algorithm
	;     
	    scrap_imaginary(Roots,RealRoots),
	    sort(RealRoots,SortedRealRoots),
%	    interval_value(ArithExp,SortedRealRoots,Intervals)
	    interval_value_p(Pol,SortedRealRoots,Intervals)
	).



:- doc(validate_polynom/2, "validating polynom so the highest order not equal 0").
validate_polynom([A],[A]):- %remove 0 on highest order of polynom
	A\=0.

validate_polynom([0],[]).

validate_polynom([A|P],[A|Q]):-
	validate_polynom(P,Q),
	Q\=[].

validate_polynom([A|P],[]):-
	A=0,
	validate_polynom(P,[]).

validate_polynom([A|P],[A]):-
	A\=0,
	validate_polynom(P,[]).

%--------------------------------- compute_safe_intervals ---------------------
%
% change note:
%  - 31 May 2011 adding Error parameter to be passed across module
%    this solution is cleaner than putting error code on the result
%    More possible error can occur due to additional feature introduced.
%    Some code rewriting is also done to avoid append
%  - 24Nov2010 the polynom input is change into arithmetic expression
%------------------------------------------------------------------------------
:- doc(compute_safe_intervals/3, "Compute the safe interval position using
the original cost function. Since it is using the original function, it is
important to work on integer. Some functions are @em{undefined} in non-integer
domain.").
compute_safe_intervals(ArithExpr,Intervals,SafeIntervals, Error):- 
%3 or last 3 element interval, case + x -
	Intervals=[Sign1|RestIntervals1],
	Sign1>=0,
	RestIntervals1=[Boundary1|RestIntervals2],
	RestIntervals2=[Sign2|RestInterval3],
	RestInterval3=[], %to avoid cut, ensure disjoint
	Sign2<0,
	Boundary2 is Boundary1 + 1000, %Boundary2 is dummies
	compute_safe_root(ArithExpr,Boundary1,Boundary2,NewBoundary,Error1),
	(
	    Error1 == 0 ->
	    compute_safe_intervals(ArithExpr,RestIntervals2,RestSafeIntervals, Error2),    
	    Error is Error1 + Error2,
	    SafeIntervals = [Sign1,NewBoundary|RestSafeIntervals]
	;
	    Error = Error1
	).

compute_safe_intervals(ArithExpr, Intervals, SafeIntervals, Error):-
%more than 3 element interval, case + x -
	Intervals=[Sign1|RestIntervals1],
	Sign1>=0,
	RestIntervals1=[Boundary1|RestIntervals2],
	RestIntervals2=[Sign2|RestInterval3],
	RestInterval3=[Boundary2|_RestInterval4], %to avoid cut, ensure disjoint
	Sign2<0,
	compute_safe_root(ArithExpr, Boundary1, Boundary2, NewBoundary, Error1),
	(
	    Error1 == 0 ->
	    compute_safe_intervals(ArithExpr, RestIntervals2, RestSafeIntervals, Error2),
	    Error is Error1 + Error2,
	    SafeIntervals = [Sign1,NewBoundary|RestSafeIntervals]
	;
	    Error = Error1
	).

compute_safe_intervals(ArithExpr,Intervals,SafeIntervals, Error):-
%3 or last 3 element interval, case - x +
	Intervals=[Sign1|RestIntervals1],
	Sign1<0,
	RestIntervals1=[Boundary1|RestIntervals2],
	RestIntervals2=[Sign2|RestInterval3],
	RestInterval3=[], %to avoid cut, ensure disjoint
	Sign2>=0,
	Boundary2 is Boundary1 + 1000, %Boundary2 is dummies
	compute_safe_root(ArithExpr,Boundary1,Boundary2,NewBoundary,Error1),
	(
	    Error1 == 0 ->
	    compute_safe_intervals(ArithExpr,RestIntervals2,RestSafeIntervals,Error2),    
	    Error is Error1 + Error2,
	    SafeIntervals = [Sign1,NewBoundary|RestSafeIntervals]
	;
	    Error = Error1
	).

compute_safe_intervals(ArithExpr,Intervals,SafeIntervals, Error):-
%more than 3 element interval, case - x +
	Intervals=[Sign1|RestIntervals1],
	Sign1<0,
	RestIntervals1=[Boundary1|RestIntervals2],
	RestIntervals2=[Sign2|RestInterval3],
	RestInterval3=[Boundary2|_RestInterval4], %to avoid cut, ensure disjoint
	Sign2>=0,
	compute_safe_root(ArithExpr,Boundary1,Boundary2,NewBoundary, Error1),
	(
	    Error1 == 0 ->
	    compute_safe_intervals(ArithExpr,RestIntervals2,RestSafeIntervals, Error2),
	    Error is Error1 + Error2,
	    SafeIntervals = [Sign1,NewBoundary|RestSafeIntervals]
	;
	    Error = Error1
	).

compute_safe_intervals(ArithExpr,Intervals,SafeIntervals, Error):-
%case + x +
	Intervals=[Sign1|RestIntervals1],
	Sign1>=0,
	RestIntervals1=[Boundary1|RestIntervals2],
	RestIntervals2=[Sign2|_],
	Sign2>=0,
	compute_safe_intervals(ArithExpr,RestIntervals2,RestSafeIntervals, Error),    
	SafeIntervals=[Sign1,Boundary1|RestSafeIntervals].

compute_safe_intervals(ArithExpr,Intervals,SafeIntervals, Error):-
%case - x -
	Intervals=[Sign1|RestIntervals1],
	Sign1<0,
	RestIntervals1=[Boundary1|RestIntervals2],
	RestIntervals2=[Sign2|_],
	Sign2<0,
	compute_safe_intervals(ArithExpr,RestIntervals2,RestSafeIntervals, Error),    
	SafeIntervals=[Sign1,Boundary1|RestSafeIntervals].

%residual 1 element for sign
compute_safe_intervals(_ArithExpr,[Sign],[Sign],0).

%empty case
compute_safe_intervals(_ArithExpr,[],[],0).

%------------------------------------------------------------------------------
:- doc(compute_safe_root/5, "look for safe root position").
%------------------------------------------------------------------------------
compute_safe_root(Expression, Boundary1, Boundary2, NewBoundary, Error):-
	Right1 is Boundary1 + 1,
	(
	    Right1 < Boundary2 ->
	    Right = Right1
	;
	    Right is (Boundary1+Boundary2)/2
	),
	eval_arith(Expression, Boundary1, ResEval),
	ResEvalLeft = ResEval,
	eval_arith(Expression, Right, ResEvalRight),
	% warning the result of these computation upon might be number 0.Nan
	%   in that case we force to use search safe root 
	%   comparison operator (<,>,=<,>=) fail on 0.Nan
	(   
	    %goes up, take ceiling
	    ResEvalLeft < ResEvalRight -> 
	    Boundary1p is ceiling(Boundary1),
	    Dir = right
	; %goes down, take floor
	    ResEvalLeft >= ResEvalRight -> 
	    Boundary1p is floor(Boundary1),
	    Dir = left
	; %otherwise, it will always go here on 0.Nan
	    Boundary1p is floor(Boundary1),
	    Dir = unknown
	),
	( 
	    ResEval >= 0 -> % it will be failed on 0.Nan
	    NewBoundary = Boundary1p,
	    Error = 0
	;% ResEval < 0
	    search_safe_root(Expression, Dir, Boundary1p, NewBoundary, Error)
	).

%------------------------------------------------------------------------------
% auxiliary pred of look for safe root
% here the root to be test is an integer
%------------------------------------------------------------------------------
search_safe_root(Expression, Dir, Boundary, NewBoundary, Error):-
	eval_arith(Expression, Boundary, ResEvalBoundary),
	(
	    ResEvalBoundary >= 0 ->
	    NewBoundary = Boundary,
	    Error = 0
	;    
	    (
		Dir = right ->
		Right is Boundary + 1,
		search_right(Expression, ResEvalBoundary, Right, NewBoundary, Error)
	    ;
		Dir = left->
		Left is Boundary - 1,
		search_left(Expression, ResEvalBoundary, Left, NewBoundary, Error)
	    ;
		Dir = unknown ->
		Left is Boundary - 1,
		Right is Boundary + 1,
		eval_arith(Expression, Left, ResEvalLeft),
		eval_arith(Expression, Right, ResEvalRight),
		(
		    ResEvalLeft < ResEvalRight -> %goes up, go right
		    search_right(Expression, ResEvalBoundary, Right, NewBoundary, Error)
		; % go left
		    search_left(Expression, ResEvalBoundary, Left, NewBoundary, Error)
		)
	    )
	).

search_left(Expr, PrevEval, Root, NewRoot, Error):-
	eval_arith(Expr, Root, Res),
	(
	    Res < 0 ->
	    (
		Res > PrevEval ->
		Root1 is Root - 1,
		search_left(Expr, Res, Root1, NewRoot, Error)
	    ;
		%computation is not convergent
		Error=1
	    )
	; %Res >=0
	    NewRoot = Root,
	    Error = 0
	).

search_right(Expr, PrevEval, Root, NewRoot, Error):-
	eval_arith(Expr, Root, Res),
	(
	    Res < 0 ->
	    (
		Res > PrevEval ->
		Root1 is Root + 1,
		search_right(Expr, Res, Root1, NewRoot, Error)
	    ;
		%computation is not convergent
		Error=1
	    )
	; %Res >=0
	    NewRoot = Root,
	    Error = 0
	).
