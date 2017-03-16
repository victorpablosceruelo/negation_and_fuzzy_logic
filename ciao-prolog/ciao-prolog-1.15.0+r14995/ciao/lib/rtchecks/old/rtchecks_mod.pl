:- module( rtchecks_mod , [
	                 check/1,
	                 check/2,
%			 icheck/2,
			 checkc/2,
%			 checkif/2,
			 checkif/3,
			 check_comp/4,
			 push_parent_goal/1,
			 pop_parent_goal/1,
%			 rt_det_ncp/1,
%			 rt_det_1c/1,
%			 rt_nf/1,
			 dispatch_error/3,
			 inst/1
		      ] , [assertions, regtypes] ).


pop_parent_goal( _ ).
push_parent_goal( _ ).


:- use_package( library( 'ciaopp/api/ciaopp_api'   ) ).
%:- use_module(  library( 'ciaopp/api/xtchecks_msg' ) ).
:- use_module(engine(internals)).
:- use_module(library(terms_check)).
:- use_module(library(messages)).
% :- use_module(  library( vndict      ) ).

%       det   nf
% det      1   1
% semidet  1   0
% multidet 0   1
% nondet   0   0
%
% throw( rtcheck( det_nf  , MSG   , GOAL ) )  MSG == notdet ; MSG == fail
% throw( rtcheck( pre     , PRE   , GOAL ) )
% throw( rtcheck( post    , PRE   , GOAL ) )
% throw( rtcheck( pp_check, PRE   , LOC  ) )
% throw( rtcheck( assrt   , ASSRT , LOC  ) )

:- meta_predicate checkc( goal , ? ).
:- meta_predicate check(  goal ).
:- meta_predicate check(  goal , addmodule ).
:- meta_predicate checkif( ? , goal , addmodule ).

% :- meta_predicate icheckif( ? , goal ).
% :- meta_predicate icheck( goal , addmodule ).
% :- meta_predicate checkif( ? , goal ).

:- meta_predicate check_comp( ? , goal , ? , goal ).

:- meta_predicate rt_det_ncp( goal ).
:- meta_predicate rt_nf(      goal ).
:- meta_predicate rt_det_1c(  goal ).

:- doc( module , 

"This library package can be used by programmers directly and it is
also used by the run-time transformations performed by CiaoPP (see the
@tt{CiaoPP} manual) .  Predicates @pred{check/1}, @pred{checkc/2}, and
@pred{checkif/2} are meant typically to be used by programmers,
while @pred{check/2}, @pred{checkc/2}, and @pred{checkif/3} are
typically used by the CiaoPP transformations.  Note that the CiaoPP
run-time checks transformation will replace @pred{check/1} predicates
that appear in the original program with calls to @pred{check/2}, but
the end effect is the same.

As this is a reference manual, we only comment on predicates
@pred{check/1}, @pred{checkc/2} and @pred{checkif/2}.
Documentation on other predicates is left for the @tt{internals}
manual.  Predicate @pred{check/1} allows writing program-point check
assertions.  In other words, the user can write
@verbatim+check(Condition)+ whenever he/she thinks that Condition has
to hold.  The @pred{check/1} call will whther Conditions holds, and if
it does not, it will raise an exception.

The other two predicates: @pred{checkc/2} and @pred{checkif/2} are
used in conjunction and are useful to check postconditions that depend
on a precondition.  For example, assume that condition B has to be
checked only if condition A holds before calling predicate P.  After
calling predicate P it is not possible to check condition A because
some variables may have changed value, so we are need to check whether
condition A holds before calling P and pass this information to B. The
scheme used is:

@begin{verbatim}
clause :-
  ...,
  C is true if condition A holds,
  P,
  ( C -> check( B ) ; true ),
  ...
@end{verbatim}

@noindent
This is exactly what these two predicates do:

@begin{itemize}

@item check( CondA , H ). H is bound to 'true' if Cond holds.

@item checkif( H , CondB ). Performs a check( CondB ) if H is
   @tt{true} and an exception is raised only if H is @tt{true} and
   condB does not hold.

@end{itemize}
"

% "This library package can be used either by the user or by CiaoPP
% (see @tt{CiaoPP} manual) run-time transformations. When used by user
% the predicates @pred{check/1}, @pred{checkc/2} and
% @pred{checkif/2} are expected to be used. On the other hand,
% CiaoPP run-time transformacion will generate code for @pred{icheck/2},
% @pred{checkc/4} and @pred{checkif/3}. In fact, CiaoPP run-time
% transformation will generate code for @pred{check/1}, @pred{checkc/3}
% and @pred{checkif/3}, and it will be the run-time package which
% will add the 3rd (location) and 4th argument (module).

% The CiaoPP run-time transformation stores assertions in the
% transformated files, that are referred by the check-point
% predicates. These stored assertions are neccesary because the run-time
% transformation simulates compile-time checking process but in run
% time.

% All predicates raise an exception when the condition fails. This
% exception is caugh by the top-level and is printed depending on the
% run-time pretty printer is loaded in that moment. The conexion between
% run-time checks library and the run-time pretty printer is done via
% @pred{print} predicate (see @index{portray/1})."

% % " This library package allows the use of run-time
% % 	checks for the assertions introduced in a program.

% %         The recommended way of performing @index{run-time checks} of
% % 	predicate assertions in a program is via the Ciao preprocessor
% % 	(see @tt{CiaoPP} manual), which performs the required program
% % 	transformation. However, this package can also be used to
% % 	perform checking of program-point assertions.
% % "
).


:- regtype boolean/1.

boolean( true  ).
boolean( false ).

%----------------------------------------------------------------------------%

:- trust pred checkc( A , B ) : callable * boolean

# "@var{B} is boolean variable that express whether @var{A} had succeed.".

checkc( A , Out ) :-
	copy_term( A , AC ),
	AC,
	!,
	( instance( A , AC ) ->
	    Out = true 
	;
	    Out = false
	).
checkc( _A , false ).


%----------------------------------------------------------------------------%

:- pred check( X ) : term( X ) + native(check(X)).
:- trust pred check( A ) : callable

# "@var{A} is executed. If it fails, an exception is raised.".

:- trust pred check( A ) : term.

:- set_prolog_flag( multi_arity_warnings , off ).

% USED by someone that include rtcheck_mod and do not use the transformation.
% ALSO, user put check/1 and hope that CiaoPP translate it
check( A ) :-
	check_internal( A ),
	!.
check( A ) :-
	throw( rtcheck( pp_check , A , true ) ).

:- pred check( X , As ) : term( X ) + native(check(X)).
:- trust pred check( A , As , M ) : (callable(A), term(As), atom(M))

# "@var{A} is executed. If it fails, an exception is raised. @var{As}
  indicated the assertion that failed. @var{As} is a number that is
  used as key in '$saved_assertions'/3 multifile predicate.".

:- trust pred check( A , As , M ) : (term(A),term(As)).

% check( CC , AS , M ) :-
% 	icheck( CC , AS , M ).

% icheck( A , _ErrorInfo , _M ) :-
% 	check_internal( A ),
% 	!.
%
% icheck( Goal , ErrorInfo , M ) :-
%         dispatch_error( ErrorInfo , Goal , M ),
% 	!.
%
% % % Case 1: the pp check point comes from an assertion
% % icheck( _ , as(ErrorInfo,Loc) , M ) :-
% % %	num( ErrorInfo ),
% % 	'$saved_assertion'( M , ErrorInfo , MessageInfo ),
% % 	!,
% % 	throw( rtcheck( assrt , MessageInfo , Loc ) ).
% % % Case 2: it is user program check point => we only have the locator
% % icheck( Goal , Loc , M ) :-
% % 	functor( Loc , loc , _ ),
% % 	!,
% % 	transform_locator( Loc, M , Info ),
% % 	throw( rtcheck( pp_check, Goal, Info ) ).
% % Case 3: None of the above options => ERROR, something was added and
% %         we do not consider it here.
% % icheck( _ , ErrorInfo , M ) :-
% % 	error_message( "Program check point failed. INTERNAL ERROR: " ||
% % 		       "Unconsidered case of error ~q (module ~q)" , 
% % 		       [ ErrorInfo , M ] ).


check( A , _ErrorInfo , _M ) :-
	check_internal( A ),
	!.

check( Goal , ErrorInfo , M ) :-
        dispatch_error( ErrorInfo , Goal , M ),
	!.


%:- set_prolog_flag( multi_arity_warnings , on ).

check_internal( A ) :-
	copy_term( A , AC ),
	AC,
	!,
	instance( A , AC ).

% :- trust pred icheck( A , B , M ) : callable * term * atom
%
% # "The same as @pred{check/2} but with error information".


%----------------------------------------------------------------------------%

% :- set_prolog_flag( multi_arity_warnings , on ).


% :- trust pred checkif( A , B ) : boolean * callable
%
% # "If @var{Condition} is true, then @var{Post} is executed. If
% 	@var{Post} fails, then an exception is raised.".
%
% % --- used???
% % checkif( C , A ) :-
% % 	icheckif( C , A ),
% % 	!.
% % checkif( true  , A ) :- 
% % 	throw( rtcheck( post , A , true ) ).


:- trust pred checkif( Condition , Post , ErrorInfo , Module ) 
	: boolean * callable * term * atm

# "If @var{Condition} is true, then @var{Post} is
  executed. @var{ErrorInfo} and @var{Module} are used for printing
  error message. These values are got from CiaoPP run-time
  transformation in conjuntion with run-time library package.".

% checkif( C , A , _ , _ ) :-
% 	icheckif( C , A ),
% 	!.
checkif( false , _ , _ , _ ).
checkif( true  , Goal , _ , _ ) :- 
	check_internal( Goal ),
	!.
checkif( true  , Goal , ErrorInfo , M ) :- 
	dispatch_error( ErrorInfo , Goal , M ).
% checkif( true  , _ , as(ErrorInfo,Loc) , M ) :- 
% 	'$saved_assertion'( M , ErrorInfo , MessageInfo ),
% 	!,
% 	throw( rtcheck( assrt , MessageInfo , Loc ) ).
% checkif( true  , A , ErrorInfo , M ) :- 
% 	functor( ErrorInfo , loc , _ ),
% 	!,
% 	transform_locator( ErrorInfo, M , Info ),
% 	throw( rtcheck( pp_check , A , Info ) ).
% checkif( true , _ , ErrorInfo , M ) :-
% 	error_message( "Program check point failed. INTERNAL ERROR: " ||
% 		       "Unconsidered case of error ~q (module: ~q)" , 
% 		       [ ErrorInfo , M ] ),
% 	throw( rtcheck( pp_check , unknown , loc( unknown , 0 , 0 , M ) ) ).


%----------------------------------------------------------------------------%

	
:- set_prolog_flag( multi_arity_warnings , on ).

:- multifile '$saved_assertion'/3.

:- pred dispatch_error( ErrorInfo , Goal, M )
	: term * term * atom

# "Throws the appropiate assertion for the @var{ErrorInfo} term found
  in test cases predicates generated by @apl{CiaoPP}. @var{Goal} is
  the Goal that made the test case fails. @var{M} is the module of the
  @var{Goal}.".

% Case 1: the pp check point comes from an assertion
dispatch_error( ErrorInfo , _, M ) :- 
	functor( ErrorInfo , as , _ ),
	arg( 1 , ErrorInfo , AsNum ),
	arg( 2 , ErrorInfo , Loc ),
	( arg( 3 , ErrorInfo , Goal  ) -> true ; Goal = unknown ),
	!,
	( '$saved_assertion'( M , AsNum , Assrt ) ->
	      throw( rtcheck( assrt , assrt_goal( Assrt , Goal ) , Loc ) )
	;
	      error_message( "Program check point failed. INTERNAL ERROR: " ||
		       "When trying to locate assertion number ~w from module ~q)" , 
		       [ AsNum , M ] ),
	      throw( rtcheck( pp_check , unknown , loc( unknown , 0 , 0 , M ) ) )
	).
	    
% Case 2: it is user program check point => we only have the locator
dispatch_error( ErrorInfo , Goal , M ) :- 
	functor( ErrorInfo , loc , _ ),
	!,
	transform_locator( ErrorInfo, M , Info ),
	throw( rtcheck( pp_check , Goal , Info ) ).
% Case 3: None of the above options => ERROR, something was added and
%         we do not consider it here.
dispatch_error( ErrorInfo , _ , M ) :-
	error_message( "Program check point failed. INTERNAL ERROR: " ||
		       "Unconsidered case of error ~q (module: ~q)" , 
		       [ ErrorInfo , M ] ),
	throw( rtcheck( pp_check , unknown , loc( unknown , 0 , 0 , M ) ) ).



transform_locator( loc( F , LB, LE ), M, L ) :-
	!,
	L = loc${file => F, line_begin => LB, line_end => LE, module => M}.
transform_locator( L , _ , L ).


%----------------------------------------------------------------------------%

% :- trust pred icheckif( Condition , Post ) 
% 	: boolean * callable

% # "If @var{Condition} is true, then @var{Post} is executed.".

% icheckif( false , _ ).
% icheckif( true  , A ) :- 
% 	check_internal( A ).

:- trust pred inst( A ) : callable.

inst( A ) :-
	copy_term( A , AC ),
	AC,
	!,
	instance( A , AC ).

%----------------------------------------------------------------------------%

:- pred check_comp( Condition , CompGoal, CompGoalArg , Head )

# "If @var{Condition} is @tt{true} then the @var{CompGoal} containing
the nested comp predicate calls is called with @var{Head} as
argument. To allow efficient implementation, @var{CompGoalArg} is the
last nested argument of @var{CompGoal}, so unifiying with @var{Head}
we have the comp check, and calling directly to @var{Head} we 
skip the test. An example call could be:

@begin{verbatim}
check_comp(C,not_fails(is_det(A)),A,partition(_1,_2,_3,_4))
@end{verbatim}

so if C is true (it should come from @pred{checkc/2} predicate), then
A is unified with partition(_1,_2,_3,_4) and
not_fails(is_det(partition(_1,_2,_3,_4))) is called. Else, just
partiton(_1,_2,_3,_4) is called.".


:- trust pred check_comp( Condition , CompGoal, CompGoalArg , Head ) 
	: boolean * callable * var * callable.

check_comp( true , CompGoal , CompGoalArg , Head ) :-
	!,
	CompGoalArg = Head,
	call( CompGoal ).
check_comp( _ , _CompGoal , _CompGoalArg , Head ) :-
	call( Head ).


%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%


%:- meta_predicate rt_det_nf( goal ).
%% it is like rt_det( rt_nf ) == rt_nf( rt_det )
% rt_det_nf(X) :- 
% 	'$metachoice'( C0 ),
% 	X,
% 	'$metachoice'( C1 ),
% 	( C1 == C0 -> ! ; rtcheck( det_nf , nodet , X  ) ).
%
% rt_det_nf(X) :- throw( rtcheck( det_nf , fail , X  ) ).

%rt_det_nf(X) :- rt_det( rt_nf( X ) ).

% det Not Choice Points
rt_det_ncp(X) :- 
	'$metachoice'( C0 ),
	X,
	'$metachoice'( C1 ),
	( C1 == C0 -> true ; throw( rtcheck( det , nodet , X  ) ) ).

rt_nf(X) :- 
	'$metachoice'( C0 ),
	X,
	'$metachoice'( C1 ),
	( C1 == C0 -> ! ; true ).
rt_nf(X) :- throw( rtcheck( nf , fail , X  ) ).

% No choice-points are allowed after returning from the predicate 
rt_det_1c(X) :-
	Solved = solved(no),
	X,
	( 
	    arg(1, Solved, no) 
	-> 
	    true
	; 
	    throw( rtcheck( det , nodet , X  )) % more than one solution!
	),
        % Update without trailing: be careful!
        % (in this case, the operation is safe because we are 
        % writing one-cell terms without dereferencing 
        % chains)
	'$setarg'(1, Solved, yes, off). 

