:- module( rtchecks_common,
	[ 
	    is_the_first/1,
	    get_internal_name/2,
	    remember_name/2,
	    undo_internalnames/0,
	    comment_all_module_assertions/0,

	    process_assertions/8,
	    tail_comp/3,
	    specialize_comp/2
	] , 
	[assertions, api(ciaopp_api)] 
	).



%%% precompiler library
:- use_module(library(lists), [append/3]).


%%% LOW level

:- data internal_name/2.

undo_internalnames:-
	retractall_fact( internal_name( _ , _ ) ).




is_the_first( ClKey ) :-
	functor( ClKey , F , A ),
	\+ current_fact( internal_name( F/A , _ ) ).




get_internal_name( ClKey , ClIntName ) :-
	functor( ClKey , F , A ),
	current_fact( internal_name( F/A , ClIntName ) ).




comment_all_module_assertions :-
	get_key_predicates( [internal] , Ps ),
	member( P , Ps ),
	get_assertions( P , A ),
	erase_assertions( A ),
	add_commented_assertions( A ),
	fail.

comment_all_module_assertions.




% The order here is really important, because
% this function is used with call and success and entry and exit.
% See this case:
% :- entry p...
% :- call p...
% p:-
%    ...
%    p
%    ...
%
% p :- ...
% Now the process is this:
%
% has_assertions( entry and exit , p)?
%   then => 
%           p :-
%                CALL, p_1
%           p_1 :- orignal p (remember_name(p,p_1))
%  
%   has_assertions( call and success , p )?
%   then => 
%           p :-
%                CALL, p_2
%           p_2 :- orignal p (remember_name(p,p_2))
%      BUT here original p is p_1, so:
%
%  p   :- check(entry and exit), p_1
%  p_1 :- check(calls and success), p_2
%  p_2 : just the p that the programmer wrote!
%
%  Take the second clause of p... we have to rename p to p_2 (it is the
%  internal one, it is, the two pass user p predicate.

remember_name( ClKey , ClNew ) :-
	functor( ClKey , F , A ),
	asserta_fact( internal_name( F/A , ClNew ) ).



:- pred process_assertions( Assrt , Number , Call , 
                            Loc , OHead ,
			    Head , NHead, Result ) 
 : ( list(Assrt, t_as), int(Number), t_cls_key(Call), 
     t_loc(Loc) , term(OHead), term(Head), term(NHead), term(Result) )

# "@var{Assrt} is a list of assertions. Each assertion is assigned an
  id (a number) that identifies them univocally. This id is used to
  identify the assertion when a test case fails (so this number is
  passed as argument to the test case). @var{Number} is the starting
  number to use when assignating an id to an assertion. @var{Call} is
  the ClauseKey used to unify with the assertion head of each element
  of the @var{Assrt} list. @var{Loc} and @var{OHead} (clause locator
  and original predicate head) are the terms passed to every test case
  that will be used when reporting an error to the user. @var{Head} is
  the head of the internal predicate to be called, this will be only
  used in the check_comp test cases. @var{NHead} is the head comming
  from the combination of check_comp test cases. @var{Result} is a
  pair (Pre,Post) in which Pre are the test cases to be called before
  the orignal predicate (@var{NHead}) and Post the ones after.

As an Example, if we are transformating a simple qsort and the final
scheme looks like:


@begin{verbatim}
qsort(A,B) :-
        _1=loc('/home/dtm/CiaoDE/ciaopp/examples/qsort.pl',20,27,qsort),
        check((list(A,num),var(B)),as(0,_1,'qsort:qsort_1'(A,B))),
        qsort_1(A,B).
@end{verbatim}

the call to process_assertions would be something like:

@begin{verbatim}
process_assertions( AssrtList , 0 , qsort(A,B) , 
	            LOC , GOAL
                    loc( where the clause is ), 
		    NHead, Result ) 
@end{verbatim}

Notice that @var{Call} and @var{Head} are used to unify header
 arguments. LOC and GOAL are variables because you usually want to
 have something like: 

@begin{verbatim}
qsort(A,B) :-
        _1=loc('/home/dtm/CiaoDE/ciaopp/examples/qsort.pl',20,27,qsort),
	_2='qsort:qsort_1'(A,B),
        check((list(A,num),var(B)),as(0,_1,_2)),
        qsort_1(A,B).
@end{verbatim}

instead of

@begin{verbatim}
qsort(A,B) :-
        check((list(A,num),var(B)),
	      as(0,loc('/home/dtm/CiaoDE/ciaopp/examples/qsort.pl',20,27,qsort),
	      'qsort:qsort_1'(A,B))),
        qsort_1(A,B).
@end{verbatim}".


process_assertions( [ A | As ], N , Call , 
	            Locator , OHead , 
		    Head , NHead , (P, PP)) :-
	A = as${ head => Call , call => Pre , succ => Post , comp => Comp },
	N1 is N + 1,
	(
	    Comp == []
	->
            Head1 = Head
	;
	    tail_comp( Comp , CompGoal , CompGoalArg ),
	    Head1 = 'rtchecks_mod:check_comp'(NewChecks, CompGoal, 
	                                      CompGoalArg, Head)
	),
	( 
	    Pre == []
	->
	    NewChecks = true,
	    ( 
		Post == [] 
	    ->
		process_assertions(As, N1, Call, Locator, OHead, 
		                   Head1, NHead, (P, PP))
	    ;
		list_to_conj( Post, PostC ),
		C = 'rtchecks_mod:check'( PostC , as(N, Locator, OHead) ),
		process_assertions(As, N1, Call, Locator, OHead,
		                   Head1, NHead, (P, PP1)),
		literal_concat( C , PP1 , PP )
	    )
	;
	    ( 
		Post == []
	    ->
	        (
		    Comp == []
		->
		    C = 'rtchecks_mod:check'( PreC , as( N, Locator, OHead ) )
		;
		    C = 'rtchecks_mod:checkc'( PreC , NewChecks )
		),
	        list_to_conj( Pre, PreC ),
		process_assertions(As, N1, Call, Locator, OHead, 
		                   Head1,NHead,(P1, PP)),
		literal_concat( C , P1 , P )
	    ;
		list_to_conj( Post, PostC ),
		list_to_conj( Pre, PreC ),
		C  = 'rtchecks_mod:checkc'( PreC  , NewChecks ),
		C1 = 'rtchecks_mod:checkif'( NewChecks , PostC , 
		                             as(N, Locator, OHead)),
		process_assertions(As, N1, Call, Locator, OHead,
		                   Head1, NHead, (P1, PP1)),
		literal_concat( C  , P1  , P  ),
		literal_concat( C1 , PP1 , PP )
	    )
	).

process_assertions( [] , _ , _ClCall , _Loc , _OHead, Cl , Cl , ( true , true ) ).



:- pred specialize_comp( Comp , EComp )

# "Specialize a @pred{'rtchecks_mod:check_comp/4} in @var{Comp}.".

specialize_comp( 'rtchecks_mod:check_comp'(TRUE, CompGoal, CompGoalArg, Head),
	         CompGoal ) :-
	TRUE == true,
	!,
	specialize_comp( Head , EHead ),
	CompGoalArg = EHead.
specialize_comp( 
	'rtchecks_mod:check_comp'(NewChecks, CompGoal, CompGoalArg,  Head),
	'rtchecks_mod:check_comp'(NewChecks, CompGoal, CompGoalArg, EHead) ) :-
	specialize_comp( Head , EHead ),
	!.
specialize_comp( A , A ).


tail_comp( [] , A , A ).
tail_comp( [C|Cs] , CompGoal , PredCall ) :-
	C =.. CL,
	append( CG , [_] , CL ),
	tail_comp( Cs , CG1 , PredCall ),
	append( CG , [CG1] , CCC ),
	CompGoal =.. CCC.
