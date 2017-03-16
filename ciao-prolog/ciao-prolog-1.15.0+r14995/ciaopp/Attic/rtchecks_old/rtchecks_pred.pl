:- module( rtchecks_pred,
	[
	    rtchecks_pred/0
	],
	[ api( api_types ) ] ).


:- use_module(rtchecks(rtchecks_common)).

:- use_module(program(p_unit), [new_predicate/3]).
:- use_module(library(lists), [append/3]).
:- use_module(ciaopp(preprocess_flags), [current_pp_flag/2]).

:- use_module(ciaopp(api(api_base))).
:- use_module(ciaopp(api(api_stuff))).
:- use_module(ciaopp(api(api_processing))).
:- use_module(ciaopp(api(api_dict))).
:- use_module(ciaopp(api(api_module))).
:- use_module(ciaopp(api(api_direc_assrt))).

%========================================================================
% Programmed by: David Trallero Mena
% Started      : March 2004
%
% Adds run-time checks for the given "check" assertions
%========================================================================

rtchecks_pred :-
	%% --- there should be something authomatic like: load_file_declarations
        %% --- so this line: load_file_declarations( rtchecks_mod ) is missing
        current_pp_flag( rt_inline , X ),
	( X == on 
	->  add_package_to_output( [rtchecks_inline,nativeprops] ),
	    % We do expand inline the rtchecks predicate with
	    % rtchecks_inline package, but CiaoPP needs to know some 
	    % information, like for instance rtchecks_mod:check/2 is
            % is a meta_predicate and also we can remove rtcheck_mod
	    ( load_package_info( rtchecks ) -> true ;
		error_message( "Could not load rtcheck package" ))
	;   add_package_to_output( rtchecks )
	),
	undo_internalnames,
	reset_asn,
	%% There could happend that the file already have some
	%% '$saved_assertions' facts
	process_clauses( cl_asn_process , SavedAssertions ),
	% lets process everything for calls and success assertions
	process_clauses( cl_process( calls_success ) , Cls ),
	add_clauses( Cls ),
	add_clauses( SavedAssertions ),
	undo_internalnames.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                              ASSERTION NUMBER
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- data assertion_number/1.

reset_asn :-
	retractall_fact( assertion_number( _ ) ),
	asserta_fact( assertion_number( 0 ) ).


current_asn( X ) :-
	current_fact( assertion_number( X ) ).


inc_asn :-
	retract_fact( assertion_number( X ) ),
	X1 is X + 1,
	asserta_fact( assertion_number( X1 ) ).


set_asn( X ) :-
	retract_fact( assertion_number( _ ) ),
	asserta_fact( assertion_number( X ) ).


%*********************************%
%** PROCESSING SAVED ASSERTIONS **%
%*********************************%

%
% In case there are some '$saved_assertion'( N , _ ) clauses (from
% other rtc transformation), we read from them to know the index to use
% to enumerate the "new" assertions (I guess added after the rtc
% transformation)
%
cl_asn_process( Cl , Action , A ) :-
	(                      
	    Cl = cls${ head => '$saved_assertion'( N , _ ) }
	-> 
	    Action = A
	;
	    Action = [e(Cl),a(end,NCl)|A],
	    % the version expanded by rtchecks package
	    Cl  = cls${ head    => 'multifile:$saved_assertion'( _ , N , AI )
		      , body    => Body
		      ,	dic     => Dic
		      ,	locator => Loc },
	    NCl = cls${ head    => '$saved_assertion'( N , AI )
		      , body    => Body
		      ,	dic     => Dic
		      ,	locator => Loc }
	),
	!,
	assertion_number( AN ),
	(
	    AN > N
	-> 
	    true 
	;
	    N1 is N + 1,
	    set_asn( N1 )
	).

cl_asn_process( _ , A , A ).
	


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                            CLAUSE PROCESSING
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% cl_process_check_points( ) => call to cl_process
cl_process( Cl , Which , Out , ROut ) :-
	Cl = cls${ key => ClKey , head => H , id => ID },
	AS = as${ status => Status, type   => Type },
        get_assrt_search_cond( Which , Status , Type , AS_COND ),
	!,
	(
	    ( has_assertions( ClKey , AS , AS_COND ) ;
    	      get_internal_name( ClKey , _ ) )
	->
	    (
		is_the_first( ClKey )
	    ->
	        % Try with entry_exit before calls_success
	        cl_process_second_pass( Cl , Which , Out , Out2 ),
		rename_and_change_body( Cl , Which , ClNewBody ),
		% res_RTCP_1: it is supposed that add class will be
 		% the 1st
		Out2 = [ a(ClNewBody) , u(ID,ClUpdated) | ROut ]
	    ;
		Out  =                [ u(ID,ClUpdated) | ROut ]
	    ),

	    %
	    %       Key   => is the internal call
	    % prev( Key ) => is the header name
	    %
    	    get_internal_name( prev(ClKey) , ClInternal ),
   	    get_internal_name(      ClKey  , ClLastName ),
	    rename_literal(    Cl   , H , ClInternal , ClU1  ),
	    rename_clause(     ClU1 , ClLastName , ClUpdated1 ),
	    transform_pp_user_checkpoints( ClUpdated1 , ClUpdated ),
	    update_memo_table( Cl   , ClNewBody )
	;
	    % no assertion => 
	    % check if there are any entry or exit assertion
	    % OR just do nothing
	    cl_process_second_pass( Cl , Which , Out , ROut )
	).


:- use_module(ciaopp(api(api_printer))).



cl_process_second_pass( Cl , calls_success , Out , ROut ) :-
 	cl_process( Cl , entry_exit , Out , ROut ),
	!.
cl_process_second_pass( Cl , entry_exit , Out , ROut ) :-
	% The clause has no assertions
        transform_pp_user_checkpoints( Cl , ClT ),
	( cls_are_equal( Cl, ClT ) -> Out = ROut ; Out = [u(ClT)|ROut] ),
	!.
cl_process_second_pass( _ , _ , Out , Out ).



cls_are_equal( A , B ) :-
	A = cls${ id   => A_ID  , key => A_Key, head    => A_Head, 
	          body => A_Body, dic => A_Dic, locator => A_locator },
	B = cls${ id   => B_ID  , key => B_Key, head    => B_Head, 
	          body => B_Body, dic => B_Dic, locator => B_locator },
	A_ID      == B_ID,
	A_Key     == B_Key,
	A_Head    == B_Head,
	A_Body    == B_Body,
	A_Dic     == B_Dic,
	A_locator == B_locator.


:- pred rename_and_change_body( Cl , Which , NewCl ) 
	:  t_cls * list * var
        => t_cls( NewCls )

# "For a given clause @var{Cl}, and a list @var{Which} of two elements
([call,success] or [entry,exit]), which are used to look for the
assertions of @var{Cl} and it create a new clause @var{NewCl}, in the
way:
@begin{verbatim}
NewCl-Head :- 
	All preconditions, 
	Call to Renamed-Cl,
	All posconditions.
@end{verbatim}
Also, internal rtchecks database is modified to remember the
asociation between Cl and Renamed-Cl.".

%% Cases:
%% I am going to explain the possible cases with predicate p. The
%% wrapper predicate is always p. p_1 and p_2 are created if necessary.
%%
%% pA :- pB is a recursive predicate (pA = pB, but I have to 
%% distinguish them here). Notice that the recursive call is not
%% always to the original predicate (pA), but it can be also to a 
%% new created predicate (Look case A).
%%
%% A) Only call and sucess assertions:
%%
%% p   :- p_1
%% p_1 :- p.
%%
%% B) Only entry and exit assertions:
%%
%% p   :- p_1
%% p_1 :- p_1.
%%
%% C) Both
%%
%% p   :- p_1.
%% p_1 :- p_2.
%% p_2 :- p_1.
%%
%% I use {remember_name} and {get_internal_key} to remember 
%% these changes:
%% get_internal_key(      ClKey  , Head_Name )
%% get_internal_key( prev(ClKey) , Internal_Call_Name )
%%
%% Example:
%% orignal clause:  p :- p.
%% modified clause: Head_Name :- Internal_Call_Name.
%%
%% In the prev cases:
%% A) get_internal_key(      p  , p_1 )
%%    get_internal_key( prev(p) , p   )
%%
%% B) get_internal_key(      p  , p_1 )
%%    get_internal_key( prev(p) , p_1 )
%%
%% C) 
%% C.B) (It _MUST_ to be executed first)
%%    get_internal_key(      p  , p_1 )
%%    get_internal_key( prev(p) , p_1 )
%% C.A)
%%    get_internal_key(      p  , p_2 )
%%    get_internal_key( prev(p) , p_1 )
%%
%% Note that case A get_internal_key refers to the last transformation
%% done. If noone exists, then it is the _current head name_. The
%% variable F2 represents this value.

rename_and_change_body( Cl , Which , NewCl ) :-
	Cl = cls${ key => ClKey , head    => ClHead , 
	           dic => Dic   , locator => L      },
	%% Change from my_pred( A , B , C )
        %% to my_pred_1(A, B, C)
	%% Here we get my_pred_1
	functor( ClHead , F , A ),
	new_predicate( F , A , Fn ),

	current_pp_flag( rt_instrumentation , INSTR ),

	( 
	    INSTR \== low
	->
	    ExtraAdd = ( 'rtchecks_mod:push_parent_goal'( ClH ) , 
	                 'rtchecks_mod:pop_parent_goal'(  ClH ) )
	;
	    ExtraAdd = ( true , true )
	),
	
	% I have moved this line here because of the remeber_name
        functor( ClH , F  , A ),
	ClH            =.. [ F  | Fs ],
	InternalClHead =.. [ Fn | Fs ],

 	(
	    get_internal_name( ClKey , Cl_Prev_Trans )
	->  % this can be only call_success
	    functor( Cl_Prev_Trans , F2 , _ ),
	    remember_name(      ClKey  , ClH           ),
	    remember_name( prev(ClKey) , Cl_Prev_Trans )
	;
	    F2 = F,
	    (
		Which == calls_success
	    ->

	        remember_name(      ClKey  , InternalClHead ),
		remember_name( prev(ClKey) , ClHead         )
	    ; % entry_exit case
		remember_name(      ClKey  , InternalClHead ),
		remember_name( prev(ClKey) , InternalClHead )
	    )
	),

	%% Create the renamed clause
        functor( ClHR , F2 , A ),
	ClHR =.. [ F2 | Fs ],

	%% Remember name for future renamings (if necesary).
        %% _Read comment_ in ctchecks_common:remember_name/2.
        remember_name( ClKey , InternalClHead ),

	%% Get the assertions
        get_assrt_search_cond( Which , Status , Type , AS_COND ),
	get_assertions( ClKey , 
	                as${ status => Status, type => Type } , 
			AS_COND,
			As ),
	
	%% Generate checking goals from assertions
        current_asn( AS_N ),
	process_assertions( As , AS_N , ClH , 
	                    GoalLoc , OrigCall, 
			    InternalClHead , 
	                    CompClHead1 , Assertions ),
	specialize_comp( CompClHead1 , CompClHead ),
	Assertions = ( APre  , APost  ),
	ExtraAdd   = ( EAPre , EAPost ),

	(
	    (nonvar( APost ), APost \== true)
	->
	    (
		INSTR \== low
	    ->
	        literal_concat( ( 'rtchecks_mod:pop_parent_goal'( ClH ) , 
		                  'rtchecks_mod:push_parent_goal'( ClH ) ),
				  APost, ExPost )
	    ;
		ExPost = APost
	    )
	;
	    ExPost = APost
	),
	
	literal_concat( EAPre  , APre  , NPre  ),
	literal_concat( ExPost , EAPost, NPost ),

	NAssertions = ( NPre  , NPost  ),
	
	%% now lets create:
        %% my_pred(A, B , C) :- Pre, my_pred_1(A, B , C), Post.
        %% where "Pre, my_pred_1(A, B , C), Post." is suppoused to be NewBody
        %% Assertion == (Pre,Post)... so it is easy!
        create_pre_post( CompClHead , NAssertions , NewBody ),

%	copy_term( (ClKey,Dic) , (UnifyKeyVars, NDic) ),
%	UnifyKeyVars =.. [ _ | UDV ],
%	ClHR         =.. [ _ | UDV ],

	get_key( ClHR , ClKeyR ),
	
	ClHeadFake =.. [ F | Fs ],
	
	% We just complete de dictionary
	FakeNewCl = cls${  head => ClHeadFake,
                           body => (GoalLoc=L,OrigCall=ClH,NewBody),
			   dic  => Dic },
	complete_clause_dic( FakeNewCl , cls${dic=>NDic} ),

	% We put the new dictionary to the final clause
	NewCl = cls${ key  => ClKeyR  , 
	              head => ClHR,
	              body => (GoalLoc=L,OrigCall=ClH,NewBody), 
		      dic  => NDic,
		      locator => L },
		      
	%% We have transformed assertions into pp_check, so lets delete them
        erase_assertions( As ),
	add_commented_assertions( As ),
	add_assertions_clauses_for_error_printing( As ).



transform_pp_user_checkpoints( Clause , NewClause ) :- % Modified
	process_literals( Clause , transform_user_checks , NewClause ).



transform_user_checks( Lit , NewLit ) :-
	user_check_point( Lit , Cond ),
	!,
	literal_info( Lit , lit${ locator => Loc } ), % dic => Dict
%	append( Cond , locdict(Loc,Dict) , CondLoc ),
	append( Cond , [Loc] , CondLoc ),
	Goal =.. CondLoc,
	literal_ID( Lit , ID ),
	literal( NewLit , Goal ),
	literal_ID( NewLit , ID ).
transform_user_checks( Lit , Lit ).




user_check_point( A , _ ) :-
	var( A ),
	!,
	fail.
% Normal one
user_check_point( check(Cond) , [check, Cond] ) :-
	!.
% Module Expanded
user_check_point( 'rtchecks_mod:check'(Cond) , ['rtchecks_mod:check', Cond] ) :-
	!.
user_check_point( G:_ , B ) :-
	% To remove program point info
%	literal( A , G ),
	user_check_point( G , B ),
	!.




:- pred add_assertions_clauses_for_error_printing( As ) 
	: list( As )

# "Given a list of assertions, a new clause, '$saved_assertion'/3 is
  added to programa that is currently been analyzed. This clauses will
  be used by the runtime library for error reporting.".

add_assertions_clauses_for_error_printing( [] ).
add_assertions_clauses_for_error_printing( [ A | As ] ) :-
	current_asn( N ),
	inc_asn,
	loc_unknown( Loc ),
	null_dict( NDict ),
	Cls = cls${ head    => '$saved_assertion'(N,A)
		  , id      => _
	          , key     => Key
	          , body    => true
		  , dic     => NDict
		  , locator => Loc  },
	get_key( Cls , Key ),
	add_clauses( [a(end,Cls)] ),
	add_assertions_clauses_for_error_printing( As ).
	



:- regtype t_as_search_cond/1.

t_as_search_cond( calls_success ).
t_as_search_cond( entry_exit    ).

:- pred get_assrt_search_cond( Search , Status , Type , SCond )
	: t_as_search_cond * var * var * var
       => nonvar( SCond )

# "Returns in @var{SCond} the search condition of the mode
  @var{Search}. The variables @var{Status} and @var{Type} have to be
  the same as used in @pred{get_assertions/4}. Notice that this
  predicate has been though to be used in conjuntion with
  @pred{get_assertions/4}. The use mode is:

@begin{verbatim}
AS = as${ status => Status, type   => Type },
get_assrt_search_cond( Which , Status , Type , AS_COND ),
has_assertions( ClKey , AS , AS_COND ) OR
get_assertions( ClKey , AS , AS_COND , Assertions )
@end{verbatim}
".

% --- a flag here should decide if also trust assertions have
%     to be checked!
get_assrt_search_cond( calls_success , Status , Type , AS_COND ) :-
	!,
	AS_COND = (Status==check,(Type==calls;Type==success;Type==comp)).

get_assrt_search_cond( entry_exit , Status , Type , AS_COND ) :-
	!,
	AS_COND = ((Type==entry);
	           (Status==check,Type==exit)).

get_assrt_search_cond( Which , _ , _ , false ) :-
	error_message( "INTERNAL ERROR: get_assrt_search_cond: Unkown ~w",
	               [Which] ).



%% :- comment( bug , "1.- When doing a call-success transformation, the code
%% 	          is badly generated, because the internal call is
%% 	          referred to the transformated clause instead the on
%% 	          which do the checks. For example:
%% qsort_1(_1,_2) :-
%%         check(list(_1,num)), <-- calls checks
%%         qsort_2(_1,_2),
%%         check((ground(_2),sorted_num_list(_2))).
%%
%% qsort_2([X|L],R) :-
%%         partition(L,X,L1,L2),
%%         qsort_2(L2,R2),  <-- This should be qsort_1
%%         qsort_2(L1,R1),
%%         append(R2,[X|R1],R).
%% qsort_2([],[]). " ).


% --- put the assert information on the other rtchecks transformations
% --- unify the messages...
