:- module( ctchecks ,
	[
	ctchecks_pp/1,
	show_assert/0
        ],
	[ assertions,
	  api( ciaopp_api )
	]).

:- use_module(ctchecks(ctchecks_common)).

:- use_module(ciaopp(preprocess_flags)).

%-------------------------------------------------------------------%
:- doc(bug,"1. When a goal fails, no assertion is shown saying this.
    In 0.8 we had this feature.").
%-------------------------------------------------------------------%

ctchecks_pp( T ) :-
	process_clauses( cl_process( T ) , _ ).




cl_process( Cl , Abs , Out , Out ) :-
	catch( 
	       check_clause( Cl , Abs ),
	       ctchecks( _Goal , As ),
	       (
		 clause_head( Cl , ClHead ),
		 print_ctcheck_error( ctchecks( ClHead , As , Cl ) ) 
	       )
	     ).




% CHECK CLAUSE HEAD
%% call type incompatible with head of the clause
check_clause( Cl , Abs ) :-
% 	clause_ID( Cl , K ),
% 	clause_head( Cl , ClHead ),
% 	get_key( Cl , Key ),

% 	get_assertions( Key , 
% 	                as${ status=>check , type=> calls },
% 			true,
% 			Calls ),

        clause_body( Cl , Body ),
% 	get_next_literal_id( Body , K1 ),

% 	% --- esto no me queda claro
%         clause_dict( Cl , Dic ),
% 	check_goal_assertions( Calls , K , K1 , ClHead , Dic , Abs ),

	%% It is compatible call, lets check the body
	(
	    (Body == true ; Body == !)
	->
	    % The clause is a fact or a cut
	    true
	;
	    check_body( Body , Abs , Cl )
	).

%-------------------------------------------------------------%
%-------------------------------------------------------------%

check_body( true , _ , _ ) :-
	!.

check_body( (A,B) , Abs , Cl ) :-
	!,
	get_next_literal_id( B , K1 ),
	check_body_lit( A , K1 , Abs , Cl ),
	(check_never_succeeds(A,K1,Abs,Cl) ->
	    true
	;
	    check_body( B , Abs , Cl )
	).

check_body( Literal , Abs , Cl ) :-
	get_clause_id_from_literal( Literal , K1 ),
	check_body_lit( Literal , K1 , Abs , Cl ),
	(check_never_succeeds(Literal,K1,Abs,Cl) ->
	    true
	;
	    true),
	!.

check_body( [Literal|Ls] , Abs , Cl ) :-
	message( error , 
	         ['INTERNAL ERROR: check_body: checking literal ',
		  Literal] ),
	check_body( Ls , Abs , Cl ).


check_never_succeeds(Goal,K,[Types,Modes],Cl):-
	clause_dict( Cl , Dic ),
	dic_vars( Dic , Vars ),
	get_info( Types , K , Vars , TInfo  ),
	get_info( Modes , K , Vars , TModes ),
	( 
	    is_bottom( TInfo  )  ;  is_bottom( TModes ) 
	),
	warning_message( "Literal: ~p does not succeed!",
	                   ['$goal'(Goal,Dic)]).

	


check_body_lit( Literal , K1 , Abs , Cl ) :- 
	clause_dict( Cl , Dict ),
	catch( 
	       check_goal( Literal , K1 , Dict , Abs ),
	       CTCHECKS,
	       (
		literal( Literal , Goal ),
		( 
		    CTCHECKS = ctchecks( _ , As ) 
		->
		   print_ctcheck_error( ctchecks( Goal, As, Cl ) ) 
		; 
		  CTCHECKS = ctchecks( _ , As , Called ),
		  print_ctcheck_error( ctchecks( called( Goal , Called ), As, Cl ) )
		)
	       ) 
	     ).




check_goal( Literal , K1 , Dic , Abs ) :-
	kind_of_goal( Literal , KindOf ),
	check_goal_type( KindOf , Literal , K1 , Dic , Abs ).




check_goal_type( cut , _ , _ , _ , _ ).

check_goal_type( pp_check( Prop ) , Literal , K1 , Dic , Abs ) :-
	literal_info( Literal , lit${id  =>K, goal=>_Goal } ),
	dic_vars( Dic , Vars ),
	copy_term( Vars , Vars2 ),
	Goal =.. [f|Vars2],
	get_loc( Literal , LLoc ),
	Assertion = [as${ 
			    ref    => _ ,
			    status => check ,
			    type   => ppcheck ,
			    head   => Goal ,
			    call   => Prop ,
			    succ   => []   ,
			    comp   => []   ,
			    dic    => []   , 
			    locator=> LLoc ,
			    comment=> [] 
			}
		    ],
	check_goal_assertions( Assertion , K , K1 , Goal , Dic , Abs ).

% Goal is a builtin whose call is violated using type info
check_goal_type( L , Literal , K1 , Dic , Abs ) :-
	( L = normal( _ ) , ! ; L = builtin( _ ) ),
	literal_info( Literal , lit${id  =>K, goal=>Goal} ),
	get_key( Goal , Key ),

	get_assertions( Key , 
	                as${ status=>Status , type=> calls },
			(
% Status \== false , 
% --- DTM: and this??
% Status \== true , 
			 Status \== checked),
			Calls ),

	check_goal_assertions( Calls , K , K1 , Goal , Dic , Abs ).




check_goal_assertions( [Call|Cs] , K , K1 , Goal , Dict , [Types,Modes] ) :-
	check_one_goal_assertion( Types , Call , K , K1 , Goal , Dict ),
	check_one_goal_assertion( Modes , Call , K , K1 , Goal , Dict ),
	check_goal_assertions( Cs , K , K1 , Goal , Dict , [Types,Modes] ).

check_goal_assertions( [] , _K , _K1 , _G , _Dict , _Abs ).
%% 	dic_vars( Dict , Vars ),
%% 	get_info( Types , K , Vars , TInfo  ),
%% 	get_info( Modes , K , Vars , TModes ),
%%         (
%% 	    ( is_bottom( TInfo  )  ;  is_bottom( TModes ) )
%% 	->
%% 	    warning_message( "Literal: ~p does not succeed!",
%% 	                   ['$goal'(G,Dict)])
%% 	;
%% 	    true
%% 	).




check_one_goal_assertion( none , _Call , _K , _K1 , _Goal , _Dict ).

check_one_goal_assertion( T , As , K , K1 , Goal , Dict ) :-
	dic_vars( Dict , Vars ),
	get_info( T , K , Vars , TInfo ),

	%%% --- typeslib should solve this rtX([bot]) is an empty type
        %%% --- Wondering why asking by bottom
        (
	    not_is_bottom( TInfo )
	->
	    % As we unify head with Goal, some variables from head can
	    % change. For example:
	    % :- pred p(X): list(X).
	    % if goal is p(1), then the assertions becomes:
	    % :- pred p(1) : list(1), and when trying to print, the
	    % dictionary will be ['X' = 1] instead of ['X'=_12312]
	    % (in other words, printing will be fail)
	    copy_term( As   , AsForPrint ),
	    As = as${ head    => Head   , call => CallBody , 
	              succ    => Post   ,
		      status  => Status , type => Type      },
	    abs_execute( T , Head , CallBody , Goal , Vars, TInfo , SOL ),
	    decide_error( SOL , Status , Type , TypeOfMsg ),
	    treat_error( TypeOfMsg , AsForPrint , Goal , (T,TInfo) ),
	    (
		SOL  \== fail,
		Post \== []
	    ->
		get_info( T , K1 , Vars , TInfo1 ),
		abs_execute( T , Head , SOL , Goal , Vars, TInfo1 , Post_SOL ),
		decide_error( Post_SOL , Status , Type , TypeOfMsg2 ),
		treat_error( TypeOfMsg2 , AsForPrint , Goal , (T,TInfo1) )
	    ;
		true
	    )
	;
%% 	    warning_message( "Literal: ~p does not succeed!",
%% 	                   ['$goal'(Goal,Dict)])
	    true
        ).



% :- use_module(program(assrt_norm),[ denorm_goal_prop/3 ]).
% :- use_module(typeslib(typeslib), [ pretty_type_lit_rules/4 ] ).
% :- use_module(program(p_unit),[native_prop/2]).


:- use_module(plai(domains),       [asub_to_info/5, project/5]).
:- use_module(library(terms_vars), [varset/2]).

treat_error( E , As , Goal , (T,Info) ) :-
	( E = error ; E = warning ),
	 !,
% 	copy_term( Goal , RGoal ),
% %        inline_types(RProps),
	
% 	typeslib:pretty_type_lit_rules(RGoal,R_Info,_Types2,Rules2),
% 	filter_required_rules(Rules2,ReqRules2,FormRules2),
% 	display( Rules2    ) , nl,
% 	display( R_Info    ) , nl,
% 	display( ReqRules2 ) , nl,
% 	display( FormRules2 ) , nl,

	(
%	    varset_in_args( Goal, Vars ),

%	    varsbag( Goal, BVars , [] ),
%	    reverse( BVars , Vars ),
	    varset( Goal , Vars ),
%	displayq( goal(Goal,Vars) ), nl,
	    project( T , Vars , _ , Info , Proj ),
%	displayq( nvars( Proj ) ) ,nl,
	    asub_to_info( T , Proj , Vars , O1 , _O2 ),
%	displayq( O1 ), nl,
%	displayq( O2 ), nl,
%	    transform_to_pretty_types( O1 , NO1 ),
%	displayq( NO1 ), nl,
	    copy_term( (Goal,O1) , (Print_Goal,OO1) ),
	    unify_and_transform_to_pretty_types( OO1 ),
	    varset( Print_Goal , FreeVars ),
	    unify_to_pretty_vars( FreeVars )
%	displayq( Print_Goal ), nl,nl,
%        note_message( "Called: ~p~n" , [ '$goal'( Print_Goal ) ] )
	-> 
	    throw( ctchecks( Goal , As , Print_Goal ) )
        ; 
	    throw( ctchecks( Goal , As ) )
        ).


treat_error( _ , _ , _ , _ ).

/*
transform_to_pretty_types( [ A | As ] , [ B | Bs ] ) :-
	A =.. [B , _Arg],
	!,
	transform_to_pretty_types( As , Bs ).
transform_to_pretty_types( [ A | As ] , [ B | Bs ] ) :-
	A =.. [F , _Arg , Type],
	!,
	B =.. [F , Type],
	transform_to_pretty_types( As , Bs ).
transform_to_pretty_types( [ _ | As ] , [ _ | Bs ] ) :-
	!,
	transform_to_pretty_types( As , Bs ).
transform_to_pretty_types( [ ] , [ ] ).
*/

unify_and_transform_to_pretty_types( [ A | As ] ) :-
	A =.. [B , B],
	!,
	unify_and_transform_to_pretty_types( As ).

unify_and_transform_to_pretty_types( [ A | As ] ) :-
	A =.. [F , Arg , Type],
	!,
	Arg =.. [F , Type],
	unify_and_transform_to_pretty_types( As ).

unify_and_transform_to_pretty_types( [ var | As ] ) :-
	!,
	unify_and_transform_to_pretty_types( As ).

unify_and_transform_to_pretty_types( [ _ | As ] ) :-
	!,
	unify_and_transform_to_pretty_types( As ).

unify_and_transform_to_pretty_types( [ ] ).




unify_to_pretty_vars( [] ).

unify_to_pretty_vars( [var] ).

unify_to_pretty_vars( [var|R] ) :-
	unify_to_pretty_vars( R ).




% filter_required_rules([typedef(::=(T,_))|Ds],Rs,Fs):-
% 	functor(G,T,1),
% 	type(G), !,  % not inferred
% 	filter_required_rules(Ds,Rs,Fs).
% filter_required_rules([typedef(::=(T,D))|Ds],[T,D|Rs],"~n    ~w ::= ~w"||Fs):-
% 	filter_required_rules(Ds,Rs,Fs).
% filter_required_rules([],[],[]).

% type(Goal):- native_prop(Goal,regtype(_Prop)).




% case: fail
decide_error( fail , _Status , _Type , error    ).

% case: true
decide_error( true , _Status , _Type , nothing  ).

% case: I dont know
decide_error( _    , check   , _Type , WhatToDo ) :-
	current_pp_flag( ass_not_stat_eval , WhatToDo ).

decide_error( _    , _Status , _Type , nothing  ).




% not_already_bottom('$bottom',_):-!, fail.
% not_already_bottom(_,'$bottom'):-!, fail.
% not_already_bottom(Type_term,_):-
% 	arg(1,Type_term,Type),
% 	Type = bot,!,
% 	fail.
% not_already_bottom(_Type_term,_Modes).

not_is_bottom( A ) :-
	\+ is_bottom( A ).

is_bottom( '$bottom' ) :- !.
is_bottom( Type_term ) :-
	arg( 1 , Type_term , bot ).

% are_bottom( '$bottom' , _ ) :- !.
% are_bottom( _ , '$bottom' ) :- !.
% are_bottom( Type_term , _ ) :-
% 	arg( 1 , Type_term , bot ).




:- doc( bug , "1.- error_message has to be used when printing message
                  (instead of doing it ad-hoc). A new string version
                  of pretty and assertions_write predicates have to be
                  implemented.").

:- doc( bug , "2.- api_internals should be used instead of
                   pretty_print (It converts 'module:pred' to
                   module:pred" ).

	

print_ctcheck_error( ctchecks( Goal, Assertion, Cl ) ) :-
	clause_dict( Cl , Dic ),
	clause_loc( Cl , Loc ),
	get_loc( Assertion , ALoc ),
	print_xtcheck_error( xtcheck( Goal , Dic , Loc , Assertion , ALoc ) ),
	!.

print_ctcheck_error( WHAT ) :-
	error_message( "Internal Error in printer handler!!!~qArgs: ~w" , [WHAT] ).



print_xtcheck_error( xtcheck( FakeGoalArg , Dic , GoalLoc , Assertion , _AsLoc) ) :-
	Assertion = as${ type => ppcheck , call => RealCheckPoint },
	!,
	% It is like runtime exception
	GoalLoc = loc${file => S , line_begin => LB , line_end => LE },

	% we transform from f(X,Y,Z) to (X,Y,U)
	get_xtchecks_goal( FakeGoalArg , FakeGoal ),
	FakeGoal =.. [ _ | Goal ],

	list_to_conj(RealCheckPoint, CheckPoint ),

	error_message( loc( S , LB , LE ),
                       " VarsChecked: ~p~n~8|"||
		       "Check Point Assertion failed:~n~p", 
		       [ '$goal'( Goal , Dic ),
			 '$goal'( CheckPoint , Dic ) ] ).

print_xtcheck_error( xtcheck(GoalArg , Dic , GoalLoc , Assertion , AsLoc) ) :-
	get_xtchecks_goal( GoalArg , Goal ),
	
	% Add Called: the term at the end if it was specified
	( 
	    get_xtchecks_called( GoalArg , Called )
	->
 	    EndString = "~2|Called: ~p",
	    EndArgs   = [ '$goal'( Called ) ]
	;
	    EndString = "",
	    EndArgs   = []
	),

	GoalLoc = loc${file =>  S , line_begin =>  LB , line_end =>  LE },
	AsLoc   = loc${file => AS , line_begin => ALB , line_end => ALE },
 	get_loc( Assertion , ALoc ),
	error_message( loc(S,LB,LE) ,
                       "~p Assertion failed:~n~4|In ~p~n~4|~p" || EndString, 
		       [ '$goal'( Goal , Dic ) , ALoc , Assertion | EndArgs ] ),
	error_message( loc( AS , ALB , ALE ) , "Assertion failed." , [] ),
	!.

print_xtcheck_error( A ) :-
	error_message( "print_xtcheck_error: While printing ~w~n" , [A] ).




get_xtchecks_goal( called( A , _ ) , A ) :-  !.
get_xtchecks_goal(         A       , A ).

get_xtchecks_called( called( _ , A ) , A ).



show_assert :-
	get_all_assertions( _X , Y ),
	display( Y ), fail.

% success have to be implemented
