:- module( rtchecks_printer , [ rt_print/1 ] , [assertions, regtypes] ).


:- use_package( library( 'ciaopp/api/ciaopp_api'   ) ).
:- use_module(library(ciaopp(api(xtchecks_msg)))).


rt_print( rtcheck( det , nodet , X  ) ) :-
	message( error , [ 'Goal ' , X , ' was supposed to be deterministic' ] ),
	!.
rt_print( rtcheck( nf  , fail  , X  ) ) :-
	message( error , [ 'Goal ' , X , ' was supposed not to fail' ] ),
	!.
rt_print( rtcheck( pre , PRE  , _X  ) ) :-
	message( error , [ 'Precondition ' , PRE , ' failed' ] ),
	!.
rt_print( rtcheck( post , POST  , _X  ) ) :-
	message( error , [ 'Postcondition ' , POST , ' failed' ] ),
	!.
rt_print( rtcheck( ass , POST  , _X  ) ) :-
	message( error , [ 'Assertion ' , POST , ' failed' ] ),
	!.
rt_print( rtcheck( pp_check , GoalArgs , GoalLoc ) ) :-
	NDict = no,
	( GoalArgs = '$:'(GoalArgs2) -> true ; GoalArgs2 = GoalArgs ),
       	Assertion = as${ type    => calls
		       , status  => check
		       , head    => program_point_check
		       , compat  => []
		       , call    => [GoalArgs2]
		       , succ    => []
		       , comp    => []
		       , dic     => []
		       , comment => [] 
		       , locator => GoalLoc
		       },
%	( Loc = loc( S , LB , LE ), M = unknown ; Loc = loc( S , LB , LE , M ) ),
%	GoalLoc = loc${file => S, line_begin => LB, line_end => LE, module => M},
	print_xtcheck_error( xtcheck( GoalArgs , NDict , GoalLoc , Assertion ) ),
	!.
rt_print( rtcheck( assrt , AssrtGoal , GoalLoc ) ) :-
        get_assrt_and_goal( AssrtGoal , Assrt , Goal ),
	print_xtcheck_error( xtcheck( called( Goal , Goal ) , 
	                              ([],[]) , GoalLoc , Assrt ) ), 
%	show_stack( _ ),
	!.
rt_print( X ) :-
	message( error , [ 'Internal error while printing:' , X ] ).

get_assrt_and_goal( assrt_goal( A , G ) , A , G ) :- !.
get_assrt_and_goal( A , A , unknown ) :- !.
