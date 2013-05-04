:- module( rtchecks_inline_tr , 
	[ transform_icheck/3
	, transform_as_saved/3 ] , []  ).


transform_as_saved( '$saved_assertion'( Num , AS ) , 
 	            '$saved_assertion'( Module , Num , AS ) , 
 		    Module ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% In this module I just transform:
%%
%% check( X , _ ) => X
%% checkc( X , Y , _ ) => ( \+ \+ X -> Y = true ; Y = false ).
%% checkiftrue( Y , X ) => ( Y == true -> X ; true ).
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% transform_icheck( A , _ , _ ) :-
% 	nonvar( A ),
% 	A = check( _X , _Info ) , 
% 	display( tengo( A ) ),
% 	fail.

% transform_icheck( A , _ , _ ) :-
% 	nonvar( A ),
% 	display( extra( A ) ),nl,
% 	fail.

% transform_icheck( rtchecks_mod:check(X, Info)   , check(X, Info) ).
% transform_icheck( rtchecks_mod:check(X)         , check(X)       ).
% transform_icheck( rtchecks_mod:checkc(X)        , checkc(X)      ).
% transform_icheck( rtchecks_mod:check_inline(X,I), check_inline(X,I) ).
% transform_icheck( rtchecks_mod:checkiftrue(C,P,Info),checkiftrue(C,P,Info) ).
% transform_icheck( rtchecks_mod:check_comp(A,B,C,D),check_comp(A,B,C,D) ).
	

transform_icheck( check(X, Info), 
	         ( (\+ \+ X)
		   -> true
		   ; dispatch_error( Info, X, M )
		 ),
		 M
	       ).

transform_icheck( check( X ),
	         (\+ \+ X 
		   -> true 
		   ; throw(rtcheck(pp_check, X, true))
		 ),
		 _
	       ).

transform_icheck( check_inline( X , Info ) , 
 	         ( \+ \+ X 
 		   -> true
 		   ; dispatch_error( Info, X, M )
 		 ),
		 M
 	       ).

transform_icheck( checkc( X , Y ) , 
	         (\+ \+ X 
		   -> Y = true 
		   ; Y = false
		 ),
		 _
	       ).

transform_icheck( checkif( C , P, Info ),
	        (C == true 
		 -> ( \+ \+ P 
		     -> true
		     ; dispatch_error( Info, P , M ))
		 ; true
		), 
		M
	       ).

transform_icheck( check_comp( C, P, P_Arg, PeeledGoal ),
	         ( C == true
		   -> P
		   ;  PeeledGoal
		 ),
		 _
		 ) :-
                 P_Arg=PeeledGoal.






