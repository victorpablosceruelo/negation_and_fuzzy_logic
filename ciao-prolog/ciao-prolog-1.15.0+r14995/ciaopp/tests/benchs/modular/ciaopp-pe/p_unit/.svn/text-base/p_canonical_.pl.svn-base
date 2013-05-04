:- module( p_canonical_ , [
	                   to_simple_assrt/0,
			   module_to_simple_assrt/1,
			   current_module_to_simple_assrt/0,
			   compound_to_simple_assrt/2,
			   compound_to_simple_assrt_same_pred/2
			 ], [.('../api/ciaopp_api_'),assertions] ).

:- use_module(assrt_db_, [assertion_read/9, assertion_body/7]).

:- use_module(library(assertions(assertions_props)), [assrt_status/1]).

:- use_module(library(aggregates), [findall/3]).


% to_simple_assrt.

% --- in the future
% low_level_to_api( L , L ).


to_simple_assrt :-
	module_to_simple_assrt( _ ).

	


current_module_to_simple_assrt :-
 	get_key_predicates( [internal] , P ),
	transform_assertions_key( P ).




module_to_simple_assrt( M ) :-
	get_all_assertions_key_of_module( M , L ),
	transform_assertions_key( L ).
	



transform_assertions_key( L ) :-
 	member( Key , L ),
 	get_assertions( Key , as${ type => pred } , true , L ),
	compound_to_simple_assrt_same_pred__( L , CannonAs , [] ),
	process_simple_assrt( L , CannonAs ),
	fail.

transform_assertions_key( _ ).




process_simple_assrt( L , CannonAs ) :-
	erase_assertions( L ),
	add_assertions( CannonAs ),
	display( commented( L ) ), nl , nl,
	add_commented_assertions( L ).




compound_to_simple_assrt( L , CannonAs ) :-
	compound_to_simple_assrt__( L , CannonAs , [] ),
	!.
	



compound_to_simple_assrt__( [] , A , A ).

compound_to_simple_assrt__( L , NAs , TTNAs ) :-
	get_same_pred_assertions( L , As , L_left ),
	get_only_pred_assertions( As , PredAs , NAs , As_Tleft ),
	compound_to_simple_assrt_same_pred__( PredAs , As_Tleft , TNAs ),
	compound_to_simple_assrt__( L_left , TNAs , TTNAs ).




% +++ ground( A )
compound_to_simple_assrt_same_pred( A , NA ) :-
	get_only_pred_assertions( A , Pred , NA , NA1 ),
	compound_to_simple_assrt_same_pred__( Pred , NA1 , [] ).
	



compound_to_simple_assrt_same_pred__( [] , A , A ) :-
	!.

compound_to_simple_assrt_same_pred__( As , Calls , T ) :-
%	join_pred_assertions( As , Call ),
	separate_and_join_assertions( As , Calls , L ),
	do_success_comp( As , L , T ).




get_same_pred_assertions( [] , [] , [] ).

get_same_pred_assertions( [L|Ls] , [L|As] , L_left ) :-
	L = as${ head => H },
	get_same_pred_assertions__( Ls , H , As , L_left ).




get_only_pred_assertions( [] , [] , B , B ).

get_only_pred_assertions( [ A | As ] , PredAs , B , TB ) :-
	A = as${ type => pred },
	!,
	PredAs = [ A | RPredAs ],
	get_only_pred_assertions( As , RPredAs , B , TB ).

get_only_pred_assertions( [ A | As ] , PredAs , [ A | B ] , TB ) :-
	get_only_pred_assertions( As , PredAs , B , TB ).
	
	


get_same_pred_assertions__( [] , _ , [] , [] ).

get_same_pred_assertions__( [ L | Ls ] , H , [ L | As ] , L_left ) :-
	L = as${ head => H2 },
	\+ \+ H = H2,
	!,
	get_same_pred_assertions__( Ls , H , As , L_left ).

get_same_pred_assertions__( [ L | Ls ] , H , As , [ L | L_left ] ) :-
	get_same_pred_assertions__( Ls , H , As , L_left ).
	



do_success_comp( [ ]       , A , A       ).
do_success_comp( [ A | B ] , L , TailOut ) :-
	pred_to_success( A , L  , L1 ),
	pred_to_comp(    A , L1 , L2 ),
	do_success_comp( B , L2 , TailOut ).
	



pred_to_success( As , Suc , Suc__ ) :-
	As = as${
		    status => Status,
		    call   => Call  ,
		    succ   => Succ  
                },
	( 
%	    Succ == [] 
%	-> 
%	    Suc = Suc__
%	; 
	    decide_status( Status , success( SStatus ) ),
	    SAs = as${
		    status => SStatus,
		    type   => success,
		    call   => Call  ,
		    compat => []    ,
		    succ   => Succ  ,
		    comp   => []    
		   },
            copy_the_rest( As , SAs ),
	    Suc = [ SAs | Suc__ ]
	).





pred_to_comp( As , Suc , Suc__ ) :-
	As = as${
		    status => Status,
		    call   => Call  ,
		    comp   => Comp  
                },
	( 
	    Comp == [] 
	->
	    Suc = Suc__
	;
	    decide_status( Status , comp( CoStatus ) ),
	    CAs = as${
		    status => CoStatus,
		    type   => comp  ,
		    call   => Call  ,
		    compat => []    ,
		    succ   => []    ,
		    comp   => Comp  },
            copy_the_rest( As , CAs ),
	    Suc = [ CAs | Suc__ ]
	).




copy_the_rest( A , B ) :-
	A = as${ locator => L , comment => C , fromwhere => F ,
     		 head    => H , dic       => D },
	B = as${ locator => L , comment => C , fromwhere => F ,
     		 head    => H , dic       => D }.




separate_and_join_assertions( L , A , TA ) :-
	findall( S , assrt_status( S ) , Status ),
	separate_and_join_assertions__( Status, L , A , TA ).
	



separate_and_join_assertions__( [] , _ , A , A ).

separate_and_join_assertions__( [ S | Ss ] , L , Call , TA ) :-
	gather_pred_assertions_of_same_status( L , S , LS ),
	( 
	    LS == []
	->
	    Call = TailA
	;
	    join_pred_assertions( LS , A ),
	    Call = [ A | TailA ]
	),
	separate_and_join_assertions__( Ss , L , TailA , TA ),
	!.

separate_and_join_assertions__( [ S | Ss ] , L , A , TA ) :-
	error_message( "INTERNAL ERROR: When gathering pred assertions"||
	               " of status ~w: ~p. Skiping...", [ S , L ] ),
	separate_and_join_assertions__( Ss , L , A , TA ).
		       




gather_pred_assertions_of_same_status( [ ] , _Status , [] ).


gather_pred_assertions_of_same_status( [ A | As ] , Status , [ A | Ls ] ) :-
	A  = as${ status => Status },
	!,
	gather_pred_assertions_of_same_status( As , Status , Ls ).
	

gather_pred_assertions_of_same_status( [ _ | As ] , Status , Ls ) :-
	gather_pred_assertions_of_same_status( As , Status , Ls ).
	
	


:- pred join_pred_assertions( L , A ) 
	: ( list( L , as ), var(A) )
        => (as( A ))

# "Given a list of assertions, @var{L}, a call assertions is returned
  on @var{A}. All list 'pred' assertions member of
  the list have to have the same status".

join_pred_assertions( L , A ) :- 
	L = [ LA | _ ],
	LA = as${
		    status    => Status
		},
	decide_status( Status , calls( PredStatus ) ),
	A  = as${
		    status    => PredStatus,
		    type      => calls ,
		    succ      => []    ,
		    compat    => []    ,
		    comp      => []    ,
		    comment   => []    ,
		    fromwhere => read
		},
	join_pred_assertions__( L, A ).



join_pred_assertions__( [A] , As ) :- 
	!,
	As  = as${
		    head      => Head  ,
		    call      => Calls ,
		    dic       => Dic   ,
		    locator   => Loc 
		},
	A   = as${
		    head      => Head  ,
		    call      => Calls ,
		    dic       => Dic   ,
		    locator   => Loc 
		}.

join_pred_assertions__( [B|Bs] , As ) :- 
	join_pred_assertions__( Bs , AsR ),
	B  = as${
		    head      => B_Head  ,
		    call      => B_Calls ,
		    dic       => B_Dic   ,
		    locator   => B_Loc 
		},
	AsR= as${
		    head      => As_Head ,
		    call      => As_Calls,
		    dic       => As_Dic  ,
		    locator   => As_Loc 
		},
	As = as${
		    head      => A_Head  ,
		    call      => A_Calls ,
		    dic       => A_Dic   ,
		    locator   => A_Loc 
		},

	copy_term( (B_Head, B_Dic , As_Head, As_Dic , B_Calls , As_Calls ), 
                   (A_Head, B_Dic2, A_Head , As_Dic2, B_Calls2, As_Calls2) ),

	( 
	    As_Calls2 = [(C1;C2)]
	->
	    add_if_different( B_Calls2, (C1;C2)   , A_Calls )
	;
	    add_if_different( B_Calls2, As_Calls2 , A_Calls )
	),
	join_loc( B_Loc , As_Loc , A_Loc ),
% 	display( input ), nl,
% 	display(  B_Dic2 ),nl,
% 	display( As_Dic2 ),nl,
	join_dic( B_Dic2 , As_Dic2 , A_Dic ).
% 	display( output ),
% 	display( A_Dic ) , nl.



add_if_different( A , B , Out ) :-
	add_if_different__( A , B , C ),
	( C = (_;_) -> Out = [C] ; Out = C ),
	(
	    list( Out )
	->
	    true
	;
	    error_message( "INTERNAL ERROR: add_if_different: " ||
			   "~w has to be a list" , [C] ),
			  dtm3
	).
	  

dtm3.


add_if_different__( A , (B;C), Out ) :-
	!,
	(
	    % --- DTM: This comparation should be more sophisticated, because
	    % [int,var] \== [var,int]
	    A == B
	->
	    Out = (B;C)
	;
	    add_if_different__( A , C, Out2 ),
	    Out = (B;Out2)
	).

add_if_different__( A , C, Out ) :-
	(A == C -> Out = C ; Out = (C;A)).




join_loc( loc${module    => M  ,
	       file      => F  ,
	       line_begin=> ALB,
	       line_end  => ALE
	      },

	  loc${ line_begin=> BLB,
	        line_end  => BLE
	      },

	  loc${module    => M  ,
	       file      => F  ,
	       line_begin=> CLB,
	       line_end  => CLE 
	      }
	) :-
         min( ALB , BLB , CLB ),
         max( ALE , BLE , CLE ).



	
join_dic( [] , A , A ).

join_dic( [B|Bs] , A , AS ) :-
	join_dic( Bs , A , AT ),
	(
	    is_in_dic( AT , B )
	->
	    AS = AT
	;
	    AS = [ B | AT ]
	).




is_in_dic( [ A | As ] , B ) :-
	A = (_=VA),
	B = (_=VB),
	(
	    VB == VA
	->
	    true
	;
	    is_in_dic( As , B )
	).

	
	
	


max( X , Y , X ) :-
	X > Y,
	!.

max( _X , Y , Y ).




min( X , Y , X ) :-
	X < Y,
	!.

min( _X , Y , Y ).




:- pred decide_status( S , PredS )
	: assrt_status * var

# "For a given status @var{S} from a pred assertions, the status of
  calls and success are returned on @var{PredS}.".

decide_status( true  , calls(   true ) ) :- !.
decide_status( true  , success( true  ) ) :- !.
decide_status( true  , comp(    true  ) ) :- !.

decide_status( trust , calls(   check ) ) :- !.
decide_status( trust , success( trust ) ) :- !.
decide_status( trust , comp(    trust ) ) :- !.

decide_status( check , calls(   check ) ) :- !.
decide_status( check , success( check ) ) :- !.
decide_status( check , comp(    check ) ) :- !.

decide_status( X     , calls(     X   ) ) :- member( X , [false, checked]), !.
decide_status( X     , success(   X   ) ) :- member( X , [false, checked]), !.
decide_status( X     , comp(      X   ) ) :- member( X , [false, checked]), !.

decide_status( X     , Y ) :-
	error_message( "decide_status: no entry for: ~w (output: ~w)",
	               [ X , Y ] ),
	fail.




% OLD SPECIFICATION
% decide_pred_status( [] , _ ).
%
% decide_pred_status( [ L | Ls ], Status ) :-
% 	L = as${ status => S },
% 	decide_status( S , calls( CS ) ),
% 	(
% 	    Status = CS
% 	->
% 	    decide_pred_status( Ls, Status )
% 	;
% 	    error_message( "When using several pred of the same pred, "||
% 			   "the status has to be the same (there cannot "||
% 			   "be :- check pred p(X) and :- trust pred p(X))."||
% 			   "Look at:~n\n~4|~p~n" ,
% 			   [L | Ls] )
% 	).



pred_assertion(Goal,M,Status,Body,Dict,S,LB,LE):-
	current_fact( assertion_read(Goal,M,Status,pred,Body,Dict,S,LB,LE) ).



group_calls([],_Goal,[]).
group_calls([(Goal,Call)|More],Goal,[Call|Calls]):-
	group_calls(More,Goal,Calls).



:- doc(version_maintenance,dir('../version')).

:- doc(version(1*0+863,2004/11/12,22:05*20+'CET'), "Commented
   predicate @pred{decide_pred_status/2} which belongs to old
   specification implementation.  (David Trallero Mena)").

:- doc(version(1*0+862,2004/11/12,14:32*43+'CET'), "Status of
   calls assertions derived from @tt{trust pred} is now @tt{check}.
   (Pawel Pietrzak)").

:- doc(version(1*0+735,2004/10/12,21:11*26+'CEST'), "Solved a bug
   that did not convert compound pred assertions with true as a
   precondition to success assertions.  (David Trallero Mena)").

:- doc(version(1*0+657,2004/09/20,20:37*40+'CEST'), "pred
   assertions are expanded now according to their status.  (David
   Trallero Mena)").

:- doc(version(1*0+656,2004/09/20,20:37*24+'CEST'), "There was a
   bug in @pred{decide_status/2}, the possible status of an assertion
   is false, not fail.  (David Trallero Mena)").

:- doc(version(1*0+603,2004/08/08,14:32*49+'CEST'), "Added
   @pred{compound_to_simple_assrt/2} and
   @pred{compound_to_simple_assrt_same_pred/2}. (David Trallero Mena)").

:- doc(version(1*0+597,2004/08/03,00:29*30+'CEST'), "Not asked for
   get_key_predicates, as we have to normalize all pred assertions.
   (David Trallero Mena)").

:- doc(version(1*0+595,2004/07/30,21:37*44+'CEST'), "Recoded a
   bit, to allow better integration with c_itf and Ciao in the future.
   (David Trallero Mena)").

:- doc(version(1*0+558,2004/07/16,17:26*55+'CEST'), "Several
   @tt{pred} are transformed into @tt{calls} assertion with disjuntion
   of call pred assertion body as body.  (David Trallero Mena)").

