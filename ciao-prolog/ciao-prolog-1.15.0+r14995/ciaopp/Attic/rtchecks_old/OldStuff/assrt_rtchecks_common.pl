:- module( assrt_rtchecks_common,
	[ 
	    is_the_first/1,
	    get_internal_name/2,
	    remember_name/2,
	    undo_internalnames/0,

	    process_assertions/3
	] , 
	[api(ciaopp_api)] 
	).



%%% precompiler library
:- use_module(library(dynamic), [retractall/1]).


%%% LOW level

:- data internal_name/2.

undo_internalnames:-
	retractall( internal_name( _ , _ ) ).




is_the_first( ClKey ) :-
	functor( ClKey , F , A ),
	\+ current_fact( internal_name( F/A , _ ) ).




get_internal_name( ClKey , ClIntName ) :-
	functor( ClKey , F , A ),
	current_fact( internal_name( F/A , ClIntName ) ).




remember_name( ClKey , ClNew ) :-
	functor( ClKey , F , A ),
	assertz_fact( internal_name( F/A , ClNew ) ).




process_assertions( [ A | As ] , Call , ( P , PP ) ) :-
	A = as${ head => Call , call => Pre , succ => Post },
	( 
	    Pre == []
	->
	    list_to_conj( Post, PostC ),
	    C = 'rtchecks_mod:check'( PostC ),
	    process_assertions( As , Call , ( P , PP1 ) ),
	    PP = [ C | PP1 ]
	;
	    ( 
		Post == []
	    ->
	        list_to_conj( Pre, PreC ),
		C = 'rtchecks_mod:check'( PreC ),
		process_assertions( As , Call , ( P1 , PP ) ),
		P = [ C | P1 ]
	    ;
		list_to_conj( Post, PostC ),
		list_to_conj( Pre, PreC ),
		C  = 'rtchecks_mod:checkc'( PreC  , NewChecks ),
		C1 = 'rtchecks_mod:checkiftrue'( NewChecks , PostC ),
		process_assertions( As , Call , ( P1 , PP1 ) ),
		P  = [ C  | P1  ] , 
		PP = [ C1 | PP1 ]
	    )
	).

process_assertions( [] , _ClCall , ( [] , [] ) ).
