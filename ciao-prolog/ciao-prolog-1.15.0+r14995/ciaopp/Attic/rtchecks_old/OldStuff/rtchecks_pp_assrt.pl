/*             Copyright (C)1997 UPM-CLIP				*/
%% module( qsort ) , transform( rtcheck_pred ) , output( qsort_opt2 ).

:- module(rtchecks_pp_assrt,
	[
	    rtchecks_pp_assrt/0
	],
	[ 
	    api(ciaopp_api)
	]).

:- use_module(rtchecks(rtchecks_common)).

%========================================================================
% Programmed by: David Trallero Mena
% Started      : March 2004
%
% Adds run-time checks for the given "check" assertions
%========================================================================

rtchecks_pp_assrt :-
	add_package_to_output( rtchecks ),
	undo_internalnames,
	process_clauses( cl_process , Cls ),
	add_clauses( Cls ),
	undo_internalnames,
	comment_all_module_assertions.



	
cl_process( Cl , [ u(NCl) | T ] , T ) :-
	process_literals( Cl , lit_process , NCl ).




lit_process( Lit , NL , TNL ) :-
	AS = as${ status => Status, type => Type , call => Pre  },
	AS_COND = (Status==check,(Type==calls;(Type==success,Pre==[]))),
	(
	    get_key( Lit , ClKey ),
	    has_assertions( ClKey , AS , AS_COND )
	->
	    get_assertions( ClKey , AS , AS_COND , Ass ),
            %% Generate clauses from assertions
	    literal( Lit , LitH ),
	    literal_loc( Lit , Loc ),
	    process_assertions( Ass , 0 , LitH , Loc , Assertions ),
	    create_pre_post( LitH , Assertions , NL , TNL )
        ;
	    NL = ( Lit , TNL )
	).
