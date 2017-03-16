/*             Copyright (C)1997 UPM-CLIP				*/
%% module( qsort ) , transform( rtcheck_pred ) , output( qsort_opt2 ).

:- module(assrt_rtchecks_pp_ass_optimize,
	[
	    add_tests/4
	],
	[ 
	    assertions,
	    api(ciaopp_api)
	]).

:- use_module(rtchecks(assrt_rtchecks_common)).

:- use_module(library(sort)).
:- use_module(library(vndict)).
:- use_module(program(p_unit), [new_predicate/3]).
:- use_module(library(lists), [append/3]).
:- use_module(library(write)).

:- use_module(library(messages)).

%========================================================================
% Programmed by: David Trallero Mena
% Started      : March 2004
%
% Adds run-time checks for the given "check" assertions
%========================================================================

% :- pred add_tests(+Cs,+Ds,+S,-NCs,-NDs) # "NCs and NDs are the
%  resulting clauses and dictionaries after introducing run-time
%  tests. S is an accumulator of the predicates for which run-time tests
%  have already been introduced.". 


add_tests( A , Ad , NA , NAd ) :-
	unify_args(   A , Ad , B ),
	add_tests_1(  B , NB ),
	unify_args(  NA , NAd , NB ).




add_tests_1( [] , [] ) :-
	!,
	undo_internalnames.

add_tests_1( [ Dir | Cs ] , [ Dir | Ds ]  ) :-
        is_directive( Dir ),
	!,
	add_tests_1(Cs,Ds).	

add_tests_1( [Cl|Cls] , NCls ) :-
	is_clause( Cl ),
        % this is a clause. We may need to rewrite it to introduce
        % run-time tests
	check_pred(  Cl  , NCls , R ),
	!,
	add_tests_1( Cls , R        ).

add_tests_1( [Cl|Cls] , NCls ) :-
	!,
	message( error , ['Internal error: assrt_rtchecks_pred_optimize: add_test_1: Unable to process ' , Cl ] ),
	add_tests_1( Cls , NCls ).




check_pred( Cl  , [ ClNew | Out ] , Out ) :-
	clause_head( Cl , ClHead ),
 	clause_body( Cl , ClBody ),
	clause_dict( Cl , ClDict ),

	process_body( ClBody , ClNewBody ),
	!,
        complete_dict(  ClDict , ClNewBody , ClNewDict ),
	new_clause( ClHead , ClNewBody , ClNewDict , ClNew ).

check_pred( Cl  , [ Cl | Out ] , Out ) :-
%	message( error , [ 'rtchecks_pp_code: processing (in check_pred): ' , Cl ] ).
	error_message( "rtchecks_pp_code: processing (in check_pred): ~w" , [ Cl ] ).




process_body( ( Cl , Br ) , BodyOut ) :-
	!,
	process_body( Cl , NewCl ),
	process_body( Br , BodyRest ),
	appendconj( NewCl , BodyRest , BodyOut ).

process_body( Cl , NewCl ) :-
	AS = as${ status => Status, type => Type , call => Pre  },
	AS_COND = (Status==check,(Type==calls;(Type==success,Pre==[]))),
	(
	    get_key( Cl , ClKey ),
	    has_assertions( ClKey , AS , AS_COND )
	->
	    get_assertions( ClKey , AS , AS_COND , Ass ),
            %% Generate clauses from assertions
	    literal( Cl , ClH ),
	    process_assertions( Ass , ClH , Assertions ),
	    create_pre_post( ClH , Assertions , NewCl )
        ;
	    Cl = NewCl
	).
