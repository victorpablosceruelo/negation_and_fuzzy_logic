/*             Copyright (C)1997 UPM-CLIP				*/
%% module( qsort ) , transform( rtcheck_pred ) , output( qsort_opt2 ).

:- module(assrt_rtchecks_pred_optimize,
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
:- use_module(library(write)).


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

% this is a clause. We may need to rewrite it to introduce
% run-time tests
add_tests_1( [Cl|Cls] , NCls ) :-
	check_pred(  Cl  , NCls , R ),
	!,
	add_tests_1( Cls , R ).

add_tests_1( [Cl|Cls] , NCls ) :-
	!,
	message( error , ['Internal error: assrt_rtchecks_pp_code_optimize: add_test_1: Unable to process ' , Cl ] ),
	add_tests_1( Cls , NCls ).




check_pred( Cl  , Out , ROut ) :-
	get_key( Cl , ClKey ),
	AS = as${ status => Status, type   => Type },
	AS_COND = (Status==check,(Type==calls;Type==success)),
	!,
	(
	    has_assertions( ClKey , AS , AS_COND )
	->
	    (
		is_the_first( ClKey )
	    -> 
		rename_and_change_body( Cl , ClNewBody ),
		Out = [ ClNewBody , ClUpdated | ROut ]
	    ;
		Out =             [ ClUpdated | ROut ]
	    ),
	    update_cl(         Cl , ClUpdated ),
	    update_memo_table( Cl , ClNewBody )
%	    write( ClUpdated) , nl , nl
	;
	    % no assertion => just do nothing
	    Out = [ Cl | ROut ]
	).




rename_and_change_body( Cl , NewCl ) :-
	%% Change from my_pred( A , B , C )
        %% to my_pred_1(A, B, C)
	clause_head( Cl , ClHead ),

	%% Here we get my_pred_1
	functor( ClHead , F , A ),
	new_predicate( F , A , Fn ),
	%% Remember name for future renamings
	get_key( Cl , ClKey ),
	remember_name( ClKey , Fn ),
	
	%% Create the renamed clause
        functor( ClH , F , A ),
	ClH            =.. [ F  | Fs ],
	InternalClHead =.. [ Fn | Fs ],

	%% Get the assertions
        get_key( Cl , ClKey ),
	get_assertions( ClKey , 
	                as${ status => Status, type   => Type } , 
			(Status==check,(Type==calls;Type==success)),
			Ass), 
	
	%% Generate clauses from assertions
	process_assertions( Ass , ClH , Assertions ),

	%% now lets create:
        %% my_pred(A, B , C) :- Pre, my_pred_1(A, B , C), Post.
        %% where "Pre, my_pred_1(A, B , C), Post." is suppoused to be NewBody
        %% Assertion == (Pre,Post)... so it is easy!
        create_pre_post( InternalClHead , Assertions , NewBody ),
	
        %% When creating the new clause => get obtain an id!!
        clause_dict( Cl , ClDict ),
	new_clause( ClH , NewBody , NewClDict , NewCl ),
	% if we compete the dict before we update de ID var, then
        % the free "id" var is completed by complete_dict
        complete_dict( ClDict , NewBody , NewClDict ).





%% rename all calls bodys we find
%% --- memotalbes
update_cl( Cl , ClUpdated ) :-
	get_key( Cl , ClKey ),
	get_internal_name( ClKey , ClInternal ),

	clause_head( Cl , ClHead ),
	clause_body( Cl , ClBody ),
	clause_dict( Cl , ClDict ),

	functor(   ClHead , ClToRename , _ ),
	rename_literal( ClHead , ClToRename , ClInternal , ClHeadUpdated ),
	compose_clause( ClHeadUpdated, ClBody , ClDict , ClUpdated ).
