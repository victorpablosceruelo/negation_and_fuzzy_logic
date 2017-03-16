:- module(p_dump,[dump/1,restore/1,restore/2,show_dump/1],[assertions, regtypes]).

:- doc(module, "This module contains the machinery for dumping the
	current analysis information to disk and for later restoring
	such information from disk. This has many applications since
	writing the analysis results to disk and later restoring them
	should hopefully be more efficient than reanalizing the module
	from scratch. The information saved to disk can be more or
	less detailed. Sometimes we may be interested in saving only
	part of the information since the missing part can be
	recomputed afterwards. Conceptually, the analysis information
	can be split into three components, the answer table
	(completes), the program point info (memo_table) and the
	dependency table (parent pointers). Note that the last two can
	be efficiently recomputed from the first one, since a fixpoint
	is guaranteed to be found in only one iteration. (to be continued)").

:- use_module(infer(infer_db), [domain/1, inferred/3]).
:- use_module(plai(plai_db), [complete/7, memo_table/6]).
:- use_module(plai(fixpo_ops), [iter/1]).
:- use_module(typeslib(dumper)).
:- use_module(ciaopp(preprocess_flags), [current_pp_flag/2]).
:- use_module(library(conc_aggregates), [findall/3]).
:- use_module(library(fastrw), [fast_write/1, fast_read/1]).
:- use_module(library(prolog_sys), [statistics/2]).
%% --------------------------------------------------------------------

:- pred dump(Module) : atm(Module)
# "Writes on disk all information related to @var{Module}, so it can
   be restored to continue analysing this module.".


%% Let's prepare info to be printed
% generate_dump_file( _File ):-
% 	fail,
% 	curr_file(_,Module),
% 	cleanup_output(Module),
% 	current_pp_flag(dump_ai,on),
% 	current_pp_flag(collapse_ai_vers,Collapse0),
% 	current_pp_flag(pp_info,PPoints),
% 	current_fact(domain(AbsInt)),
% 	( AbsInt==nf -> Collapse=off ; Collapse=Collapse0 ),
% 	( prepare_ai_output(PPoints,cl,AbsInt,Collapse) -> true ),
% 	fail.

dump( File ):-
%% *** MH
%%	message( note , ['Doing dump of file ' , File , ' (complete).\n'] ),

	findall(domain(AbsInt),
	        current_fact( domain(AbsInt) ),
		L0 ),

	look_for_complete( L1 ),
/*
	findall( complete(A1,A2,A3,A4,A5,A6,A7),
	         ( current_fact( complete(A1,A2,A3,A4,A5,A6,A7) ),
		   acc_auxiliary_info(A2,[A4|A5])
		 ),
		 L1 ),
*/
	findall( inferred( B1 , B2 , B3 ),
		  current_fact( inferred( B1 , B2 , B3 ) ),
		  L2 ),

%%         curr_file(_,Module),
%% 	findall( assertion_read(C1,Module,C3, C4,C5,C6, C7,C8,C9 ),
%% 	         assertion_read(C1,Module,C3, C4,C5,C6, C7,C8,C9 ),
%% 		 L3 ),

/*
	current_pp_flag( dump_level , Dump ),


        ( member(Dump, [pp,full]) ->
	       ( current_pp_flag(dump_level_ext,on) ->
		     findall( memo_table( M1, M2, M3, M4, M5, M6),
		     ( current_fact( memo_table( M1, M2, M3, M4, M5, M6) ),
		       acc_auxiliary_info(M2,M6)
		     ),
		     L4 )
	       ;
		   findall( memo_table( M1, M2, M3, M4, M5, M6),
		   ( current_fact( memo_table( M1, M2, M3, M4, M5, M6) ),
		     M4 \== no,
		     acc_auxiliary_info(M2,M6)
		   ),
		   L4 ))
	   ;
	       L4 = []
	),
*/
	look_for_memo( L4 ),

	%display( L1 ) ,nl,display( L2 ) ,nl,display( L3 ) ,nl,

	open(File,write,Stream),
        current_output(O),
        set_output(Stream),

%	dump_auxiliary_info( _AbsInt, fast_write ),
	dump_auxiliary_info( fast_write ),
	fast_write( end_of_auxiliary_info ),
	list( L0 , fast_write ),
	list( L1 , fast_write ),
	list( L2 , fast_write ),
%	list( L3 , fast_write ),
	list( L4 , fast_write ),

	close(Stream),
        set_output(O).


eliminate_deps( [] , [] ) :- !.
eliminate_deps( 
	        [ complete(A1,A2,A3,A4,A5,A6,_)|R  ],
	        [ complete(A1,A2,A3,A4,A5,A6,v)|R1 ]
	      ) :-
 eliminate_deps( R , R1 ).


eliminate_deps_in_memo( [] , [] ) :- !.
eliminate_deps_in_memo(
	        [ memo_table( M1, M2, M3, M4, _M5  , _M6 ) | R  ],
	        [ memo_table( M1, M2, M3, M4,  v   , v   ) | R1 ]
	      ) :-
 eliminate_deps_in_memo( R , R1 ).


look_for_complete( L1 ) :-
	current_pp_flag( dump_pred , C   ),
	current_pp_flag( dump_ext  , Ext ),
	(
	    C = off
	-> 
	    L1 = []
	;
		findall( complete(A1,A2,A3,A4,A5,ID,A7),
		        ( current_fact( complete(A1,A2,A3,A4,A5,ID,A7) ),
			  (
			      Ext=int
			  -> 
			     (ID \== no),
			      acc_auxiliary_info(A2,[A4|A5])
			  ;
			      acc_auxiliary_info(A2,[A4|A5]),
			      (
				  Ext=iter
			      ->
			          iter( A5 )
			      ;
				  true
			      )
			  )
			),
			L1_WT ),
		(
		    C = nodep
		->
		    eliminate_deps( L1_WT , L1 )
		;
		    L1_WT = L1
		)
	).
			


look_for_memo( L4 ) :-
	current_pp_flag( dump_pp   , C ),
	current_pp_flag( dump_ext  , Ext ),
	(
	    C = off
	-> 
	    L4 = []
	;
	    findall( 
		      memo_table( M1, M2, M3, ID, M5, M6),
		      
		      ( current_fact( memo_table( M1, M2, M3, ID, M5, M6) ),
		       acc_auxiliary_info(M2,M6),
			  (
			      Ext=int
			  -> 
			      \+ (ID==no)
			  ;
			      (
				  Ext=iter
			      ->
			          iter( M3 )
			      ;
				  true
			      )
			  )
			),
			L4_WT ),
		(
		    C = deps
		->
		    eliminate_deps_in_memo(  L4_WT , L4 )
		;
		    L4_WT = L4
		)
	).



:- multifile dump_flags_list/2.

dump_flags_list( dump , [ dump_pred , dump_pp , dump_ext ] ).


%% --------------------------------------------------------------------


:- push_prolog_flag(multi_arity_warnings,off).

:- pred restore(Module,Info) :: module * list 

# "Restores from disk analysis information related to
@var{Module}. It is like call to restore_with_flag( File , current
). @var{Info} returns the time required for this process. ".

restore( File, [time(T1,[])] ):-
	statistics(runtime,_),
	restore_with_flag( File , current ),
	statistics(runtime,[_,T1]),
	inform_user(['{restored analysis in ',T1, ' msec.}']).
	

:- pred restore(Module) :: module 
# "Restores from disk analysis information related to @var{Module}. It
is like call to restore( File , current )".

restore( File ):-
	restore( File, _ ).



:- pred restore_with_flag(Module,Flag) :: module * atom

# "Restores from disk analysis information related to @var{Module}.
Depending on Flag, which can take the values @tt{current} and
@tt{prev}, it restores disk analysis information into complete or
complete_prev respectively".

restore_with_flag( File , F ) :-
	open(File,read,Stream),
        current_input(O),
        set_input(Stream),

	restore_auxiliary_info( restore_aux, Dict ),

	( fast_read( T )
	-> read_all_terms( T, Dict , F )
	 ; true
	),

	close(Stream),
	set_input(O).

:- pop_prolog_flag(multi_arity_warnings).

% backtrackable and failing at end:
restore_aux( X ):-
	repeat,
	  ( fast_read(X)
	  -> true
	   ; !, 
	     fail
	  ).

read_all_terms( end_of_file, _Dict , _Flag ) :- !.
read_all_terms( end_of_auxiliary_info, Dict , Flag ) :-
	    fast_read( X ),!,
	    read_all_terms( X, Dict , Flag ).
read_all_terms( T0, Dict , Flag ) :-
%	    display(T),nl,
	    may_be_translated( T0, T_to_check , Dict ),
	    ( 
		Flag == current
	    ->
	        T = T_to_check
	    ;
		T_to_check =.. [T_Func|T_Args],
		rename_pred( T_Func , T_New ),
		T =.. [T_New | T_Args]
	    ),
	    assertz_fact( T ),	
	    fast_read( X ),!,
	    read_all_terms( X, Dict , Flag ).
% if fast read fails, we have to finish.
read_all_terms( _, _ , _ ).	

rename_pred( complete , complete_prev ). 
rename_pred( memo_table , memo_table_prev ). 

may_be_translated( complete(A1,A2,A3,A4,A5,A6,A7), T, Dict ):- !,
	T = complete(A1,A2,A3,Proj,Primes,A6,A7),
	imp_auxiliary_info(A2,Dict,[A4|A5],[Proj|Primes]).
may_be_translated( memo_table(M1,M2,M3,M4,v,v), T, _Dict ):- !,
	T = memo_table(M1,M2,M3,M4,v,v).
may_be_translated( memo_table(M1,M2,M3,M4,M5,M6), T, Dict ):- !,
	T = memo_table(M1,M2,M3,M4,M5,Call),
	imp_auxiliary_info(M2,Dict,M6,Call).
may_be_translated( T, T, _Dict ).



%% --------------------------------------------------------------------

show_dump( File ) :-
	open(File,read,Stream),
        current_input(O),
        set_input(Stream),
	read_and_show,
	close(Stream),
	set_input(O).


read_and_show :-
	fast_read( T ) ,
	display( T ) , nl, nl,
	read_and_show.

read_and_show.


:- regtype module/1.

module( X ) :- atm( X ).
