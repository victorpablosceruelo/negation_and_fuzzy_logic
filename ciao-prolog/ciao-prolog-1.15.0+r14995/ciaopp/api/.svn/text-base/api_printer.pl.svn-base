:- module( api_printer,
	[
	% For programmers
	  api_pwrite/1
	% Pretty printer
	, api_write/1
	, api_write/2
	% Locators
	, api_write_loc/1
	, api_write_loc/2
	% 
	, print_program/0
	, print_program/1
	, my_pprint/0
	
	, insert_inter_clause_comments/2

	%% This predicate is _TEMPORARY_ and NOONE SHOULD USE IT
        %% it can be _only_ called from p_unit
	, internal_remember_disj_conj_cls_names/1
	] , 
	[ assertions
	, regtypes
	, api( api_types )
	] 
	).


:- doc( bug , "
%% --- clause_* cannot fail
%% --- create exception
%% --- should api_write consider clause/2 and assertion_read?
%% --- WHY THERE ARE DIRECTIVES THAT ARE ASSERTIONS IF THEY ARE IN CANONICAL
%%     VERSION!!!
%% --- functions like: is_last_clause, is_last_literal... -> iterators
" ).

%% If you uncomment this line, the API will have rt checks!
%% :- compilation_fact( api_rt ).

:- use_module(library(lists), [append/3, difference/3]).
:- use_module(library(messages)).
:- use_module(library(aggregates)).
:- use_module(ciaopp(preprocess_flags)).
:- use_module(ciaopp(api(api_predcl))).
:- use_module(ciaopp(api(api_direc_assrt))).
:- use_module(ciaopp(api(api_module))).
:- use_module(ciaopp(api(api_order))).
:- use_module(ciaopp(api(api_dict))).

:- use_module(ciaopp(api(api_internal_write))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%   OUTPUT
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- pred api_write( Stream , Term ) 
# "Prints on Stream @var{Stream} whatever @var{Term} obtained from
  any CiaoPP API predicate.".

:- pred api_write( Term ) 
# "Same as @pred{api_write/2} but setting Stream to the current output.".

:- set_prolog_flag( multi_arity_warnings, off ).

api_write( A ) :-
	current_output( CO ),
	api_write_( A , CO ).

api_write( S , A ) :-
	api_write_( A , S ).

:- set_prolog_flag( multi_arity_warnings, on ).


api_write_( A , S ) :-
	var(A),
	!,
	display( S , A ).

api_write_( [ A | As ] , S ) :-
	api_write_( A , S ),
	!,
	api_write_( As , S ).

api_write_( [ ] , _S ) :-
	!.

api_write_( a(A) , S ) :-
	!,
	display( S , '\nHas to be added:\n' ),
	api_write_( A , S ).

api_write_( a(B,A) , S ) :-
	!,
	display( S , '\nHas to be added at '),
        display( S , B ),
	display( S , ':\n' ),
	api_write_( A , S ).

% api_write_( mark(A) ) :-
% 	!,
% 	display( '\n*** INTERNAL MARK: '),
%         display( A ),
% 	nl.

api_write_( u(A) , S ) :-
	!,
	display( S , '\nNeed to be updated:\n' ),
	api_write_( A , S ).
	
api_write_( u(K,A) , S ) :-
	!,
	display( S , '\nNeed to be updated with the key: ' ),
	displayq( S , K ), 
	nl( S ),
	api_write_( A , S ).

api_write_( As , S ) :-
	As  = as${ ref => Ref , dic => no },
	!,
	\+ \+ (
	complete_assertion_dic( As , NAs ),
	( var(Ref) -> true ; update_assertion( As , NAs )),
	api_internal_write( NAs , S )).

api_write_( Cl , S ) :-
	Cl  = cls${ dic => no , id => ID },
	!,
	complete_clause_dic( Cl , NCl ),
	update_clause( ID , NCl ),
	api_internal_write( NCl , S ).

api_write_( A , S ) :-
	api_internal_write( A , S ),
	!.

api_write_( Unknown , _ ) :- 
	error_message( "Internal error: failed to write ~q", [Unknown] ).





:- set_prolog_flag( multi_arity_warnings, off ).

:- pred api_write_loc( ClsOrDirec ) 
	: (t_cls( ClsOrDirec ) ; t_direc( ClsOrDirec ))
# "Print the locator of @var{ClsOrDirec}.".

api_write_loc( As ) :-
	current_output( CO ),
	api_write_loc( CO , As ).

:- pred api_write_loc( S , ClsOrDirec ) 
	: (t_cls( ClsOrDirec ) ; t_direc( ClsOrDirec ))
# "Same as @pred{api_write_loc/2} but setting Stream to current output.".

api_write_loc( S , As ) :-
	api_write_loc_( As , S ).

:- set_prolog_flag( multi_arity_warnings, on ).
	



api_write_loc_( As , S ) :-
	As = as${ locator => Loc },
	!,
	api_write( S , Loc ).

api_write_loc_( As , S ) :-
	As = cls${ locator => Loc },
	!,
	api_write( S , Loc ).

api_write_loc_( Unknown , _ ) :- 
	error_message( "Internal error: failed to write locator of ~q",
	                [ Unknown ] ).




api_pwrite( As ) :-
	As  = as${ ref     => ID     ,
		   status  => Status ,
	           type    => Type   ,
		   head    => Head   ,
		   compat  => Compat ,
		   call    => Call   ,
		   succ    => Succ   ,
		   comp    => Comp   ,
		   dic     => Dic    ,
		   locator => L      ,
		   comment => Comment,
		   fromwhere => From
		 },
	note_message(
"
 ref    : ~q
 status : ~q
 type   : ~q
 head   : ~q
 compat : ~q
 call   : ~q
 success: ~q
 comp   : ~q
 dic    : ~q
 locator: ~q
 comment: ~q
 from   : ~q
" ,
		       [ ID , Status , Type , Head , Compat , Call ,
		         Succ, Comp, Dic , L , Comment , From ] ),
	!.

api_pwrite( Cl ) :-
	Cl = cls${ %ref , 
		   id      => ID,
		   key     => Key,
		   head    => H,
		   body    => B,
		   dic     => Dic,
		   locator => Loc
		 },
	note_message( "~n  id     : ~q~n  key    : ~q~n  head   : ~q~n\
  body   : ~q~n  dic    : ~q~n  locator: ~q~n",
		       [ ID , Key , H , B , Dic , Loc ] ),
	!.

api_pwrite( Cl ) :-
	Cl = direc${ %ref , 
		   key     => Key,
		   body    => B,
		   dic     => Dic,
		   locator => Loc
		 },
	note_message( "~n  key    : ~q~n  body   : ~q~n\
  dic    : ~q~n  locator: ~q~n",
		       [ Key , B , Dic , Loc ] ),
	!.

api_pwrite( a(A) ) :-
	!,
	display( '\nNeed to be added:\n' ),
	api_pwrite( A ).

api_pwrite( a(B,A) ) :-
	!,
	display( '\nHas to be added at '),
        display( B ),
	display( ':\n' ),
	api_pwrite( A ).

% api_pwrite( mark(A) ) :-
% 	!,
% 	display( '\n*** INTERNAL MARK: '),
%         display( A ),
% 	nl.

api_pwrite( u(A) ) :-
	!,
	display( '\nNeed to be updated:\n' ),
	api_pwrite( A ).

api_pwrite( u(K,A) ) :-
	!,
	display( '\nNeed to be updated with the key: ' ),
	displayq( K ), 
	nl,
	api_pwrite( A ).
	
api_pwrite( [] ) :-
	!.

api_pwrite( [A|As] ) :-
	api_pwrite( A ),
	!,
	nl,
	api_pwrite( As ).


api_pwrite( Unknown ) :-
	error_message( "Internal error: failed to pwrite ~q", [Unknown] ).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%   PORTRAY
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- multifile portray/1.


portray( A ) :-
	A \== [_|_],
	api_internal_write( A ),
	!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%   PROGRAM PRINTER
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- set_prolog_flag( multi_arity_warnings, off ).

:- use_module(syntax(tr_syntax_new)).

:- pred print_program

# "Prints the current program information into human readable format.".

%% --- IMPLEMENT PRINTING HOOKS!
%% --- first clause should be avoid!!!
print_program :-
	current_output(CO),
	print_program(CO).

:- pred print_program(S)

# "Same as @pred{print_program/1} but using the stream @var{S} instead
  the current output.".

print_program(S) :-
	swap_to_module_operators,
	( current_pp_flag(dump_ai, on)
	  -> analysis_info_to_assertions ; true ),
	push_prolog_flag(write_strings, on),

	% print the clauses and its assertions in the proper order
	pr_order_get(P),
	
	( current_pp_flag(output_rebuild_djcd, on) ->
	       gather_disj_cond_clauses(Names, RenCls),
	       % Use tr_syntax to transform then into conditions
	       transform_cls_to_disj_and_cond(RenCls, TRenCls),
	       % Auxiliary predicates does not count here
               difference(P, Names, PDif)
%	       api_write( RenCls ),nl,nl
	;
	       TRenCls = [],
	       PDif    = P
	),
	
	% Get the begining comments
	get_all_comments(begin, InitialComments),
	api_write(S, InitialComments),
	
	% Print the directives
	get_directives(Direcs),
	(
	    api_write(S, Direcs)
	;
	    % When entering by fail here,
	    % means that we are printing at the end of the program
	    % Write possible new redefining directives added
	    % when unexpansion
	    get_directives(Direcs2),
	    member(D2, Direcs2),
	    \+ member(D2, Direcs),
	    api_write(S, D2),
	    fail
	),
	member( Key , PDif ),
	% Get everything for the given key
        get_commented_assertions( Key , CAs ),
	get_assertions( Key , As ),
	get_clauses( Key , Cls ),

	% HERE: Transform clauses to cond and disjuntions
	( current_pp_flag( output_rebuild_djcd , on ) ->
	    substitute_disj_in_body( Cls , Names , TRenCls , NCls )
	;
	    NCls = Cls
	),
	
	insert_clause_comments( NCls , NewCls ),
	% Print everything
	api_write( S , CAs ),
	( current_pp_flag( output_show_tautologies, off ) 
	->  filter_tautologies( As, AsFiltered )
	;   As = AsFiltered ),
	api_write( S , AsFiltered ),
	api_write( S , NewCls ),
	nl( S ),
	fail.
print_program( S ) :-
	get_all_comments( end , EndComments ),
	api_write( S , EndComments ),
	swap_to_ciaopp_operators,
	( pop_prolog_flag( write_strings ) -> true ; true ).



filter_tautologies( [], [] ).
filter_tautologies( [A|As], AsF ) :-
	is_tautology( A ),
	!,
	filter_tautologies( As, AsF ).
filter_tautologies( [A|As], [A|AsF] ) :-
	filter_tautologies( As, AsF ).


is_tautology( A ) :-
	A = as${ type => calls, call => Call },
	it_has_some_true( Call ).

it_has_some_true( [A|As] ) :- 
	(it_has_some_true_1( A ) ; it_has_some_true( As )),!.
it_has_some_true( (A;As) ) :- 
	(it_has_some_true_1( A ) ; it_has_some_true( As )),!.

it_has_some_true_1( [] ) :- !.

	

:- set_prolog_flag( multi_arity_warnings, on ).


:- data disj_conj_cls_names/1.

:- pred internal_remember_disj_conj_cls_names( Names ) 
	: (list(Names,term))

# "@var{Names} is a list of Clauses Keys (headers) that indicates the
  new predicates that were creates when removing conditions and
  disjuntions from the original file.".

internal_remember_disj_conj_cls_names(X) :-
	retractall_fact( disj_conj_cls_names(_)),
	asserta_fact( disj_conj_cls_names(X)).


:- pred internal_get_disj_conj_cls_names( Names ) 
	=> (list(Names,term))

# "Retrieves the fact asserted by
  @pred{internal_remember_disj_conj_cls_names/1}.".

internal_get_disj_conj_cls_names( X ) :-
	( current_fact( disj_conj_cls_names( X ) ) -> true ; X = [] ).



gather_disj_cond_clauses( Names , Cls ) :-
	internal_get_disj_conj_cls_names( Names ),
	findall( Cl , 
	         (member( Head , Names ),
		  gather_disj_cond_bodies( Head , Bodies ),
		  (get_clause( Head , Clause )->true),
		  '$~'( Clause , cls${body=>Bodies} , Cl)
		 ),
		 Cls ),
	!.
gather_disj_cond_clauses( [] , [] ).


gather_disj_cond_bodies( Head , Bodies1 ) :-
	findall( (Head,B) , 
		 get_clause( Head , cls${ head => Head , body => B } ),
		 Bodies ),
	remove_head_and_unify_vars( Bodies , Head , Bodies1 ).


remove_head_and_unify_vars( [] , _ , [] ).
remove_head_and_unify_vars( [B|Bs] , H , [BT|BTs] ) :-
	B = (H,BT),
	remove_head_and_unify_vars( Bs , H , BTs ).


:- pred insert_clause_comments( Cl , Cls ) 
	: list( Cl , t_cls )

# "For a given list of clauses @var{Cl}, @var{Cls} has those clauses
with comments before or after as it was specified when calling
@pred{add_comment/3}.".

insert_clause_comments( Cls , Cl_Out ) :-
	Cls = [Cl|_],
	Cl  = cls${ key => K },
	insert_inter_clause_comments( Cls , ClsWithComments ),
	( 
	    get_all_comments( before( K ) , CBefore ),
	    CBefore \== []
	->
	    append( CBefore , ClsWithComments , L )
	;
	    L = ClsWithComments
	),
	( 
	    get_all_comments( after( K ) , CAfter ),
	    CAfter \== []
	->
	    append( L , CAfter , Cl_Out )
	;
	    Cl_Out = L
	).

insert_inter_clause_comments( [] , [] ) :- 
	!.
insert_inter_clause_comments( [Cl|Cls] , L ) :-
	Cl = cls${ id => K },
	!,
	insert_inter_clause_comments( Cls , L2 ),
	( 
	    get_all_comments( before( K ) , CBefore ),
	    CBefore \== []
	->
	    append( CBefore, [Cl | L1] , L )
	;
	    L = [ Cl | L1 ]
	),
	( 
	    get_all_comments( after( K ) , CAfter ),
	    CAfter \== []
	->
	    append( CAfter , L2 , L1 )
	;
	    L1 = L2
	).
insert_inter_clause_comments( [Cl|Cls] , [Cl|NCls] ) :-
	insert_inter_clause_comments( Cls , NCls ).





:- pred get_all_comments( Where , Comments ) 
	=> list( Comments, t_comment )

# "Gather all comments in @var{Comments} specified by @var{Where}. For
  example it returns all comments at the begining of the file with
  get_all_comments( begin , C ).".

get_all_comments( Where , Comments ) :-
	C = comment${ where => Where },
	findall( C , get_comment( C ) , CL ),
	gather_comments( CL , Comments ).

gather_comments( [] , [] ).
gather_comments( L , [NCs|T1] ) :-
	L = [C|_],
	C = comment${ type => T },
	gather_comments__( L , T , NCs , Rest ),
	gather_comments( Rest , T1 ).

gather_comments__( [C|Cs] , T , NC1 , R ) :-
	C = comment${ type => T },
	!,
	gather_comments__( Cs , T , NC , R ),
	join_comments( C , NC , NC1 ).
gather_comments__( Cs , _ , comment${ comment => "" } , Cs ).



join_comments( A , B , C ) :-
	C = comment${ comment => CC , where => W , type => T },
	A = comment${ comment => AC , where => W , type => T },
	B = comment${ comment => BC },
	to_string( AC , ACS ),
	to_string( BC , BCS ),
	append( ACS , BCS , CC ).
	


to_string( A , S ) :-
	A \== [],
	atom( A ),
	!,
	atom_codes( A , S ).
to_string( A , A ).
	


%% --- just a test.
my_pprint :-
	get_key_predicates( [internal] , P ),
	member( Key , P ),
	get_assertions( Key , As ),
	get_clauses( Key , Cls ),
	api_pwrite( As ),
	api_pwrite( Cls ),
	nl,
	fail.
my_pprint.
