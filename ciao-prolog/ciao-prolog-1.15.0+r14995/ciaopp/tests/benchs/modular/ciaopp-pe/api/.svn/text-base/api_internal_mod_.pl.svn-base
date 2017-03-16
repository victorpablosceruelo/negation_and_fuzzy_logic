%% --- clause_* cannot fail
%% --- create exception
%% --- should api_write consider clause/2 and assertion_read?

:- module( api_internal_mod_,
	[ 
	    cleanup_api/0,
	    
	    appendconj/3,
	    list2conj/2,
	    body2list/2,

	    asbody_to_conj/2,

	    unify_args/3,
	    literal_concat/2,
	    literal_concat/3,
	    remove_true_literals/2,

	    clause_head/2,
	    clause_body/2,
	    clause_ID/2,
	    clause_dict/2,
	    clause_loc/2,
	    clause_info/2,
	    
	    literal/2,
	    literal_ID/2,
	    literal_info/2,

	    is_directive/1,
	    is_clause/1,
	    
	    dic_vars/2,
	    dic_names/2,
	    dic_join/3,
	    
            create_pre_post/4,
            create_pre_post/3,

	    rename_literal/4,
	    rename_clause/3,

	    loc_unknown/1,

	    has_assertions/1,
	    has_assertions/3,

	    get_key/2,
	    get_id/2,
	    get_loc/2,
	    get_clause/1,
	    get_clause/2,
	    get_clauses/2,
	    get_all_assertions/2,
	    get_assertion/3,
	    get_assertion/2,
	    get_assertions/2,
	    get_assertions/4,
	    get_all_assertions_key/1,
	    get_all_assertions_key_of_module/2,
	    get_commented_assertions/2,
	    get_packages_to_output/1,

	    process_literals/3,
	    process_clauses/2,
	    
	    add_assertions/1,
	    add_assertion/1,
	    add_commented_assertions/1,
	    add_commented_assertion/1,
	    add_clause/1,
	    add_clauses/1,
	    
	    add_package_to_output/1,

	    update_clause/2,
	    update_clause/1,
	    update_assertion/2,

	    erase_assertions/1,
	    erase_assertion/1,
	    erase_clauses/1,
	    erase_clause/1,

	    update_memo_table/2,
	    
	    % For programmers
	    api_pwrite/1,

	    api_write/1,
	    api_write_loc/1,
	    
	    kind_of_goal/2,
	    
	    get_key_predicates/2,
	    my_print/0,
	    
	    % TYPES!!!
	    t_body/1,
            assert_body_type/1
	] , 
	[ assertions, regtypes , hiord ] 
	).


% precompiler library
:- use_module('..'(p_unit(clidlist_)), [
                                         rewrite_source_clause/4,
	                                 atom2data/5,
					 clause_key/2 
				       ] ).
:- use_module('..'(p_unit(assrt_db_)), [
	                                 assertion_read/9,
	                                 assertion_body/7 
				       ] ).
% --- this module should be integrated with api_internal_mod
:- use_module('..'(p_unit(clause_db_)), [
	                                 source_clause/3,
					 clause_locator/2,
					 add_clause_locator/2
				       ] ).
% --- Really terrible
:- use_module('..'(p_unit(itf_db_)), [curr_file/2]).
:- use_module(library(dynamic), [retractall/1]).
:- use_module('..'(spec(s_simpspec_)), [make_atom/2]).
% :- use_module( library( aggregates ) , [
% 	                                 findall/3,
% 	                                 setof/3
% 				       ] ).
:- use_module(library(aggregates)).
:- use_module(library(lists), [
	                                 append/3,
	                                 reverse/2    
                                       ] ).
:- use_module('..'(p_unit(p_unit_)), [
	                                 type_of_goal/2 , 
	                                 internal_predicate_names/1,
					 predicate_names/1
				       ] ).
% This 2 modules goes together. Should be
% separated into api_internal_write.pl
:- use_module(library(pretty_print), [
	                                 pretty_print/3 ,
	                                 pretty_print/2
				       ] ).
:- use_module('..'(p_unit(unexpand_)), [
	                                 module_spec/2,
					 transform_clause_list/3,
					 transform_assrt_body/3,
					 transform_head/3,
					 transform_body/3,
					 transform_assrt_body/3,
					 transform_name/3
				       ]).
:- use_module(library(vndict)).


:- reexport(   library( messages )  ).

:- use_module(library(assertions(assrt_write))).


:- use_module(library(format)).

:- use_package(.(api_internal_dec_)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%   CLEANING  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cleanup_api :-
	retract_fact( commented_as( _ ) ),
	fail.

cleanup_api :-
	retract_fact( output_packages( _ ) ),
	fail.

cleanup_api.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%   CONVERTING
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%
%% BASE
%%

list2conj( []    , true  ) :-
	!.

list2conj( [A|B] , (A,Br) ) :-
	B \== [],
	!,
	list2conj( B , Br ).

list2conj( [A]   , A      ) :-
	!.

list2conj( A    , _  ) :-
	error_message(
		"Internal Error: Bad Arguments ~w when calling list2conj/2",
	        [ A ] ),
	fail.


%%
%% list2disj
%%

list2disj( []    , true  ) :-
	!.

list2disj( [A]   , A      ) :-
	!.

list2disj( [A|B] , (A;Br) ) :-
	!,
	list2disj( B , Br ).

list2disj( A    , _  ) :-
	error_message(
		"Internal Error: Bad Arguments ~w when calling list2disj/2",
	        [ A ] ),
	fail.


:- pred asbody_to_conj( A , B )
	: ( assert_body_type( A ) , var( B ) )
        => conj_disj_type( B )

# "Transforms assertion body @var{A} into a conjuntion (@var{B}). It
  runs in both ways".

:- pred asbody_to_conj( A , B ) 
	:  ( var( A ) , conj_disj_type( B ) )
        => assert_body_type( A ).


asbody_to_conj( A , B ) :-
	var( A ),
	!,
	conj_to_list_of_list( B ,  A  , [] ).

asbody_to_conj( A , B ) :-
	list_of_list_to_conj( A ,  B ).




% list_of_list_to_conj( [ A , B ] , (AC;BC) ) :-
% 	list( A ),
% 	list( B ),
% 	!,
% 	list_of_list_to_conj( A , AC ),
% 	list_of_list_to_conj( B , BC ).

list_of_list_to_conj( [ ( A ; B ) | C ] , Out ) :-
	!,
 	list2conj( A , AC ),
 	list2conj( B , BC ),
 	list_of_list_to_conj( C , CC ),
	( 
	    CC == true
	->
	    Out = (AC;BC)
	;
	    Out = ( (AC;BC) , CC )
	).

% list_of_list_to_conj( [ A , B | C ] , ( (AC;BC) , CC ) ) :-
% 	list( A ),
% 	list( B ),
% 	!,
% 	list_of_list_to_conj( A , AC ),
% 	list_of_list_to_conj( B , BC ),
% 	list_of_list_to_conj( C , CC ).

list_of_list_to_conj( [ A | B ] , (AC , BC ) ) :-
	B \== [],
	!,
	list_of_list_to_conj( A , AC ),
	list_of_list_to_conj( B , BC ).

list_of_list_to_conj( [AL] , A ) :-
	list_of_list_to_conj( AL , A ),
	!.

list_of_list_to_conj( AL , A ) :-	
	list( AL ),
	!,
	list2conj( AL , A ).

list_of_list_to_conj( A , A ).



conj_to_list_of_list( (A,B) ,  Ac  , TAc ) :-
	!,
	conj_to_list_of_list( A , Ac , T   ),
	conj_to_list_of_list( B , T  , TAc ).

%conj_to_list_of_list( (A;B) , [ [ AC , BC ] | T ] , T ) :-
conj_to_list_of_list( (A;B) , [ ( AC ; BC ) | T ] , T ) :-
	!,
	conj_to_list( A , AC ),
	conj_to_list( B , BC ).

conj_to_list_of_list( true , T , T ) :-
	!.

conj_to_list_of_list( A , [ A | T ] , T ).




conj_to_list( (A,B) , [A|Bs] ) :-
	!,
	conj_to_list( B , Bs ).
conj_to_list( A , [A] ).


%-------------------------------------------------------------------%
% body2list(+,-)                                                    %
% body2list(Body,List)                                              %
%  Transform the body of a clause into a list of goals              %
%-------------------------------------------------------------------% 
body2list( (First,Rest) , [ NewFirst | More ] ) :-
        !,
	p_exp2list( First , NewFirst ),
        body2list( Rest , More ).

body2list( Last , [ NewLast ] ) :-
	p_exp2list( Last , NewLast ).

%-------------------------------------------------------------------%
% p_exp2list(+,-)                                                   %
% p_exp2list(Parall_expression,List)                                %
%  Transform a set of parallel goals to a list of goals             %
%-------------------------------------------------------------------%
p_exp2list( '&'(G,Goals) , [ G | More ] ) :-
	!,
	p_exp2list__( Goals , More ).

p_exp2list( Goal , Goal ).




p_exp2list__( '&'(G,Goals) , [ G | More ] ) :-
	p_exp2list__( Goals , More ).

p_exp2list__( Goal , [ Goal ] ).




%%
%% LIB
%%

unify_args( [] , [] , [] ) :-
	!.

unify_args( [ A | As ] , [ NA | NAs ] , [ (A,NA) | R ] ) :-
	!,
	unify_args( As , NAs , R ).

unify_args( [] , A , _ ) :-
	!,
	message( error , ['unify_agrs: list have different size: (2nd arg):\n' , A] ).

unify_args( A , [] , _ ) :-
	message( error , ['unify_agrs: list have different size: (1st arg):\n' , A] ).




appendconj( (A,B) , R , (A,Bs) ) :-
	!,
	appendconj( B , R , Bs ).
	
appendconj( B  , R , (B,R) ).



:- pred create_pre_post( IB , (Pre,Post) , B , TB )
	: (t_body( IB ), t_body( Pre ), t_body( Post ))
        => (t_body(B), var(TB))

# "The body @var{B} is the result of place, literals @var{Pre} and
  @var{Post} before and after the litheral @var{IB}. @var{TB} is the
  tail of @var{B}.".

create_pre_post( ClCall , (Pre,Post) , B , TB ) :-
	literal_concat( Post, TB     , NPost ),
	literal_concat( Pre , ClCall , A ),
	literal_concat( A   , NPost   , B ).




:- pred create_pre_post( IB , (Pre,Post) , B )
	: (t_body( IB ), t_body( Pre ), t_body( Post ) )
        => (t_body(B), var(TB))

# "Similar to @pred{create_pre_post/3}, but with no tail.".

create_pre_post( A , B , D ) :- 
	create_pre_post( A , B , C , true ),
	remove_true_literals( C , D ).


%%% LOW level

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%   CLAUSE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% --- this clause is deprecated
clause_head( ((clause( H,_B):_Id),_D) , H  ).
clause_head( cls${ head => H  }       , H  ).



% --- this clause is deprecated
clause_body( ((clause(_H, B):_Id),_D) , B  ).
clause_body( cls${ body => B  }       , B  ).




% --- this clause is deprecated
clause_ID(   ((clause(_H,_B): Id),_D) , Id ).
clause_ID(   cls${ id => Id }         , Id ).



% --- this clause is deprecated
clause_dict( ((clause(_H,_B):_Id), D) , D  ).
clause_dict( cls${ dic  => D  }       , D  ).




% --- this clause is deprecated
clause_loc(  Clause , L ) :-
	Clause = ((clause(_H,_B):Id),_D),
	!,
 	L = loc${ module => M , file => Source , line_begin => LB ,
	          line_end => LE },
	(
	    clause_locator( Id, loc( Source, LB, LE) )
	->
	    curr_file(_File,M)
	;
	    Source = unknown,
	    LE = 0 ,
	    LB = 0,
	    M  = unknown,
	    error_message( error , 
	                   "Internal error: Could not read locator from clause "||
		           "db: ~p.~n Location set to unknown (clause id: ~w).",
			    [Clause, Id] )
	),
	% Internal check
	(
	    ground( L )
	->
	    true
	;
	    error_message( error , "Internal error: Bad Locator read from "||
		                   "clause db: ~w. ~w was returned." ,
				   [Id,L] ),
	    fail
	).

clause_loc(  cls${ locator => L} , L ).



% --- DTM: Use internal only
clause_info( Clause , CLS ) :-
	Clause = ((clause(H,B):Id),Dic),
	!,
 	CLS = cls${ id => Id  , key => Key , head => H , 
 	            body => B , dic => Dic , locator => Loc },
 	get_key( Clause , Key ),
	clause_loc( Clause , Loc ).
	    

clause_info( Clause , _ ) :-
	error_message( error , "Invalid clause: ~w" , [Clause] ),
	fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%   LITERALS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

literal(     A : _ , A ) :- !.
literal(     A     , A ).

% For the time being, cuts do not have identifiers
literal_ID(  !      , ID ) :-!,
	ID = unknown_id.
literal_ID(  _ : ID , ID         ) :- !.
literal_ID(  L      , unknown_id ) :-
	error_message( 
	      "Internal Error: Literal ~w has no ID!.~n Set it to unknown_id" ,
	      [L] ).


literal_info( Literal ,
	      lit${id  =>ID   , goal=>Goal,
	           type=>Type , key =>Key}
	    ) :-
            literal(    Literal , Goal ),
            kind_of_goal(  Goal , Type ),
	    literal_ID( Literal , ID   ),
	    get_key(       Goal , Key  ),
	    !.

literal_info( Literal , _ ) :-
	error_message( "Internal Error: literal_info of ~w failed!~n" ,
	               [Literal] ),
	!.




literal_loc( _ , L ) :-
	L = loc${module     => unknown , file     => unkown,
	         line_begin => 1       , line_end => 1      },
	error_message( "literal_loc still not implemented" ).


:- set_prolog_flag( multi_arity_warnings, off ).


%% --- review
literal_concat( [] , true ) :- 
	!.

literal_concat( [ A ] , AS ) :- 
	!,
	(   
	    A = (B:_)
	->
	    AS = B
	; 
	    AS = A
	).

literal_concat( [ A | B ] , BId ) :-
	( A == (true:_) ; A == true ),
	!,
	literal_concat( B , BId ).

literal_concat( [ A | B ] , ( AS , BId ) ) :-
	literal_concat( [ A ] , AS ),
	!,
	literal_concat( B , BId ).

literal_concat( [ A | B ] , ( A , BId ) ) :-
	!,
	literal_concat( B , BId ).



%%% --- New Version
% literal_concat( (A,B) , C , D ) :- 
% 	(A = true ; A = true:_),
% 	!,
% 	literal_concat( B , C , D ).

literal_concat( (A,B) , C , D ) :- 
	!,
	literal_concat( B , C  , D1 ),
	literal_concat( A , D1 , D  ).

% literal_concat( (A,B) , C , (A,D) ) :- 
% 	!,
% 	literal_concat( B , C , D ).
	
literal_concat( (A;B) , C , ((A;B),C) ) :- 
	!.
	
literal_concat( A , B , B ) :-
	( A == true ; A == true:_ ),
	!.

literal_concat( A , B , A ) :-
	( B == true ; B == true:_ ),
	!.

literal_concat( A , B , (A,B) ).



:- pred remove_true_literals( L , NL )
	: ( t_body( L ), var( NL ) )
       => t_body( NL )
# "Remove 'true' from the body @var{L}. The result is @var{NL}. ".

:- pred remove_true_literals( L , NL )
	: ( t_body_no_ppi( L ) , var( NL ) )
       => t_body_no_ppi( NL ).



remove_true_literals( (B,A) , B ) :-
	( A == (true:_) ; A == true ),
        !.

remove_true_literals( (B;A) , B ) :-
	( A == (true:_) ; A == true ),
        !.

remove_true_literals( (A,B) , C ) :-
	( A == (true:_) ; A == true ),
        !,
	remove_true_literals( B , C ).

remove_true_literals( (A,B) , (A,C) ) :-
	!,
	remove_true_literals( B , C ).

remove_true_literals( (A;B) , C ) :-
	( A == (true:_) ; A == true ),
        !,
	remove_true_literals( B , C ).

remove_true_literals( (A;B) , (A,C) ) :-
	!,
	remove_true_literals( B , C ).

remove_true_literals( A , A ).




:- set_prolog_flag( multi_arity_warnings, on ).



rename_literal( Cl, ClI, ClU , NewCl ) :-
        Cl = cls${ id      => Id , 
	           head    => H ,
		   body    => B ,
		   dic     => Dic ,
		   locator => Loc,
		   key     => Key },
	!,
	functor( ClI , FClI , AClI ),
	functor( ClU , FClU , AClU ),
	process_literals( B , rename_literal__( FClI/AClI , FClU/AClU ) , NB ),
        NewCl = cls${ 
		   id      => Id , 
	           head    => H ,
		   body    => NB ,
		   dic     => Dic ,
		   locator => Loc,
		   key     => Key }.




rename_literal__( Pred:ID , ClI, ClU , (NPred:ID,Ta) , Ta ) :-
	!,
	rename_literal__( Pred , ClI, ClU , (NPred,Ta), Ta ).

rename_literal__( Pred , ClI/A , ClU/A , (NPred,Ta) , Ta ) :-
	 Pred =.. [ ClI | Cls ],
	NPred =.. [ ClU | Cls ],
	!.

rename_literal__( Pred , _ , _ , (Pred,A) , A ).




:- pred rename_clause( Cl , NewName , ClRenamed ) 
	: (t_cls( Cl ), term( NewName ), var( ClRenamed ))
        => t_cls( ClRenamed )

# "Given @var{Cl}, the @var{ClRenamed} is the same clause but with
@var{NewName} as header name. @var{NewName} is supposed to be a
predicate (p(_,_,_)), because the same predicate with different arity
can occur (p(_,_) and p(_) for example). ".


rename_clause( Cl , NewName , ClRenamed ) :-
	nonvar( Cl ),
	nonvar( NewName ),
	!,
	Cl = cls${ id      => ID , 
	           head    => H  , 
		   body    => B  ,
		   dic     => D  ,
		   locator => L  },
	H  =.. [    _    | Args ],
	functor( NewName , NName , _ ),
	HR =.. [ NName | Args ],
	ClRenamed = cls${ id      => ID ,
	                  head    => HR ,
			  body    => B  ,
			  dic     => D  ,
			  locator => L  ,
			  key     => Key},
	% To generate the new key
	get_key( HR , Key ).

rename_clause( Cl , NewName , ClRenamed ) :-
	error_message( "INTERNAL ERROR: rename_clause: it was not possible "||
		       "to rename clause header ~p to ~w" ,
		       [ Cl , NewName ] ),
	ClRenamed = Cl.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%   GOALS   
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

kind_of_goal( Literal , cut ) :-
	literal( Literal , ! ),
	!.
		  
kind_of_goal( Literal , pp_check( LProp ) ) :-
	literal( Literal , Goal ),
	type_of_goal( metapred('rtchecks_mod:check'(MetaProp) , _ ) , Goal ),
	arg( 1 , MetaProp , Prop ),
	body2list( Prop , LProp ),
	!.

kind_of_goal( Literal , builtin( Goal ) ) :-
 	literal( Literal , Goal ),
 	type_of_goal( builtin(_) , Goal ),
 	!.

kind_of_goal( Literal , normal( Goal ) ) :-
 	literal( Literal , Goal ),
	!.

kind_of_goal( Literal , Out ) :-
	nonvar( Literal ),
	Literal = _:_,
	literal( Literal , Goal ),
	kind_of_goal( Goal , Out ),
	!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%   IDENTIFYING
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

is_directive( ( (directive(_):_) , _) ).
	
is_clause(    ( (clause(_,_):_) , _) ).





% --- hacer un sort despues del complete (o antes)
new_clause( H , B , D , ( NewCl : Cl_ID , ND ) ) :-
	clause_key( H , Cl_ID ),
	rewrite_source_clause( H , B , Cl_ID , NewCl ),
	complete_dict( D , NewCl , ND ).




compose_clause( H , B , D , ( clause( H , B ) : Cl_key , D ) ) :-
	clause_key( H , Cl_key ).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%   PROCESSING
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- meta_predicate process_clauses( pred(3) , ? ).

process_clauses( Hook , NCls ) :-
	get_clauses( Cls ),
	process_clauses__( Cls , Hook , NCls , [] ).




:- meta_predicate process_clauses__( pred(3) , ? , ? ).

process_clauses__( [ ] , _Hook , A , A ).

process_clauses__( [ Cl | RCl ] , Hook , Cls , TCls ) :-
	Hook( Cl , Cls , Cls1 ),
	!,
	process_clauses__( RCl , Hook , Cls1 , TCls ).

process_clauses__( [ Cl | RCl ] , Hook , Cls , TCls ) :-
	error_message( 
	     "Internal error: process_clauses: Unable to process ~p" , [Cl] ),
	process_clauses__( RCl , Hook , Cls , TCls ).





:- meta_predicate process_literals( ? , pred( 3 ) , ? ).

process_literals( Cl , Hook , NCl ) :-
	process_literals_internal( Cl , Hook , NCl ),
	!.

process_literals( Cl , _ , Cl ) :-
	error_message( 
	     "Internal error: process_literals: Unable to process ~p." , [Cl] ).




% Clause
process_literals_internal( Cl , Hook , NCl ) :-
	Cl = cls${ id      => ID , 
	           key     => Key ,
		   head    => H ,
		   body    => B ,
		   dic     => Dic ,
		   locator => L },
	!,
	process_literals( B , Hook , NB ), %Body
	sort_dict( Dic , Dic1 ),
	complete_dict( Dic1 , NB , NDic ),
	NCl = cls${ id      => ID   , 
	           key     => Key  ,
		   head    => H    ,
		   body    => NB   ,
		   dic     => NDic ,
		   locator => L }.

% Body
process_literals_internal( B , Hook , NNB ) :-
	process_literals__( B , Hook , NB , true ),
	remove_true_literals( NB , NNB ).




:- meta_predicate process_literals__( ? , pred( 2 ) , ? ).

process_literals__( (A,B) , Hook , (L,TL) , TL ) :-
	!,
	process_literals__( A , Hook , NA, TA ),
	process_literals__( B , Hook , TA, true ),
	remove_true_literals( NA , L ).

process_literals__( (A;B) , Hook , ((NNA;NNB),TD) , TD ) :-
	!,
	process_literals__( A , Hook , NA , true ),
	process_literals__( B , Hook , NB , true ),
	remove_true_literals( NA , NNA ),
	remove_true_literals( NB , NNB ).

process_literals__( A , Hook , NA , NB ) :-
	Hook( A , NA , NB ),
	!.
	
process_literals__( A , _Hook , (fail,Tail), Tail ) :-
	error_message( 
	     "Internal error: process_literals: Unable to process ~p." , [A] ).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%   GET
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_packages_to_output( X ) :-
	current_fact( output_packages( X ) ),
	!.

get_packages_to_output( [ ] ).




get_key( ((clause(H,_B): _Id),_) , ClKey ) :- 
	!,
	(
	    var( ClKey ) 
	->
	    functor( H     , F , A ),
	    functor( ClKey , F , A )
	;
	    H = ClKey
	).

% just a body literal
get_key( H:_Id , ClKey ) :- 
	!,
	functor( H     , F , A ),
	functor( ClKey , F , A ).

get_key( cls${ key => Key } , Key ) :-
	nonvar( Key ),
	!.

get_key( cls${ head => H  } , ClKey ) :-
	functor( H     , F , A ),
	functor( ClKey , F , A ),
	!.

%just a new body literal
get_key( H , ClKey ) :- 
	functor( H     , F , A ),
	functor( ClKey , F , A ),
	!.

get_key( H , _ ) :- 
	error_message( "Internal Error: Cannot find the key of ~q",
	                [H] ),
	fail.




get_id( S , ID ) :-
	clause_ID( S , ID ),
	!.

get_id( S , ID ) :-
	literal_ID( S , ID ).





get_loc( cls${ locator=> Loc } , Loc ) :-
	!.

get_loc( lit${ locator=> Loc } , Loc ) :-
	!.

get_loc( as${ locator=> Loc } , Loc ) :-
	!.

get_loc( Clause , Loc ) :-
	clause_loc( Clause , Loc ),
	!.

get_loc( Clause , Loc ) :-
	literal_loc( Clause , Loc ),
	!.




loc_unknown( Loc ) :-
	Loc = loc${ line_end   => 0 , file   => unkown ,
	            line_begin => 0 , module => unknown  }.


:- set_prolog_flag( multi_arity_warnings, off ).

get_clause( Cls ) :-
	Cls = cls${ ref => Ref , id => ID , head => H , body => B },
	C = clause(H,B),
	current_fact( source_clause(ID,C,Dict) , Ref ),
	clause_info( ((C:ID),Dict) , Cls ).




get_clause( ClKey , Cls ) :-
	nonvar( ClKey ),
	!,
	get_key( ((clause(H,_): _),_) , ClKey ),
	current_fact( source_clause(ID,clause(H,B),Dict) , Ref ),
	Cls = cls${ ref => Ref , id => ID /*, key => ClKey*/ },
	clause_info( ((clause(H,B):ID),Dict) , Cls ).

get_clause( ClKey , Cls ) :-
	current_fact( source_clause(ID,clause(H,B),Dict) , Ref ),
	Cls = cls${ ref => Ref , id => ID , key => ClKey },
	clause_info( ((clause(H,B):ID),Dict) , Cls ).




get_clauses( ClKey , Cls ) :-
	findall( C , get_clause( ClKey , C ) , Cls ).




get_clauses( Cls ) :-
	findall( C , get_clause( _ , C ) , Cls ).




%% --- Do internal tables
:- pred get_key_predicates( Kind , List ) 
	: (list( Kind , pred_types ) , list( List ) )

# "Depending on @var{Kind} returns a list of predicates in @var{List}".

:- regtype pred_types/1.

pred_types( internal   ).
pred_types( imported   ).
pred_types( reexported ).
pred_types( exported   ).
pred_types( metapred   ).
pred_types( dynamic    ).
pred_types( data       ).
pred_types( multifile  ).


get_key_predicates( [internal] , L2 ) :-
	predicate_names( L ),
	reverse( L , L1 ),
	transform_to_keys( L1 , L2 ). % Temporal




transform_to_keys( [] , [] ).

transform_to_keys( [A|As] , [B|Bs] ) :-
	A = F/Ar ,
	functor( B , F , Ar ),
	transform_to_keys( As , Bs ).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%   UPDATE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

update_clause( Cls ) :-
	Cls = cls${ id => ID },
	update_clause( ID , Cls ).




%% --- tree => need to change something?
%% --- type_of_goal => be coherent
update_clause( Id , Cls ) :-
	ClsOrig = cls${ ref  => RefOrig , id   => Id },
	get_clause( ClsOrig ),	
	Cls     = cls${ id   => Id },
	% Check if name has changed
	rename_cls_if_necessary( ClsOrig , Cls , Cls1 ),
	% Check if new literals has been added
	update_body( Cls1 , Cls2 ),
	% Check for dictionary
	update_dic( ClsOrig , Cls2 , Cls3 ),
	internal_update_clause( RefOrig , Cls3 ).

:- set_prolog_flag( multi_arity_warnings, on ).




rename_cls_if_necessary( Cls , Cls1 , Cls1 ) :-
	Cls  = cls${ head => H  },
	Cls1 = cls${ head => H1 }, 
	functor( H  , HF , HA ),
	% No changes
	functor( H1 , HF , HA ),
	!.

% Clause has been renamed
% --- check the tree?
rename_cls_if_necessary( _Cls , Cls1 , ClsOut ) :-
	Cls1   =
        cls${id => Id , head => H , body => B , dic => Dic , locator => Loc},
	ClsOut =
        cls${id => Id , head => H , body => B , dic => Dic , locator => Loc,
             key => Key },
	get_key( ClsOut , Key ).




update_dic( _Cls , Cls1 , Cls1 ).
	



update_body( Cls , Cls2 ) :-
	Cls = cls${ head => H   , body    => B , key => _Cl_Key , 
		    dic  => Dic , locator => Loc , id => ID },
	rewrite_source_clause( H , B , ID , clause( H2 , B2 ) ),
	Cls2 = cls${ head => H2  , body    => B2 , key => Cl_Key2 , 
		     dic  => Dic , locator => Loc , id => ID },
	get_key( Cls2 , Cl_Key2 ).



% --- a version with ref nonvar should exists?
update_assertion( OldAs , NewAs ) :-
	OldAs = as${ ref => Ref },
	nonvar( Ref ),
	!,
	erase( Ref ),
	add_assertion( NewAs ).
	
update_assertion( OldAs , _NewAs ) :-
	error_message( "Internal Error: update_assertions: ref field "||
		       "is neccesary to update. Old Assertion:" , [] ),
	api_pwrite( OldAs ),
	nl,
	fail.
	


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%   ADD
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

add_clause( Cls ) :-
	Cls = cls${ id   => _  , key => Key , head    => H , 
	            body => B  , dic => Dic , locator => Loc },

	add_cl_check( (ground( Loc  ),Loc = loc${}), 
	              "Loc have to be of type loc${}. Use " ||
		      "loc_unknown/1 if you dont have " ||
		      "enougth information.",
		      [Loc] ),

	add_cl_check( (nonvar(H),nonvar(B)),
	              "Neither head (~q) nor body (~q) can be var.",
		      [H , B] ),

	add_cl_check( nonvar(Dic),
	              "Dictionary is var (~q).",
		      [Dic] ),
		       
	add_cl_check( (nonvar(Key),
	               % A kludge, only to check if the key is correct
	               get_key( H ,  Key )),
		       "Invalid Key ~w in ~q. Please use get_key/2 or " ||
		       "left the key field free",
		      [Key , H] ),
		       
        new_clause( H , B , Dic , (clause( H2 , B2 ) : Id , NDic) ),

	get_key( H ,  Key ),

	Cls2 = cls${ id   => Id , key => Key    , head    => H2 , 
	             body => B2 , dic => NDic   , locator => Loc },
	
	internal_add_clause( Cls2 ).




add_clauses( [] ).

add_clauses( [ u(C) | Cls ] ) :-
	!,
	update_clause( C ),
	add_clauses( Cls ).

add_clauses( [ u(K,C) | Cls ] ) :-
	!,
	update_clause( K , C ),
	add_clauses( Cls ).

% --- type_of_goal( internal is modified ) !!!
add_clauses( [ a(C) | Cls ] ) :-
	!,
	add_clause( C ),
	add_clauses( Cls ).

add_clauses( [ C | Cls ] ) :-
	!,
	add_clause( C ),
	add_clauses( Cls ).



add_cl_check( Goal , Message , Opts ) :-
	api_check( Goal , Message , Opts , add_clause ).





:- pred is_predicate( P , Kind ) : pred_types( Kind )
# "Depending on @var{Kind} returns a list of predicates in @var{List}".

is_predicate( _P , _KindOf ) :-
	!.

is_predicate( P , KindOf ) :-
	nonvar( P ),
	P = _:_,
	literal( P , G ),
	is_predicate( G , KindOf ),
	!.




dic_vars( dic( V , _ ) , V ).

dic_names( dic( _ , N ) , N ).

dic_join( dic( A , B ) , dic( A1 , B1 ) , dic( AJ, BJ ) ) :-
	append( A , A1 , AJ ),
	append( B , B1 , BJ ).


:- set_prolog_flag( multi_arity_warnings, off ).

:- meta_predicate has_assertions( ? , ? , goal ).

has_assertions( ClKey , Ass , Cond ) :-
	get_assertion( ClKey , Ass ), 
	\+ \+ Cond.




has_assertions( ClKey ) :-
	get_assertion( ClKey , _ ).




:- meta_predicate get_assertions( ? , ? , goal , ? ).

get_all_assertions_key( L2 ) :-
% 	findall( ClKey , 
% 	       assertion_read( ClKey , _ , _Status , _Type , _Body , _ , 
% 	                       _ , _ , _ ),
% 		L ),
% 	remove_repited( L , [] , L2 ),
% 	!.
	setof( ClKey , 
	       M^Status^Type^Body^Dic^S^LE^LB^
	       assertion_read( ClKey , M , Status , Type , Body , Dic , 
	                       S , LE , LB ),
	       L2 ),
	!.




get_all_assertions_key_of_module( M , L2 ) :-
	findall( ClKey , 
	         assertion_read( ClKey , M , _ , _ , _ , _ , _ , _ , _ ),
		 L ),
	remove_repited( L , [] , L2 ),
	!.

get_all_assertions_key_of_module( _ , [ ] ).




get_assertions( ClKey , Ass , Cond , AssList ) :-
	findall( Ass , get_assertion( ClKey , Ass , Cond ), AssList ).



get_assertions( ClKey , AssList ) :-
	findall( Ass , get_assertion( ClKey , Ass ), AssList ).




get_all_assertions( ClKey , AssList ) :-
	findall( Ass , get_assertion( ClKey , Ass ), AssList ).




:- meta_predicate get_assertion( ? , ? , goal ).

get_assertion( ClKey , Ass , Condition ) :-
	get_assertion( ClKey , Ass ),
	\+ \+ Condition.




get_assertion( ClKey , Ass ) :-
	Ass = as${ ref       => ID     ,
		   status    => Status ,
	           type      => Type   ,
		   head      => Key    ,
		   compat    => Compat ,
		   call      => Call   ,
		   succ      => Succ   ,
		   orig_call => Call   ,
		   orig_succ => Succ   ,
		   comp      => Comp   ,
		   dic       => Dic    ,
		   comment   => Comment, 
		   locator   => Loc    
		 },
	Loc = loc${ file       => S,
		    line_begin => LB ,
		    line_end   => LE ,
		    module     => M
		  }, 
	current_fact( assertion_read( ClKey , M , Status , Type , Body , Dic , 
	                              S , LE , LB ), ID ),
	assertion_body( Key , Compat, Call, Succ, Comp, Comment, Body).




get_commented_assertions( ClKey , Ass ) :-
	findall( A1, 
	         (current_fact( commented_as( A1 ) ),
		  A1 = as${ head => ClKey }),
		 Ass ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%   ADD
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- set_prolog_flag( multi_arity_warnings, on ).

:- regtype assert_body_type/1.

assert_body_type(  X  ) :- list( X , assert_body_type__ ).


assert_body_type__( A ) :-
	A = ( _ ; _ ),
	!,
	abt_only_disj( A ).

assert_body_type__( _ ).



abt_only_disj( (A;B) ) :-
	!,
	list(A),
	abt_only_disj__2( B ).



abt_only_disj__2( (A;B) ) :-
	!,
	list(A),
	abt_only_disj__2( B ).

abt_only_disj__2( A ) :- list( A ).



:- regtype conj_disj_type/1

# "The usual prolog way of writting conjuntions and disjuntions in a
  body using ',' and ';'".

% conjuntion
conj_disj_type( ( _ , B) ) :-
	conj_disj_type( B ).

% disjuntion
conj_disj_type( ( _ ; B) ) :-
	conj_disj_type( B ).

% a goal
conj_disj_type( _ ).







add_assertions( [] ).

add_assertions( [ A| As ] ) :-
	add_assertion( A ),
	add_assertions( As ).




add_assertion( As ) :-
	As = as${
		    ref       => ID    ,
		    status    => Status,
		    type      => Type  ,
		    head      => Head  ,
		    compat    => Compat,
		    call      => Calls ,
		    succ      => Succ  ,
%		    orig_call => Calls ,
%		    orig_succ => Succ  ,
		    comp      => Comp  ,
		    dic       => Dic   ,
		    locator   => AsLoc ,
		    comment   => Com   ,
		    fromwhere => From
		},
	add_as_check( var(    ID     ), "id ~q is not var"           , [ID    ] ),
	add_as_check( ground( Status ), "status ~q is not ground"    , [Status] ),
	add_as_check( ground( Type   ), "type ~q is not ground"      , [Type  ] ),
	add_as_check( (nonvar( Calls ),
	               list( Calls ) ), "calls ~q has to be a list"  , [Calls ] ),
	add_as_check( (nonvar( Compat),
		       list( Compat) ), "compat ~q has to be a list" , [Compat] ),
	add_as_check( (nonvar( Succ  ),
	               list( Succ )  ), "success ~q has to be a list", [Succ  ] ),
	add_as_check( nonvar( Comp   ), "comp ~q is var"             , [Comp  ] ),
	add_as_check( nonvar( Dic    ), "dic ~q is var"              , [Dic   ] ),
	add_as_check( ground( Com    ), "status ~q is not ground"    , [Com   ] ),
	add_as_check( (ground( AsLoc  ),
	               AsLoc = loc${ file       => S ,
		                      line_begin => LB,
			 	      line_end   => LE,
				      module     => M
				    }
		      ),
		      "locator ~q has to be of type loc${...}" , [AsLoc] ),
	( ground( From ) , ! ; From = asserted ),
	assertion_body(Head,Compat,Calls,Succ,Comp,Com,Body),
% 	display( Dic ) , nl,
% 	varnamesl2dict( Dic , VNDic ),
% 	sort_dict( Dic , SDic ),
% 	display( ' despues sort: ' ),
% 	display( SDic ) , nl,
% 	display( Body ) , nl,
% 	complete_dict( SDic , Body , NewDic ),
% 	display( NewDic ) , nl,
	asserta_fact( assertion_read(Head,M,Status,Type,Body,Dic,S,LB,LE), ID),
	!.

add_assertion( As ) :-
	error_message( "Internal Error: add_assertion: Could not add ~p" , [ As ] ).




add_as_check( Goal , Message , Opts ) :-
	api_check( Goal , Message , Opts , add_assertion ).




:- pred add_commented_assertions( A )
 	: list( A , t_as )

# "Add the assertions list @var{A} to the commented assertions DB.".

:- data commented_as/1.

add_commented_assertions( [] ).

add_commented_assertions( [ A | As ] ) :-
	add_commented_assertion( A ),
	add_commented_assertions( As ).




:- pred add_commented_assertion( A )
	: term( A )

# "Add assertion @var{A} to the commented assertions DB.".

add_commented_assertion( A ) :-
	A  = as${ status  => S  , type    => T  , head => H  , compat => C , 
	          call    => Ca , succ    => Su , comp => Co , dic => D, 
		  orig_call    => Ca , orig_succ    => Su ,
	          locator => L  , comment => Com },
	!,
	A1 = as${ status  => S  , type    => T   , head => H  , compat => C ,
	          call    => Ca , succ    => Su  , comp => Co , dic => D, 
		  orig_call    => Ca , orig_succ    => Su ,
		  locator => L  , comment => Com , fromwhere => commented },
	asserta_fact( commented_as( A1 ) ).

add_commented_assertion( A ) :-
	error_message( "INTERNAL ERROR: add_commented_assertion: "||
		       "~q is not a valid assertion" , [ A ] ).




:- pred add_package_to_output( A )
 	: atom( A )

# "Add the package @var{A} to the output packages list. These packages
   are asked when doing the output and will be the third argument of
   ':- module' declaration.".


:- data output_packages/1.

add_package_to_output( A ) :-
	current_fact( output_packages( X ) , Ref ),
	!,
	( 
	    member( A , X ) 
	->
	    true
	;
	    erase( Ref ),
	    asserta_fact( output_packages( [A|X] ) )
	).
	
add_package_to_output( A ) :-
	    asserta_fact( output_packages( [A] ) ).
	    


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%   ERASE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

erase_assertions( [] ) :- 
	!.

erase_assertions( [ A | As ] ) :-
	erase_assertion( A  ),
	!,
	erase_assertions( As ).

% erase_assertions( [ A | As ] ) :-
% 	!,
% 	message( error , ['erase_assertions: Invalid assertion: ' , A] ),
% 	erase_assertions( As ).




erase_assertion( A ) :-
	A = as${ ref => ID },
	!,
	(
	    erase( ID )
	->
	    true
	;
	    error_message( "The assertion: ~p has invalid ID" , [ A ] )
	).





erase_clauses( [] ) :- 
	!.

erase_clauses( [ A | As ] ) :-
	erase_clause( A  ),
	erase_clauses( As ).

erase_clauses( [ A | As ] ) :-
	!,
	error_message( "erase_clauses: Invalid clause: ~p" , [ A ] ),
	erase_clauses( As ).




erase_clause( A ) :-
	A = cls${ ref => ID },
	!,
	(
	    erase( ID )
	->
	    true
	;
	    error_message( "The clause: ~p has invalid ID" , [ A ] )
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%   OUTPUT
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


api_internal_write( '$goal'( G , D ) ) :-
	!,
	(
	    D = [_|_]
	->
	    varnamesl2dict( D , D1 )
	;
	    D1 = D
	),
	print_goal( G , D1 ).

api_internal_write( '$goal'( G ) ) :-
	!,
	print_goal( G ).


api_internal_write( '$clause'( H , B , Dic ) ) :-
	remove_ppi_from_clauses( [clause( H , B )] , Cls1 ),
	(
%	    varnamesl( Dic )
	    list( Dic )
	->
	    varnamesl2dict( Dic , D )
	;
	    D = Dic
	),
	pretty_print( Cls1 , [] , [D] ).

api_internal_write( '$clause'( H , B ) ) :-
	remove_ppi_from_clauses( [clause( H , B )] , Cls1 ),
	pretty_print( Cls1 , [] ).


api_internal_write( As ) :-
	As  = as${ %ref      => ID     ,
		   status  => Status ,
	           type    => Type   ,
		   head    => Head   ,
		   compat  => Compat ,
		   call    => Call   ,
		   succ    => Succ   ,
		   comp    => Comp   ,
		   dic     => Dic    ,
		   locator => loc${ module => M },
		   fromwhere => From
		 },
	!,
	( 
	    ( var( Head ) )
	->
	    format( "Empty Assertion" , [] )
	;
	    % --- inverse rewrite program
 	    assertion_body( Head , Compat , Call , Succ , Comp , [] , Body ),
	    transform_head( Head , M , HeadT ),
	    transform_assrt_body( Body , M , BodyT ) ,
	    check_global_props( BodyT , BodyT2 ),
	    ( 
		Dic = no
	    -> 
	        create_dict( (HeadT:-BodyT) , _VN ),
		dict2varnamesl( _VN , VN )
	    ; 
		VN = Dic
	    ),
	    ( 
		( Type = entry ; Type = prop )
	    -> 
	        WriteStatus = nostatus
	    ; 
		WriteStatus = status 
	    ),
	    (
		From == commented
	    ->
	        \+ \+ write_assertion_as_comment( HeadT , Status , Type , BodyT2 , 
	                                      VN , WriteStatus )
	    ;
	        \+ \+ write_assertion( HeadT , Status , Type , BodyT2 , VN , 
	                           WriteStatus )
	    )
	),
	!.

api_internal_write( Cl ) :-
	Cl = cls${ 
		   head    => H,
		   body    => B,
		   dic     => Dic,
		   locator => Loc
		 },
	Loc = loc${ module => M },
	!,
	(
	    nonvar( H ), nonvar( B ),
	    nonvar( Dic ), nonvar( M )
	->
	    remove_ppi_from_clauses( [clause( H , B )] , Cls1 ),
	    transform_clause_list( Cls1 , M , Cls2 ),
	    pretty_print( Cls2  , [] , [Dic] )
	;
	    format( "Empty Clause" , [] )
	).

api_internal_write( Loc ) :-
	Loc  = loc${
		    file       => S   ,
		    line_begin => LB  ,
		    line_end   => LE  
		   },
	atom( S ),
	num( LB ),
	num( LE ),
        !,
	format( "~w: ~d-~d:", [ S , LB ,  LE ] ).



api_write( A ) :-
	var(A),
	!,
	display( A ).

api_write( [ A | As ] ) :-
	api_write( A ),
	!,
	api_write( As ).

api_write( [ ] ) :-
	!.

api_write( a(A) ) :-
	!,
	display( '\nHave to be added:\n' ),
	api_write( A ).

api_write( u(A) ) :-
	!,
	display( '\nNeed to be updated:\n' ),
	api_write( A ).
	
api_write( u(K,A) ) :-
	!,
	display( '\nNeed to be updated with the key: ' ),
	displayq( K ), 
	nl,
	api_write( A ).
	
api_write( A ) :- 
	api_internal_write( A ),
	!.

api_write( Unknown ) :- 
	error_message( "Internal error: failed to write ~q", [ Unknown ] ).




api_write_loc( As ) :-
	As   = as${  locator => Loc },
	api_write( Loc ).

api_write_loc( As ) :-
	As   = cls${ locator => Loc },
	api_write( Loc ).

api_write_loc( Unknown ) :- 
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

api_pwrite( a(A) ) :-
	!,
	display( '\nNeed to be added:\n' ),
	api_pwrite( A ).

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



check_global_props( In, Out ) :-
	assertion_body(Pred,Compat,Call,Succ,Comp,Comm,In),
	change_global_props( Comp , Comp_C ),
	assertion_body(Pred,Compat,Call,Succ,Comp_C,Comm,Out).

change_global_props( [] , [] ) :- !.
change_global_props( [A|AR] , [B|BR] ) :- !,
	change_global_props(A , B ),
	change_global_props(AR , BR ).
change_global_props(A , B ) :-
        remove_first_argument(A, B).




remove_first_argument(M:A, M:B):- !,
	remove_first_argument(A, B).

remove_first_argument(A, B):-
        A =.. [F,_|Args], 
        !,
        B =.. [F|Args]. 

remove_first_argument(A, B):-
        A =.. [B].



:- set_prolog_flag( multi_arity_warnings, off ).

:- pred print_goal( G ) : ground 
# "Print goal as better as possible. It create a pretty_dictionary
 (calling @pred{create_pretty_dict/2} from vndict library.".

print_goal( G ) :-
	create_pretty_dict( G , D ),
	\+ \+ print_goal( G , D ).

:- pred print_goal( G , D ) : ground * varnamedict 
# "Print goal @var{G} as better as possible, using @var{D} as a dictionary.".

print_goal( G , D ) :-
	\+ \+ print_goal__( G , D ).

print_goal__( G , D ) :-
	curr_file( _ , M ),
	rename( G , D ),
	transform_body( G , M , G1 ),
	remove_module_qualificator( G1 , G2 ),
	internal_transform_body( G2 , M , G3 ),
	format( "~w" , [ G3 ] ),
	!.

print_goal__( G , D ) :-
	( rename( G , D ) -> true ; true ),
	format( "~w" , [ G ] ).




internal_transform_body( A , M , B ) :-
	A =.. [ F | LA  ],
	internal_transform_body__( LA , M , LA2 ),
	B =.. [ F | LA2 ].




internal_transform_body__( [] , _ , [] ).

internal_transform_body__( [ '$VAR'(A) | As ] , M , [ '$VAR'(A) | Bs ] ) :-
	internal_transform_body__( As , M , Bs ).

internal_transform_body__( [ A | As ] , M , [ AT2 | Bs ] ) :-
	transform_body( A , M , AT ),
	remove_module_qualificator( AT , AT1 ),
	internal_transform_body( AT1 , M , AT2 ),
	internal_transform_body__( As , M , Bs ).




remove_module_qualificator( ':'(_,L) , L ) :- !.

remove_module_qualificator( L        , L ).

:- set_prolog_flag( multi_arity_warnings, on ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%   PORTRAY
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- multifile portray/1.


portray( A ) :-
	A \== [_|_],
	api_internal_write( A ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%   API INTERNALS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

api_check( Goal , _Message , _Opts , _T ) :-
	Goal ,
	!.

api_check( _Goal , Message , Opts , T ) :-
	append( "Internal Error: ~w: " , Message, M ),
	error_message( M , [T|Opts] ),
	fail.



% --- in the text: this clause DOES NOT change clause_location
internal_update_clause( REF , Cls ) :-
	(erase( REF ) -> true ; error_message( "Invalid ID in update_clause: ~q", [ REF ] )),
	Cls = cls${ id   => ID , key => _Key , head    => H , 
	            body => B  , dic =>  Dic , locator => _Loc },
	assertz_fact( source_clause( ID , clause( H , B ) , Dic ) ).
% 	Loc = loc${ file => Source ,
% 	            line_begin => LB ,
% 	            line_end   => LE }.
	% --- how to erase the old locator?
        % clause_locator( Id, loc( Source, LB, LE) ).




internal_add_clause( Cls ) :-
	Cls = cls${ id   => ID , key => _Key , head    => H , 
	            body => B  , dic =>  Dic , locator => Loc },
	assertz_fact( source_clause( ID , clause( H , B ) , Dic ) ),
	Loc = loc${ file => Source ,
	            line_begin => LB ,
	            line_end   => LE },
	% check( inst( ground( Source , LB , LE ) ) )
	add_clause_locator( ID, loc( Source, LB, LE) ),
	!.

internal_add_clause( Cls ) :-
	error_message( "Internal Error: Cannot add clause: "||
		       "~p to the clause db~n" , [ Cls ] ).



:- pred remove_ppi_from_clauses( A , B ) 
	:  ( list( A , t_clause ) , var( B ) )
        => list( B , t_clause_wo_ppi )

# "It takes a list of @pred{clause_type/1} in the argument @var{A} and
returns in @var{B} the same list of clauses but without program point info.
This is an internal predicate. Should not be exported.".

remove_ppi_from_clauses( [] , [] ).
remove_ppi_from_clauses( [ A | B ] , [ AT | BT ] ) :-
	remove_ppi_from_clause( A , AT ),
	remove_ppi_from_clauses( B , BT ).



% Remove the pp_info from literals
remove_ppi_from_clause( clause( A , B ), clause( A , BT ) ) :-
	remove_ppi_from_literals( B , BT ),
	!.

remove_ppi_from_clause( A , _ ) :-
	error_message(
	  "Internal Error: remove_ppi_from_clause: "||
          "Cannot clean: ~p unexpected format~n",
	  [ A ] ),
	fail.




remove_ppi_from_literals( ( L , Ls ), ( LT , LTs ) ) :-
	!,
	remove_ppi_from_literals( L , LT ),
	remove_ppi_from_literals( Ls , LTs ).

remove_ppi_from_literals( ':'(L,_) , L ) :-
	!.

remove_ppi_from_literals( L , L ).




update_memo_table( _ , _ ).




remove_repited( [ L | Ls ] , A , L_out ) :-
	(
	    member( L , A )
	->
	    remove_repited( Ls , A , L_out )
	;
	    remove_repited( Ls , [ L | A ] , L_out )
	).
	
remove_repited( [] , A , A ).
	


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%   REGTYPES     
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- regtype t_head/1.

t_head( B ) :-
	nonvar( B ).

:- regtype t_body/1.

t_body( (L1;L2) ) :-
%	!,
	t_literal( L1 ),
	t_literal( L2 ).

t_body( (L1,L2) ) :-
%	!,
	t_literal( L1 ),
	t_literal( L2 ).

t_body( L ) :-
	t_literal( L ).

t_body( 0 ). % for directives

:- regtype t_body_no_ppi/1 # 
"Type body with no program point info id.".


t_body_no_ppi( (L1;L2) ) :-
%	!,
	t_literal_no_ppi( L1 ),
	t_literal_no_ppi( L2 ).

t_body_no_ppi( (L1,L2) ) :-
%	!,
	t_literal_no_ppi( L1 ),
	t_literal_no_ppi( L2 ).

t_body_no_ppi( L ) :-
	t_literal_no_ppi( L ).

t_body_no_ppi( 0 ). % for directives

:- regtype t_literal/1 # 
"Type literal. It _has_ to have program point info id".

t_literal( _:B ) :-
	%nonvar( A ) % ???
	t_lit_ppi( B ).


:- regtype t_literal_no_ppi/1 
# "Type literal with no program point info id.".

t_literal_no_ppi( B ) :-
	term( B ).

:- regtype t_lit_ppi/1 
# "Type literal with program point info id.".

:- use_module(program(clidlist), [
	                                clid2data/4,
					atom2data/5
				     ] ).

t_lit_ppi( B ) :-
	atom( B ),
	atom2data( B , _ , _ , _ , _ ).

:- regtype t_cls_ppi/1 # "Type program point info clause id.".

t_cls_ppi( B ) :-
	atom( B ),
	clid2data( B , _ , _ , _ ).

:- regtype t_clause/1 # "Type clause. Literals have program point info
id.".

t_clause( clause( B , H ) ) :-
	t_head( B ),
	t_body( H ).

:- regtype t_clause_wo_ppi/1 # 
"Same as t_clause but without program point info id in literals.".

t_clause_wo_ppi( clause( B , H ) ) :-
	t_head( B ),
	t_body_no_ppi( H ).



%% --- just a test.
my_print :-
	get_key_predicates( [internal] , P ),
	member( Key , P ),
	get_assertions( Key , As ),
	get_clauses( Key , Cls ),
	api_write( As ),
	api_write( Cls ),
	nl,
	fail.

my_print.






%% assertion_body(Pred,Compat,Call,Succ,Comp,Comm,%
%	       (Pred::Compat:Call=>Succ+Comp#Comm)).

:- doc(version_maintenance,dir('../version')).

:- doc(version(1*0+823,2004/11/06,17:00*12+'CET'), "Since ciaopp
   currently does not add literal identifiers to cuts,
   @pred{literal_ID/2} returns @tt{unknown_id} for cuts but no longer
   issues error messages.  (German Puebla)").

:- doc(version(1*0+812,2004/11/05,16:05*11+'CET'), "Modifications
   for writing original assertions in the output, if simplified in
   2-steps.  (Pawel Pietrzak)").

:- doc(version(1*0+781,2004/10/21,21:21*14+'CEST'), "Added
   @pred{asbody_to_conj/2}.  (David Trallero Mena)").

:- doc(version(1*0+780,2004/10/21,21:21*03+'CEST'), "Defined
   @pred{assert_body_type/1} type.  (David Trallero Mena)").

:- doc(version(1*0+667,2004/09/22,12:04*06+'CEST'), "Typo in
   @pred{add_commented_assertion/1}.  (David Trallero Mena)").

:- doc(version(1*0+663,2004/09/21,14:23*44+'CEST'), "Some ~w were
   changed to ~q in format messages.  (David Trallero Mena)").

:- doc(version(1*0+633,2004/09/13,20:17*10+'CEST'), "Added mode
   test when calling to @pred{rename_clause/3}.  (David Trallero
   Mena)").

:- doc(version(1*0+629,2004/09/12,12:08*30+'CEST'),
   "@pred{process_liteals/3} have now 4 arguments, tha last one is the
   tail.  (David Trallero Mena)").

:- doc(version(1*0+628,2004/09/12,12:07*34+'CEST'), "Added
   @pred{remove_true_literals/2}.  (David Trallero Mena)").

:- doc(version(1*0+627,2004/09/12,12:07*19+'CEST'),
   "@pred{update_assertions/1} added.  (David Trallero Mena)").

:- doc(version(1*0+605,2004/08/08,14:37*35+'CEST'), "Added
   @pred{get_all_assertions_key/1},@pred{get_all_assertions_key_of_module/2}.
   (David Trallero Mena)").

:- doc(version(1*0+604,2004/08/08,14:37*04+'CEST'), "Stable
   version of CiaoPP Program unit (API).  (David Trallero Mena)").

