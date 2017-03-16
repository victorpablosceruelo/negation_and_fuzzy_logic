:- module( api_predcl , 
	[
         %% LITERALS
	  literal/2
	, literal_ID/2
	% --- deprecated
	, get_next_literal_id/2
	, literal_info/2
	, literal_loc/2
	% LITERAL OPERATIONS
	, literal_concat/2
	, literal_concat/3
	, remove_true_literals/2
	, rename_literal/4
	%% GOALS
	, kind_of_goal/2
	%% CLAUSES
	, is_clause/1
	% CLAUSE OPERATIONS
	, clause_head/2
	, clause_body/2
	, clause_ID/2
	, clause_dict/2
	, clause_loc/2
	, clause_info/2
	%
	, get_clause_id_from_literal/2
	, insert_ppi/2
% Dead Code?
%	, output_program/3
	%
	, get_clause/1
	, get_clause/2
	, get_clauses/1
	, get_clauses/2
	% ADD
	, add_clause/1
	, add_clauses/1
	% ERASE
	, erase_clause/1
	, erase_clauses/1
	% UPDATE
	, update_clause/1
	, update_clause/2
	, update_body/2
	, add_defined_pred/2
	% RENAME
	, rename_clause/3
	%% PREDICATES
	, is_predicate/2
	, predicate_ID/2
	, get_key_predicates/2
	, select_pred_type/2
	% STUFF
	, update_memo_table/2
	, get_id/2
	, get_loc/2
	% KLUDGES
	% DTM: This will dissapear when everything uses the API!!!
	, kludge_assert_new_pred_info/1
	], 
	[condcomp, assertions, regtypes]).



:- use_module(library(formulae)).
:- use_module(library(aggregates)).
:- use_module(library(lists), [reverse/2]).
% precompiler library
:- use_module(program(clidlist), [
                                         rewrite_source_clause/4
%	                                 atom2data/5,
%					 clid2data/4,
%					 pred2data/3,
				       , clause_key/2 
				       ] ).

:- use_module(spec(s_simpspec), [make_atom/2]).
:- use_module(program(unexpand), [add_unexpanded_data/4]).
:- use_module(program(itf_db), [curr_file/2
	                               , assert_itf/5          ] ).
:- use_module(program(p_unit), [type_of_goal/2
				       , predicate_names/1
				       ] ).
% --- this module should be integrated with api_internal_mod
:- use_module(program(clause_db), [
	                                 source_clause/3,
					 clause_locator/2,
					 add_clause_locator/2
				       ] ).

:- use_module(ciaopp(api(api_processing)), 
	                               [ process_literals/3 
				       , process_literals_with_iterator/3 
				       , iterator_is_last/1 ] ).
:- use_module(ciaopp(api(api_internal_types))).
:- use_module(ciaopp(api(api_base))).
:- use_module(ciaopp(api(api_order))).

:- use_package(.(api_internal_dec)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%   LITERALS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- pred literal( A , B ) 
	: (t_literal(A) ; t_literal_wo_ppi(B))
	=> t_literal_wo_ppi(B)

# "@var{B} is the literal of @var{A}
independently whether it has program point info or not.".

literal( A : _ , A ) :- !.
literal( A     , A ).


:- pred literal_ID( Literal , ID ) 
	: (term( Literal ), var( ID ))
        => t_lit_ppi_id( ID )

# "Return the literal id in @var{ID} for a given literal
  @var{Literal}. This is the ID asked in the Program Point Info (ppi)
  program interface predicates.".

literal_ID( lit${ id => ID } , ID ) :-
	!.
% For the time being, cuts do not have identifiers
literal_ID(  !      , ID ) :-
	!,
	ID = unknown_id.
literal_ID(  _ : ID , ID         ) :-
	!.
literal_ID(  L      , unknown_id ) :-
	error_message( 
	      "Internal Error: Literal ~w has no ID!.~n Set it to unknown_id" ,
	      [L] ).





:- pred get_next_literal_id( RestBody , ID )
	: (t_body( RestBody ), var( ID ))
        => t_lit_ppi_id( ID )

# "This can be strange function. Usually when processing a clause
  body, you have the current literal and the rest of the body. Given
  latter one as @var{RestBody}, the next literal ID is returned in
  @var{ID}. A more common function will be: \"For a given literal ID,
  give me the next literal ID\". This predicate is expected to be
  implemented soon, but nowadays it is not possible.".

% --- deprecated
get_next_literal_id( Lit , K1 ) :-
	Lit == (B,_),
	!,
	literal_ID( B , K1 ).

get_next_literal_id( B , K1 ) :-
	literal_ID( B , K1 ).



:- pred literal_info( Literal , Info )
	:  (t_literal( Literal ) , var( Info ))
        => t_lit( Info )

# "For a given literal @var{Literal} (warning! with Program Point Info
  (ppi)!), a record @var{Info} with all aviable information is
  returned.".

literal_info( Literal ,
	      L
	    ) :-
            L = lit${id      => ID   , goal=>Goal,
	             type    => Type , key =>Key ,
		     locator => Loc  },
            literal(     Literal , Goal ),
            kind_of_goal(   Goal , Type ),
	    literal_ID(  Literal , ID   ),
	    get_key(        Goal , Key  ),
	    literal_loc( Literal , Loc  ),
	    !.

literal_info( Literal , _ ) :-
	error_message( "Internal Error: literal_info of ~w failed!~n" ,
	               [Literal] ),
	!.




:- pred literal_loc( Literal , Loc )
	: (t_literal( Literal ) , var( Loc ))
        => t_loc( Loc )

# "Returns the locator in @var{Loc} for a given literal
  @var{Literal}. In the current implementation, this locator is the
  same locator as the clause that contains the literal.".

literal_loc( Lit , Loc ) :-
	get_clause_id_from_literal( Lit , ClId ),
%	get_key( ClId, ClKey ),
	clause_locator( ClId , Loc ).

% literal_loc( _ , L ) :-
% 	L = loc${module     => unknown , file     => unkown,
% 	         line_begin => 1       , line_end => 1      },
% 	error_message( "literal_loc still not implemented" ).

:- set_prolog_flag( multi_arity_warnings, off ).



:- use_module(library(terms_vars), [varset/2]).
:- use_module(library(sort), [sort/2]).
:- use_module(infer(infer_db), [domain/1
	                               , point_info/5 ] ).



% -------------------------------------------------------------------------

% --- DEAD CODE?
%
% :- pred output_program( Cls, Dicts, NCls )
%
% # "--- TO BE FILLED!".
%
% output_program(Cls,Dicts,NewCls):-
% 	findall(AbsInt,domain(AbsInt),Domains),
% 	output_program_(Cls,Dicts,Domains,NewCls).
%
% output_program_([Clause:Clid|Cls],[Dict|Dicts],Domains,[NewClause|NewCls]):-
% 	dump_clause(Clause,Clid,Dict,Domains,NewClause),
% 	output_program_(Cls,Dicts,Domains,NewCls).
% output_program_([],_Dicts,_Domains,[]).


% -------------------------------------------------------------------------

:- pred insert_ppi( Cls, NewCls )
	: t_cls(Cls) => t_cls( NewCls )

# "Insert Program Point Info between literals.".

insert_ppi( Cls, NewCls ) :-
	findall(AbsInt,domain(AbsInt),Domains),
	insert_ppi_(Cls,Domains,NewCls).

insert_ppi_( Cl, Domains, NCl ):-
        Cl = cls${ id      => Clid , 
	           head    => H    ,
		   body    => B    ,
		   dic     => Dict ,
		   locator => Loc  ,
		   key     => Key  },
        NCl = cls${ 
		   id      => Clid , 
	           head    => NH   ,
		   body    => NB   ,
		   dic     => Dict ,
		   locator => Loc  ,
		   key     => Key  },
	dump_clause( clause( H , B ), Clid, Dict, Domains, 
	             clause( NH, NB) ).
insert_ppi_( [Cl|Cls], Domains, [NCl|NCls] ):-
	insert_ppi_(  Cl, Domains, NCl  ),
	insert_ppi_( Cls, Domains, NCls ).
insert_ppi_( [], _Domains, [] ).


dump_clause(directive(D),_Id,_Dict,_Domains,directive(D)).
dump_clause(clause(Head,Body),Clid,_Dict,Domains,clause(Head,NewBody)):- 
	varset((Head,Body),Vars),
	process_literals_with_iterator(Body,dump_body(Clid,Vars,Domains),NewBody).

dump_body(At:Key,Clid,Vars,Domains, Ite, AtInfo ) :-
	iterator_is_last( Ite ),
	!,
	atom_info( Domains, Key , Vars, AtInfo, (At:Key,ClInfo) ),
	atom_info( Domains, Clid, Vars, ClInfo, true ).
dump_body(At:Key, _Clid, Vars, Domains, _Ite, AtInfo ) :-
	atom_info( Domains, Key, Vars, AtInfo , At:Key ),
	!.
dump_body(At    , _Clid, _Vars, _Domains, _Ite, At ).

atom_info( [Dom|Domains], Key, Vars, (Info,InfoT), Tail ) :-
	atom_info_( Dom, Key, Vars, Info ),
	atom_info( Domains, Key, Vars, InfoT, Tail ).
atom_info( [] , _Key, _Vars, Tail, Tail ).

atom_info_( AbsInt , Key , Vars , G ) :-
	current_fact(point_info(Key,AbsInt,_Vars,_FVars,_Info)),
	!,
	findall((Vars,Info),
	        current_fact(point_info(Key,AbsInt,Vars,_FVars,Info)),
		List),
	get_infos( List, Vars, ListInfo0 ),
	% take out identical info
	sort( ListInfo0, ListInfo ),
	llist_to_disj( ListInfo, Goal ),
	( type_of_goal(builtin(true(Goal)),G) -> true
	; G = true(Goal) ).
atom_info_( _Dom, _Key, _Vars, true ).

get_infos([(Vars,Info)|List],Vars,[Info|ListInfo]):-
	get_infos(List,Vars,ListInfo).
get_infos([],_Vars,[]).




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

literal_concat( Lit , C , D ) :- 
	Lit == (A,B),
	!,
	literal_concat( B , C  , D1 ),
	literal_concat( A , D1 , D  ).
	
literal_concat( Lit , C , ((A;B),C) ) :- 
	Lit == (A;B),
	!.
	
literal_concat( A , B , B ) :-
	( A == true ; A == true:_ ),
	!.

literal_concat( A , B , A ) :-
	( B == true ; B == true:_ ),
	!.

literal_concat( A , B , (A,B) ).


% was:
% literal_concat( Lit , C , D ) :- 
% 	Lit == (A,B),
% 	!,
% 	literal_concat( B , C  , D1 ),
% 	literal_concat( A , D1 , D  ).

% % literal_concat( (A,B) , C , (A,D) ) :- 
% % 	!,
% % 	literal_concat( B , C , D ).
	
% literal_concat( Lit , C , ((A;B),C) ) :- 
% 	Lit == (A;B),
% 	!.
	
% literal_concat( A , B , B ) :-
% 	( A == true ; A == true:_ ),
% 	!.

% literal_concat( A , B , A ) :-
% 	( B == true ; B == true:_ ),
% 	!.

% literal_concat( A , B , (A,B) ).





:- pred remove_true_literals( L , NL )
	: ( t_body( L ), var( NL ) )
       => t_body( NL )
# "Remove 'true' from the body @var{L}. The result is @var{NL}. ".

:- pred remove_true_literals( L , NL )
	: ( t_body_wo_ppi( L ) , var( NL ) )
       => t_body_wo_ppi( NL ).


remove_true_literals( A , A ) :-
	var( A ),
	!.

remove_true_literals( (B,A) , B ) :-
	( A == (true:_) ; A == true ),
        !.

remove_true_literals( (A,B) , C ) :-
	( A == (true:_) ; A == true ),
        !,
	remove_true_literals( B , C ).

remove_true_literals( (A,B) , C ) :-
	remove_true_literals( A , AC ),
	!,
	remove_true_literals( B , BC ),
	literal_concat( AC , BC , C ).

%% remove_true_literals( (A,B) , (A,C) ) :-
%% 	!,
%% 	remove_true_literals( B , C ).

% Reason: (Cond-> A ; B)
% remove_true_literals( (A;B) , C ) :-
% 	( A == (true:_) ; A == true ),
%         !,
% 	remove_true_literals( B , C ).

% remove_true_literals( (A->B;C) , (AC->BC;CC) ) :-
% 	!,
% 	remove_true_literals( A , AC ),
% 	remove_true_literals( B , BC ),
% 	remove_true_literals( C , CC ).
	
remove_true_literals( (A->B) , (AC->BC) ) :-
	!,
	remove_true_literals( A , AC ),
	remove_true_literals( B , BC ).

remove_true_literals( (A;B) , (A;C) ) :-
	!,
	remove_true_literals( B , C ).

remove_true_literals( A , A ).

% Cases to not put this:
% ..., ( current_fact( p(A) ), display( A ), fail ; true), ...
% remove_true_literals( (B;A) , true ) :-
% 	( A == (true:_) ; A == true ),
%         !.




:- set_prolog_flag( multi_arity_warnings, on ).


:- pred rename_literal( Cl , ClI , ClU , NewCl ) 
	: (t_cls( Cl ), nonvar( ClI ) , nonvar( ClU )) 
        => nonvar( NewCl )

# "Renames all literals with the same functor and arity of @var{ClU}
  to @var{ClI} in a given clause @var{Cl}. Notice that @var{ClI} and
  @var{ClU} are in the form of p(1,2,3) and not p/3. Also the have to
  have the same arity!".

rename_literal( Cl, ClI, ClU , NewCl ) :-
	var( Cl ),
	error_message( error , "INTERNAL ERROR: rename_literal: "||
                               "Expected clause (1st arg) in call ~w" , 
			       [rename_literal( Cl, ClI, ClU , NewCl )] ),
	!,
	fail.

rename_literal( Cl, ClI, ClU , NewCl ) :-
	var( ClI ),
	error_message( error , "INTERNAL ERROR: rename_literal: "||
                               "Expected non-var term (2nd arg) in call ~w" , 
			       [rename_literal( Cl, ClI, ClU , NewCl )] ),
	!,
	fail.

rename_literal( Cl, ClI, ClU , NewCl ) :-
	var( ClU ),
	error_message( error , "INTERNAL ERROR: rename_literal: "||
                               "Expected non-var term (3rd arg) in call ~w" , 
			       [rename_literal( Cl, ClI, ClU , NewCl )] ),
	!,
	fail.

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




rename_literal__( Pred:ID , ClI, ClU , NPred:ID ) :-
	!,
	rename_literal__( Pred , ClI, ClU , NPred ).

rename_literal__( Pred , ClI/A , ClU/A , NPred ) :-
	 Pred =.. [ ClI | Cls ],
	NPred =.. [ ClU | Cls ],
	!.

rename_literal__( Pred , _ , _ , Pred ).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%   GOALS   
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

kind_of_goal( Literal , cut ) :-
	literal( Literal , ! ),
	!.

/*		  
kind_of_goal( Literal , pp_check( LProp ) ) :-
	literal( Literal , Goal ),
	type_of_goal( metapred('rtchecks_mod:check'(MetaProp) , _ ) , Goal ),
	arg( 1 , MetaProp , Prop ),
	( nonvar( Prop ) -> body2list( Prop , LProp ) ; LProp = Prop ),
	!.
*/

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


:- pred internal_add_goal( Goal )

# "@var{Goal} can be Module:Functor(..), 'module:functor'(...) or
funtor(...). In the latter case, the current module is used as the
module containing the predicate. This predicate is only for internal
use. It adds all the necessary information to make
@pred{type_of_goal/2} and unexpansion process coherent.".

internal_add_goal( M:Goal ) :-
	!,
	functor( Goal , F , A ),
	% assert current_itf(defines, MF, A) ??
	curr_file( _ , CM ),
	add_unexpanded_data( M , F , A , CM ).
internal_add_goal( Goal ) :-
	Goal =.. [ MF | A ],
	atom_concat(Module, F0, MF),
	atom_concat(  ':' , F , F0),
	!,
	MGoal =.. [ F | A ],
	internal_add_goal( Module:MGoal ).
internal_add_goal( Goal ) :-
	curr_file( _ , M ),
	internal_add_goal( M:Goal ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%   CLAUSES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- pred is_clause( Cls )

# "Success if @var{Cls} is an old clause format. It is deprecated.".

is_clause(    ( (clause(_,_):_) , _) ).


:- pred clause_head( Cls , Head )
# "@var{Head} is the head of the clause @var{Cls}.".

% --- this clause is deprecated
clause_head( ((clause( H,_B):_Id),_D) , H  ).
clause_head( cls${ head => H  }       , H  ).


:- pred clause_body( Cls , Body )
# "@var{Body} is the body of the clause @var{Cls}.".

% --- this clause is deprecated
clause_body( ((clause(_H, B):_Id),_D) , B  ).
clause_body( cls${ body => B  }       , B  ).


:- pred clause_ID( Cls , ID )
# "@var{ID} is the id of the clause @var{Cls}.".

% --- this clause is deprecated
clause_ID(   ((clause(_H,_B): Id),_D) , Id ).
clause_ID(   cls${ id => Id }         , Id ).


:- pred clause_dict( Cls , Dict )
# "@var{Dict} is the dictionary of the clause @var{Cls}.".

% --- this clause is deprecated
clause_dict( ((clause(_H,_B):_Id), D) , D  ).
clause_dict( cls${ dic  => D  }       , D  ).


:- pred clause_loc( Cls , Loc )
# "@var{Loc} is the locator of the clause @var{Cls}.".

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

clause_loc(  cls${ locator => L } , L ).



% +++ DTM: Use internal only
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


:- pred get_clause_id_from_literal( Literal, ParentClauseID )
	: (t_lit( Literal ), var( ParentClauseID ))
        => t_cls_ppi_id( ParentClauseID )
 
# "For a given literal @var{Literal}, its parent clause Program Point
  Info (ppi) is returned in @var{ParentClauseID}.".


get_clause_id_from_literal( Literal , K1 ) :-
	literal_ID( Literal , K ),
	litid2data( K , P , A , C  , _ ),
	make_atom( [P,A,C] , K1 ).



:- set_prolog_flag( multi_arity_warnings, off ).

:- pred get_clause( Cls ) : (var(Cls);t_cls(Cls))

# "Retrieves the clause that match with @var{Cls}. If @var{Cls} is
  var, then all clauses can be got by fail.".

get_clause( Cls ) :-
	Cls = cls${ ref => Ref , id => ID , head => H , body => B },
	!,
	C = clause(H,B),
	current_fact( source_clause(ID,C,Dict) , Ref ),
	clause_info( ((C:ID),Dict) , Cls ).

:- pred get_clause( IDorKey , Cls )
	:  (term(IDorKey),var(Cls)) 
        => t_cls(Cls).

:- pred get_clause( IDorKey , Cls ) 
	: (t_cls_ppi_id(IDorKey),t_cls(Cls))
        => t_cls(Cls)

# "@var{IDorKey} can be the program @tt{point info} of a clause or
  just its @tt{key}. This predicate retrieves the clause that has the
  named @tt{key} or @tt{ppi id}.".

get_clause( Cls_ID , Cls ) :-
	t_cls_ppi_id( Cls_ID ),
	!,
	current_fact( source_clause( Cls_ID , clause(H,B),Dict) , Ref ),
	Cls = cls${ ref => Ref , id => Cls_ID },
	clause_info( ((clause(H,B):Cls_ID),Dict) , Cls ).

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


:- pred get_clauses( IDorKey , Cls ) 
	: (t_cls_ppi_id(IDorKey),t_cls(Cls))
        => list( Cls , t_cls )

# "Like findall(C, @pred{get_clause/2}, Cls ).".

get_clauses( ClKey , Cls ) :-
	findall( C , get_clause( ClKey , C ) , Cls ).



:- pred get_clauses( Cls ) 
	: (var(Cls))
        => list( Cls , t_cls )

# "Retrieves all clauses from the current loaded program.".

get_clauses( Cls ) :-
	findall( C , get_clause( _ , C ) , Cls ).


:- push_prolog_flag( multi_arity_warnings, off ).

:- pred add_clause( C )
	: t_cls( C )

# "The clause @var{C} is added to the program data base. This clause
  will be considered as normal clause, i.e., analizers,
  transformations and output will use/show it. Please look at
  @tt{t_cls} for information about the fields of @var{C}.".

add_clause( Cls ) :-
	add_clause( Cls , true ).

% --- if head or body is not modules expanded -> if the modulo of the
% locator is not unknown then -> epxanded.  The problem with the body
% -> if the predicate can be imported from other module then issude a
% warning.


add_clause( Cls , AddOrder ) :-
	Cls = cls${ id   => Id , key => Key , head    => H , 
	            body => B  , dic => Dic , locator => Loc },
	!,
	add_clause_rt( Cls ),
		       
	clause_key( H , Id ),
	rewrite_source_clause( H , B , Id , clause( H2 , B2 ) ),

	get_key( H ,  Key ),

	Cls2 = cls${ id   => Id , key => Key , head    => H2 , 
	             body => B2 , dic => Dic , locator => Loc },
	
	internal_add_clause( Cls2 ),
	( 
	    AddOrder == true 
	->
	    curr_file( _ , Mod ),
	    add_defined_pred( Key , Mod ),
	    pr_order_add( end , Key ),
	    cl_order_add( Key , Id )
	;   
	    true 
	).
add_clause( Cls , _ ) :-
	error_message(
     "INTERNAL ERROR: add_clause: Type t_cls was expected. Found ~p" , [Cls]
                     ).


%compose_clause( H , B , D , ( clause( H , B ) : Cl_key , D ) ) :-
%	clause_key( H , Cl_key ).




:- if(defined(api_rt)).

add_clause_rt( Cls ) :-
	Cls = cls${ id   => Id , key => Key , head    => H , 
	            body => B  , dic => Dic , locator => Loc },
	add_cl_check( var(Id),
	              "Clause (~p) have program point info already: ~w.",
		      [Cls , Id] ),

	add_cl_check( (ground( Loc ),Loc = loc${}), 
	              "Loc have to be of type loc${}. Use " ||
		      "loc_unknown/1 if you do not have " ||
		      "enough information.",
		      [Loc] ),

	add_cl_check( (nonvar(H),nonvar(B)),
	              "Neither head (~q) nor body (~q) can be var.",
		      [H , B] ),

	add_cl_check( nonvar(Dic),
	              "Dictionary is var (~q).",
		      [Dic] ),
		       
	add_cl_check( (% nonvar(Key),
	               % A kludge, only to check if the key is correct
	               get_key( H ,  Key )),
		       "Invalid Key ~w in ~q. Please use get_key/2 or " ||
		       "left the key field free",
		       [Key , H] ).

add_cl_check( Goal , Message , Opts ) :-
	api_check( Goal , Message , Opts , add_clause ).

:- else.

add_clause_rt( _ ).

:- endif.


:- pred add_clauses( Cls )
	: list( Cls , t_acls )

# "@var{Cls} is a list with the terms a(_), u(_), u(_,_), e(_) that
  add, update or erase a current clause.".


add_clauses( Cls ) :-
	add_clauses_internal( Cls , [] , RevNewClauseOrder ),
	reverse( RevNewClauseOrder , NewClauseOrder ),
	cl_order_update( NewClauseOrder ),
	!.


%
% NOTE:
% As we process the Clause order list (1st argument) on forward
% and avoid to do append to the end on keep_order predicate,
% the restul list in reversed :S
%
add_clauses_internal( [] , O , O ).

add_clauses_internal( [ Cl | Cls ] , O , NO2 ) :-
	Cl = u(C),
	!,
	keep_order( Cl , O , NO1 ),
	update_clause( C ),
	add_clauses_internal( Cls , NO1 , NO2 ).

add_clauses_internal( [ Cl | Cls ] , O , NO2 ) :-
	Cl = u(K,C),
	!,
	(
	    nonvar(K)
	->
	    keep_order( Cl , O , NO1 ),
	    update_clause( K , C ),
	    add_clauses_internal( Cls , NO1 , NO2 )
	;
	    error_message( "add_clause: u(~w,~w) Invalid Key", [K,C] )
	).

% --- type_of_goal( internal is modified ) !!!
add_clauses_internal( [ Cl | Cls ] , O , NO2 ) :-
	Cl = a(K,C),
	!,
	(
	    nonvar(K)
	->
	    % WE NEED TO GENERATE THE ID BEFORE CHECK THE ORDER
	    add_clause( C , false ),
	    keep_order( Cl , O , NO1 ),
	    add_clauses_internal( Cls , NO1 , NO2 )
	;
	    error_message( "add_clause: a(~w,~w) Invalid Key", [K,C] )
	).

add_clauses_internal( [ Cl | Cls ] , O , NO2 ) :-
	Cl = a(C),
	!,
	add_clause( C , false ),
	keep_order( Cl , O , NO1 ),
	add_clauses_internal( Cls , NO1 , NO2 ).

add_clauses_internal( [ Cl | Cls ] , O , NO2 ) :-
	Cl = e(C),
	!,
	keep_order( Cl , O , NO1 ),
	erase_clause( C ),
	add_clauses_internal( Cls , NO1 , NO2 ).

add_clauses_internal( [ C | Cls ] , O , NO2 ) :-
	!,
	add_clause( C , false ),
	keep_order( a(C) , O , NO1 ),
	add_clauses_internal( Cls , NO1 , NO2 ).




internal_add_clause( Cls ) :-
	Cls = cls${ id   => ID , key => _Key , head    => H , 
	            body => B  , dic =>  Dic , locator => Loc },

	assertz_fact( source_clause( ID , clause( H , B ) , Dic ) ),

	Loc = loc${ file       => Source ,
	            line_begin => LB     ,
	            line_end   => LE     },

	% check( inst( ground( Source , LB , LE ) ) )
	add_clause_locator( ID, loc(Source, LB, LE) ),
	internal_add_goal( H ),
	set_modified_flag,
	!.

internal_add_clause( Cls ) :-
	error_message( "Internal Error: Cannot add clause: "||
		       "~p to the clause db~n" , [ Cls ] ).


:- pred erase_clause( Cls ) 
	: t_cls

# "Erase a clause from internal DB.".

%% --- What happends with the analysis info???
%% --- Erase locator!!!
% This is what should work
erase_clause( Cls ) :-
	Cls = cls${ id => ID , key => Key , ref => Ref },
	nonvar( Ref ),
	!,
	set_modified_flag,
	erase( Ref ),
	cl_order_erase( Key , ID ).

% I am sure that a user would like to erase clause that has not been
% previously consulted.
erase_clause( Cls ) :-
	Cls = cls${ id   => ID , head => H , key => Key ,
	            body => B  , dic  => Dic },
	retract_fact( source_clause( ID , clause( H , B ) , Dic ) ),
	cl_order_erase( Key , ID ),
	set_modified_flag,
	!.
erase_clause( Cls ) :-
	error_message( "INTERNAL ERROR: erase_clause: clause ~p has "||
		       "invalid ID or it has not "||
 	               "enough information." , [ Cls ] ).




% :- pred delete_clause( C ) : t_cls
%
% # "The same as @pred{erase_clause/2}. Just an alias.".
%
% delete_clause( C ) :- 
% 	erase_clause( C ).


:- pred erase_clauses( ClsLst ) 
	: list(t_cls)

# "Erase list of clause from internal DB.".


erase_clauses( [] ) :- 
	!.
erase_clauses( [ A | As ] ) :-
	erase_clause( A  ),
	erase_clauses( As ).
erase_clauses( [ A | As ] ) :-
	!,
	error_message( "erase_clauses: Invalid clause: ~p" , [ A ] ),
	erase_clauses( As ).


:- push_prolog_flag( multi_arity_warnings, off ).



:- pred update_clause( Cls ) 
	: t_cls( Cls )

# "Update the clause with the same program point info id as
  @var{Cls} with the data of @var{Cls}.".

update_clause( Cls ) :-
	Cls = cls${ id => ID },
	update_clause( ID , Cls ).



:- pred update_clause( Id , Cls ) 
	: (t_cls_ppi_id(Id), t_cls( Cls ))

# "Substitute the current clause with program point info @var{Id} by
  @var{Cls}.".

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



% --- complete if nececesary
update_dic( _Cls , Cls1 , Cls1 ).



% --- in the text: this clause DOES NOT change clause_location
internal_update_clause( REF , Cls ) :-
	(erase( REF ) -> true ;
         error_message( "Invalid ID in update_clause: ~q", [ REF ] )
        ),
	Cls = cls${ ref => R , 
	            id   => ID , key => _Key , head    => H , 
	            body => B  , dic =>  Dic , locator => _Loc },
	set_modified_flag,
	assertz_fact( source_clause( ID , clause( H , B ) , Dic ) , R ).
% 	Loc = loc${ file => Source ,
% 	            line_begin => LB ,
% 	            line_end   => LE }.
	% --- how to erase the old locator?
        % clause_locator( Id, loc( Source, LB, LE) ).



:- pred update_body( Cls , Cls2 ) 
	: t_cls( Cls )
        => t_cls( Cls )

# "@var{Cls2} is the result of generating new literal program point
  info for the literals which do not have it (i.e., they have been
  added to the original clause @var{Cls}).".

update_body( Cls , Cls2 ) :-
	Cls = cls${ head => H   , body    => B , key => _Cl_Key , 
		    dic  => Dic , locator => Loc , id => ID },
	rewrite_source_clause( H , B , ID , clause( H2 , B2 ) ),
	Cls2 = cls${ head => H2  , body    => B2 , key => Cl_Key2 , 
		     dic  => Dic , locator => Loc , id => ID },
	get_key( Cls2 , Cl_Key2 ).



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



:- pred rename_cls_if_necessary( Cls , Cls1 , ClsOut )
	:  (t_cls( Cls ) , t_cls( Cls1 ))
        => t_cls( ClsOut )

# "Check if @var{Cls} and @var{Cls1} have different header name. If
  so, @var{ClsOut} is the result of update the neccesary data
  from @var{Cls1} to be correct.".

rename_cls_if_necessary( Cls , Cls1 , Cls1 ) :-
	Cls  = cls${ head => H  },
	Cls1 = cls${ head => H1 }, 
	functor( H  , HF , HA ),
	% No changes
	functor( H1 , HF , HA ),
	!.

% Clause has been renamed => We do have to regenerate the key
% --- check the tree?
rename_cls_if_necessary( _Cls , Cls1 , ClsOut ) :-
	Cls1   =
        cls${id => Id , head => H , body => B , dic => Dic , locator => Loc},
	ClsOut =
        cls${id => Id , head => H , body => B , dic => Dic , locator => Loc,
             key => Key},
	get_key( ClsOut , Key ),
	internal_add_goal( H ).



:- pred add_defined_pred( ClKey , M )
	: (term( ClKey ) , atom(M))

# "Add the necessary data in itf_db to define the predicate
  @var{ClKey} in the module @var{M}.".

add_defined_pred( Key , M ) :-
	pr_order_get( PredList ),
	\+ member( Key , PredList ),
	functor( Key , F , A ),
	assert_itf( defined , M , F , A , _ ),
	!.
add_defined_pred( _ , _ ).
 
:- pop_prolog_flag( multi_arity_warnings ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%   PREDICATES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- pred is_predicate( P , Kind ) => pred_type( Kind )
# "Return in @var{Kind} the type of predicate of @var{P}".

is_predicate( _P , _KindOf ) :-
	!.
is_predicate( P , KindOf ) :-
	nonvar( P ),
	P = _:_,
	literal( P , G ),
	is_predicate( G , KindOf ),
	!.


:- pred predicate_ID( P , Key ) 
	: term( P )
        => t_pred_ppi_id( Key ).

predicate_ID( P, PK ) :-
	functor( P , F , A ),
	pred2data( PK , F , A ).


%% --- Do internal tables
:- pred get_key_predicates( Kind , List ) 
	: (list( Kind , pred_type ) , list( List ) )

# "Depending on @var{Kind} returns a list of predicates in @var{List}".

:- regtype pred_type/1 
# "Predicate properties.".

pred_type( internal     ).
pred_type( imported     ).
pred_type( reexported   ).
pred_type( exported     ).
pred_type( metapred     ).
pred_type( dynamic      ).
pred_type( data         ).
pred_type( concurrent   ).
pred_type( multifile    ).
pred_type( impl_defines ).


get_key_predicates( [internal] , L2 ) :-
	predicate_names( L ),
	reverse( L , L1 ),
	transform_to_keys( L1 , L2 ). % Temporal


:- regtype t_pred_types/1.

t_pred_types(PredTypes) :-
	list(PredTypes, pred_type).
t_pred_types(all).

:- pred select_pred_type(List, Selector)
	:  t_pred_types(List) => t_pred_type_selector(Selector)

# "For a given list @var{List} with @em{pred_type} as elements, it returns
a pred_type_selector. @var{List} can take the atom value @tt{all} and
@var{Selector} will be returned with all fields set to @tt{true}.".


select_pred_type( all , PS ) :-
	PS = pred_type_selector${
				    internal    => true,
				    imported    => true,
				    reexported  => true,
				    exported    => true,
				    metapred    => true,
				    dynamic     => true,
				    data        => true,
				    concurrent  => true,
				    multifile   => true,
				    impl_defines=> true
				}.
select_pred_type( []  , pred_type_selector${} ).
select_pred_type( [P|Ps] , S  ) :-
	select_pred_type( Ps , S ), 
	put_pred_type( P , S ).

put_pred_type( internal    , pred_type_selector${internal    => true} ).
put_pred_type( imported    , pred_type_selector${imported    => true} ).
put_pred_type( reexported  , pred_type_selector${reexported  => true} ).
put_pred_type( exported    , pred_type_selector${exported    => true} ).
put_pred_type( metapred    , pred_type_selector${metapred    => true} ).
put_pred_type( dynamic     , pred_type_selector${dynamic     => true} ).
put_pred_type( data        , pred_type_selector${data        => true} ).
put_pred_type( concurrent  , pred_type_selector${concurrent  => true} ).
put_pred_type( multifile   , pred_type_selector${multifile   => true} ).
put_pred_type( impl_defines, pred_type_selector${impl_defines=> true} ).


transform_to_keys( [] , [] ).
transform_to_keys( [A|As] , [B|Bs] ) :-
	A = F/Ar ,
	functor( B , F , Ar ),
	transform_to_keys( As , Bs ).



update_memo_table( _ , _ ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%   STUFF
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- pred get_id( Lit , PPI_ID ) : t_cls(Lit) => t_cls_key(PPI_ID)
# "@var{PPI_ID} is program proint info ID of @var{Lit}".

:- pred get_id( Cls , PPI_ID ) : t_lit(Cls) => t_ppi_id(PPI_ID)
# "@var{PPI_ID} is program proint info ID of @var{Cls}".

get_id( S , ID ) :-
	clause_ID( S , ID ),
	!.

get_id( S , ID ) :-
	literal_ID( S , ID ).



:- pred get_loc( Cls , Loc ) : t_cls(Cls) => t_loc(Loc)
# "@var{Loc} is the locator for the clause @var{Cls}".

get_loc( cls${ locator => Loc } , Loc ) :-
	!.

get_loc( lit${ locator => Loc } , Loc ) :-
	!.

get_loc( as${  locator => Loc } , Loc ) :-
	!.

get_loc( Clause , Loc ) :-
	clause_loc( Clause , Loc ),
	!.

get_loc( Clause , Loc ) :-
	literal_loc( Clause , Loc ),
	!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%   KLUDGES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

kludge_assert_new_pred_info( [] ).
kludge_assert_new_pred_info( [ (clause(H,_):_) | Cls] ) :-
	internal_add_goal( H ),
	!,
	kludge_assert_new_pred_info( Cls ).
kludge_assert_new_pred_info( [ _ | Cls] ) :-
	kludge_assert_new_pred_info( Cls ).



