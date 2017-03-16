:- module( api_processing ,
	[ process_literals/3
	, process_literals_with_iterator/3
	, process_clauses/2
	, process_clauses/3
	, get_iterator/2
	, iterator_next/2
	, iterator_is_last/1
	, iterator_prev/2
	, iterator_current/2
	],
	[ assertions
	, regtypes
	, hiord
	, api( api_types )
	] ).


:- use_module(library(messages)).
:- use_module(library(vndict)).
:- use_module(library(formulae)).

:- reexport(ciaopp(api(api_predcl))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%   PROCESSING
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- push_prolog_flag( multi_arity_warnings , off ).

:- meta_predicate process_clauses( pred(3) , ? ).

:- pred process_clauses( Hook , NCls )
	: callable( Hook )
        => list( NCls , t_cls )

# "All the current program loaded clauses are processed invoking
  @var{Hook} and the result is returned in @var{NCls} ".

process_clauses(Hook, NCls) :-
	get_clauses(Cls),
	process_clauses(Cls, Hook, NCls).

:- meta_predicate process_clauses( ? , pred(3) , ? ).

:- pred process_clauses( Cls , Hook , NCls )
	: (list( Cls , t_cls ), callable( Hook ))
        => list( NCls , t_cls )

# "The clause list @var{Cls} is processed invoking @var{Hook} with
  each element as argument and the result is returned in @var{NCls} ".

process_clauses( Cls , Hook , NCls ) :-
	process_clauses__( Cls , Hook , NCls , [] ).

:- pop_prolog_flag( multi_arity_warnings ).



:- meta_predicate process_clauses__( pred(3) , ? , ? ).

process_clauses__( [ ] , _Hook , A , A ).

process_clauses__( [ Cl | RCl ] , Hook , Cls , TCls ) :-
	Hook( Cl , Cls , Cls1 ),
%	clause_ID( Cl , Key ),
	!,
%	Cls = [ mark(Key) | Cls_ ],
	process_clauses__( RCl , Hook , Cls1 , TCls ).

process_clauses__( [ Cl | RCl ] , Hook , Cls , TCls ) :-
	error_message( 
	     "Internal error: process_clauses: Unable to process ~p" , [Cl] ),
	process_clauses__( RCl , Hook , Cls , TCls ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                         PROCESS_LITERALS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- meta_predicate process_literals( ? , pred( 2 ) , ? ).

:- pred process_literals( Cl , Hook , NCl )
	: (t_cls( Cl ) , callable( Hook ) )
       => t_cls( NCl )

# "For a given clause @var{Cl} (or body), the predicate @var{Hook} is
  invoqued which each @var{Cl} body literals. @var{Hook} have to
  return in the last argument the literal(s) that will substitute the
  current one. Empty list is used to denote erase the literal. These
  returned literals will be also processed. The possible modified
  literals are stored and returned in @var{NCl} as a new clause (or
  body).".


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
	process_literals_internal( B , Hook , NB ), %Body
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
	process_literals__( B , Hook , NB ),
	remove_true_literals( NB , NNB ).




:- meta_predicate process_literals__( ? , pred( 2 ) , ? ).


process_literals__( Lit , _Hook , Lit ) :-
	var( Lit ),
	!.

% process_literals__( Lit , Hook , NLit ) :-
% 	Lit = (A,B),
% 	!,
% 	process_literals__( A , Hook , NA ),
% 	( NA == A -> NNA = NA 
% 	; process_literals__( NA , Hook , NNA )	),
% 	process_literals__( B , Hook , NB ),
% 	( NB == B -> NNB = NB 
% 	; process_literals__( NB , Hook , NNB ) ),
% 	literal_concat( NNA , NNB , NLit ).

process_literals__( Lit , Hook , Out ) :-
	( Lit = (A, B)                   ;
          Lit = (A; B), Out = (NNA;NNB)  ;
	  Lit = (A->B), Out = (NNA->NNB) ),
	!,
	process_literals__(    A , Hook , NA ),
	process_subliterals__( A , NA , Hook , NNA ),
	process_literals__(    B , Hook , NB ),
	process_subliterals__( B , NB , Hook , NNB ),
	( Lit = (_,_) -> literal_concat( NNA , NNB , Out ) ; true ).

% process_literals__( Lit , Hook , (NNA->NNB) ) :-
% 	Lit = (A->B),
% 	!,
% 	process_literals__( A , Hook , NA ),
% 	( NA == A -> NNA = NA 
% 	; process_literals__( NA , Hook , NNA )	),
% 	process_literals__( B , Hook , NB ),
% 	( NB == B -> NNB = NB 
% 	; process_literals__( NB , Hook , NNB )	).

process_literals__( B , Hook , B3 ) :-
	Hook( B , B1 ),
	!,
	(  
	    nonvar(B1), list(B1) 
	->
	    list_to_conj( B1 , B2 )
	;
	    B2 = B1
	),
	process_subliterals__( B , B2 , Hook , B3 ).
	
process_literals__( A , _Hook , A ) :-
	error_message( 
	     "Internal error: process_literals: Unable to process ~p." , [A] ).



process_subliterals__( B , NB , _Hook , NB ) :-
	(NB == true ; NB == B), 
	!.
process_subliterals__( _B , NB , Hook , NNB ) :-
	process_literals__( NB , Hook , NNB ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                 PROCESS_LITERALS_WITH_ITERATOR
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- meta_predicate process_literals_with_iterator( ? , pred( 3 ) , ? ).

:- pred process_literals_with_iterator( Cl , Hook , NCl )
	: (t_cls( Cl ) , callable( Hook ) )
       => t_cls( NCl )

# "For a given clause @var{Cl} (or body), the predicate @var{Hook} is
  invoqued which each @var{Cl} body literals and the iterator
  corresponding to the literal. @var{Hook} have to return in the last
  argument the literal(s) that will substitute the current one. Empty
  list is used to denote erase the literal. These returned literals will
  be also processed. When @var{Hook} is invoked with new literals
  obtained from a previous call to @var{Hook} the iterator passed as
  argument would point at the literal which originated them. The
  possible modified literals are stored and returned in @var{NCl} as a
  new clause (or body).".


process_literals_with_iterator( Cl , Hook , NCl ) :-
	process_literals_with_iterator_internal( Cl , Hook , NCl ),
	!.

process_literals_with_iterator( Cl , _ , Cl ) :-
	error_message( 
      "Internal error: process_literals_with_iterator: Unable to process ~p.",
        [Cl] ).




% Clause
process_literals_with_iterator_internal( Cl , Hook , NCl ) :-
	Cl = cls${ id      => ID , 
	           key     => Key ,
		   head    => H ,
		   body    => B ,
		   dic     => Dic ,
		   locator => L },
	!,
	process_literals_with_iterator_internal( B , Hook , NB ), %Body
	sort_dict( Dic , Dic1 ),
	complete_dict( Dic1 , NB , NDic ),
	NCl = cls${ id      => ID   , 
	           key     => Key  ,
		   head    => H    ,
		   body    => NB   ,
		   dic     => NDic ,
		   locator => L }.
% Body
process_literals_with_iterator_internal( B , Hook , NNB ) :-
	get_iterator( B , Iterator ),
	process_literals_wi__( B , true , Iterator , Hook , NB ),
	remove_true_literals( NB , NNB ).




:- meta_predicate process_literals__( ? , pred( 2 ) , ? ).

process_literals_wi__( Lit , _ , _ , _Hook , Lit ) :-
	var( Lit ),
	!.

process_literals_wi__( Lit , F , Ite , Hook , Out ) :-
	( Lit = (A, B)                   ;
          Lit = (A; B), Out = (NA;NB)  ;
	  Lit = (A->B), Out = (NA->NB) ),
	!,
	process_literals_wi__( A , F , Ite , Hook , NA ),
	decide_iterator_next( F , Ite , IteN ),
	process_literals_wi__( B , F , IteN , Hook , NB ),
	( Lit = (_,_) -> literal_concat( NA , NB , Out ) ; true ).

% Already processed?
process_literals_wi__( B , false(B1) , _Ite , _Hook , B ) :-
	B == B1,
	!.

process_literals_wi__( B , F , Ite , Hook , NNNB ) :-
	Hook( B , Ite , NB ),
	!,
	(  
	    nonvar(NB), list(NB) 
	->
	    list_to_conj( NB , NNB )
	;
	    NNB = NB
	),
	process_subliterals_wi__( B , NNB , F , Ite , Hook , NNNB ).
	
process_literals_wi__( A , _ , _Ite , _Hook , A ) :-
	error_message( 
	     "Internal error: process_literals: Unable to process ~p." , [A] ).


decide_iterator_next( true , Ite , IteN ) :-
	!,
	iterator_next( Ite , IteN ).
decide_iterator_next( _F , Ite , Ite ).


process_subliterals_wi__( B1 , NB , F, _IteN , _Hook , NB ) :-
	process_subliterals_is_processed( F , B1 , NB ),
	 !.
process_subliterals_wi__( B , NB , F , IteN , Hook , NNB ) :-
	(F = true , F2 = false( B ) ; F2 = F),
	!,
	process_literals_wi__( NB , F2 , IteN , Hook , NNB ).
 

process_subliterals_is_processed( false( B ) , B1 , NB ) :-
	(NB == true ; NB == B ; NB == B1 ),
	 !.
process_subliterals_is_processed( true , B , NB ) :-
	(NB == true ; NB == B ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ITERATORS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- use_module(library(ddlist)).

get_iterator( Cl , It ) :-
	process_literals( Cl , get_lit_ppi , Body ),
	plain_to_list( Body , BList , [] ),
	create_from_list( BList , It ).


plain_to_list( A , [A|T] , T ) :-
	var( A ),
	!.
plain_to_list( A , B , T ) :-
	(A = (A1,A2);
	 A = (A1->A2);
	 A = (A1;A2)),
	!,
	plain_to_list( A1 , B  , B1 ),
	plain_to_list( A2 , B1 , T  ).
plain_to_list( A , [A|T] , T ).


get_lit_ppi( _:K , K ).
get_lit_ppi(  A  , A ).

iterator_next( It , ItN ) :-
	ddlist:next( It , ItN ).

iterator_prev( It , ItN ) :-
	ddlist:prev( It , ItN ).

iterator_is_last( It ) :-
	ddlist:length_next( It , 0 ).

iterator_current( It , PPI ) :-
	ddlist:top( It , PPI ).

/*
iterator_goto_end( It , ItN ) :-
	forward( It , ItN ).

iterator_goto_being( It , ItN ) :-
	rewind( It , ItN ).

iterator_find( It , Key , It ) :-
	iterator_current( It , Key ),
	!.
iterator_find( It , Key , NIt ) :-
	iterator_next( It , It1 ),
	iterator_find( It1 , Key , NIt ).
*/

% :- export(dump_body/6).

% dump_body(At:Key,Clid,Vars,Domains, Ite, last(Key) ) :-
% 	iterator_is_last( Ite ),
% 	!.
% dump_body(At:Key,Clid,Vars,Domains, Ite, Key ).
% dump_body(At,Clid,Vars,Domains, Ite, At ).
