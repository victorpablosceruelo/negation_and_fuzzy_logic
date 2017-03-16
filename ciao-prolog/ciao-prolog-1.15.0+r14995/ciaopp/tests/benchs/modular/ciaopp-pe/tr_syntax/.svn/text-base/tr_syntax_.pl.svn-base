
:- module(tr_syntax_,[ cleanup_tr_syntax/0, traverse_clauses/5 ],
	[ assertions, basicmodes ]).

:- use_module(remotecut_, [cleanup_remcut/0, transform_remote_cut/8]).
:- use_module(remdisj_, [cleanup_remdisj/0, remove_disjunctions/10]).
%:- use_module(syntax(cge2ite),[cge_to_ite/2,unravel_cges/8]).
cge_to_ite(Cl,Cl).
unravel_cges(Cl,_D,_,Cl,NCls,NCls,NDs,NDs).

:- doc(bug,"1. Make predicate change/2 disappear.").

% :- doc(bug,"2. Clauses result from remove cut are no processed by
%                 remove_disjuntions.").

:- doc(cleanup_tr_syntax,"Cleanups internal database.").

cleanup_tr_syntax:-
	cleanup_remdisj,
	cleanup_remcut.

%-------------------------------------------------------------------------
:- pred traverse_clauses(+Cls,+Ds,+Flag,-NewCls,-NewDs)
	: member(Flag,[all,remove,unravel,unravel_remove])
   # "It traverses program clauses @var{Cls} suitably changing the format
      for the following step (analysis, annotation, etc).  In general, it
      will have to remove disjunctions, if-then-elses, nested CGEs, etc. 
      The particular format is indicated by @var{Flag}. In doing this, the 
      old clauses will be transformed and new clauses (and thus new 
      dictionaries) can be created.".

/*
This new clauses and dictionaries will be accumulated
      in an incomplete list (NCls with tail TNCls and NDs with tail TNDs)
      in order to include them (if possible) after the definition of the
      predicate whose clauses have genereted them I.e. whenever the Id
      (Functor/Arity) for the next clause would be different from the
      previous traversed clause. Note that if a declarative appears in
      the middle of different clauses belonging to the same predicate, or
      the clauses do not appear together, the new clauses will not appear
      at the end of the predicate.  Difference lists are used.
*/

traverse_clauses(Cls,Ds,Flag,NewCls,NewDs):-
	traverse(Cls,Ds,Flag,NewCls0,NewDs),
	change(NewCls0,NewCls).

change([],[]).
change([(Cl,K)|P0s],[Cl:K|Ps]):-
	change(P0s,Ps).

clause_id(Cl:K,Cl,K).
clause_id((Cl,K),Cl,K).

traverse([Clause|Cls],Ds,Flag,NewCls,NewDs):-
	clause_id(Clause,Cl,K),
	clause_pred(Cl,Id),
	traverse_([(Cl,K)|Cls],Ds,Id,Flag,NewCls,NewDs,NCls,NCls,NDs,NDs).
traverse([],[],_Flag,[],[]).

traverse_([],[],_,_F,NewCls,NewDs,NCls,TCls,NDs,TDs):- !,
	NewCls = NCls,
	NewDs  = NDs,
	TCls = [],
	TDs  = [].
traverse_([(Cl,Clid)|Cls],[D|Ds],Id,F,NewCls,NewDs,NCls,TCls,NDs,TDs):-
	clause_pred(Cl,Id), !,
	traverse_clause(F,(Cl,Clid),D,NewCls,NewDs,TNewCls,TNewDs,TCls,TNCls,
                                                                    TDs,TNDs),
	traverse_(Cls,Ds,Id,F,TNewCls,TNewDs,NCls,TNCls,NDs,TNDs).
traverse_(Cls,Ds,_,F,NCls,NDs,NCls0,Cls,NDs0,Ds):-
	traverse(NCls0,NDs0,F,NCls,NDs).

clause_pred(directive(_),directive).
clause_pred(clause(Head,_),F/A):-
	functor(Head,F,A).

:- pred traverse_clause(+Flag,+Cl,+D,
                       -NewCls,-NewDs,-sTNewCls,-TNewDs,-NCls,-TNCls,-NDs,-TNDs)
   # "Transforms the format of a given clause depending on Flag.".

traverse_clause(all,Cl,D,NewCls,NewDs,TNewCls,TNewDs,NCls,TNCls,NDs,TNDs):-
	unravel_cges(Cl,D,non,NCl0,NCls,TNCls0,NDs,TNDs0),
	cge_to_ite(NCl0,NCl00),
	transform_remote_cut(NCl00,D,NCl1,ND1,TNCls0,TNCls1,TNDs0,TNDs1),
 	remove_disjunctions( NCl1    , ND1    ,
 	                     NewCls  , NewDs  ,
 			     TNewCls , TNewDs ,
 			     TNCls1  , TNCls  ,
 			     TNDs1   , TNDs   ).

	
% 	display( 'NCl00: '  ), display( NCl00  ), nl,
% 	display( 'NCl1: '   ), display( NCl1   ), nl,
% 	display( 'TNCls0: ' ), display( TNCls0 ), nl,
% 	display( 'TNCls1: ' ), display( TNCls1 ), 
% 	display( '\n----\n' ), 
	% HERE... TNCls0 CAN HAVE ; , so remove_disjuntions have to be called!

%  	(
%  	    nonvar( TNCls0 )
%  	->
% 	   remove_disjunctions__( TNCls0  , TNDs0   ,
% 	                          TNewCls , TNewDs  ,
% 				  TNewCls1, TNewDs1 ,
% 				  TNCls1  , TNCls2  ,
% 				  TNDs1   , TNDs2   )
%  	;
% 	    TNewCls1 = TNewCls,
% 	    TNewDs1  = TNewDs ,
% 	    TNCls2   = TNCls1 ,
% 	    TNDs2    = TNDs1  
% 	),
% 	remove_disjunctions( NCl1    , ND1    ,
% 	                     NewCls  , NewDs  ,
% 			     TNewCls1, TNewDs1,
% 			     TNCls2  , TNCls  ,
% 			     TNDs2   , TNDs   ).


% 	TNCls1 = [NCl1|TNCls2],
% 	TNDs1  = [ND1 |TNDs2 ],
% 	NewCls = TNCls2,
% 	NewDs  = TNDs2,
%         remove_disjunctions__( TNCls0  , TNDs0  ,
%  	                       TNCls2  , TNDs2,
%  			       TNewCls , TNewDs ,
%  			       TNCls2  , TNCls  ,
%  			       TNDs2   , TNDs   ).

%  	(
%  	    nonvar( TNCls0 )
%  	->
% 	   remove_disjunctions__( TNCls0  , TNDs0  ,
% 	                          TNewCls1, TNewDs1 ,
% 				  TNewCls , TNewDs ,
% 				  TNCls2  , TNCls  ,
% 				  TNDs2   , TNDs   )
%  	;
% 	    TNewCls1 = TNewCls,
% 	    TNewDs1  = TNewDs ,
% 	    TNCls2   = TNCls  ,
% 	    TNDs2    = TNDs   
% 	),
% 	remove_disjunctions( NCl1    , ND1    ,
% 	                     NewCls  , NewDs  ,
% 			     TNewCls1, TNewDs1,
% 			     TNCls1  , TNCls2 ,
% 			     TNDs1   , TNDs2  ).


traverse_clause(remove,Cl,D,NewCls,NewDs,TNewCls,TNewDs,NCls,TNCls,NDs,TNDs):-
	remove_disjunctions(Cl,D,NewCls,NewDs,TNewCls,TNewDs,NCls,TNCls,
	                                                          NDs,TNDs).
traverse_clause(unravel,Cl,D,NewCls,NewDs,TNewCls,TNewDs,NCls,TNCls,NDs,TNDs):-
	unravel_cges(Cl,D,':',NCl,NCls,TNCls,NDs,TNDs),
	NewCls = [NCl|TNewCls],
	NewDs = [D|TNewDs].
traverse_clause(unravel_remove,Cl,D,NewCls,NewDs,TNewCls,TNewDs,NCls,TNCls,
	                                                           NDs,TNDs):-
	unravel_cges(Cl,D,':',NCl,NCls,TNCls0,NDs,TNDs0),
	remove_disjunctions(NCl,D,NewCls,NewDs,TNewCls,TNewDs,TNCls0,TNCls,
	                                                          TNDs0,TNDs).




remove_disjunctions__( [Cl|Cls] ,[D|Ds]   ,
 	               TailCls  , TailDs  ,
		       TTailCls , TTailDs ,
		       TCls     , TNCls   ,
		       TDs      , TNDs    ) :-
        nonvar( Cl ) ,
	!,
	display( Cl ) , nl,
 	remove_disjunctions( Cl        , D ,
 	                     TailCls   , TailDs  ,
 			     TailCls1  , TailDs1 ,
 			     TCls      , TCls1   , 
			     TDs       , TDs1    ),
 	remove_disjunctions__( Cls       , Ds ,
 	                       TailCls1  , TailDs1 ,
 		               TTailCls  , TTailDs ,
			       TCls1     , TNCls   ,
			       TDs1      , TNDs    ).

remove_disjunctions__( _A    , _B   ,
	                C    ,  D   ,
		        C   ,   D   ,
			G    ,  G   ,
			I    ,  I   ).
%  display( 'A: ' ) , display( A ) , nl ,
%  display( 'B: ' ) , display( B ) , nl ,
%  display( 'C: ' ) , display( C ) , nl ,
%  display( 'D: ' ) , display( D ) , nl ,
%  display( 'E: ' ) , display( E ) , nl ,
%  display( 'F: ' ) , display( F ) , nl ,
%  display( 'G: ' ) , display( G ) , nl ,
%  display( 'H: ' ) , display( H ) , nl ,
%  display( 'I: ' ) , display( I ) , nl ,
%  display( 'J: ' ) , display( J ) , nl ,
%  C = E,
%  D = F,
%  G = H,
%  I = J.
 


%% *** Delete this comment after reading: it is only a reminder! ***
%% 
%% The "assertions" library needs to be included in order to support
%% ":- doc(...,...)." declarations such as below, i.e., insert: 
%% 
%% :- module(_,_,[assertions]).
%% 
%% At the beginning of the file:
%% The following version comment(s) can be moved elsewhere in the 
%% file. Subsequent version comments will always be placed above 
%% the last one inserted.


:- doc(version_maintenance,dir('../version')).

:- doc(version(1*0+560,2004/07/16,19:26*57+'CEST'), "Last clause
   of predicate @pred{remove_disjunctions__/10} left some free
   variables.  (David Trallero Mena)").

:- doc(version(1*0+550,2004/07/16,14:16*23+'CEST'), "bug 2 fixed
           (David Trallero Mena)").

