:- module( api_dict  ,
	[
	% DICTIONARIES
	  complete_goal_dic/4
	, complete_clause_dic/2
	, complete_assertion_dic/2
	, complete_dict_with_assrt_and_cl/3
	],
	[ assertions
	, regtypes, api(api_types) ] ).

:- use_module(library(vndict)).
:- use_module(library(aggregates)).
:- use_module(program(assrt_db)).

:- use_module(ciaopp(api(api_predcl))).

:- use_package(.(api_internal_dec)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%   GOAL
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- pred complete_goal_dic( HDLIST , GoalArgsList , InitialDic , Dics )

# "@var{HDLIST} is a list with hd(Goal,Dict) as
  elements. @var{GoalArgList} are the list of arguments of the goal we
  want to complete the dictionary. @var{InitialDic} is the initial
  dictionary we could have. If you do not have it, just use an empty
  list, []. @var{Dics} is the dictionary returned as a list of pair
  atom=variable. Example:

@begin{verbatim}
?- complete_goal_dic( [hd(ap(A1,A2,A3),
                      [=('A',A1),=('B',A2),=('C',A3)])], [E,D,F] , [] , DF ).

D = A2,
DF = ['C'=A3,'B'=A2,'A'=A1],
E = A1,
F = A3 ? 
@end{verbatim}
".

complete_goal_dic( [hd(G,D) | Hds] , Goal , Dic , DicS ) :-
	G =.. [_|GArgs],
	complete_goal_dic__( GArgs , D , Goal , Dic , Dic1 ),
	complete_goal_dic( Hds , Goal , Dic1 , DicS ).
complete_goal_dic( [] , _ ,  D , D ).




complete_goal_dic__( [GA|GAs] , D , [G|Gs] , Dic , Dics ) :-
	var( GA ),
	var( G ),
	% We dont have it already
	\+ mmember( _=G , Dic ),
	mmember( A=GA , D ),
	% we dont have clash name
	\+ member( A=_ , Dic ),
	GA = G,
	complete_goal_dic__( GAs , D , Gs , [ A=GA | Dic ] , Dics ),
	!.

complete_goal_dic__( [_|GAs] , D , [_|Gs] , Dic , Dics ) :-
	complete_goal_dic__( GAs , D , Gs , Dic , Dics ).

complete_goal_dic__( [] , _ , _ , Dic , Dic ).


	

mmember( A=VB , [ A=VA | _ ] ) :-
	VB == VA,
	!.
mmember( A , [ _ | As ] ) :-
	mmember( A , As ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%   CLAUSE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- pred complete_clause_dic( Cl , NCl ) : t_cls(Cl) => t_cls(NCl)

# "Completes the dictionary of the clause @var{Cl} using other
  assertions or clauses that match with the head. The output is
  returned in @var{NCl}.".


complete_clause_dic( Cl , NCl ) :-
	Cl = cls${ id      => ID
		 , key     => Key
		 , head    => Head
		 , body    => Body
		 , dic     => Dic
		 , locator => Loc },
	!,
	complete_dict_with_assrt_and_cl( Head , Dic , NDic1 ),
	( Dic == no -> varnamesl2dict( NDic1 , NDic2 ) ; NDic2 = NDic1 ),
	complete_dict( NDic2 , (Head,Body) , NDic ),
	NCl = cls${ id      => ID
		  , key     => Key
		  , head    => Head
		  , body    => Body
		  , dic     => NDic
		  , locator => Loc },
	!.
complete_clause_dic( Cl , Cl ).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%   ASSERTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- pred complete_assertion_dic( As , NAs ) : t_as(As) => t_as(NAs)

# "Completes the dictionary of the assertion @var{As} using other
  assertions or clauses that match with the head. The output is
  returned in @var{NAs}.".

complete_assertion_dic( As , NAs ) :-
	As  = as${ 
		   status  => Status ,
	           type    => Type   ,
		   head    => Head   ,
		   compat  => Compat ,
		   call    => Call   ,
		   succ    => Succ   ,
		   comp    => Comp   ,
		   dic     => Dic    ,
		   comment => Comm   ,
		   locator => Loc    ,
		   fromwhere => From
		 },
	complete_dict_with_assrt_and_cl( Head , Dic , NDic ),
	NAs = as${ 
		   status  => Status ,
	           type    => Type   ,
		   head    => Head   ,
		   compat  => Compat ,
		   call    => Call   ,
		   succ    => Succ   ,
		   comp    => Comp   ,
		   dic     => NDic   ,
		   comment => Comm   ,
		   locator => Loc    ,
		   fromwhere => From
		 },
	!.
complete_assertion_dic( As , As ).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%   COMMON
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- pred complete_dict_with_assrt_and_cl( Head , Dic , NDic )
	: (term(Head),var(NDic))

# "For a given head @var{Head} (from assertion or a clause) and
  starting dictionary @var{Dic}, a new dictionary @var{NDic} is
  returned completed with the dictionaries found in the assertions and
  clauses that match the same head. @var{Dic} can be the atom @tt{no},
  so an empty dictionary is got as starting dictionary. Be careful, if
  @var{Dic} is @{no} then @var{NDic} is returned in Assertions
  Dictionary format. You would have to use @pred{varnamesl2dict/2} in
  order to transform it to Clauses Dictionary. Otherwise, @var{NDic}
  is returned in the same format as @var{Dic}. Note that @var{Head}
  can have any constructor on its arguments, but those wont be
  completed.".


complete_dict_with_assrt_and_cl( Head , Dic , NDic ) :-
	functor( Head    , FF , AA ),
	functor( HeadKey , FF , AA ),
	findall( hd(HeadKey,D), 
	         ((assertion_read( HeadKey ,_,_,_,_, D,_,_,_), D\==no);
		  get_clause( HeadKey ,
                              cls${dic=>VND} ), dict2varnamesl(VND, D)),
		 Dicts),
	Head =.. [ _ | Args ],
	( 
	    Dic == no 
	-> 
	    Dic2 = [] 
	; (
	      functor( Dic , dic , _ ) 
	  ->
	      dict2varnamesl( Dic , Dic2 )
	  ;
	      Dic2 = Dic 
	  )
	),
	complete_goal_dic( Dicts, Args , Dic2 , NDic1 ),
	(
	    functor( Dic , dic , _ ) 
	->
	    varnamesl2dict( NDic1 , NDic )
	;
	    NDic = NDic1
	).
