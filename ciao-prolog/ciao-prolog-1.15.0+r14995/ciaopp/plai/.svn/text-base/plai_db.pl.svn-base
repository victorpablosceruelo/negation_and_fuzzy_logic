
:- module(plai_db,
	[ complete/7, lub_complete/6, 
	  memo_call/5, memo_table/6, memo_lub/5, pragma/5,complete_parent/2,
	 cleanup_plai_db/1],
	[assertions]).

:- use_module(engine(data_facts), [retractall_fact/1]).

:- doc(bug,"The cleanup does not work when in debugging mode!!!").

:- doc(complete(SgKey,AbsInt,Sg,Proj,Prime,Id,Parents),
	"The predicate @var{SgKey} has a variant success pattern 
	  @code{(Sg,Proj,Prime)} on the domain @var{AbsInt}. The and-or
	  graph node is @var{Id}, and is called from the program points
	  in list @var{Parents}.").
:- data complete/7.

:- doc(lub_complete(SgKey,AbsInt,Lub,Sg,Proj,Prime),
	"The predicate @var{SgKey} has in all variants success pattern 
	  @code{(Sg,Proj,Prime)} on the domain @var{AbsInt}. 
          If @var{Lub} is yes, @var{Proj} and @var{Prime} are substitutions, 
	  if @var{Lub} is no, they are lists of substitutions.").
:- data lub_complete/6.

:- doc(memo_table(PointKey,AbsInt,Id,Child,Vars_u,Call),
	"Before calling the goal at program point @var{PointKey}, 
	  there is a variant in which
	  @var{Call} on the domain @var{AbsInt} holds upon the program
	  clause variables @var{Vars_u}. These variables need be sorted
	  conveniently so that @var{Call} makes sense. The and-or graph
	  node that causes this is @var{Id} and the call originated to
	  the goal at this program point generates and-or graph node
	  @var{Child}.").
:- data memo_table/6.
:- data memo_call/5.

:- doc(memo_lub(PointKey,AbsInt,Lub,Vars_u,Call),
	"Before calling the goal at program point @var{PointKey}, 
	  in all variants
	  @var{Call} on the domain @var{AbsInt} holds upon the program
	  clause variables @var{Vars_u}. If @var{Lub} is yes, @var{Call}
	  is a substitution, if If @var{Lub} is no, @var{Call}
	  is a list of substitutions.").
:- data memo_lub/5.

:- doc(complete_parent(Id,Parents), "Used to keep the trace of the
	parents in the and-or graph for the di fixpoint. The parents
	are used to choose previous aproximations to apply the
	widening operators on calls The and-or graph node is @var{Id},
	and @var{Parents} is a list of couples of graph node and
	program points.").


:- data complete_parent/2.


%% ?????????????????
:- data pragma/5.

:- doc(cleanup_plai_db(AbsInt),"Erases all data in this module for
	the analysis domain @var{AbsInt}.").

cleanup_plai_db(AbsInt):-
	retractall_fact(complete(_,AbsInt,_,_,_,_,_)),
	retractall_fact(lub_complete(_,AbsInt,_,_,_,_)),
	retractall_fact(memo_lub(_,AbsInt,_,_,_)),
	retractall_fact(memo_table(_,AbsInt,_,_,_,_)),
	retractall_fact(memo_call(_,AbsInt,_,_,_)),
	retractall_fact(complete_parent(_,_)),
	retractall_fact(pragma(_,_,_,_,_)).
