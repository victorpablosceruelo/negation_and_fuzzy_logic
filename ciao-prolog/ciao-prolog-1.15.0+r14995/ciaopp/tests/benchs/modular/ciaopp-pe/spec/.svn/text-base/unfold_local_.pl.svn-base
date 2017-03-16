:- doc(title,"Efficient Local Unfolding with Ancestor Stacks").

:- doc(author, "Elvira Albert").

:- doc(module," This file contains the implementation of local
unfolding with ancestor stacks including the efficient treatment of
builtins").

:- doc(local_select(Strategy,Body,NewBody,AS,Emb), "@var{Strategy}
  can be @tt{leftmost}, local unfolding of builtins @tt{local_builtin}
  or local unfolding with builtins taking into account the embedding
  ordering (i.e., @tt{local_emb}).  @var{Body} corresponds to a given
  goal (i.e., a list of atoms) with three different kind of atoms: the
  mark @tt{top}, a builtin predicate or a user-defined
  predicate.").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%     LEFTMOST COMPUTATION RULE    %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

select_atom(leftmost,Body,Body,_AS,_Done).

select_atom(local_builtin,Body,NewBody,_AS,_Done):-
	select_builtin(Body,[],NewBody).

select_atom(local_emb,Body,NewBody,AS,Emb):-
	select_builtin_emb(Body,[],NewBody,AS,Emb,_Pop).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%    SELECTION OF EVALUABLE BUILTINS  %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


select_builtin([],Visited,Res):-
	reverse(Visited,Res).
 
select_builtin([L|R],Visited,Res):-
        ( can_be_evaluated(L) -> 
             reverse(Visited,Head),
	     append([L|Head],R,Res)
	   ;
          select_builtin(R,[L|Visited],Res)
        ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%    SELECTION OF EVALUABLE BUILTINS OR NON EMBEDDED TERMS    %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- doc(select_builtin_emb(Body,Visited,NewBody,AS,Emb,_Pop),"Predicate
  @pred{select_builtin_emb/6} traverses the @var{Body} looking for an atom
  to the left of the mark @tt{top} which can be evaluated, i.e., it is
  a builtin which can be evaluated or it a user defined predicate
  which does not subsume any ancestor atom.  If there exists such an
  evaluable atom, then it is placed to the leftmost
  position. Otherwise, the same @var{Body} will be returned in
  @var{NewBody}. Finally, @var{Emb} is used to indicate whether the
  embedding ordering does not have to be checked later during
  unfolding. @var{AS} is the ancestor stack which contains the set of
  atoms that should be used to check the embedding subsumption.").


select_builtin_emb([],Visited,Res,_AS,true,_):-
	reverse(Visited,Res).

select_builtin_emb(['$pop$'|R],Visited,Res,_AS,true,Pop):-
	(Pop==false ->
	    reverse(Visited,Head),
	    append(Head,['$pop$'|R],Res)
         ;
	    reverse(Visited,Head),
	    append(['$pop$'|Head],R,Res)
         ).

select_builtin_emb([L|R],Visited,Res,AS,Emb,_Pop):-
	literal_for_orig_pred(L),!,
        functor(L,F,Arity),
	functor(Atom,F,Arity),
 	( member(Atom,AS),
          homeomorphic_embedded(Atom,L) -> 
	  select_builtin_emb(R,[L|Visited],Res,AS,Emb,false)
         ;
          reverse(Visited,Head),
	  append([L|Head],R,Res),
	  Emb=false
        ).
  
select_builtin_emb([L|R],Visited,Res,AS,Emb,Pop):-
        ( can_be_evaluated(L) -> 
             reverse(Visited,Head),
	     append([L|Head],R,Res),
	     Emb=false
	   ;
          select_builtin_emb(R,[L|Visited],Res,AS,Emb,Pop)
        ).




:- doc(version_maintenance,dir('../version')).

:- doc(version(1*0+707,2004/10/08,17:07*02+'CEST'), "Reorganized
   the code and integrated the local unfolding rule in the module
   @tt{unfolding} with the remaining unfolding strategies. Now, only
   the local selection of atoms lives here.  (Elvira Albert)").

:- doc(version(1*0+688,2004/10/04,18:26*51+'CEST'), "Any unfolding
   strategy can now be performed with 3 different computation rules:
   By default, we have @tt{leftmost} unfolding. Then,
   @tt{local_builtin} implements a computation rule which first
   selects builtins which can be evaluated. Finally, @tt{local_emb}
   tries to select atoms which do not endanger the embedding ordering
   or evaluable builtins whenever possible.  (Elvira Albert)").

:- doc(version(1*0+675,2004/09/28,17:52*39+'CEST'), "Unfolding is
   no longer restricted to leftmost selection and can be used now with
   @emph{local} selection rules.  (Elvira Albert)").

