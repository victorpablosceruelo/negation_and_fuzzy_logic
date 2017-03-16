:- module(depth_first_spec, [df_spec/9], [assertions]).

:- doc(title,"Depth-first traversal of search space").

:- doc(author, "Claudio Ochoa").

:- doc(module," This module implements a depth-first traversal
	of search space of poly-controlled partial evaluation.").

:- use_module(control_pcpe).
:- use_module(search_tree_pcpe, [merge_trees/2]).
:- use_module(common_spec,      [complete_configurations/6]).
:- use_module(db_pcpe).
:- use_module(qsort_confs).
:- use_module(evaluation_pcpe).
:- use_module(ciaopp(preprocess_flags), [current_pp_flag/2]).
:- use_module(library(aggregates), [findall/3]).
:- use_module(library(llists), [flatten/2]).
:- use_module(library(lists)).
:- use_module(library(terms_check)).

:- pred df_spec(+Confs,+G,+U,+D,+D_Lim,+,+SolsIn,-SolsOut,-Tree)#
	 "Takes a list @var{Confs} of configurations, and applies
	 @tt{PCPE} on them using a set @var{G} of global control
	 rules, and a set @var{U} of local control rules. The search
	 spce is traversed in a @tt{depth-first} fashion. When the
	 current depth @var{D} reaches @var{D_Lim}, pruning can be
	 performed. Current solutions are stored in
	 @var{SolsIn} and later returned in @var{SolsOut}. A
	 representation of the search space is (optionally) returned
	 in @var{Tree}".

df_spec(Confs,G,U,D,D_Lim,Int,SolsIn,SolsOut,tree(MId,Tree)):-
	assertz_fact(max_id(MId)),
	assertz_fact(evals(0)),
	findall((Sol,Node),spec_depth(Confs,G,U,D,D_Lim,Int,SolsIn,Sol,Node),SolsTmp),
	split_sols_n_trees(SolsTmp,Sols,Trees),
	(current_pp_flag(output_info,high) ->	        
	    flatten(Trees,Flat_tree),
	    merge_trees(Flat_tree,Tree)
	;
	    Tree = []),
	flatten(Sols,FlatSols),
	(bnb,
	    qsort_confs(FlatSols,SortedSols),
	    (SortedSols == [] ->
	        SolsOut = []
	    ;
		SortedSols =  [Best|_],
		SolsOut = [Best])
	 ;
	    SolsOut = FlatSols),!.

:- pred spec_depth(+Confs,+,+,+,+,+,+,-,-) # "Performs a depth-first
	traversal. When a final configuration is reached, the next one
	is performed by failing on the different global and control
	rules @var{G} and @var{U}".

spec_depth([],_,_,_,_,_,Sol,Sol,[]):-!.
spec_depth([e([],Vis,Val)|As],G,U,D,D_Lim,Int,SolsIn,SolsOut,Tree):-!,
	(bnb,
	    update_evals,
	    eval_solution(e([],Vis,Val),Score),
	    (current_fact(best_value(Ref))->
 	        (Score < Ref ->
	           retract_fact(best_value(_)),
	           assertz_fact(best_value(Score)),
	           spec_depth(As,G,U,D,D_Lim,Int,[e([],Vis,Score)],SolsOut,Tree)
	        ;
		   spec_depth(As,G,U,D,D_Lim,Int,SolsIn,SolsOut,Tree))
            ;
	        assertz_fact(best_value(Score)),
	        spec_depth(As,G,U,D,D_Lim,Int,[e([],Vis,Score)],SolsOut,Tree))
	;
	    spec_depth(As,G,U,D,D_Lim,Int,[e([],Vis,Val)|SolsIn],SolsOut,Tree)),!.
spec_depth([e([(A,Id)|TV],Vis,Val)|As],G,U,D,D_Lim,Int,SolsIn,SolsOut,Tree):-
	prune_conf(e([(A,Id)|TV],Vis,Val),D,ND,D_Lim),
 	member(Gen,G),
 	member(Unf,U),
	generalize(Gen,A,Vis,Generalized),
	unfold_one_step(Unf,Generalized,Vis,Unfolded),
 	retract_fact(max_id(Mid)),	    
	(check_unfolded(Id,Unfolded) ->
	    complete_configurations([Unfolded],TV,Vis,Int,Confs,nodes(Mid,NMid,Childs,Sols)),
	    (current_pp_flag(output_info,high) ->
		append([(Id,A,Childs)],Sols,TSols),
	        Tree = [TSols|TS]
	    ;
		Tree = TS)
	;
	    Confs=[],NMid = Mid,
	    (current_pp_flag(output_info,high) ->	        
		Cid is Id + 1,
		append([(Id,A,[(Cid,'')])],Sols,TSols),
		Tree = [TSols|TS]
	    ;
		Tree = TS)),
	assertz_fact(max_id(NMid)), 
	append(Confs,As,NAs),
	spec_depth(NAs,G,U,ND,D_Lim,Int,SolsIn,SolsOut,TS).


:- pred check_unfolded(+Id,+) #"Every configuration is assigned an
	@var{Id}, and asserted when processed. Thus, when backtracking
	is performed, we can check if the current configuration has
	already been processed, and check if the newly unfolded
	configuration is equivalent to any of the already processed
	configurations, in order to eliminate it".

check_unfolded(Id,Unfolded):-
	\+ current_fact(processed(Id,_L)),!,
	assertz_fact(processed(Id,[Unfolded])).
check_unfolded(Id,Unfolded):-
	current_fact(processed(Id,L)),
	\+ equivs(L,Unfolded),
	retract_fact(processed(Id,L)),
	assertz_fact(processed(Id,[Unfolded|L])).

equivs(L,t(_,_,_,TreeA,_)):-
	member(t(_,_,_,TreeB,_),L),
	variant(TreeA,TreeB),!.

split_sols_n_trees([],[],[]).
split_sols_n_trees([(Sol,Tree)|Ls],[Sol|Sols],[Tree|Trees]):-
	split_sols_n_trees(Ls,Sols,Trees).


:- pred prune_conf(+Conf,+,-,+D_Lim) # "If the desired depth
          @var{D_Lim} has been reached then @var{Conf}is evaluated. If
          the fitness value of this configuration is worse than
          current best one then it is pruned (by returning fail)".

prune_conf(Conf,D,ND,D_Lim):-
	bnb,
	current_fact(best_value(Ref)),
	(D == D_Lim ->
	    ND = 1,
	    update_evals,
	    eval_conf(Conf,Val),!,
	    Val < Ref
	;
	    ND is D + 1
	).
prune_conf(_Conf,D,D,_D_Lim).


update_evals:-
	retract_fact(evals(N)),
	N1 is N+1,
	assertz_fact(evals(N1)).
	
