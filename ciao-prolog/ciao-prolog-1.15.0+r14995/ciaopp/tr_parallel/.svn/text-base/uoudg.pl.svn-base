%========================================================================
% Developer: Amadeo Casas
% Started:   May 2007
%------------------------------------------------------------------------
% UOUDG annotator
%
%   Version that preserves the order of the solutions
% 
% This version exposes all possible parallelism from a dependency graph.
% It includes determinism analysis.
%========================================================================

:- module(uoudg,
	[exp_uoudg/5],
	[andprolog, assertions]
	).

:- doc(title, "UOUDG annotator with high-level operators.").
:- doc(subtitle, "Version that preserves the order of the solutions.").

:- doc(author, "Amadeo Casas"||
                   " (@href{http://www.ece.unm.edu/~amadeo},"||
                   " University of New Mexico)").

:- use_module(library(sort), [sort/2]).
:- use_module(library(sets), [ord_subtract/3, ord_union/3]).
:- use_module(library(lists), [insert_last/3, append/3, reverse/2, 
	                            contains_ro/2, last/2, delete/3]).

:- use_module(uudg, [
		      is_there_a_cut/4,
		      remove_edges/3,
		      extract_det_info_gr/4,
		      obtain_dependences_per_node/3,
		      obtain_handler/4,
 		      shrink_dep_list/3,
                      split_vertices/4,
                      determinism_of_current_module/1,
		      remove_transitive_dep/2,
		      all_connected/2,
		      add_small_goals/8,
		      update_edges/4,
		      vertex_to_seq/3,
		      get_directly_dependent_to_group/5,
		      all_small/1,
		      edges_from_outside/3
		    ]).

:- data grouping/2.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ANNOTATION
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

exp_uoudg(graph(Vertices,Edges),Dict,GoalDict,Exp,NewDict) :-
	determinism_of_current_module(Det),
	remove_transitive_dep(Edges,Edges2),
	compute_expr(Vertices,Edges2,Det,Dict,[],GoalDict,_,Exp,NewDict),
	retractall_fact(grouping(_,_)).

% COMPUTES THE PARALLEL EXPRESSION
% COMPUTES THE PARALLEL EXPRESSION
compute_expr([],[],_,Dict,_,_,Exp,Exp,Dict).
compute_expr(Vs,Es,DetInfo,Dict,GPub,GDict,Exp,NExp,NDict) :-
	is_there_a_cut(Vs,Vs_before,Cut,Vs_after),
	sort(Es,Es_Ord),
	(
	    Vs == Vs_before ->

	    % Grouping of nodes
	    sort(Vs,Vs_Ord),
	    sort(GPub,GPub_Ord),
	    split_vertices(Vs,Es,_,Ind),
	    sort(Ind,Ind_Ord),
	    ord_subtract(Ind_Ord,GPub_Ord,Ind2),
 	    assert_group_nodes(Ind2,Vs,Es),
	    simplify_graph(Vs_Ord,Es_Ord,VsGroup,EsGroup),

	    % Rest of algorithm
	    split_vertices(VsGroup,EsGroup,DepVs,IndVs),
	    obtain_small_goals(IndVs,SmallVs),
	    sort(SmallVs,SMALL1),
	    sort(IndVs,IndVs_Ord),
	    sort(GPub,GPub_Ord),
	    obtain_dependences_per_node(DepVs,EsGroup,DepList),
	    (
		(DepVs == []) ->
		 L = IndVs_Ord
	    ;
		shrink_dep_list(DepList,IndVs,DepList1),
		DepList1 = [(_,_,L)|_]
	    ),
	    sort(L,L_Ord),
	    ord_subtract(IndVs_Ord,GPub_Ord,IndVs_Ord1),
	    consecutive_goals(IndVs_Ord1,Consecutive),
	    (
		( SMALL1 = [Small1|_], contains_ro(Consecutive,Small1) ) ->
		less_than(Consecutive,Small1,Consecutive1),
		sort(Consecutive1,Consecutive_Ord),
		ord_subtract(Consecutive_Ord,L_Ord,FORK),
		ord_subtract(Consecutive_Ord,FORK,AND),
		JOIN = [],
		SMALL = [Small1]
	    ;
		sort(Consecutive,Consecutive_Ord),
		last_elements_in_common(Consecutive_Ord,L_Ord,AND),
		ord_subtract(Consecutive_Ord,AND,FORK),
		ord_subtract(L_Ord,AND,JOIN),
		SMALL = []
	    ),
	    publish(FORK,AND,JOIN,SMALL,DetInfo,GDict,GPub,NGPub,Dict,Dict1,
	            LJ,Exp,Exp1),
	    sort(LJ,LJ_Ord),
	    sort(VsGroup,VsGroup_Ord),
	    sort(EsGroup,EsGroup_Ord),
	    remove_edges(EsGroup_Ord,LJ_Ord,Es1),
	    ord_subtract(VsGroup_Ord,LJ_Ord,Vs1),
	    compute_expr(Vs1,Es1,DetInfo,Dict1,NGPub,GDict,Exp1,NExp,NDict)
        ;
	    remove_edges(Es_Ord,Vs_after,Es_before1),
	    remove_edges(Es_before1,Cut,Es_before),
	    compute_expr(Vs_before,Es_before,DetInfo,Dict,GPub,GDict,Exp,
                         Exp1,Dict1),
	    publish([],[],[],Cut,DetInfo,GDict,GPub,_,Dict1,Dict2,
	            LJ,Exp1,Exp2),
	    remove_edges(Es_Ord,Vs_before,Es_after1),
	    remove_edges(Es_after1,Cut,Es_after),
	    compute_expr(Vs_after,Es_after,DetInfo,Dict2,GPub,GDict,Exp2,
                         NExp,NDict)
        ).

% Returns a list with the last common elements of the two input lists
last_elements_in_common([],_,[]).
last_elements_in_common(_,[],[]).
last_elements_in_common(L1,L2,L3) :-
	last(L1,Last1),
	last(L2,Last2),
	(
	    Last1 == Last2 ->
	    delete(L1,Last1,L11),
	    delete(L2,Last2,L22),
	    last_elements_in_common(L11,L22,Rest),
	    append(Rest,[Last1],L3)
	;
	    L3 = []
	).

% Returns the list of the first consecutive goals of the input list
consecutive_goals([],[]).
consecutive_goals([V|Vs],[V|Cs]) :-
	(
	    current_fact(grouping(V,Gr)) ->
	    sort(Gr,Gr_Ord),
	    last(Gr_Ord,U),
	    U = vertex(N3,_),	    
	    consecutive_goals_(Vs,N3,Cs)
	;
	    V = vertex(N,_),
	    consecutive_goals_(Vs,N,Cs)
	).
consecutive_goals_([],_,[]).
consecutive_goals_([V|Vs],N,Cs) :-
	V = vertex(N1,_),
	N2 is N + 1,
	(
	    N2 == N1 ->
	    Cs = [V|RestCs],
	    (
		current_fact(grouping(V,Gr)) ->
		sort(Gr,Gr_Ord),
		last(Gr_Ord,U),
		U = vertex(N3,_),	    
		consecutive_goals_(Vs,N3,RestCs)
	    ;
		consecutive_goals_(Vs,N2,RestCs)
	    )
	;
	    Cs = []
	).

% Returns the list of the goals whose index is less than the index in V
less_than(L1,V,L2) :-
	V = vertex(N,_),
	!,
	less_than_(L1,N,L2).
less_than_([],_,[]).
less_than_([V|L1],N,L2) :-
	V = vertex(N1,_),
	(
	    N1 < N ->
	    L2 = [V|Rest],
	    less_than_(L1,N,Rest)
	;
	    L2 = []
	).

% Asserts the nodes that can be grouped with each of the independent nodes
% in the graph
assert_group_nodes([],_,_).
assert_group_nodes([V|RestV],Vs,Es) :-
	assert_group_nodes_([V],Vs,Es,List),
	assertz_fact(grouping(V,List)),
	assert_group_nodes(RestV,Vs,Es).
assert_group_nodes_(Group,Vs,Es,List) :-
	sort(Group,Group_Ord),
	get_directly_dependent_to_group(Group_Ord,Group_Ord,Vs,Es,LDD),
	(
	    LDD == [] ->
	    List = Group_Ord
	;
	    ord_union(Group_Ord,LDD,Group_Aux),
	    sort(Group_Aux,Group_Aux_Ord),
	    ord_subtract(Vs,Group_Aux_Ord,RestVsNoGroup),
	    (
		(all_connected(Group_Aux_Ord,Es),
		 all_consecutive(Group_Aux_Ord),
		 \+edges_from_outside(Group_Aux_Ord,RestVsNoGroup,Es)) ->
		 assert_group_nodes_(Group_Aux_Ord,Vs,Es,FinalGroup),
		 List = FinalGroup
	    ;
		List = Group_Ord
	    )
	).

% Succeeds if all the goals are consecutive
all_consecutive([]).
all_consecutive([V|Vs]) :-
	(
	    current_fact(grouping(V,Gr)) ->
	    sort(Gr,Gr_Ord),
	    last(Gr_Ord,U),
	    U = vertex(N3,_),	    
	    all_consecutive_(Vs,N3)
	;
	    V = vertex(N,_),
	    all_consecutive_(Vs,N)
	).
all_consecutive_([],_).
all_consecutive_([V|Vs],N) :-
	V = vertex(N1,_),
	N2 is N + 1,
	N2 == N1,
	!,
	(
	    current_fact(grouping(V,Gr)) ->
	    sort(Gr,Gr_Ord),
	    last(Gr_Ord,U),
	    U = vertex(N3,_),	    
	    all_consecutive_(Vs,N3)
	;
	    all_consecutive_(Vs,N2)
	).

% Removes from the graph those nodes that have been grouped
simplify_graph(VsBefore,EsBefore,VsAfter,EsAfter) :-
	simplify_graph_(VsBefore,VsBefore,VsAfter,EsBefore,EsAfter).
simplify_graph_([],V,V,E,E).
simplify_graph_([V|Vs],Vers,VersAfter,Edges,EdgesAfter) :-
	current_fact(grouping(V,Gr)),
	Gr = [V|Rest],
	ord_subtract(Vers,Rest,Vers1),
	ord_subtract(Vers,Gr,VsnoGr),
	update_edges(Gr,VsnoGr,Edges,Edges1),
	remove_edges(Edges1,Rest,Edges11),
	simplify_graph_(Vs,Vers1,VersAfter,Edges11,EdgesAfter).
simplify_graph_([_|Vs],Vers,VersAfter,Edges,EdgesAfter) :-
	simplify_graph_(Vs,Vers,VersAfter,Edges,EdgesAfter).

% Returns the goals that should not be parallelized (Gr < 1)
obtain_small_goals([],[]).
obtain_small_goals([V|Vs],L) :-
	current_fact(grouping(V,Gr)),
	(
	    all_small(Gr) -> L = [V|L1]
	;
	    L = L1
	),
	obtain_small_goals(Vs,L1).

% Publishes and reads bindings of goals
publish(FORK,AND,JOIN,SMALL,DetInfo,GDict,GPub,NGPub,Dict,NDict,LJ,Exp,NExp) :-
	add_forks(FORK,DetInfo,GDict,GPub,GPub1,Dict,NDict,Exp,Exp1),
	and_parallelism(AND,DetInfo,GDict,GPub1,NGPub1,[],LJ1,Exp1,Exp2),
	add_joins(JOIN,DetInfo,GDict,NDict,LJ1,LJ2,Exp2,Exp3),
	add_small_goals(SMALL,GDict,NGPub1,NGPub,LJ2,LJ,Exp3,NExp).

% Adds: {&>,&!>}/2
add_forks([],_,_,GPub,GPub,Dic,Dic,Exp,Exp).
add_forks([V|Verts],DetInfo,GDict,GPub,NGPub,dic(Vs,Ns),NDic,Exp,NewExp) :-
	current_fact(grouping(V,Gr)),
	vertex_to_seq(Gr,GDict,ExpSeq),
	extract_det_info_gr(Gr,DetInfo,GDict,Det),
	V = vertex(N1,_),
	number_codes(N1,N2),
	atom_codes(N,N2),
	atom_concat('H',N,H),
	(
	    var(Exp) ->
	    ( (Det == true) -> Exp1 = (ExpSeq '&!>' X) ;
		               Exp1 = (ExpSeq &> X) )
	;
	    ( (Det == true) -> Exp1 = (Exp, (ExpSeq '&!>' X)) ;
                               Exp1 = (Exp, (ExpSeq &> X)) )
	),
	insert_last(Vs,X,Vs1),
        insert_last(Ns,H,Ns1),
	add_forks(Verts,DetInfo,GDict,[V|GPub],NGPub,dic(Vs1,Ns1),NDic,
                      Exp1,NewExp).

% Adds: a & b
and_parallelism([],_,_,GPub,GPub,LJ,LJ,Exp,Exp).
and_parallelism(Vs,DetInfo,GoalDict,GPub,NGPub,LJ,NewLJ,Exp,Exp2) :-
	(
	    var(Exp) ->	Exp2 = Exp1
	;
	    Exp2 = (Exp, Exp1)
	),
	(
	    (all_deterministic_goals(Vs,GoalDict,DetInfo)) ->   
	     Op = '&!',
	     Vs1 = Vs
	;
	     Op = '&',
	     sort_goals_from_det_to_non(Vs,GoalDict,DetInfo,Vs1)
	),
	and_parallelism_(Vs1,Op,GoalDict,GPub,NGPub,LJ,NewLJ,Exp1).
and_parallelism_([],_,_,GPub,GPub,LJ,LJ,true).
and_parallelism_([V|[]],_,GDict,GPub,[V|GPub],LJ,[V|LJ],ExpSeq) :-
	current_fact(grouping(V,Gr)),
	vertex_to_seq(Gr,GDict,ExpSeq).
and_parallelism_([V|Verts],'&',GDict,GPub,NGPub,LJ,NewLJ,(ExpSeq & Gs)) :-
	current_fact(grouping(V,Gr)),
	vertex_to_seq(Gr,GDict,ExpSeq),
	and_parallelism_(Verts,'&',GDict,[V|GPub],NGPub,[V|LJ],NewLJ,Gs).
and_parallelism_([V|Verts],'&!',GDict,GPub,NGPub,LJ,NewLJ,(ExpSeq '&!' Gs)) :-
	current_fact(grouping(V,Gr)),
	vertex_to_seq(Gr,GDict,ExpSeq),
	and_parallelism_(Verts,'&!',GDict,[V|GPub],NGPub,[V|LJ],NewLJ,Gs).

% Adds: {<&,<&!}/1
add_joins([],_,_,_,L,L,Exp,Exp).
add_joins([V|Vertices],DetInfo,GDict,dic(Vs,Ns),L,NL,Exp,NewExp) :-
	retract_fact(grouping(V,Gr)),
	extract_det_info_gr(Gr,DetInfo,GDict,Det),
	V = vertex(N1,_),
	number_codes(N1,N2),
	atom_codes(N,N2),
	atom_concat('H',N,H),
	obtain_handler(Vs,Ns,H,X),
	(
	    var(Exp) ->
	    (
		(Det == true) ->
		Exp1 = (X '<&!')
	    ;
		Exp1 = (X <&)
	    )
	;
	    (
		(Det == true) ->
		Exp1 = (Exp, (X '<&!'))
	    ;
		Exp1 = (Exp, (X <&))
	    )
	),
	add_joins(Vertices,DetInfo,GDict,dic(Vs,Ns),[V|L],NL,Exp1,NewExp).

% Are all the goals (and their group of nodes) in the list deterministic?
all_deterministic_goals([],_,_).
all_deterministic_goals([V|Vs],GoalDict,DetInfo) :-
	current_fact(grouping(V,Gr)),
	extract_det_info_gr(Gr,DetInfo,GoalDict,Det),
	!,
	Det == true,
	all_deterministic_goals(Vs,GoalDict,DetInfo).

% Orders goals in the list from deterministic to nondeterministic
sort_goals_from_det_to_non(Vs,GoalDict,DetInfo,Vs1) :-
	sort_goals_from_det_to_non_(Vs,GoalDict,DetInfo,[],[],Non,Det),
	append(Det,Non,Vs1).
sort_goals_from_det_to_non_([],_,_,Non,Det,NonF,DetF):-
	reverse(Non,NonF),
	reverse(Det,DetF).
sort_goals_from_det_to_non_([V|Vs],GoalDict,DetInfo,Non,Det,NonF,DetF) :-
	current_fact(grouping(V,Gr)),
	extract_det_info_gr(Gr,DetInfo,GoalDict,D),
	(
	    D == true ->
	    sort_goals_from_det_to_non_(Vs,GoalDict,DetInfo,Non,[V|Det],
                                        NonF,DetF)
	;
	    sort_goals_from_det_to_non_(Vs,GoalDict,DetInfo,[V|Non],Det,
                                        NonF,DetF)
	).

