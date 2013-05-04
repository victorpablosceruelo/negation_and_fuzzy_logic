%========================================================================
% Developer: Amadeo Casas
% Started:   May 2007
%------------------------------------------------------------------------
% UUDG annotator
%
%   Version that does not preserve the order of the solutions
% 
% This version exposes all possible parallelism from a dependency graph.
% It includes determinism analysis.
%========================================================================

:- module(uudg,
	[
	    exp_uudg/5,
	    remove_edges/3,
	    split_vertices/4,
	    determinism_of_current_module/1,
	    obtain_dependences_per_node/3,
	    obtain_small_goals/2,
	    shrink_dep_list/3,
	    is_there_a_cut/4,
	    add_small_goals/8,
	    extract_det_info_gr/4,
	    obtain_handler/4,
	    update_edges/4,
	    remove_transitive_dep/2,
	    vertex_to_seq/3,
	    get_directly_dependent_to_group/5,
	    all_connected/2,
	    edges_from_outside/3,
	    all_small/1
	],
	[andprolog, assertions]
	).

:- doc(title, "UUDG annotator with high-level operators.").
:- doc(subtitle, "Version that does not preserve the order of the solutions.").

:- doc(author, "Amadeo Casas"||
                   " (@href{http://www.ece.unm.edu/~amadeo},"||
                   " University of New Mexico)").

:- use_module(annotate,            [vertex_goal/3]).
:- use_module(library(aggregates), [findall/3]).
:- use_module(library(sort),       [sort/2]).
:- use_module(library(lists),      [insert_last/3, append/3, reverse/2]).
:- use_module(library(sets),       [ord_subtract/3, ord_union/3]).
:- use_module(spec(s_simpspec),    [make_atom/2]).
:- use_module(domain(nfplai),      [nf_compute_lub/2]).
:- use_module(plai(plai_db),       [complete/7]).

:- data grouping/2.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% READING DETERMINACY INFORMATION
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

determinism_of_current_module(DetList) :-
        findall((SgKey,Prime),complete(SgKey,det,_,_,Prime,_,_),LPrime),
	get_det_list(LPrime,DetList).

get_det_list([],[]).
get_det_list([(SgKey,Prime)|Rest],[(SgKey,Det)|T]) :-
	(
	    ( nf_compute_lub(Prime,LubPrime),
	      LubPrime = nf(_,_,nf(_,FCovered,FFails)) ) ->
	      (
		  (FCovered == covered, FFails == not_fails) ->
		   Det = true
	      ;
		  Det = false
	      )
	;
	    Det = false
	),
	get_det_list(Rest,T).

remove_transitive_dep(Edges,Edges2) :-
	member(edge(X,Y,_),Edges),
	member(edge(Y,Z,_),Edges),
	member(edge(X,Z,D),Edges),
	ord_subtract(Edges,[edge(X,Z,D)],Edges1),
	remove_transitive_dep(Edges1,Edges2).
remove_transitive_dep(Edges,Edges).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ANNOTATION
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

exp_uudg(graph(Vertices,Edges),Dict,GoalDict,Exp,NewDict) :-
	determinism_of_current_module(DetInfo),
	remove_transitive_dep(Edges,Edges2),
	compute_expr(Vertices,Edges2,DetInfo,Dict,[],GoalDict,_,Exp,NewDict),
	retractall_fact(grouping(_,_)).

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
	    sort(IndVs,IndVs_Ord),
	    obtain_small_goals(IndVs,SmallVs),
	    sort(SmallVs,SMALL),
	    (
		SmallVs = [] ->
		obtain_dependences_per_node(DepVs,EsGroup,DepList),
		(
		    (DepVs == []) ->
		     L = IndVs_Ord
		;
		    shrink_dep_list(DepList,IndVs,DepList1),
		    choose_less_dependent(DepList1,(_,_,L))
		),
		sort(L,L_Ord),
		ord_subtract(IndVs_Ord,L_Ord,FORK1_Ord),
		ord_subtract(FORK1_Ord,GPub_Ord,FORK),
		ord_subtract(IndVs_Ord,FORK,AND1_Ord),
		ord_subtract(AND1_Ord,GPub_Ord,AND),
		ord_subtract(L_Ord,AND,JOIN)
	    ;
		ord_subtract(IndVs_Ord,SMALL,FORK1_Ord),
		ord_subtract(FORK1_Ord,GPub_Ord,FORK),
		ord_subtract(IndVs_Ord,FORK,AND1_Ord),
		ord_subtract(AND1_Ord,SMALL,AND2_Ord),
		ord_subtract(AND2_Ord,GPub_Ord,AND),
		JOIN = [],
		L_Ord = SMALL
	    ),
	    publish(FORK,AND,JOIN,SMALL,DetInfo,GDict,GPub,NGPub,Dict,Dict1,
	            LJ,Exp,Exp1),
	    sort(EsGroup,EsGroup_Ord),
	    remove_edges(EsGroup_Ord,L_Ord,Es1),
	    sort(VsGroup,VsGroup_Ord),
	    sort(LJ,LJ_Ord),
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
/*
% Adds edges from nodes with granularity < 1 to the rest of the nodes
augment_graph(Vertices,Edges,NEdges) :-
	augment_graph_(Vertices,Vertices,Edges,NEdges).
augment_graph_([],_,E,E).
augment_graph_([V|Vs],VV,Edges,NEE) :-
	V = vertex(_,info(X,_)),
	!,
	(
	    X >= 1 ->
	    augment_graph_(Vs,VV,Edges,NEE)
	;
	    augment_graph__(VV,V,NEs),
	    sort(NEs,NEs_Ord),
	    sort(Edges,Edges_Ord),
	    ord_union(NEs_Ord,Edges_Ord,Edges1_Ord),
	    augment_graph_(Vs,VV,Edges1_Ord,NEE)
	).
augment_graph__([],_,[]).
augment_graph__([V|Vs],Vertex,Edges) :-
	(
	    Vertex == V -> augment_graph__(Vs,Vertex,Edges)
	;
	    (
		Vertex = vertex(N1,_),
		V = vertex(N2,_),
		N2 < N1 ->
		Edges = [edge(V,Vertex,false)|RestEdges]
	    ;
		Edges = RestEdges
	    ),
	    augment_graph__(Vs,Vertex,RestEdges)
	).
*/
% Is there a (!) as a node in the graph?
is_there_a_cut([],[],[],[]).
is_there_a_cut([V|Vs],VsBefore,Cut,VsAfter) :-
	(
	    V = vertex(_,info(-1,_)) ->
	    VsBefore = [],
	    Cut = [V],
	    VsAfter = Vs
	;
	    VsBefore = [V|NewVs],
	    is_there_a_cut(Vs,NewVs,Cut,VsAfter)
	).

% Splits the nodes of the graph in a dependent and an independent list
split_vertices(Vs,Es,DepVs_Ord,IndVs_Ord) :-
	obtain_dependent_vertices(Es,[],DepVs),
	sort(Vs,Vs_Ord),
	sort(DepVs,DepVs_Ord),
	ord_subtract(Vs_Ord,DepVs_Ord,IndVs_Ord).

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
		 \+edges_from_outside(Group_Aux_Ord,RestVsNoGroup,Es)) ->
		 assert_group_nodes_(Group_Aux_Ord,Vs,Es,FinalGroup),
		 List = FinalGroup
	    ;
		List = Group_Ord
	    )
	).

% Returns the nodes directly dependent of those in the current group
get_directly_dependent_to_group([],_,_,_,[]).
get_directly_dependent_to_group([Vertex|RestGroup],InitialGroup,Vs,Es,Dep) :-
	ord_subtract(Vs,InitialGroup,VsRest),
	get_directly_dependent_to_group_(VsRest,Vertex,Es,LDD),
	ord_union(LDD,Dep1,Dep),
	get_directly_dependent_to_group(RestGroup,InitialGroup,Vs,Es,Dep1).
get_directly_dependent_to_group_([],_,_,[]).
get_directly_dependent_to_group_([V|Vs],Vertex,Es,L) :-
	(
	    member(edge(Vertex,V,_),Es) ->
	    L = [V|L1]
	;
	    L = L1
	),
	get_directly_dependent_to_group_(Vs,Vertex,Es,L1).

% Succeeds if all the nodes in [V|Rest] are connected
all_connected([],_) :- !.
all_connected([_],_) :- !.
all_connected([V|Rest],Es) :-
	there_is_a_path_to_all(Rest,V,Es),
	all_connected(Rest,Es).

% Succeeds if there is a path between nodes V2 and all of nodes in [V1|Rest]
there_is_a_path_to_all([],_,_).
there_is_a_path_to_all([V1|Rest],V2,Es) :-
	there_is_a_path(V2,V1,Es),
	there_is_a_path_to_all(Rest,V2,Es).

% Succeeds if there is a path between nodes U and V
there_is_a_path(U,V,Edges) :-
	member(edge(U,V,_),Edges),
	!.
there_is_a_path(U,V,Edges) :-
	member(edge(U,X,_),Edges),
	there_is_a_path(X,V,Edges).

% Succeeds if there is an edge coming in from outside the Group
edges_from_outside(Group_Aux,RestVsNoGroup,Es) :-
	member(U,Group_Aux),
	member(V,RestVsNoGroup),
	member(edge(V,U,_),Es).

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

% Update de edges of the graph with respect to the grouping done
update_edges(Gr,VsnoGr,Edges,NewEdges) :-
	Gr = [V|Rest],
	member(X,Rest),
	member(Y,VsnoGr),
	member(edge(X,Y,D),Edges),
	ord_subtract(Edges,[edge(X,Y,D)],Edges1),
	Edges11 = [edge(V,Y,D)|Edges1],
	sort(Edges11,Edges11_Ord),
	update_edges(Gr,VsnoGr,Edges11_Ord,NewEdges).
update_edges(_,_,Edges,Edges).

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

% Succeeds if all the goals in the group are small
all_small([]) :- !.
all_small([V|Gr]) :-
	V = vertex(_,info(X,_)),
	X < 1,
	!,
	all_small(Gr).

% Returns in the third argument a list with the nodes of the graph that
% have dependences with respect to some other nodes
obtain_dependent_vertices([],Vertices,Vertices).
obtain_dependent_vertices([edge(_,V2,_)|Edges],Vertices,List) :-
	(
	    member(V2,Vertices) ->
	    obtain_dependent_vertices(Edges,Vertices,List)
	;
	    obtain_dependent_vertices(Edges,[V2|Vertices],List)
	).

% Returns a list of elements (V,N,L), where V is a dependent vertex, N is
% the number of dependences that it has and L is the list with all the
% vertices that it depends on
obtain_dependences_per_node([],_,[]).
obtain_dependences_per_node([V|DepVs],Edges,[(V,N,L)|ListDep]) :-
	obtain_dependences_per_node_(Edges,V,0,N,L),
	obtain_dependences_per_node(DepVs,Edges,ListDep).
obtain_dependences_per_node_([],_,N,N,[]).
obtain_dependences_per_node_([edge(V1,V,_)|Edges],V,N,NP,[V1|L]) :-
	!,
	N1 is N + 1,
	obtain_dependences_per_node_(Edges,V,N1,NP,L).
obtain_dependences_per_node_([_|Edges],V,N,NP,L) :-
	obtain_dependences_per_node_(Edges,V,N,NP,L).

% Shrinks the initial list to a list that only has those dependent vertices
% which only depend on independent vertices
shrink_dep_list([],_,[]).
shrink_dep_list([(V,N,L)|DepList],IndVs,Res) :-
	sort(L,L_Ord),
	sort(IndVs,IndVs_Ord),
	ord_subtract(L_Ord,IndVs_Ord,L1),
	(
	    (L1 == []) ->
	     Res = [(V,N,L)|List]
	;
	    Res = List
	),
	shrink_dep_list(DepList,IndVs,List).

% Returns the vertex with less number of dependences in the list
choose_less_dependent([V|ListDep],Vertex) :-
	choose_less_dependent_([V|ListDep],V,Vertex).
choose_less_dependent_([],V,V).
choose_less_dependent_([(V1,N1,L1)|ListDep],(V2,N2,L2),Vertex) :-
	(
	    (N1 < N2) ->
	     choose_less_dependent_(ListDep,(V1,N1,L1),Vertex)
	;
	     choose_less_dependent_(ListDep,(V2,N2,L2),Vertex)
	).

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

% Transforms a group of vertices into a sequentialization of them
vertex_to_seq([],_,_).
vertex_to_seq([V|Vs],GDict,Exp) :-
	(vertex_goal(V,GDict,G:_) ; vertex_goal(V,GDict,G)),
	 vertex_to_seq(Vs,GDict,Exp1),
	(
	    var(Exp1) ->
	    Exp = G
	;
	    Exp = (G, Exp1)
	).

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

% Adds small goals to the final expression
add_small_goals([],_,GPub,GPub,LJ,LJ,Exp,Exp).
add_small_goals(Vs,GoalDict,GPub,NGPub,LJ,NewLJ,Exp,Exp2) :-
	(
	    var(Exp) ->	Exp2 = Exp1
	;
	    Exp2 = (Exp, Exp1)
	),
	add_small_goals_(Vs,GoalDict,GPub,NGPub,LJ,NewLJ,Exp1).
add_small_goals_([],_,GPub,GPub,LJ,LJ,true).
add_small_goals_([V|[]],GoalDict,GPub,[V|GPub],LJ,[V|LJ],G) :-
	vertex_goal(V,GoalDict,G).
add_small_goals_([V|Verts],GoalDict,GPub,NGPub,LJ,NewLJ,(G,Gs)) :-
	vertex_goal(V,GoalDict,G),
	add_small_goals_(Verts,GoalDict,[V|GPub],NGPub,[V|LJ],NewLJ,Gs).

% Obtains the handler associated to the goal
obtain_handler([V|_],[N|_],N,V) :- !.
obtain_handler([_|Vs],[_|Ns],H,X) :-
	obtain_handler(Vs,Ns,H,X).

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

% Removes from the graph the outgoing edges of V
remove_edges([],_,[]) :- !.
remove_edges([edge(V1,V2,LD)|Edges],L,NewEdges) :-
        (
            ( member(V1,L) ; member(V2,L) ) ->
            remove_edges(Edges,L,NewEdges)
        ;
            remove_edges(Edges,L,Edges1),
            NewEdges = [edge(V1,V2,LD)|Edges1]
        ).

% Extracts the deterministic information of a group of goals
extract_det_info_gr([],_,_,_).
extract_det_info_gr([V|Gr],DetInfo,GDict,Det) :-
	(vertex_goal(V,GDict,G:_) ; vertex_goal(V,GDict,G)),
	functor(G,F,A),
	make_atom([F,A],G1),
	extract_det_info(DetInfo,G1,Det),
	Det == true,
	extract_det_info_gr(Gr,DetInfo,GDict,Det).
extract_det_info_gr(_,_,_,false).

% Extracts the deterministic information of a goal
extract_det_info([],_,false).
extract_det_info([(Goal,Det)|_],Goal,Det).
extract_det_info([_|T],Goal,Det) :-
	extract_det_info(T,Goal,Det).

