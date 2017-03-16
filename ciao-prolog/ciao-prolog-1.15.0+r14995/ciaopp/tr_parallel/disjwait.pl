%========================================================================
% Developer: Amadeo Casas
% Started:   May 2007
%------------------------------------------------------------------------
% DISJWAIT annotator
%
%   Version that does not preserve the order of the solutions and does
%   conditional wait
% 
% This version exposes all possible parallelism from a dependency graph.
% It includes determinism analysis.
%========================================================================

:- module(disjwait,
	[exp_disjwait/5],
	[andprolog, assertions]
	).

:- doc(title, "UUDG annotator with high-level operators (disjwait).").
:- doc(subtitle, "Version that does not preserve the order of the
                  solutions and uses the operators '<?' and '&?'.").

:- doc(author, "Amadeo Casas"||
                   " (@href{http://www.ece.unm.edu/~amadeo},"||
                   " University of New Mexico)").

:- use_module(annotate, [vertex_goal/3]).
:- use_module(library(sort), [sort/2]).
:- use_module(library(lists), [insert_last/3, append/3, reverse/2, 
                                    contains_ro/2]).
:- use_module(library(sets), [ord_subtract/3, ord_intersection/3, 
                                    ord_union/3]).


:- use_module(uudg, [remove_edges/3, 
                           split_vertices/4,
			   obtain_dependences_per_node/3,
			   shrink_dep_list/3,
			   is_there_a_cut/4,
			   determinism_of_current_module/1,
			   all_small/1,
			   remove_transitive_dep/2,
			   get_directly_dependent_to_group/5,
			   all_connected/2,
			   edges_from_outside/3,
			   update_edges/4,
			   obtain_handler/4,
			   vertex_to_seq/3,
			   extract_det_info_gr/4]).

:- data grouping/2.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ANNOTATION
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

exp_disjwait(graph(Vertices,Edges),Dic,GoalDic,Exp,NewDic) :-
	determinism_of_current_module(DetInfo),
	remove_transitive_dep(Edges,Edges2),
	compute_expr(Vertices,Edges2,DetInfo,Dic,[],GoalDic,_,Exp,NewDic),
	retractall_fact(grouping(_,_)).


% COMPUTES THE PARALLEL EXPRESSION
compute_expr([],[],_,Dic,_,_,Exp,Exp,Dic).
compute_expr(Vs,Es,DetInfo,Dic,GPub,GDic,Exp,NExp,NDic) :-
	is_there_a_cut(Vs,Vs_before,Cut,Vs_after),
	sort(Es,Es_Ord),
	(
	    Vs == Vs_before->

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
	    obtain_dependences_per_node(DepVs,EsGroup,DepL),
	    sort(IndVs,IndVs_Ord),
	    sort(SmallVs,SMALL),
	    sort(GPub,GPub_Ord),
	    (
		SmallVs = [] ->
		(
		    (DepVs == []) ->
		     SimpC1 = [],
		     L = IndVs_Ord
		;
		    shrink_dep_list(DepL,IndVs,DepL1),
		    obtain_simplified_conditions(DepL1,SimpC1,L)
		),
		sort(L,L_Ord),
		ord_subtract(IndVs_Ord,L_Ord,FORK1_Ord),
		ord_subtract(FORK1_Ord,GPub_Ord,FORK),
		ord_subtract(IndVs_Ord,FORK,AND1_Ord),
		ord_subtract(AND1_Ord,GPub_Ord,AND),
		ord_subtract(L_Ord,AND,JOIN1_Ord),
		(
		    SimpC1 == [X] ->
		    SimpC = [],
		    ord_union(JOIN1_Ord,X,JOIN)
		;
		    SimpC = SimpC1,
		    JOIN = JOIN1_Ord
		)
	    ;
		SimpC = [],
		L_Ord = SMALL,
		ord_subtract(IndVs_Ord,SMALL,FORK1_Ord),
		ord_subtract(FORK1_Ord,GPub_Ord,FORK),
		ord_subtract(IndVs_Ord,FORK,AND1_Ord),
		ord_subtract(AND1_Ord,GPub_Ord,AND2_Ord),
		ord_subtract(AND2_Ord,SMALL,AND),
		JOIN = []
	    ),
	    publish(FORK,AND,JOIN,SMALL,DetInfo,GDic,GPub,NGPub,Dic,Dic1,
                    LJ,Exp,Exp1),
	    sort(EsGroup,EsGroup_Ord),
	    remove_edges(EsGroup_Ord,L_Ord,Es1),
	    sort(VsGroup,VsGroup_Ord),
	    sort(LJ,LJ_Ord),
	    ord_subtract(VsGroup_Ord,LJ_Ord,Vs1),
	    (
		(SimpC == [] ; contains_ro(SimpC,[])) ->
		 compute_expr(Vs1,Es1,DetInfo,Dic1,NGPub,GDic,Exp1,NExp,NDic)
	    ;
		compute_IF(SimpC,Vs1,Es1,DetInfo,NGPub,GDic,Dic1,NDic,Exp1,NExp)
	    )
        ;
	    remove_edges(Es_Ord,Vs_after,Es_before1),
	    remove_edges(Es_before1,Cut,Es_before),
	    compute_expr(Vs_before,Es_before,DetInfo,Dic,GPub,GDic,Exp,
                         Exp1,Dic1),
	    publish([],[],[],Cut,DetInfo,GDic,GPub,_,Dic1,Dic2,
	            LJ,Exp1,Exp2),
	    remove_edges(Es_Ord,Vs_before,Es_after1),
	    remove_edges(Es_after1,Cut,Es_after),
	    compute_expr(Vs_after,Es_after,DetInfo,Dic2,GPub,GDic,Exp2,
                         NExp,NDic)
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
		 \+edges_from_outside(Group_Aux_Ord,RestVsNoGroup,Es)) ->
		 assert_group_nodes_(Group_Aux_Ord,Vs,Es,FinalGroup),
		 List = FinalGroup
	    ;
		List = Group_Ord
	    )
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

% Simplifies the conditions in the list
obtain_simplified_conditions(DepL,List,Common) :-
	obtain_list_conditions(DepL,List1),
	simplify_conditions(List1,List,Common).

% Transforms DepL into a list of dependency lists
obtain_list_conditions([],[]).
obtain_list_conditions([(_,_,L)|Vs],List) :-
	obtain_list_conditions(Vs,List2),
	append([L],List2,List).

% Simplifies the list of dependency lists and returns the simplification
simplify_conditions(Res,NRes,Int) :-
	intersection_of_all_conditions(Res,Int),
	simplify_conditions_(Res,Int,NRes).
simplify_conditions_([],_,[]).
simplify_conditions_([L|Ls],Int,Res) :-
	sort(L,L_Ord),
	ord_subtract(L_Ord,Int,L1_Ord),
	append([L1_Ord],NRes,Res),
	simplify_conditions_(Ls,Int,NRes).

% Resturns the intersection of all the lists
intersection_of_all_conditions([],[]).
intersection_of_all_conditions([L],L).
intersection_of_all_conditions([L|Ls],Int) :-
	sort(L,L_Ord),
	intersection_of_all_conditions_(Ls,L_Ord,Int).
intersection_of_all_conditions_([],Int,Int).
intersection_of_all_conditions_([L|Ls],Int,NInt) :-
	sort(L,L_Ord),
	ord_intersection(L_Ord,Int,Int1),
	intersection_of_all_conditions_(Ls,Int1,NInt).

% Computes the IF structure for the annotated expression
compute_IF(SimpCond,Vs,Es,DetInfo,GPub,GDic,Dic,NDic,Exp,(Exp1, Exp11)) :-
	create_selects(SimpCond,Dic,Exp,Exp1),
	create_if_structure(SimpCond,Vs,Es,DetInfo,GPub,GDic,Dic,NDic,Exp11).

% Makes the conditional wait (select)
create_selects(SimpCond,Dic,Exp,NExp) :-
	create_selects_ors(SimpCond,Disj),
	dnf_to_cnf(Disj,Conj),
	compute_select_expressions(Conj,Dic,[],_,Exp,NExp).
create_selects_ors([L],L2) :-
	create_selects_ands(L,L2).
create_selects_ors([L|T],or(L2,Rest)) :-
	create_selects_ands(L,L2),
	create_selects_ors(T,Rest).
create_selects_ands([V],V).
create_selects_ands([V|Ves],and(V,Rest)) :-
	create_selects_ands(Ves,Rest).

% Computes the select expressions for the CNF given
compute_select_expressions(and(A,B),Dic,L1,L2,Exp,NExp) :-
	compute_select_expressions(A,Dic,L1,L11,_,Exp1),
	compute_select_expressions(B,Dic,L11,L2,_,Exp2),
	( var(Exp) -> NExp = (Exp1, Exp2) ; NExp = (Exp, (Exp1, Exp2)) ).
compute_select_expressions(or(A,B),Dic,L1,L2,Exp,NExp) :-
	(
	    contains_ro(L1,(A,B)) -> L2 = L1, NExp = Exp
	;
	    compute_select_expressions_(A,[],LR1),
	    compute_select_expressions_(B,LR1,LR2),
	    compute_select_expressions__(LR2,Dic,Exp1),
	    L2 = [(A,B)|L1],
	    (
		var(Exp) ->
		NExp = (Exp1 <?)
	    ;
		NExp = (Exp, (Exp1 <?))
	    )
	).
compute_select_expressions_(or(A,B),L,LRes) :-
	compute_select_expressions_(A,L,L1),
	compute_select_expressions_(B,L1,LRes).
compute_select_expressions_(A,L,LRes) :-
	(
	    contains_ro(L,A) -> LRes = L
	;
	    append(L,[A],LRes)
	).
compute_select_expressions__([V],Dic,X) :-
	Dic = dic(Vs,Ns),
	!,
	V = vertex(N1,_),
	number_codes(N1,N2),
	atom_codes(N,N2),
	atom_concat('H',N,H),
	obtain_handler(Vs,Ns,H,X).
compute_select_expressions__([V|Ves],Dic,NExp) :-
	Dic = dic(Vs,Ns),
	!,
	V = vertex(N1,_),
	number_codes(N1,N2),
	atom_codes(N,N2),
	atom_concat('H',N,H),
	obtain_handler(Vs,Ns,H,X),
	compute_select_expressions__(Ves,Dic,Exp1),
	NExp = ';'(X,Exp1).

% Adds the condition for the new if-then-else structure
create_if_structure([L],Vs,Es,DetInfo,GPub,GDic,Dic,NDic,NExp) :-
	publish([],[],L,[],DetInfo,GDic,GPub,GPub,Dic,Dic,LV,_,Exp),
	sort(Vs,Vs_Ord),
	sort(Es,Es_Ord),
	sort(LV,LV_Ord),
	remove_edges(Es_Ord,LV_Ord,Es1),
	ord_subtract(Vs_Ord,LV_Ord,Vs1),
	compute_expr(Vs1,Es1,DetInfo,Dic,GPub,GDic,Exp,NExp,NDic).
create_if_structure([L|Ls],Vs,Es,DetInfo,GPub,GDic,Dic,NDic,NExp) :-
	create_if_structure_(L,Dic,Cond),
	NExp = ( Cond -> Rest ; Else ),

	publish([],[],L,[],DetInfo,GDic,GPub,GPub,Dic,Dic,LV,_,Rest1),
	sort(Es,Es_Ord),
	sort(Vs,Vs_Ord),
	sort(LV,LV_Ord),
	remove_edges(Es_Ord,LV_Ord,Es1),
	ord_subtract(Vs_Ord,LV_Ord,Vs1),
	compute_expr(Vs1,Es1,DetInfo,Dic,GPub,GDic,Rest1,Rest,NDic1),

	create_if_structure(Ls,Vs,Es,DetInfo,GPub,GDic,NDic1,NDic,Else).

create_if_structure_([V],Dic,(X &?)) :-
	Dic = dic(Vs,Ns),
	!,
	V = vertex(N1,_),
	number_codes(N1,N2),
	atom_codes(N,N2),
	atom_concat('H',N,H),
	obtain_handler(Vs,Ns,H,X).
create_if_structure_([V|Ves],Dic,NExp) :-
	Dic = dic(Vs,Ns),
	!,
	V = vertex(N1,_),
	number_codes(N1,N2),
	atom_codes(N,N2),
	atom_concat('H',N,H),
	obtain_handler(Vs,Ns,H,X),
	create_if_structure_(Ves,Dic,Exp1),
	NExp = ((X &?),Exp1).

% Converts the DNF expression into a CNF expression
dnf_to_cnf(and(P,Q),and(P1,Q1)):-
	!,
	dnf_to_cnf(P,P1),
	dnf_to_cnf(Q,Q1).
dnf_to_cnf(or(P,Q),CNF):-
	!,
	dnf_to_cnf(P,P1),
	dnf_to_cnf(Q,Q1),
	dnf_to_cnf_(or(P1,Q1),CNF).
dnf_to_cnf(CNF,CNF).

dnf_to_cnf_(or(and(P,Q),R),and(P1,Q1)):-
	!,
	dnf_to_cnf_(or(P,R),P1),
	dnf_to_cnf_(or(Q,R),Q1).
dnf_to_cnf_(or(P,and(Q,R)),and(P1,Q1)):-
	!,
	dnf_to_cnf_(or(P,Q),P1),
	dnf_to_cnf_(or(P,R),Q1).
dnf_to_cnf_(CNF,CNF).

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
	current_fact(grouping(V,Gr)),
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

