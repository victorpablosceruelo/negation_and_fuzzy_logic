:- module(siap_annotate,
	[
 	    zero_count/1,
 	    add_count/3,
 	    cgeannotate/7,
 	    cdg0/3,
 	    transitive_close/2,
 	    udg/3,
 	    take_numbers_cdg/2,
 	    take_numbers_uudg/2,
 	    take_numbers_tgudg/2,
 	    take_numbers_udg/2,
 	    count_if_cge/2
	],
	[]).

/*             Copyright (C)1990-94 UPM-CLIP				*/

%========================================================================
% Programmer: Kalyan Muthukumar
% Started   : 21st Nov 1989
%------------------------------------------------
% Programmer: Francisco Bueno
% Revised   : 13th Mar 1994
%------------------------------------------------
% Annotate PROLOG programs for goal independence
% AND-parallelism (SIAP)
%------------------------------------------------
% MEL annotator
% This version tries to maximize the length of the CGE
%------------------------------------------------
% CDG/UDG annotators
% This version tries to preserve the parallelism in the CDG
%========================================================================
% input/output formats: see cgeannotate.pl
%========================================================================

:- use_module(library(lists), 
	[
	    append/3,
	    equal_lists/2,
	    intersection/3,
	    length/2,
	    reverse/2,
	    sublist/2,
	    union/3
	]).

:- use_module(library(idlists), 
	[
	    memberchk/2,
	    subtract/3
	]).

:- use_module(library(sets), 
	[
	    insert/3,
	    merge/3,
	    ord_intersection_diff/4,
	    ord_member/2,
	    ord_subtract/3
	]).

:- use_module(annotate, 
	[
	    collapse_dep/4,
	    goal_conditions/4,
	    goal_conditions_/2,
	    remove_ampersand/2,
	    sequential_dep_vertex/1,
	    vertex_info/4,
	    vertex_goal/3,
	    vertex_goals/3,
	    dep_vertex_goal/4,
	    is_udg/2,
	    update_info/3,
	    ground_imply_indep_simplified/3,
	    inconsistent_conds/2,
	    inconsistent_lists/2,
	    no_edge_if_no_cond/5
	]).

:- use_module(simplify, 
	[
	    simplify_condition/4,
	    negate_info/2
	]).

:- use_module(library(sort)).

:- op( 950, xfy, [(&),(\&),(&>)]).
:- op( 950, yf, [(<&)]).


% --------------------------------------------------------------------------
% cgeannotate(+,+,+,+,+,-,-)
% cgeannotate(Body,Iap,Count0,Dict,Count,NewBody)
% cgeannotate/7 - MEL annotator
% It applies the MEL heuristic but independently of whether the conditions 
% are for SIAP or other independence concept.
% Body is a list [node(Id,Vars,Grain,Info)|...] 
% NewBody is in full syntax with Goal(s) which can be recovered from Id
% using Dict
% Conditions and cges are counted and added to Count0 to yield Count
% A "dep" structure is constructed following the same ordering that Body,
% and this structure is reversed so that mel/4 can procceed backwards
% --------------------------------------------------------------------------

cgeannotate(Body,Mode,Iap,Count0,Dict,Count,NewBody):-
	collapse_dep(Body,Mode,Iap,CollapsedGraph),
	reverse(CollapsedGraph,Inversed),
	mel(Inversed,Dict,Count1,NewBody),
	add_count(Count0,Count1,Count).

% --------------------------------------------------------------------------
% mel(+,+,-,-)
% mel(Nodes,Dict,Count,Body)
% mel/4 - the bulk of the mel algorithm 
% First, partition the body due to seffs, then apply the MEL heuristic and 
% partition where "false" conditions for parallelization are found
% --------------------------------------------------------------------------

mel(Nodes,Dict,Count,Body):-
	partition_seff(Nodes,Dict,RestNodes,Seq),
	mel50(RestNodes,Dict,Seq,Count,Body).

mel50([],_Dict,Seq,Count,Body):- !, Body=Seq,
	zero_count(Count).
mel50(Nodes,Dict,true,Count,Body):- !,
	partition_nodes(Nodes,Dict,Left,Right,Info,Conds),
	goal_conditions(Conds,Info,TmpCount,Checks),
	cge(Checks,Right,Cge,TmpCount,Count0),
	mel51(Left,Dict,Cge,Count1,Body),
	add_count(Count0,Count1,Count).
mel50(Nodes,Dict,Seq,Count,Body):-
	partition_nodes(Nodes,Dict,Left,Right,Info,Conds),
	goal_conditions(Conds,Info,TmpCount,Checks),
	cge(Checks,Right,Cge,TmpCount,Count0),
	mel51(Left,Dict,(Cge,Seq),Count1,Body),
	add_count(Count0,Count1,Count).

mel51([],_Dict,Right,Count,Body):- !, Body=Right,
	zero_count(Count).
mel51(Left,Dict,Right,Count,(Body,Right)):-
	mel(Left,Dict,Count,Body).

cge(true,Body,Cge,_Count0,Count):- !, Cge=Body,
	count_or_not_count(Body,Count).
cge(Checks,Body,(Checks->Body;Body1),Count,Count) :-
	remove_ampersand(Body, Body1).



% --------------------------------------------------------------------------
% partition_seff(+,+,-,-)
% partition_seff(Nodes,Dict,Left,Right)
% Traverses Nodes - which represents body goals in reversed order - and
% separates side-effects to the Right and the rest to the Left for the
% final annotated body.
% For now, every "low-grain" goal is considered a "seff" and leads to
% a partition of the body (see sequential_dep_vertex/1)
% --------------------------------------------------------------------------

partition_seff([Node|Nodes],Dict,Left,Right):-
	sequential_dep_vertex(Node), !,
	dep_vertex_goal(Node,Dict,Vertex,Goal),
	partition_seff0(Nodes,Dict,Left,[Vertex],Goal,Right).
partition_seff(Nodes,_Dict,Nodes,true).

partition_seff0([Node|Nodes],Dict,Left,RightVs,Right0,Right):-
	sequential_dep_vertex(Node), !,
	dep_vertex_goal(Node,Dict,Vertex,Goal),
	partition_seff0(Nodes,Dict,Left,[Vertex|RightVs],(Goal,Right0),Right).
partition_seff0(Nodes,_Dict,Left,RightVs,Right,Right):-
	cleanup_nodes(Nodes,RightVs,Left).

% --------------------------------------------------------------------------
% partition_nodes(+,+,-,-,-,-)
% partition_nodes(Nodes,Dict,Left,Right,Info,Conds)
% Traverses Nodes - which represents body goals in reversed order - and
% separates them where a "false" condition is found: first in the list Left 
% is the one which has the condition on some other to its right, which are
% all in Right - a parallel expression with Conds for which Info is valid
% --------------------------------------------------------------------------

% first node is the last in the (partition of the) body under consideration,
% so it cannot have a "false" condition - there are no goals to their right -
% upon which conditions can be placed, nor is it a seff because they have
% been removed before

partition_nodes([Node|Nodes],Dict,Left,Right,Info,Conds):-
	dep_vertex_goal(Node,Dict,Vertex,Goal),
	partition_nodes0(Nodes,Dict,Left,[Vertex],Goal,Right,RightVx,Conds),
	vertex_info(RightVx,_,_,Info).

partition_nodes0([],_Dict,[],[RVx|_RightVs],Right,Right,RVx,conds([],[],[])).
partition_nodes0([Node|Nodes],Dict,Left,RightVs,Right0,Right,RVx,Conds):-
	false_condition(Node,Cond),
	partition_nodes1(Cond,Node,Nodes,Dict,Left,RightVs,Right0,Right,RVx,
	                 Conds).

partition_nodes1(Cond,Node,Nodes,_Dict,Left,RightVs,Right0,Right,RVx,Conds):- 
	Cond==false, !,
	cleanup_nodes([Node|Nodes],RightVs,Left),
	RightVs=[RVx|_],
	Right=Right0, Conds=conds([],[],[]).
partition_nodes1(Cond,Node,Nodes,Dict,Left,RightVs,Right0,Right,RVx,Conds):-
	dep_vertex_goal(Node,Dict,Vertex,Goal),
	partition_nodes0(Nodes,Dict,Left,[Vertex|RightVs],(Goal&Right0),Right,
	                 RVx,NConds),
	merge_conds(Cond,NConds,Conds).

% --------------------------------------------------------------------------
% false_condition(+,-)
% false_condition(Node,Cond)
% The point to break the (part of the) body we are considering is were a
% goal on which another one depends with a "false" condition is found, or
% the goal itself is a seff or "low-grain" goal
% --------------------------------------------------------------------------

false_condition(Node,Cond):- sequential_dep_vertex(Node), !, Cond=false.
false_condition(dep(_,Deps),Cond):-
	false_condition_(Deps,conds([],[],[]),Cond).

false_condition_([],Conds,Conds).
false_condition_([to(_,false)|_Deps],_Conds,false):- !.
false_condition_([to(_,Cs)|Deps],Conds0,Conds):-
	merge_conds(Cs,Conds0,Conds1),
	false_condition_(Deps,Conds1,Conds).

merge_conds(conds(G1,I1,N1),conds(G2,I2,N2),conds(G,I,N)):-
	merge(G1,G2,G),
	merge(I1,I2,I),
	merge(N1,N2,N).

% --------------------------------------------------------------------------
% cleanup_nodes(+,+,-)
% cleanup_nodes(Nodes,RightVs,NewNodes)
% Once the partition has been decided, the Nodes corresponding to goals to
% the left (which still need processing) have to be cleaned up deleting all
% dependencies (in Deps) for the nodes in the partition to the right (RightVs)
% Both Deps in the nodes and RightVs are sorted!
% --------------------------------------------------------------------------

cleanup_nodes([],_RightVs,[]).
cleanup_nodes([dep(Vertex,Deps)|Nodes],RightVs,[dep(Vertex,Clean)|Cleans]):-
	cleanup_for_node(Deps,RightVs,Clean),
	cleanup_nodes(Nodes,RightVs,Cleans).

cleanup_for_node([],_RightVs,[]).
cleanup_for_node([to(Vertex,_)|Deps],RightVs,CDeps):-
	ord_member(Vertex,RightVs), !,
	cleanup_for_node(Deps,RightVs,CDeps).
cleanup_for_node([Dep|Deps],RightVs,[Dep|CDeps]):-
	cleanup_for_node(Deps,RightVs,CDeps).

%------------------------------------------------------------------------
% cdg0(+,+,-)
% cdg0(CDG,Dict,Exp)
% cdg0/3 - the CDG algorithm
% Builds a parallel expression from a CDG, which is graph(Vertices,Edges)
% where Vertices is a list and Edges is [edge(Vertex1,Vertex2,Cond)|...] 
% and from each Vertex and a corresponding Dict the original body goals
% can be recovered
%------------------------------------------------------------------------

% The CDG for a graph with less than 3 vertices is quite simple....

cdg0(graph([X],[]),Dict,G):- !,
	vertex_goal(X,Dict,G).
cdg0(graph([X,Y],[]),Dict,(G1 & G2)):- !,
	vertex_goals([X,Y],Dict,[G1,G2]).
cdg0(graph([_,_],[edge(X,Y,false)]),Dict,(G1,G2)):- !,
	vertex_goals([X,Y],Dict,[G1,G2]).
cdg0(graph([_,_],[edge(X,Y,Conds)]),Dict,(Check -> (G1 & G2) ; (G1,G2))):- !,
	vertex_goals([X,Y],Dict,[G1,G2]),
	goal_conditions_(Conds,Check).
% The CDG for graphs with 3 or more vertices....
cdg0(Cdg,Dict,Exp) :-
	cdg1(Cdg,global([],[],[]),[],Dict,Exp).

%-------------------------------------------------------------------------
% cdg1(+,+,+,+,-)
% cdg1(Graph,B,L,Dict,Exp)
% Graph is graph(Vertices,Edges) and Edges a list [edge(Vx,Vy,Conds)|...]
% Vertices appear in Dict so that can be converted to goals
% B is the current status of "global" information and L a list of vertices
% whose information has been already added to B (actually, it has to be
% a singleton - only leftmost node holds valid information)
% Exp is the resulting parallel expression
%-------------------------------------------------------------------------

% if the graph is an UDG, use UDG algorithm, else obtain nodes with no
% incoming edges and the edges coming out of these nodes ...

cdg1(graph(V,E),_B,_L,Dict,Exp):-
	is_udg(E,E1), !,
	transitive_close((V,E1),(V,E2)),
	udg(graph(V,E2),Dict,Exp).
cdg1(graph(V,E),B,L,Dict,Exp) :-
	p_conds(V,E,Conds,Par_nodes,Par_edges,Other_edges),
	cdg2(Conds,V,Par_nodes,Par_edges,Other_edges,B,L,Dict,Exp).

% if there are no edges coming out of them, build a expression with them
% and procceed with the rest (if there are any), else update the "global"
% info, take the combinations of conds consistent with that info and ...

cdg2([],V,Par_nodes,_Par_edges,Other_edges,_B,_L,Dict,Exp):- !,
	ord_subtract(V,Par_nodes,Other_nodes),
	no_conds(Other_nodes,Other_edges,Par_nodes,Dict,Exp).
cdg2(Condss,V,Par_nodes,Par_edges,Other_edges,B,L,Dict,Exp):-
	get_valid_info(Par_nodes,K,Info),
	update_state(B,L,K,Info,NewB,NewL),
	make_conds(Condss,NewB,Factss,CombCondss,Bs),
	cdg3(CombCondss,Factss,Bs,NewL,(V,Par_edges,Other_edges),Dict,Exp).

no_conds([],_E,Par_nodes,Dict,Exp) :- !,
	parallelize_nodes(Par_nodes,Dict,Exp).
no_conds(V,E,Par_nodes,Dict,(Exp1,Exp2)) :-
	parallelize_nodes(Par_nodes,Dict,Exp1),
	cdg0(graph(V,E),Dict,Exp2).

parallelize_nodes([X],Dict,G):- !,
	vertex_goal(X,Dict,G).
parallelize_nodes([X|Xs],Dict,(G & Par_xs)) :-
	vertex_goal(X,Dict,G),
	parallelize_nodes(Xs,Dict,Par_xs).

% for each combination of conditions update the graph with the facts derived
% from those conditions and the actual "global" info and recursively procceed
% with the updated graphs; finally convert the result to &-Prolog expressions

cdg3([],[],_Bs,_L,_Graph,_Dict,_Exp):- !,
	display(user,'Not sure this case will ever arise at all...'),
	abort.
cdg3(Condss,Factss,Bs,L,Graph,Dict,Exp):-
	form_exp(Condss,Factss,Bs,L,Graph,Dict,Exp1),
	simplify_exp(Exp1,Exp2),
	transform_exp(Exp2,Exp).

form_exp([Cs|Condss],[Facts|Factss],[B|Bs],L,Graph,Dict,[(Cs,Exp)|Exps]):-
	form_sub_exp(Facts,B,L,Graph,Dict,Exp),
	form_exp(Condss,Factss,Bs,L,Graph,Dict,Exps).
form_exp([],[],[],_L,_Graph,_Dict,[]).

form_sub_exp(Facts,B,L,(V,Par_edges,Other_edges),Dict,Exp):-
	update_graph(Facts,Par_edges,Other_edges,New_Par,New_Other),
	append(New_Par,New_Other,E),
	cdg1(graph(V,E),B,L,Dict,Exp).

%-------------------------------------------------------------------------

% the info used is that of the *first* goal (in the body) out of the
% Par_nodes found

get_valid_info(Par_nodes,K,Info):-
	sort(Par_nodes,[Vertex|_]),
	vertex_info(Vertex,K,_,Info).

% if that info has already been used to update B, don't do it again!

update_state(B,L,K,_Info,NewB,NewL):-
	member(K,L), !,
	NewB = B,
	NewL = L.
update_state(B,L,K,Info,NewB,[K|L]):-
	update_info(B,Info,NewB).

%-------------------------------------------------------------------------
% p_conds(+,+,-,-,-,-)
% p_conds(V,E,Condss,Par_nodes,Par_edges,Other_edges)
% From vertices V and edges E, get Par_nodes with no incoming edges, the
% Par_edges coming out of them, the list of Conds labelling these edges, 
% and the rest of Other_edges
%-------------------------------------------------------------------------

p_conds(V,E,Condss,Par_nodes,Par_edges,Other_edges) :-
	seq_nodes_cdg(E,[],Seq_nodes),
	ord_subtract(V,Seq_nodes,Par_nodes),
	get_conds(Par_nodes,E,Condss_u,[],Par_edges,[],Other_edges),
	sort(Condss_u,Condss).

seq_nodes_cdg([edge(_,Y,_)|Rest],Int_seq,Seq_nodes) :-
	ord_member(Y,Int_seq), !,
	seq_nodes_cdg(Rest,Int_seq,Seq_nodes).
seq_nodes_cdg([edge(_,Y,_)|Rest],Int_seq,Seq_nodes) :-
	insert(Int_seq,Y,Int1),
	seq_nodes_cdg(Rest,Int1,Seq_nodes).
seq_nodes_cdg([],Seq_nodes,Seq_nodes).

get_conds([X|Xs],E,Condss,CondssT,Par_edges,ParT,Other_edges) :-
	get_conds1(E,X,Condss,Condss1,Par_edges,Par1,Other1),
	get_conds(Xs,Other1,Condss1,CondssT,Par1,ParT,Other_edges).
get_conds([],Other_edges,Condss,Condss,Par_edges,Par_edges,Other_edges).

get_conds1([Edge|Edges],X,Condss,CondssT,Par_edges,ParT,Other_edges) :-
	Edge=edge(P,_,Conds),
	X == P, !,
	Par_edges = [Edge|Par1],
	( Conds = false ->
	  Condss = Condss1
	; Condss = [Conds|Condss1]
	),
	get_conds1(Edges,X,Condss1,CondssT,Par1,ParT,Other_edges).
get_conds1([Edge|Edges],X,Condss,CondssT,Par_edges,ParT,[Edge|Other_edges]) :-
	get_conds1(Edges,X,Condss,CondssT,Par_edges,ParT,Other_edges).
get_conds1([],_,Condss,Condss,Par_edges,Par_edges,[]).

%-------------------------------------------------------------------------
% make_conds(+,+,-,-,-)
% make_conds(Condss,B,Factss,CombCondss,Bs)
% CombCondss are the possible combinations of the conditions in Condss, 
% simplified accordingly to info B, this one updated in each case
% to yield the list Bs
% Factss is the list of lists of all facts which can be derived from
% each combination and B in each case
%-------------------------------------------------------------------------

make_conds(Condss,B,Factss,CombCondss,Bs) :-
	bool_comb(Condss,Condss1),
	cleanup_conds(Condss1,B,[],Factss,CombCondss,Bs).

cleanup_conds([Conds|Condss],B,Yet,Factss,CombCondss,Bs):-
	simplify_condition(Conds,B,Conds1,B1),
	check_and_simplify_cond(Conds1,Conds,B1,B,Factss,FactssT,
	                        CombCondss,CombCondssT,Bs,BsT,Yet,Yet1),
	cleanup_conds(Condss,B,Yet1,FactssT,CombCondssT,BsT).
cleanup_conds([],_B,_Yet,[],[],[]).

check_and_simplify_cond(false,_Cs,_B1,_B,Fss,Fss,Css,Css,Bs,Bs,Y,Y):- !.
check_and_simplify_cond(Cs1,_Cs,_B1,_B,Fss,Fss,Css,Css,Bs,Bs,Y,Y):- 
	member_identical_checks(Y,Cs1), !.
check_and_simplify_cond(Cs1,Cs,B1,B,[Fs|Fss],Fss,[Cs1|Css],Css,[B1|Bs],Bs,
	                                                     Y,[Cs1|Y]):-
	B=global(BPos,BNeg,_),
	B1=global(B1Pos,B1Neg,_),
	ord_subtract(B1Pos,BPos,Pos),
	ord_subtract(B1Neg,BNeg,Neg),
	list_to_conds(Pos,Neg,FConds),
	merge_conds(FConds,Cs,Fs).

member_identical_checks([Conds|_Condss],Conds0):-
	identical_checks(Conds,Conds0), !.
member_identical_checks([_Conds|Condss],Conds0):-
	member_identical_checks(Condss,Conds0).

identical_checks(conds(G,I,_),conds(G1,I1,_)):-
	G == G1,
	I == I1.

bool_comb([Conds],[Conds,Conds_neg]) :- !,
	negate_conds(Conds,Conds_neg).
bool_comb([Conds|Condss],CombCondss) :-
	bool_comb(Condss,CombCondss1),
	negate_conds(Conds,Conds_neg),
	merge_with_each_consistent(CombCondss1,Conds,CombCondss,CombCondssT),
	merge_with_each_consistent(CombCondss1,Conds_neg,CombCondssT,[]).

merge_with_each_consistent([Conds|Condss],Conds0,CombCondss,Tail) :-
	inconsistent_conds(Conds,Conds0), !,
	merge_with_each_consistent(Condss,Conds0,CombCondss,Tail).
merge_with_each_consistent([Conds|Condss],Conds0,[Conds1|CombCondss],Tail) :-
	merge_conds(Conds,Conds0,Conds1),
	merge_with_each_consistent(Condss,Conds0,CombCondss,Tail).
merge_with_each_consistent([],_Conds0,Tail,Tail).

list_to_conds(Pos,Neg,conds(Ground,Indep,Neg)):-
	split_ground_indep(Pos,Ground,Indep).

negate_conds(conds(Ground,Indep,Neg),conds(Ground1,Indep1,Neg1)):-
	negate_info(Neg,Pos),
	split_ground_indep(Pos,Ground1,Indep1),
	negate_info(Ground,NotGround),
	negate_info(Indep,NotIndep),
	append(NotGround,NotIndep,Neg1).

split_ground_indep([ground(X)|Pos],[ground(X)|Ground],Indep):-
	split_ground_indep(Pos,Ground,Indep).
split_ground_indep(Pos,[],Pos).

%-------------------------------------------------------------------------
% update_graph(+,+,+,-,-)
% update_graph(Conds,Par_edges,Other_edges,NewPar_edges,NewOther_edges)
% updates graph edges labels with the facts known to be true in Conds:
% Par_edges coming out of par_nodes can be updated directly, because the
% corresponding facts hold for par_nodes, but for the rest, Other_edges
% can only be safely updated with ground conds
% This is the only part of CDG algorithm which is conditions-sensitive!
%-------------------------------------------------------------------------
% HAVE TO UPDATE W.R.T. THE INFO IN THE LEFT VERTEX OF THE EDGE !!!!!!!
% or is it subsumed by the algorithm?
%-------------------------------------------------------------------------

update_graph(conds(Ground,Indep,Neg),Par_edges,Other_edges,New_Par,New_Other):-
	update_edges_ground(Par_edges,Ground,Tmp_Par),
	update_edges_ground(Other_edges,Ground,Tmp_Other),
	update_edges_indep(Tmp_Par,Indep,Tmp0_Par),
	update_edges_negated(Tmp0_Par,Neg,Tmp1_Par),
	update_edges(Tmp1_Par,New_Par),
	update_edges(Tmp_Other,New_Other).

update_edges_ground([E|Es],Facts,[E|NewEs]):-
	E = edge(_,_,false),
	update_edges_ground(Es,Facts,NewEs).
update_edges_ground([E|Es],Facts,[NewE|NewEs]):-
	E = edge(X,Y,conds(Ground,Indep,Neg)),
	ord_subtract(Ground,Facts,NewGround),
	ground_imply_indep_simplified(Facts,Indep,NewIndep),
	NewE = edge(X,Y,conds(NewGround,NewIndep,Neg)),
	update_edges_ground(Es,Facts,NewEs).
update_edges_ground([],_Facts,[]).

update_edges_indep([E|Es],Facts,[E|NewEs]):-
	E = edge(_,_,false),
	update_edges_indep(Es,Facts,NewEs).
update_edges_indep([E|Es],Facts,[NewE|NewEs]):-
	E = edge(X,Y,conds(Ground,Indep,Neg)),
	ord_subtract(Indep,Facts,NewIndep),
	NewE = edge(X,Y,conds(Ground,NewIndep,Neg)),
	update_edges_indep(Es,Facts,NewEs).
update_edges_indep([],_Facts,[]).

update_edges_negated([E|Es],Facts,[NewE|NewEs]):-
	E = edge(X,Y,conds(Ground,Indep,_)),
	append(Ground,Indep,Pos),
	inconsistent_lists(Pos,Facts), !,
	NewE = edge(X,Y,false),
	update_edges_negated(Es,Facts,NewEs).
update_edges_negated([E|Es],Facts,[E|NewEs]):-
	update_edges_negated(Es,Facts,NewEs).
update_edges_negated([],_Facts,[]).

% if the condition on an edge is true, it can be removed:

update_edges([edge(X,Y,Conds)|Es],NewEs):-
	no_edge_if_no_cond(Conds,X,Y,NewEs,TailEs),
	update_edges(Es,TailEs).
update_edges([],[]).

%-------------------------------------------------------------------------
% simplify_exp(+,-)
% simplify_exp(Exps,NewExps)
% Exps = [(Facts_n,Exp_n)|...] is traversed and expressions Exp_i which
% are identical are reduced leaving only that with the simpler condition
% of Facts_i. In doing this simplification the order of Exps must be
% respected.
%-------------------------------------------------------------------------

simplify_exp([],[]).
simplify_exp([(Facts,Sub_exp)|Exps],Exp0):-
	simplify_each(Exps,Facts,Sub_exp,Exp0,NewExps,Exp1),
	simplify_exp(NewExps,Exp1).

simplify_each([],Facts,Sub_exp,[(Facts,Sub_exp)|Exp0],[],Exp0).
simplify_each([Exp|Exps],PrevFacts,PrevSub_exp,Exp0,NewExps,Exp1):-
	Exp = (Facts,Sub_exp),
	Sub_exp == PrevSub_exp,!,
	simpler_conds(Facts,PrevFacts,Flag),
	decide_simpler(Flag,Exp,PrevFacts,PrevSub_exp,Exp0,Exps,NewExps,Exp1).
simplify_each([Exp|Exps],PrevFacts,PrevSub_exp,Exp0,[Exp|NewExps],Exp1):-
	simplify_each(Exps,PrevFacts,PrevSub_exp,Exp0,NewExps,Exp1).

simpler_conds(conds(G1,I1,_),conds(G2,I2,_),Flag):- 
	G1 == G2,
	I1 == I2, !,
	Flag = subset_2_of_1.
simpler_conds(conds([],[],_),_Conds2,Flag):- !,
	Flag = subset_1_of_2.
simpler_conds(conds(G1,I1,_),conds(G2,I2,_),Flag):- 
	ord_intersection_diff(G1,G2,IntersectG,DisjunctG),
	ord_intersection_diff(I1,I2,IntersectI,DisjunctI),
	simpler_conds_(DisjunctG,IntersectG,G2,FlagG),
	simpler_conds_(DisjunctI,IntersectI,I2,FlagI),
	decide_flag(FlagG,FlagI,Flag).
	
simpler_conds_([],_Intersect,_Facts2,subset_1_of_2).
simpler_conds_(_Disjunct,Intersect,Facts2,Flag):-
	Intersect == Facts2, !,
	Flag = subset_2_of_1.
simpler_conds_(_Disjunct,_Intersect,_Facts2,none).

decide_flag(Flag,Flag,Flag):- !.
decide_flag(_FlagG,_FlagI,none).

decide_simpler(subset_1_of_2,Exp,_,_,Exp0,Exps,[Exp|Exps],Exp0).
decide_simpler(subset_2_of_1,_Exp,Facts,Sub_exp,Exp0,Exps,NewExps,Exp1):-
	simplify_each(Exps,Facts,Sub_exp,Exp0,NewExps,Exp1).
decide_simpler(none,Exp,Facts,Sub_exp,Exp0,Exps,[Exp|NewExps],Exp1):-
	simplify_each(Exps,Facts,Sub_exp,Exp0,NewExps,Exp1).

%-------------------------------------------------------------------------
% transform_exp(+,-)
% transform_exp(Exps,Exp)
% Exps = [(Facts_n,Exp_n)|...] is transformed to an if-then-else Exp
%-------------------------------------------------------------------------

transform_exp([(conds([],[],_),Exp)],Exp):- !.
%% these two cases should never arise!
%% transform_exp([(Conds,Exp)],(Goal->Exp)):- !,
%% 	goal_conditions_(Conds,Goal).
%% transform_exp([(conds([],[],_),Sub_exp)|Exps],(ground(_)->Sub_exp;Exp0)):-
%% 	transform_exp(Exps,Exp0).
transform_exp([(Conds,Sub_exp)|Exps],(Goal->Sub_exp;Exp0)):-
	goal_conditions_(Conds,Goal),
	transform_exp(Exps,Exp0).
transform_exp([],[]).

%-------------------------------------------------------------------------
% transitive_close(+,-)
% transitive_close(Graph,ClosedGraph)
% transitive_close/2 - trnasitively closes a graph
% Graph is (Vertices,Edges) where Edges is a list [(X,Y)|...]
%------------------------------------------------------------------------

transitive_close((V,E1),(V,E2)) :-
	length(V,Nv),
	( Nv < 3 -> %% the graph is already transitively closed
	    E2 = E1
	; length(E1,Ne),
	  ( Ne < 2 -> %% the graph is already transitively closed
	      E2 = E1
	  ; sort(E1,E3),
	    transitive_close_(E3,E3,E3,E2,Nv)
	  )
	).

transitive_close_(E,E,E,E,0). %% Nv iterations are over
transitive_close_([(X,Y)|Es],E1,E2,E4,Nv) :-
	get_all_y_edges(E2,Y,Yedges),
	( Yedges = [] ->
	   transitive_close_(Es,E1,E2,E4,Nv)
	; trans_close(Yedges,X,Newedges),
	  merge(Newedges,E2,E3),
	  transitive_close_(Es,E1,E3,E4,Nv)
	).
transitive_close_([],E1,E3,E4,Nv) :-
	length(E1,N1),
	length(E3,N3),
	( N1 = N3 -> %% i.e. transitive closure is complete
	  E4 = E3
	; Nv0 is Nv-1,
	  transitive_close_(E3,E3,E3,E4,Nv0)
	).

get_all_y_edges([(A,B)|Es],Y,[(A,B)|Yedges]) :-
	A == Y,!,
	get_all_y_edges(Es,Y,Yedges).
get_all_y_edges([(_,_)|Es],Y,Yedges) :-
	get_all_y_edges(Es,Y,Yedges).
get_all_y_edges([],_,[]).

trans_close([(_Y,Z)|Yedges],X,[(X,Z)|Newedges]) :-
	trans_close(Yedges,X,Newedges).
trans_close([],_,[]).

%------------------------------------------------------------------------
% udg(+,+,+,-)
% udg(UDG,Dict,Exp)
% udg/4 - converts an unconditional dep graph into an &-Prolog expression
% UDG is graph(Vertices,Edges) and Edges is a list [(Vx,Vy)|...] where
% Dict can be used to recover a goal from the corresponding vertex
%========================================================================
%  (1) This program assumes that the given (directed) graph is closed 
%      under transitivity.
%  (2) This program doesn't take care of the case which occurs in 
%      Lemma 1 (page 6, MCC TR ACT-ST-233-89). This has to be rectified.
%   -  This has now been rectified (PBC)
%------------------------------------------------------------------------

% UDGs for graphs with =< 3 vertices is quite straightforward...

udg(graph([X],[]),Dict,G) :-
	vertex_goal(X,Dict,G).
udg(graph([X,Y],[]),Dict,((G1 & G2))) :-
	vertex_goals([X,Y],Dict,[G1,G2]).
udg(graph([_,_],[(X,Y)]),Dict,(G1,G2)) :-
	vertex_goals([X,Y],Dict,[G1,G2]).
udg(graph([X,Y,Z],[]),Dict,(G1 & G2 & G3)) :-
	vertex_goals([X,Y,Z],Dict,[G1,G2,G3]).
udg(graph([X,Y,Z],[(P,Q)]),Dict,Exp) :-
	( ((X \== P),(X \== Q)) ->
	    vertex_goals([P,Q,X],Dict,[G1,G2,G3])
	; ((Y \== P),(Y \== Q)) ->
	    vertex_goals([P,Q,Y],Dict,[G1,G2,G3])
	; ((Z \== P),(Z \== Q)) ->
	    vertex_goals([P,Q,Z],Dict,[G1,G2,G3])
	),
	Exp = (((G1,G2) & G3)).
udg(graph([_,_,_],[(W,X),(Y,Z)]),Dict,Exp) :-
	( W == Y ->
	  vertex_goals([W,X,Z],Dict,[G1,G2,G3]),
	  Exp = (G1,((G2 & G3)))
        ; X == Z ->
	  vertex_goals([W,Y,Z],Dict,[G1,G2,G3]),
	  Exp = (((G1, G2)) & G3)
        ).
udg(graph([_,_,_],[(A,B),(C,D),(E,F)]),Dict,Exp) :-
	( (A == C) -> %% E and F are the same as B and D
	   vertex_goals([A,E,F],Dict,[G1,G2,G3])
	; (A == E) -> %% B and F are the same as C and D
	   vertex_goals([A,C,D],Dict,[G1,G2,G3])
	; (C == E) -> %% A and B are the same as D and F
	   vertex_goals([C,A,B],Dict,[G1,G2,G3])
	),
	Exp = (G1,G2,G3).

% UDGs for graphs with >3 vertices...

udg(graph(V,[]),Dict,Exp) :-
	parallelize_nodes(V,Dict,Exp).
udg(graph(V,E),Dict,Exp) :-
	p_dep(E,V,P_dep,Q_edges,P_zero),
	exp(P_dep,Q_edges,Dict,Exp1),
	full_udg(P_zero,Dict,Exp1,Exp).

full_udg([],_Dict,Exp1,Exp) :- !,
	Exp = Exp1.
full_udg(P_zero,Dict,Exp1,(Exp&Exp1)):-
	parallelize_nodes(P_zero,Dict,Exp).

%------------------------------------------------------------------------
% p_dep(+,+,-,-,-)
% p_dep(E,V,P_dep,Q_edges,P_zero)
% From vertices V and edges E compute independent vertices P_nodes, and
% a structure P_dep for the Seq_nodes which depend on P_nodes, and the
% subset P_zero of P_nodes which no other vertex depends on them
%------------------------------------------------------------------------

p_dep(E,V,P_dep,Q_edges,P_zero) :-
	seq_nodes_udg(E,[],Seq_nodes),
	ord_subtract(V,Seq_nodes,Par_nodes),
	p_edges(E,Par_nodes,P_edges,P_non_zero_u),
	ord_subtract(E,P_edges,Q_edges),
	sort(P_non_zero_u,P_non_zero),
	ord_subtract(Par_nodes,P_non_zero,P_zero),
	p_dep0(P_edges,P_dep).

seq_nodes_udg([(_,Y)|Rest],Int_seq,Seq_nodes) :-
	ord_member(Y,Int_seq), !,
	seq_nodes_udg(Rest,Int_seq,Seq_nodes).
seq_nodes_udg([(_,Y)|Rest],Int_seq,Seq_nodes) :-
	insert(Int_seq,Y,Int1),
	seq_nodes_udg(Rest,Int1,Seq_nodes).
seq_nodes_udg([],Seq_nodes,Seq_nodes).

p_edges([(X,Y)|Rest],Par_nodes,P_edges,P_non_zero) :-
	ord_member(X,Par_nodes), !,
	P_edges = [(X,Y)|Rest_edges],
	P_non_zero = [X|Rest_non_zero],
	p_edges(Rest,Par_nodes,Rest_edges,Rest_non_zero).
p_edges([_|Rest],Par_nodes,P_edges,P_non_zero) :-
	p_edges(Rest,Par_nodes,P_edges,P_non_zero).
p_edges([],_Par_nodes,[],[]).

p_dep0([(X,Y)|Es],[([X|Y_vertices],Y)|P_dep]) :-
	compute_p_dep(Es,Y,Non_Y_edges,Y_vertices),
	p_dep0(Non_Y_edges,P_dep).
p_dep0([],[]).

compute_p_dep([(P,Q)|Es],Y,Non_Y_edges,Y_vertices) :-
	Y == Q, !,
	Y_vertices = [P|Rest_Y_vertices],
	compute_p_dep(Es,Y,Non_Y_edges,Rest_Y_vertices).
compute_p_dep([E|Es],Y,[E|Non_Y_edges],Y_vertices) :-
	compute_p_dep(Es,Y,Non_Y_edges,Y_vertices).
compute_p_dep([],_,[],[]).

%------------------------------------------------------------------------
% exp(+,+,+,-)
% exp(P_dep,Q_edges,Dict,Exp)
%------------------------------------------------------------------------

%% Added merge_intersecting_dep/2 to handle non-void-intersecting Sets of 
%%       dependencies (PBC)
%% This deals with conditions under which Lemma 1 does not hold
%%       length_of_lists/2 creates the (singleton) Sets
%%       merge_intersecting_dep/2 groups them in such a way that Lemma 1
%%             holds thereafter

exp(P_dep,Q_edges,Dict,Exp) :-
	length_of_lists(P_dep,Dep_plus_lengths),
	sort(Dep_plus_lengths,Temp1),
	merge_dep(Temp1,Temp2),
	sort(Temp2,Temp3),
	exp_udg(Temp3,Q_edges,Dict,Exp).

exp_udg(X,Q_edges,Dict,Exp) :-
	sub_udgs(X,Q_edges,Dict,Sub_udgs),
	udg_merge(Sub_udgs,[],Dict,Udg_merge),
	form_par_exp(Udg_merge,Exp).

form_par_exp([(_,_,Exp)],Exp).
form_par_exp([(_,_,Exp)|Rest],(Exp & Par_rest)) :-
	form_par_exp(Rest,Par_rest).

length_of_lists([(List,X)|Ls],[(N,List,[X])|Rest]) :-
	length(List,N),
	length_of_lists(Ls,Rest).
length_of_lists([],[]).

%------------------------------------------------------------------------
% sub_udgs(+,+,+,-)
% sub_udgs(Deps,Q_edges,Dict,Sub_udgs)
%------------------------------------------------------------------------

sub_udgs([(N,P_list,Q_list)|Rest],Q_edges,Dict,
	[(N,P_list,Sub_udg)|Rest_udg]):-
	project_edges(Q_edges,Q_list,Edges),
	udg(graph(Q_list,Edges),Dict,Sub_udg),
	subtract(Q_edges,Edges,Rest_edges),
	sub_udgs(Rest,Rest_edges,Dict,Rest_udg).
sub_udgs([],_,_,[]).

project_edges([(X,Y)|Rest],Par_nodes,P_edges) :-
	( (memberchk(X,Par_nodes),memberchk(Y,Par_nodes)) ->
	    P_edges = [(X,Y)|Rest_edges],
	    project_edges(Rest,Par_nodes,Rest_edges)
	; project_edges(Rest,Par_nodes,P_edges)
        ).
project_edges([],_,[]).

%------------------------------------------------------------------------
% merge_dep(+,-)
% merge_dep(Deps,MergedDeps)
%------------------------------------------------------------------------

merge_dep([D|Ds],Merge_dep) :-
	merge_intersecting_dep([D|Ds],Dep1,stop,F),
	merge_dep1(F,Dep1,Merge_dep).
merge_dep([],[]).

merge_dep1(stop,D,D).
merge_dep1(go,Di,Do):-
	merge_dep(Di,Do).


merge_intersecting_dep([X],[X],F,F).
merge_intersecting_dep([(N1,List1,Nodelist1),(N2,List2,Nodelist2)|Ds],Dep,
	               Fi,Fo):-
	N1 = N2,
	equal_lists(List1,List2), !,
	union(Nodelist1,Nodelist2,Nodelist),
	merge_intersecting_dep([(N1,List1,Nodelist)|Ds],Dep,Fi,Fo).
merge_intersecting_dep([(N1,List1,Nodelist1),(N2,List2,Nodelist2)|Ds],Dep,
	               Fi,Fo):-
	intersection(List1,List2,I),
	merge_if_intersecting_dep(I,(N1,List1,Nodelist1),(N2,List2,Nodelist2),
	                            Ds,Dep,Fi,Fo).

merge_if_intersecting_dep([],D1,D2,R,Ds,Fi,Fo):- !,
	merge_intersecting_dep([D1|R],DsT,Fi,Ft),
	merge_going_on_dep(DsT,D1,D2,Ds,Ft,Fo).
merge_if_intersecting_dep(I,(N1,List1,Nodelist1),(N2,List2,Nodelist2),R,
	                  [(N1,List1,Nodelist1)|Ds],Fi,Fo):-
	N1<N2, equal_lists(I,List1), !,
	merge_intersecting_dep([(N2,List2,Nodelist2)|R],Ds,Fi,Fo).
merge_if_intersecting_dep(_I,(_N1,List1,Nodelist1),(_N2,List2,Nodelist2),R,
	                  Ds,_Fi,Fo):-
	union(List1,List2,List),
	length(List,N),
	union(Nodelist1,Nodelist2,Nodelist),
	merge_intersecting_dep([(N,List,Nodelist)|R],Ds,go,Fo).

merge_going_on_dep([D1|R],D1,D2,[D1|Ds],Fi,Fo):- !,
	merge_intersecting_dep([D2|R],Ds,Fi,Fo).
merge_going_on_dep(R,_D1,D2,Ds,Fi,Fo):-
	merge_intersecting_dep([D2|R],Ds,Fi,Fo).

%------------------------------------------------------------------------
% udg_merge(+,+,+,-)
% udg_merge(DepUdgs,IntDepUdgs,Dict,MergedUdgs)
%------------------------------------------------------------------------

udg_merge([(N,P_list,Sub_udg)|Rest],Sub_udgs,Dict,Udg_merge) :-
	merge_exp((N,P_list,Sub_udg),P_list,Sub_udgs,[],Dict,Int_udgs),
	udg_merge(Rest,Int_udgs,Dict,Udg_merge).
udg_merge([(N,P_list,Sub_udg)|Rest],[],Dict,Udg_merge) :-
	parallelize_nodes(P_list,Dict,Par_exp),
	udg_merge(Rest,[(N,P_list,(Par_exp,Sub_udg))],Dict,Udg_merge).
udg_merge([],Sub_udgs,_,Sub_udgs).

merge_exp((N1,P1_list,Sub_udg1),[],Sub_udgs,Sub_exps,_Dict,Int_udgs) :-
	parallelize(Sub_exps,Exp),
	Int_udgs = [(N1,P1_list,(Exp,Sub_udg1))|Sub_udgs].
merge_exp((N1,P1_list,Sub_udg1),List,[(N1,P2_list,Sub_udg2)|Rest],
	   Sub_exps,Dict,[(N1,P2_list,Sub_udg2)|Int_udgs]) :-
	/* P2_list cannot be a subset of P1_list */
	merge_exp((N1,P1_list,Sub_udg1),List,Rest,Sub_exps,Dict,Int_udgs).
%% While P2_list is a subset of P1_list, we have to reduce P1_list, and
%% this is done through List, so... (PBC)
merge_exp((N1,P1_list,Sub_udg1),List,[(N2,P2_list,Sub_udg2)|Rest],
	   Exp,Dict,Int_udgs) :-
	N1 > N2,
	( sublist(P2_list,List) ->
	    subtract(List,P2_list,New_list),
	    merge_exp((N1,P1_list,Sub_udg1),New_list,Rest,
	              [Sub_udg2|Exp],Dict,Int_udgs)
	; /* P2_list is not a subset of P1_list */
            Int_udgs = [(N2,P2_list,Sub_udg2)|Int_udg1],
            merge_exp((N1,P1_list,Sub_udg1),List,Rest,Exp,Dict,Int_udg1)
	).
merge_exp((N1,P1_list,Sub_udg1),List,[(N2,P2_list,Sub_udg2)|Rest],
	   Exp,Dict,Int_udgs) :-
	N1 > N2,
	( sublist(P2_list,List) ->
	    subtract(List,P2_list,New_list),
	    merge_exp((N1,P1_list,Sub_udg1),New_list,Rest,
	              [Sub_udg2|Exp],Dict,Int_udgs)
	; /* P2_list is not a subset of P1_list or ... */
          /* it could have been, but a previous sub_udg was selected */
            Int_udgs = [(N2,P2_list,Sub_udg2)|Int_udg1],
            merge_exp((N1,P1_list,Sub_udg1),List,Rest,Exp,Dict,Int_udg1)
	).
merge_exp((N1,P1_list,Sub_udg1),List,[],Exp,Dict,Int_udg) :-
	parallelize_nodes(List,Dict,Par_exp),
	( Exp = [] ->
	    /* List = P1_list */
	    Int_udg = [(N1,P1_list,(Par_exp,Sub_udg1))]
	; parallelize([Par_exp|Exp],Par_exp1),
	  Int_udg = [(N1,P1_list,(Par_exp1,Sub_udg1))]
        ).

parallelize([X],X).
parallelize([X|Xs],(X & Par_xs)) :-
	parallelize(Xs,Par_xs).

%------------------------------------------------------------------------
% Statistics:
%------------------------------------------------------------------------

% For MEL we are counting total number of cges, of unconditional cges, 
% of ground and of indep conds
% For CDG we count additionally (first argument), the number of CDGs

zero_count(count(0,0,0,0,0)).

add_count(count(C1,T1,U1,G1,I1),count(C2,T2,U2,G2,I2),count(C,T,U,G,I)):-
	C is C1+C2, T is T1+T2, U is U1+U2, G is G1+G2, I is I1+I2.

count_or_not_count((_:_),count(0,0,0,0,0)):- !.  % single goal -> no cge
count_or_not_count(_,count(1,1,1,0,0)).      % otherwise, unconditional cge

% report_mel_stats/0 is called by the topdriver to give statistics for MEL

/*
report_mel_stats:-
	retract_fact(thiscge(count(_,T,U,G,I))),
	( T is 0 -> Avg is 0 ; Avg is (G+I)/T ),
	format_message(GM,IM),
	display(user,'[CGEs= '),
	display(user,T),
	display(user,'; checks: '),
	display(user,GM),
	display(user,'= '),
	display(user,G),
	display(user,', '), 
	display(user,IM),
	display(user,'= '),
	display(user,I),
	display(user,'; nochk= '),
	display(user,U),
	display(user,'; #checks/CGE= '),
	display(user,Avg),
	display(user,']'),
	display(user,'\n').
*/
% format_message(ground,indep):- language(lp).
% format_message(def,unlinked):- language(clp).

% For CDG we count CDGs (i.e. different combinations in the annotation of
% a body) and conditions, and we calculate their mean w.r.t. CGEs (i.e.
% different bodies annotated). 

take_numbers_cdg((A,B),Count):- !,
	take_numbers_cdg(A,Count1),
	take_numbers_cdg(B,Count2),
	add_count(Count1,Count2,Count).
take_numbers_cdg((C->A;B),Count):- !,
	take_indep_gnd(C,I,G),
	take_numbers_cdg1(A,Count1),
	take_numbers_cdg1(B,Count2),
	add_count(Count1,Count2,Count3),
	Count4 = count(1,0,0,G,I),
	add_count(Count3,Count4,Count).
% take_numbers_cdg((C=>A),Count):- !,
% 	take_indep_gnd(C,I,G),
% 	take_numbers_cdg1(A,Count1),
% 	Count2 = count(1,0,0,G,I),
% 	add_count(Count1,Count2,Count).
take_numbers_cdg(_A&_B,Count):- !,
	Count = count(1,0,1,0,0).
take_numbers_cdg(_B,Count):-
	Count = count(0,0,0,0,0).

take_numbers_cdg1((A,B),Count):- !,
	take_numbers_cdg1(A,Count1),
	take_numbers_cdg1(B,Count2),
	add_count(Count1,Count2,Count).
take_numbers_cdg1((C->A;B),Count):- !,
	take_indep_gnd(C,I,G),
	take_numbers_cdg1(A,Count1),
	take_numbers_cdg1(B,Count2),
	add_count(Count1,Count2,Count3),
	Count4 = count(1,0,0,G,I),
	add_count(Count3,Count4,Count).
% take_numbers_cdg1((C=>A),Count):- !,
% 	take_indep_gnd(C,I,G),
% 	take_numbers_cdg1(A,Count1),
% 	Count2 = count(1,0,0,G,I),
% 	add_count(Count1,Count2,Count).
take_numbers_cdg1(_B,Count):-
	Count = count(0,0,0,0,0).

take_indep_gnd((ground(LG),indep(LI)),I,G):-
	length(LG,G),
	length(LI,I).
take_indep_gnd((def(LG),unlinked(LI)),I,G):-
	length(LG,G),
	length(LI,I).
take_indep_gnd(ground(LG),0,G):-
	length(LG,G).
take_indep_gnd(def(LG),0,G):-
	length(LG,G).
take_indep_gnd(indep(LI),I,0):-
	length(LI,I).
take_indep_gnd(unlinked(LI),I,0):-
	length(LI,I).

/*
report_cdg_stats:-
	retract_fact(thiscge(count(C,T,U,G,I))),
	( T is 0 -> C0 is 0, G0 is 0, I0 is 0 
	          ; C0 is C/T, G0 is G/T, I0 is I/T ),
	( C is 0 -> Avg is 0 ; Avg is (G+I)/C ),
	format_message(GM,IM),
	display(user,'[CGEs= '),
	display(user,T),
	display(user,'; #CDGs/CGE= '),
	display(user,C0),
	display(user,'; checks/CGE: '),
	display(user,GM),
	display(user,'= '),
	display(user,G0),
	display(user,', '), 
	display(user,IM),
	display(user,'= '),
	display(user,I0),
	display(user,'; nochk= '),
	display(user,U),
	display(user,'; #checks/CDG= '),
	display(user,Avg),
	display(user,']'),
	display(user,'\n').
*/

% In the case of UDG we count the (maximum) number of parallel goals in
% the unconditional expressions - this corresponds to the maximum width 
% of the parallel execution graph
% We then calculate their mean w.r.t. CGEs (i.e. different bodies annotated). 

take_numbers_udg((A,B),Count):- !,
	take_numbers_udg(A,Count1),
	take_numbers_udg(B,Count2),
	add_count(Count1,Count2,Count).
take_numbers_udg(A&B,Count):- !,
	take_numbers_udg(A,Count1),
	take_numbers_udg(B,Count2),
	test_count(Count1,NewCount1),
	test_count(Count2,NewCount2),
	add_count(NewCount1,NewCount2,Count).
take_numbers_udg(_B,Count):-
	Count = count(0,0,0,0,0).

% Case of UUDG
take_numbers_uudg((A,B),Count):- !,
	take_numbers_uudg(A,Count1),
	take_numbers_uudg(B,Count2),
	add_count(Count1,Count2,Count).
take_numbers_uudg(('&'(_,_)),count(1,0,0,0,0)):- !.
take_numbers_uudg(('&!'(_,_)),count(1,0,0,0,0)):- !.
take_numbers_uudg(('&>'(_,_)),count(1,0,0,0,0)):- !.
take_numbers_uudg(('&!>'(_,_)),count(1,0,0,0,0)):- !.
take_numbers_uudg(_B,Count):-
	Count = count(0,0,0,0,0).

% Case of TGUDG
take_numbers_tgudg(X,Count):-
	take_numbers_udg(X,Count).

test_count(count(0,0,0,0,0),count(1,0,0,0,0)):- !.
test_count(Count,Count).

% To take the mean, each time take_numbers_c/udg is called is for a different
% CGE, so add 1.

count_if_cge(count(0,_,0,0,0),count(0,0,0,0,0)):- !.     % it is NOT a cge!
count_if_cge(count(C,_,U,G,I),count(C,1,U,G,I)).

/*
report_udg_stats:-
	retract_fact(thiscge(count(C,T,_U,_G,_I))),
	( T is 0 -> C0 is 0 ; C0 is C/T ),
	display(user,'[CGEs= '),
	display(user,T), 
	display(user,' (nocheck); #//Goals/CGE= '),
	display(user,C0),
	display(user,']'),
	display(user,'\n').
*/
%------------------------------------------------------------------------
