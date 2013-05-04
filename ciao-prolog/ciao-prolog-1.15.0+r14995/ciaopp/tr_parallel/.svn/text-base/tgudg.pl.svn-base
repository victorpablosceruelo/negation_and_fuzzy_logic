%========================================================================
% Programmers: Amadeo Casas & Jorge Navas
% Started    : Nov 2006
%------------------------------------------------------------------------
% TGUDG annotator
% This annotator algorithm for goal-level restricted independent
% and-parallelism uses as additional heuristics information about 
% expected task granularity, obtained from compile-time cost analysis.
%========================================================================

:- module(tgudg, [exp_tgudg/5], [andprolog, api(ciaopp_api)]).

:- doc(author, "Amadeo Casas").  
:- doc(author, "Jorge Navas").  

:- use_module(annotate, [vertex_goal/3]).
%% Ciao Libraries
:- use_module(library(lists), [delete/3, delete_non_ground/3]).
:- use_module(library(sets), [ord_subtract/3]).
:- use_module(library(sort), [sort/2]).
%% Resource Analysis
:- use_module(infer(infer_db), [inferred/3]).
:- use_module(resources(algebraic_res(normal_form_res)),
        [normal_form/2]).
:- use_module(resources(algebraic_res(general_form_res)), 
        [general_form/2]).
:- use_module(library(messages)).
%% For max_expr_comp/4
:- use_module(resources(algebraic_res(algebraic__res)), 
	[
	    multiply_expr/3,
	    add_expr/3
	]).
:- use_module(resources(algebraic_res(maxmin_res)), 
	[positive_expr/1,
	 negative_expr/1,
	 max_term_by_term/3]).


exp_tgudg(graph(Vertices,Edges),Dict,GoalDict,Exp,Dict) :-
%	simple_message("Vertices: ~q Edges:~q",[Vertices,Edges]),
	compute_expr(Vertices,Edges,GoalDict,Exp).

compute_expr([],[],_,_).  % base case
compute_expr(Vertices,Edges,GoalDict,Result) :-  % seq node
	ready(Vertices,Edges,ListS),
	member(V,ListS),
	delete(Vertices,V,Vs_no_V),
	there_is_a_path_to_all(Vs_no_V,V,Edges),
        !,
	delete_non_ground(Edges,edge(V,_,_),Edges1),
	delete_non_ground(Edges1,edge(_,V,_),Es_no_V),
	do_sequential([V],GoalDict,Exp),
	compute_expr(Vs_no_V,Es_no_V,GoalDict,NExp),
	( var(NExp) -> Result = Exp ; Result = (Exp,NExp) ).
compute_expr(Vertices,Edges,GoalDict,NExp) :-
	get_max(Vertices,GoalDict,Max),
	compute_expr_(Vertices,Edges,Max,GoalDict,NExp).
compute_expr_(Vertices,_,Max,GoalDict,NExp) :-  % no max
	var(Max),
	!,
	do_sequential(Vertices,GoalDict,NExp).  % substituted by next 5 goals
compute_expr_(Vertices,Edges,Max,GoalDict,NExp) :-  % rest
	delete(Vertices,Max,Vs_no_Max),
	indep(Vs_no_Max,Edges,Max,Indeps),
	(
	    Indeps = [] ->
	    do_sequential(Vertices,GoalDict,NExp)
	;
	    remove_dependencies(Indeps,Edges,GoalDict,FinalIndSet),
            sort([Max|FinalIndSet],FinalIndSet2),  %% {n} U S' in the algorithm
            ord_subtract(Vertices,FinalIndSet2,FinalIndSet3),
	    dep(FinalIndSet3,FinalIndSet2,Edges,Ant),
	    induced_graph(Edges,Ant,NewEs1),
            ord_subtract(Vertices,Ant,Vertices2),
            ord_subtract(Vertices2,FinalIndSet2,Vertices3),
	    induced_graph(Edges,Vertices3,NewEs2),
	    compute_expr(Ant,NewEs1,GoalDict,Pre_Exp),
	    compute_expr(Vertices3,NewEs2,GoalDict,Post_Exp),
	    do_parallel(FinalIndSet2,GoalDict,Parallel_Exp),  % FinalIndSet2 is never []
	    (
		(var(Pre_Exp),var(Post_Exp)) -> NExp = Parallel_Exp
	    ;
		var(Pre_Exp) -> NExp = (Parallel_Exp,Post_Exp)
	    ;
		var(Post_Exp) -> NExp = (Pre_Exp,Parallel_Exp)
	    ;
		NExp = (Pre_Exp,Parallel_Exp,Post_Exp)
	    )
	).

index_cost_by_res([steps|_],[[C]|_],C).
index_cost_by_res([_|Rs],[_|Cs],C):-
	index_cost_by_res(Rs,Cs,C).

get_pred_from_vertex(vertex(V,_),GoalDict,P):-	
	arg(V,GoalDict,Pred),
	get_pred(Pred,P).
	
get_pred(MF,Pred) :-
	arg(1,MF,P),
	functor(P,F,A),
	functor(Pred,F,A).	  

there_is_a_path_to_all([],_,_).
there_is_a_path_to_all([V|Vs],U,Edges) :-
	there_is_a_path(U,V,Edges),
	there_is_a_path_to_all(Vs,U,Edges).

/*
forward_edges_to_rest([],_,_).
forward_edges_to_rest([V|Vs],Edges,U):- 
	member(edge(U,V,_),Edges),
	!,
	forward_edges_to_rest(Vs,Edges,U).
*/

induced_graph([],_,[]).
induced_graph(_,[],[]).
induced_graph(_,[_],[]).
induced_graph([edge(V1,V2,R)|Edges],Vertices,[edge(V1,V2,R)|Res]) :-
	member(V1,Vertices),
	member(V2,Vertices),   
        !, 	
	induced_graph(Edges,Vertices,Res).
induced_graph([_|Edges],Vertices,Res) :-
	induced_graph(Edges,Vertices,Res).

do_sequential([],_,_).
do_sequential([V],GoalDict,G) :-
	vertex_goal(V,GoalDict,G).
do_sequential([V|Vs],GoalDict,(G,Exp)) :-
	vertex_goal(V,GoalDict,G),
	do_sequential(Vs,GoalDict,Exp).

do_parallel([],_,_).
do_parallel([V],GoalDict,G) :-
	vertex_goal(V,GoalDict,G).
do_parallel([V|Vs],GoalDict,(G & Exp)) :-
	vertex_goal(V,GoalDict,G),
	do_parallel(Vs,GoalDict,Exp).

/*
remove_elements([],_,[]).
remove_elements([H|T],L,R) :-
	member(H,L),
	!,
	remove_elements(T,L,R).
remove_elements(L1,L2,[H|R1]) :-
	L1 = [H|T],
	remove_elements(T,L2,R1).
*/

remove_dependencies([],_,_,[]).
remove_dependencies([A],_Edges,_GoalDict,[A]).
remove_dependencies([A|T],Edges,GoalDict,IndSet) :-
	there_is_a_path_with_set(T,A,Edges,B),
	!,
	cost_comparison(A,B,GoalDict,_Max,Min),
	delete_non_ground([A|T],Min,L),
	remove_dependencies(L,Edges,GoalDict,IndSet).
remove_dependencies([A|T],Edges,GoalDict,[A|RestSet]) :-
	remove_dependencies(T,Edges,GoalDict,RestSet).

get_max([],_,_).
get_max(L,GoalDict,Max) :-
	find_suff_large(L,X),
	( var(X) -> 
            Max = X
        ;
	    delete(L,X,L_no_X),
	    get_max_(L_no_X,X,GoalDict,Max)
        ).

find_suff_large([],_).
find_suff_large([H|_],H) :-
	sufficiently_large(H),
        !.
find_suff_large([_|T],H):-
	find_suff_large(T,H).

% Pseudocode:
% sufficiently_large(x,threshold){
%        max(x,threshold,max);
%        if threshold \leq x \leq max then
%           return true;
%        else
%           return false;
% }
% VERY IMPORTANT: Please, note that we should use some of the predicates
% defined in tr_granul/thresholds.pl (e.g. find_size_threshold/4)
sufficiently_large(_).

get_max_([],Max,_,Max).
get_max_([X|Xs],Max,GoalDict,NNMax) :-
	cost_comparison(X,Max,GoalDict,NMax,_),
	get_max_(Xs,NMax,GoalDict,NNMax).
	
cost_comparison(V1,V2,GoalDict,Max,Min) :-
	calculate_cost(V1,GoalDict,Cost_V1),
	calculate_cost(V2,GoalDict,Cost_V2),!,
	max_gen_form(Cost_V1,Cost_V2,Max0),
	( Max0 == Cost_V1 ->
          Max = V1, Min = V2
        ;
          Max = V2, Min = V1
        ).

calculate_cost(V,GoalDict,Cost):-	
	get_pred_from_vertex(V,GoalDict,Pred),
	current_fact(inferred(resources,Pred,Info)),
	Info = complexity(_,_,_,_,_,_,_,_,ub,Res,ResVec,_),
	( index_cost_by_res(Res,ResVec,Cost) ->
          true
        ; 
          warning_message("No Cost information for ~q",[Pred])
        ).

max_gen_form(Ex1, Ex2,GfEx):-
	normal_form(Ex1, NfEx1),
	normal_form(Ex2, NfEx2),
	max_expr_comp(NfEx1, NfEx2, NfEx,Comp),!,
	( Comp = 0 -> % Expressions are 'comparable'
          true % do nothing
        ;
          true % we should apply either heuristics OR run-time checks
        ), 
	general_form(NfEx,GfEx).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% max_expr_comp(Exp1,Exp2,Max,Flag) 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% As max_expr defined in resources('algebraic_res/maxmin_res'), but if Flag = '0'
% then Exp1 and Exp2 are 'comparable' expressions. Otherwise, Flag is '1'
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
max_expr_comp(_, bot, bot,0) :-
	!.
max_expr_comp(bot, E2, bot,0) :-
	E2 \== bot,
	!.
max_expr_comp(E1, inf, inf,0) :-
	E1 \== bot,
	!.
max_expr_comp(inf, E2, inf,0) :-
	E2 \== bot,
	E2 \== inf,
	!.
max_expr_comp(E1, E2, E,Flag) :-
	Minus1 is -1,
	normal_form(Minus1,Y1),
	multiply_expr(Y1, E2, Y2),
	add_expr(E1, Y2, Y),
	max_sign_expr_comp(E1, E2, Y, E,Flag).

max_sign_expr_comp(E1, _, Y, E1,0) :-
	positive_expr(Y),
	!.
max_sign_expr_comp(_, E2, Y, E2,0) :-
	negative_expr(Y),
	!.
max_sign_expr_comp(E1, E2, Y, E,Flag) :-
	(
	    ( positive_expr(Y) ; negative_expr(Y) ) ->
	    Flag = 0,  
	    fail
	;
            Flag = 1,
	    max_term_by_term(E1, E2, E)
	).


% min_gen_form(Ex1, Ex2,GfEx):-
% 	normal_form(Ex1, NfEx1),
% 	normal_form(Ex2, NfEx2),
% 	min_expr(NfEx1, NfEx2, NfEx),
% 	general_form(NfEx,GfEx).


ready([],_,[]).
ready([V|Vertices],Edges,List) :-
	member(edge(_,V,_),Edges),
	!,
	ready(Vertices,Edges,List).
ready([V|Vertices],Edges,[V|List]) :-
	ready(Vertices,Edges,List).

there_is_a_path(U,V,Edges) :-
	member(edge(U,V,_),Edges),
	!.
there_is_a_path(U,V,Edges) :-
	member(edge(U,X,_),Edges),
	there_is_a_path(X,V,Edges).

there_is_a_path_with_set([V1|_],X,Edges,V1) :-
	there_is_a_path(X,V1,Edges),
	!.
there_is_a_path_with_set([_|Vertices],X,Edges,Node) :-
	there_is_a_path_with_set(Vertices,X,Edges,Node).

indep([],_,_,[]).
indep([V|Vertices],Edges,U,List) :-
	sufficiently_large(V),
	( there_is_a_path(U,V,Edges) ;
	  there_is_a_path(V,U,Edges) ),
	!,
	indep(Vertices,Edges,U,List).
indep([V|Vertices],Edges,U,[V|Rest]) :-
	indep(Vertices,Edges,U,Rest).
	
dep([],_,_,[]).
dep([S|RestS],ListV,Edges,[S|Rest]) :-
	there_is_a_path_with_set(ListV,S,Edges,_),
	!,
	dep(RestS,ListV,Edges,Rest).
dep([_|RestS],ListV,Edges,Result) :-
	dep(RestS,ListV,Edges,Result).


% use_module(ciaopp(ciaopp)).
% module(qsort), analyze(shfr), analyze(eterms), analyze(resources), transform(tgudg).

% AbsInt = resources,
% Info = complexity(['basic_props:num'(_A),var(_B)],
%                   ['basic_props:num'(_A),'basic_props:int'(_B)],
%                   [+,-],
%                   [int,int],
%                   [[1],[2],[3]],
%                   [1],
%                   [$(0,1),0.4472135954999579*exp(1.618033988749895,$(0,1))-0.4472135954999579*exp(-0.6180339887498949,$(0,1))],
%                   inf,
%                   ub,
%                   [steps],
%                   [[1.447213595499958*exp(1.618033988749895,$(0,1))+0.5527864045000421*exp(-0.6180339887498949,$(0,1))-1.0]],_),
% Pred = 'fib:fib'(_A,_B) ? 

