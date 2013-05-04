:- module(classify, [classify/4], [andprolog,assertions]). 

:- doc(author,"Pedro L@'{o}pez").  

:- use_module(infercost(gran(gran_table)), 
	[
	    insert_gran_field/4, 
	    find_gran_field/4   
	]).

:- use_module(infercost(init), [find_symbol_field/4]). 

:- use_module(gran_call_graph, [find_gran_call_field/4]). 

:- use_module(library(messages), [warning_message/2]).

:- push_prolog_flag(multi_arity_warnings,off).

% Predicates are classified in types: 
% do_test:
% supply_sizes:
% sequential:


classify([], _, _, _).
classify([Comp|SCCG], GT, GCG, ST):-
        classify_comp(Comp, GT, GCG, ST),
        classify(SCCG, GT, GCG, ST).


classify_comp(Comp, GT, GCG, ST):-
       classify_comp(Comp, GT, GCG, PL, Flag, ST),
       (
	   nonvar(Flag) -> assign_type(PL, GT, supply_sizes)
       ;
	   assign_type(PL, GT, sequential)
       ).

classify_comp([], _, _, [], _, _).
classify_comp([Pred|Comp], GT, GCG, PL, Flag, ST):-
	infer_type(Pred, GT, GCG, Type, ST),
        % at this point Type is either do_test or supply_sizes, or 
        % an unbound variable.
        % Is the type is an unbound variable, then it cannot be of
        % do_test type, and thus will be bound
        % to either supply_sizes or sequential depending on the type
        % of other predicates in the strongly connected component.
        % That is, if there is some predicate the strongly connected
        % component whose type has been inferred by infer_type/5 as 
        % do_test or supply_sizes, then will be supply_sizes, otherwise will be 
        % sequential.
	(
	    var(Type) ->
	    (
		PL = [Pred|TPL],
		classify_comp(Comp, GT, GCG, TPL, Flag, ST)
	    )
	;
	    (
		Flag = need_sizes,
		insert_gran_field(GT, Pred, type, Type),
		classify_comp(Comp, GT, GCG, PL, Flag, ST)
	    )
	).


% Added by PLG (23-Mar-97)
%
%
infer_type(Pred, GT, GCG, Type, ST):-
	find_symbol_field(ST, Pred, time, TFun),
	TFun = [TimeFunc], 
	(
	    (TimeFunc \== bot,TimeFunc \== inf,parallel_predicate(Pred, GT))->
	     (Type = do_test)
	;
	    (
		find_gran_call_field(GCG, Pred, edge, EdgeList),
		edgelist_infer_type(EdgeList, GT, Type)
	    )
	).

% Added by PLG (23-Mar-97)
 % Commented out by PLG (23-Mar-97)
 %% infer_type(Pred,GT,GCG,Type):- 
 %%   ( parallel_predicate(Pred,GT)->
 %%       (%nl,write('Es paralelo : '),nl,write(Pred),nl, 
 %%        Type = do_test);     
 %%        (find_gran_call_field(GCG,Pred,edge,EdgeList),
 %%         edgelist_infer_type(EdgeList,GT,Type))
 %%   ).

 %% If a predicate does not perform any test and calls another one which
 %% is not of sequential type (i.e. do_test or supply_sizes type), then set the
 %% type Type to supply_sizes, otherwise leave Type as an unbound
 %% variable.

edgelist_infer_type(EdgeList, _, _):- var(EdgeList).
edgelist_infer_type(EdgeList, GT, Type):-
	nonvar(EdgeList),
	EdgeList = [Pred|EList],
	find_gran_field(GT, Pred, type, EdgeType),
        (
	    var(EdgeType) ->
	    warning_message("Predicate type has not been computed for ~q.",
                            [Pred])
	;
	    (
		EdgeType == sequential ->
		edgelist_infer_type(EList, GT, Type)
	    ;
		Type = supply_sizes
	    )
        ).

 %% Insert in the granul. table the type of all the predicates in a
 %% strongly connected component such that their types has not been
 %% inferred by the predicate infer_type. This type is Type (the same for all
 %% of them, an can be either supply_sizes or sequential).

assign_type([], _, _).
assign_type([Pred|PredList], GT, Type):-
	insert_gran_field(GT, Pred, type, Type),
	assign_type(PredList, GT, Type).

%%---------------------------------------------------------------------
% Check if a predicate is a parallel one.
%%---------------------------------------------------------------------

parallel_predicate(Pred, GT):-
	find_gran_field(GT, Pred, par_clauses, Clauses),
	clauses_parallel_predicate(Clauses, IsPar),
        IsPar =\= 0.

clauses_parallel_predicate(Clauses, 0):-
	var(Clauses).
clauses_parallel_predicate(Clauses, IsPar):-
	nonvar(Clauses),
	Clauses = [Clause|CList],
	parallel_clause(Clause, IsPar1),
	clauses_parallel_predicate(CList, IsPar2),
	IsPar is IsPar1 + IsPar2.

parallel_clause(( _ :- Body), IsPar):- !, parallel_body(Body, IsPar).
parallel_clause(_,0).


%%---------------------------------------------------------------------
% Check if a body is parallel.
%%---------------------------------------------------------------------

parallel_body((GoalA, GoalB), IsPar):-
	!,
	parallel_body(GoalA, IsPar1), 
	parallel_body(GoalB, IsPar2),
	IsPar is IsPar1 + IsPar2.
parallel_body((_G1 => G2), IsPar):-
	!,
	parallel_body(G2, IsPar).
parallel_body((_ & _), 1):- 
       !.
parallel_body('andprolog_rt:&'(_,_), 1):- 
       !.
parallel_body((_G1 -> G2 ; G3 ), IsPar):- 
       !,
       parallel_body(G2, IsPar2), 
       parallel_body(G3, IsPar3),
       IsPar is IsPar2 + IsPar3.
parallel_body(_, 0).


:- pop_prolog_flag(multi_arity_warnings).


