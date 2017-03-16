:- module(input_sizes,
	[
	    needed_input_sizes/2, 
	    get_needed_body_sizes/2 
	],
	[assertions]). 

:- doc(author,"Pedro L@'{o}pez").  

% ciao library
:- use_module(library(lists), [reverse/2]). 

% ciaopp library:
:- use_module(infercost(gran(gran_table)), 
	[
	    find_gran_field/4, 
	    get_pred_clauses_sizes_time_info/4
	]). 

:- use_module(infercost(init), 
	[
	    find_symbol_field/4,
	    get_input_arglist_from_st/3, 
	    clause_type/2
	]). 

% own library:
:- use_module(dynamic_size, [generate_initial_litnodes/8]). 
:- use_module(gen_expre, [substitute_sizevars_using_argposdict/3]). 
:- use_module(need_size_table, 
	[
	    find_needed_sizes/3, 
	    insert_needed_sizes/4, 
	    find_size/3
	]).
 
:- use_module(basic_trans, 
	[
	    seq/2,
	    flat_seq_body/2,           
	    mark_parallel_threads_0/2, 
	    get_pred_literal/2
	]).
 
:- use_module(g_utility, [open_union/2]). 

:- push_prolog_flag(multi_arity_warnings,off).

 %% Given a clause: compute needed head sizes.  Then add this sizes to the
 %% sizes computed in the previous iteration.  If there has been a change,
 %% then mark the flag Changes as true, otherwise leave the flag as an
 %% unbound variable.


needed_input_sizes([], _Info):-
	!.
needed_input_sizes([Comp|SCCG], Info):-
	needed_input_sizes_comp(Comp, Info),
	needed_input_sizes(SCCG, Info).


needed_input_sizes_comp(Comp, Info):-
	needed_input_sizes_comp_0(Comp, Info, Changes),
	(
	    Changes == true ->
	    needed_input_sizes_comp(Comp, Info)
	;
	    true
	).

needed_input_sizes_comp_0([], _Info, _Changes):-
	!.
needed_input_sizes_comp_0([Pred|Comp], Info, Changes):-
	infer_one_pred_in_needed_sizes(Pred, Info, Changes),
	needed_input_sizes_comp_0(Comp, Info, Changes).

infer_one_pred_in_needed_sizes(Pred, Info, Changes):-
	get_pred_type_info(Pred, Info, Type), 
	infer_one_pred_in_needed_sizes_1(Type, Pred, Info, Changes).


infer_one_pred_in_needed_sizes_1(do_test, Pred, Info, Changes):-
	!,
	infer_one_pred_in_needed_sizes_do_test(Pred, Info, Changes).
infer_one_pred_in_needed_sizes_1(supply_sizes, Pred, Info, Changes):-
	!,
	infer_one_pred_in_needed_sizes_do_test(Pred, Info, Changes).
infer_one_pred_in_needed_sizes_1(sequential, _Pred, _Info, _Changes).


infer_one_pred_in_needed_sizes_do_test(Pred, Info, Changes):-
	get_pred_clauses_sizes_time_info(Pred, Info, Clauses, SizeRel), %
	clauses_in_needed_sizes_do_test(Clauses, SizeRel, Info, Changes).


clauses_in_needed_sizes_do_test(Clauses, _, _, _):-
	var(Clauses), 
        !.
clauses_in_needed_sizes_do_test(Clauses, [SizeRel|SList], Info, Changes):-
	nonvar(Clauses),
	Clauses = [Clause|CList],
	clause_needed_sizes_do_test(Clause, SizeRel, Info, Changes),
	clauses_in_needed_sizes_do_test(CList, SList, Info, Changes).


clause_needed_sizes_do_test(Clause, SizeRel, Info, Changes):- 
	clause_type(Clause, Type), 
	clause_needed_sizes_do_test(Type, Clause, SizeRel, Info, Changes).

% A fact
clause_needed_sizes_do_test(3, _Clause, _SizeRel, _Info, _Changes).
% A rule
clause_needed_sizes_do_test(2, (Head:-Body), SizeRel, Info, Changes):- 
	functor(Head, Name, Arity),
	body_needed_sizes_do_test(Name/Arity, Body, SizeRel, Info, Changes).


% warning! Check that the transformed literal is before the thread.
body_needed_sizes_do_test(Pred, Body, SizeRel, Info, Changes):-
	mark_parallel_threads_0(Body, Threads), 
	threads_needed_sizes(Threads, SizeRel, Info, ArgPosDict), 
	get_needed_head_body_sizes(ArgPosDict, _NeededHeadSizes, 
                                   NeededBodySizes), 
        Info = info(GT, ST, _NT, NS),
	get_input_arglist_from_st(ST, Pred, InSizes),  
	seq(Body,SeqBody), %
	flat_seq_body(SeqBody, FlatBody), %
	generate_initial_litnodes(NeededBodySizes, SizeRel, InSizes, 
                                  FlatBody, GT, ST, LitNodes, _IrTrans), 
        get_needed_sizes_from_litnodes(LitNodes, ArgPosDict), %
	get_needed_head_body_sizes(ArgPosDict, NeHeadSizes2, _NeeBodySizes2),
	insert_needed_sizes(Pred, NS, NeHeadSizes2, Changes). %


get_needed_sizes_from_litnodes(LitNodes, _ArgPosDict):-
	var(LitNodes), 
	!.
get_needed_sizes_from_litnodes(LitNodes, ArgPosDict):-
	nonvar(LitNodes),
	LitNodes = [litnode(_LitNum, _Label, CompSizes)|Nodes], 
	collect_needed_sizes_0(CompSizes, ArgPosDict),
	get_needed_sizes_from_litnodes(Nodes, ArgPosDict). 


collect_needed_sizes_0([], _ArgPosDict).
collect_needed_sizes_0([(_Pos, SizeExpre)|RSizeExps], ArgPosDict):-
	substitute_sizevars_using_argposdict(SizeExpre, _, ArgPosDict),
	collect_needed_sizes_0(RSizeExps, ArgPosDict).


threads_needed_sizes([Lit|Litrs], SizeRel, Info, ArgPosDict):-
	!,
	reverse([Lit|Litrs], RLiterals),
	one_thread_needed_sizes(RLiterals, SizeRel, Info, ArgPosDict).
threads_needed_sizes((Lit1,Literals), SizeRel, Info, ArgPosDict):-
	!,
	threads_needed_sizes(Lit1, SizeRel, Info, ArgPosDict),
	threads_needed_sizes(Literals, SizeRel, Info, ArgPosDict).
threads_needed_sizes(Literal, SizeRel, Info, ArgPosDict):-
	one_literal_needed_sizes_not_in_thread(Literal,SizeRel,Info,
                                               ArgPosDict).


one_thread_needed_sizes([], _SizeRel, _Info, _ArgPosDict).
one_thread_needed_sizes([Lit|Literals], SizeRel, Info, ArgPosDict):-
	one_literal_needed_sizes(Lit, SizeRel, Info, ArgPosDict),
	one_thread_needed_sizes(Literals, SizeRel, Info, ArgPosDict).


one_literal_needed_sizes(Lit, SizeRel, Info, ArgPosDict):-
	get_pred_literal(Lit, LitPred),
	get_pred_time_neededsizes_info(LitPred,Info,TimeFunc,
	                               NeededHeadSizes1),
     % Get the literal's predicate needed head sizes from the cost function.
     % This is wrong!
        substitute_sizevars_using_argposdict(TimeFunc,_NTimeFunc,ArgPosDict1),
        get_needed_head_body_sizes(ArgPosDict1, NeededHeadSizes,
                                  _NeededBodySizes),
     % Make the union of previous iteration needed head sizes and the
     % needed head sizes by the cost function. 
        open_union(NeededHeadSizes1, NeededHeadSizes),
        arguments_needed_sizes(NeededHeadSizes, Lit, SizeRel, ArgPosDict).


one_literal_needed_sizes_not_in_thread(Lit, SizeRel, Info, ArgPosDict):-
	get_pred_literal(Lit, LitPred),
	get_pred_neededsizes_info(LitPred, Info, NeededHeadSizes),
     % Get the literal's predicate needed head sizes from the cost function.
        arguments_needed_sizes(NeededHeadSizes, Lit, SizeRel, ArgPosDict).


arguments_needed_sizes(NeededHeadSizes, _Literal, _SizeRel, _ArgPosDict):-
	var(NeededHeadSizes), !.
arguments_needed_sizes(NeededHeadSizes, Literal, SizeRel, ArgPosDict):-
	nonvar(NeededHeadSizes), !,
	NeededHeadSizes = [SizeVar|RestNeeded],
	get_arg_num(SizeVar, ArgNum),
	Key = Literal/ArgNum,
	find_size(SizeRel, Key, Size),
	substitute_sizevars_using_argposdict(Size, _NSize, ArgPosDict),
	arguments_needed_sizes(RestNeeded, Literal, SizeRel, ArgPosDict).


get_arg_num($(0, ArgNum), ArgNum).


% Data Management

get_pred_time_neededsizes_info(Pred, Info, TimeFunc, NeededSizes):-
	Info = info(_GT, ST, _NT, NS),
	find_symbol_field(ST, Pred, time, TimeFunc),
	find_needed_sizes(Pred, NS, NeededSizes).


get_pred_neededsizes_info(Pred, Info, NeededSizes):-
	Info = info(_GT, _ST, _NT, NS),
	find_needed_sizes(Pred, NS, NeededSizes).


%% (better move to granul_table)
get_pred_type_info(Pred, Info, Type):- 
	Info = info(GT, _ST, _NT, _NS),
	find_gran_field(GT, Pred, type, Type).


get_needed_head_body_sizes(ArgPosDict, _NeededHeadSizes, _NeededBodySizes):-
	var(ArgPosDict),
	!.
get_needed_head_body_sizes(ArgPosDict, NeededHeadSizes,
                           [$(LitNum, ArgNum)|NeededBodySizes]):-
        nonvar(ArgPosDict),
	ArgPosDict = [($(LitNum, ArgNum), _Var)|Rest],
	LitNum =\= 0, 
	!,
	get_needed_head_body_sizes(Rest, NeededHeadSizes, NeededBodySizes).
get_needed_head_body_sizes(ArgPosDict, [$(LitNum,ArgNum)|NeededHeadSizes],
                           NeededBodySizes):-
        nonvar(ArgPosDict),
	ArgPosDict = [($(LitNum, ArgNum), _Var)|Rest],
	LitNum =:= 0,
	get_needed_head_body_sizes(Rest, NeededHeadSizes, NeededBodySizes).


get_needed_body_sizes(ArgPosDict, _NeededBodySizes):-
	var(ArgPosDict),
	!.
get_needed_body_sizes(ArgPosDict, [$(LitNum, ArgNum)|NeededBodySizes]):-
	nonvar(ArgPosDict),
	ArgPosDict = [($(LitNum, ArgNum), _Var)|Rest],
	LitNum =\= 0, 
	!,
	get_needed_body_sizes(Rest, NeededBodySizes).
get_needed_body_sizes(ArgPosDict, NeededBodySizes):-
	nonvar(ArgPosDict),
	ArgPosDict = [($(LitNum, _ArgNum), _Var)|Rest],
	LitNum =:= 0,
	get_needed_body_sizes(Rest, NeededBodySizes).


:- pop_prolog_flag(multi_arity_warnings).


