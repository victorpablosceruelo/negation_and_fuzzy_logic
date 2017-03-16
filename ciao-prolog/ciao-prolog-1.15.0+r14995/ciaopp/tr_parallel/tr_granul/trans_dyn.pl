:- module(trans_dyn,
	[
	    annotate_gran/7, 
	    create_output_ann_program/7 
	],
	[andprolog, assertions, api(ciaopp_api)]). 

:- doc(author,"Pedro L@'{o}pez").  

% ciao library:
:- use_module(library(vndict), 
	[
	    complete_dict/3,
	    create_dict/2
	]). 
:- use_module(library(lists),    [length/2, reverse/2]).
:- use_module(library(messages), [warning_message/2]).

% ciaopp library:
:- use_module(ciaopp(preprocess_flags), [current_pp_flag/2]). 
:- use_module(infercost(init), 
	[
	    get_input_arglist_from_st/3, 
	    find_symbol_entry/3, 
	    find_symbol_field/4,
	    find_symbol_field_clause/3,
	    clause_type/2
	]).
:- use_module(infercost(gran(gran_table)), 
	[
	    find_gran_clauses_dicts/4, 
	    find_gran_field/4,  
	    insert_gran_field/4, 
	    insert_gran_clauses_type_dicts/5 
	]). 
:- use_module(infercost(size), [ith_body_literal/3]). 
% Own library
:- use_module(name_basic, [create_dyn_pred_name/2]). 
:- use_module(classify, [classify/4]). 
:- use_module(names, [create_names/9]). 
:- use_module(input_sizes, 
	[
	    get_needed_body_sizes/2, 
	    needed_input_sizes/2
	]). 
:- use_module(need_size_table, 
	[
	    actualize_nt/4, 
	    find_size/3
	]). 
:- use_module(names_table, [find_name_entry/3]). 
:- use_module(g_utility, 
	[
	    ith_element/3,  % (see ciao library)
	    is_builtin_pred/1
	]). 
:- use_module(basic_trans, 
	[
	    translate_to_internal/2, 
	    seq/2,                     
	    flat_seq_body/2,           
	    get_pred_literal/2, 
	    replace_literal/4,  
	    mark_parallel_threads_0/2
	]). 
:- use_module(gen_expre, 
	[
	    decide_type_gen_eval_expresion/4, 
	    gen_eval_expresion_using_argposdict/4, 
	    actual_time_function_parameters/7, 
	    gen_expresion/6, 
	    insert_dyn_size_info/3
	]). 
:- use_module(thresholds, [find_size_threshold/4]). 
:- use_module(dynamic_size, [generate_initial_litnodes/8]). 


:- push_prolog_flag(multi_arity_warnings,off).

annotate_gran(SCCG, ST, GT, GCG, _OutFileName, PE, NNT):- 
	classify(SCCG, GT, GCG, ST), 
	create_names(GT, ST, PE, [], [], 0, _, NT, _FL), 
        Info = info(GT, ST, NT, NS),
        needed_input_sizes(SCCG, Info), 
        actualize_nt(NT, ST, NS, NNT), 
        !,
	dyn_clas_annotate(SCCG, NNT, ST, GT). 

%% If a predicate is of sequential type then its sequential name is the same
%% as the original. If is of type do_test or supply_sizes then the sequential
%% version has the suffix "s_" (provided that there are no conflicts with 
%% other predicates in the program). 


dyn_clas_annotate([], _, _, _).
dyn_clas_annotate([Comp|SCCG], NT, ST, GT):-
	dyn_clas_annotate_comp(Comp, NT, ST, GT),
	dyn_clas_annotate(SCCG, NT, ST, GT).

% Warning. clas_annotate_comp_1 is not implemented!
% Currently the predicate is called with PE bound to the atom noname
% so that clas_annotate_comp_2 is always called.  

 %% clas_annotate_comp(Comp,NT,ST,GT,PE):-
 %% 	(member(PE,Comp)->
 %% 	        clas_annotate_comp_1(Comp,Comp,NT,ST,GT,PE);
 %% 	        clas_annotate_comp_2(Comp,Comp,NT,GT, ST)).


dyn_clas_annotate_comp(Comp, NT, ST, GT):-
	dyn_clas_annotate_comp_2(Comp, Comp, NT, GT, ST).

%%%--------------------------------------------------------------------------
% Annotates a component that no contains the entry point.
% Test if the first predicate of a component is of type sequential. If it is, 
% then all the predicates in the component are sequential and nothing is done.
%%%--------------------------------------------------------------------------

dyn_clas_annotate_comp_2([], _, _, _, _).
dyn_clas_annotate_comp_2([Pred|_], Com, NT, GT, ST):-
	find_gran_field(GT, Pred, type, Type),
	(
	    Type \== sequential ->
	    dyn_clas_annotate_comp_2(Com, NT, GT, ST)
	;
	    true
	).

dyn_clas_annotate_comp_2([], _, _, _).
dyn_clas_annotate_comp_2([Pred|Comp], NT, GT, ST):-
        dyn_clas_annotate_pred_3(Pred, NT, GT, ST),
        dyn_clas_annotate_comp_2(Comp, NT, GT, ST).

 %% A predicate is of do_test type if it does perform some 
 %% spawning. 
 %% 
 %% A predicate is of supply_sizes type if it does not perform any
 %% spawning test but has to be annotated with extra arguments standing
 %% for the sizes of input arguments that are needed by some literals
 %% in the body. The predicate simply pass the sizes to these literals.

dyn_clas_annotate_pred_3(Pred, NT, GT, ST):-
        find_gran_field(GT,Pred,par_clauses,Clauses),
        % find_name_field(NT,Pred,actual_sizes,ASizeRel),
        find_gran_field(GT, Pred, sizes, ASizeRel), 
        % Size relations from CASLOG
        dyn_gran_transform_clauses(Clauses, ASizeRel, ST, NT, GT, AClauses),
        insert_gran_field(GT, Pred, ann_clauses, AClauses).
%
% Annnotates clauses for a predicate with "supply_sizes" type by adding 
% the expresions that compute input argument sizes for literals in the body,
% and renaming these literals.
% 

dyn_gran_transform_clauses(Clauses, _ASizeRel, _ST, _NT, _GT, _AnnClauses):-
	var(Clauses),
        !.
dyn_gran_transform_clauses(Clauses, [ASizeRel|SList], ST, NT, GT, 
                           [AClause|Alist]):-
	nonvar(Clauses),
	Clauses = [Clause|CList],
	dyn_gran_transform_clause(Clause, ASizeRel, ST, NT, GT, AClause),
	dyn_gran_transform_clauses(CList, SList, ST, NT, GT, Alist).


dyn_gran_transform_clause(Clause, ASizeRel, ST, NT, GT, AClause):- 
	clause_type(Clause, Type),
	dyn_gran_transform_clause(Type, Clause, ASizeRel, ST, NT, GT, AClause).


dyn_gran_transform_clause(3, Clause, _ASizeRel, _ST, NT, _GT, AClause):- 
	functor(Clause, F, A), 
	find_name_entry(NT, F/A, st(_,_,GranName/_,InArList,_,_)),
	dyn_transform_head(GranName, InArList, Clause, AClause, _).
dyn_gran_transform_clause(2, (Head:-Body), ASizeRel, ST, NT, GT, 
                          (TranHead :- TranBody)):- 
	functor(Head, Name, Arity),
	find_name_entry(NT,Name/Arity,st(_,_,GranName/_,InArList,_,_)),
	dyn_transform_head(GranName, InArList, Head, TranHead, ArgPosDict),
	dyn_transform_body(Name/Arity, Body, ST, NT, GT, ArgPosDict, 
                           ASizeRel, TranBody).


get_size_args([], _LitNum, _Head, _ArgPosDict).
get_size_args([ArgNum|RArgNums], LitNum, Head, ArgPosDict):-
	arg(ArgNum, Head, Arg),
	insert_dyn_size_info($(LitNum, ArgNum), ArgPosDict, Arg),
	get_size_args(RArgNums, LitNum, Head, ArgPosDict).


put_size_args([], _LitNum, _TrHead, _TrArgNum, _ArgPosDict).
put_size_args([ArgNum|RArgNums], LitNum, TrHead, TrArgNum, ArgPosDict):-
	arg(TrArgNum, TrHead, TrArg),
	insert_dyn_size_info($(LitNum, ArgNum), ArgPosDict, TrArg),
	TrArgNum1 is TrArgNum + 1,
	put_size_args(RArgNums, LitNum, TrHead, TrArgNum1, ArgPosDict).


dyn_transform_head(Name, InArList, Head, TranHead, ArgPosDict):-
	functor(Head, _, Ari),
	input_args_number(InArList, ExArList, NArgList, 0, ExArgNum),
	NewAri is Ari + ExArgNum,
	ArgNum is Ari + 1,
	functor(TranHead, Name, NewAri),
        put_Normal_Arguments(TranHead, Head, Ari),
        get_size_args(NArgList, 0, Head, ArgPosDict),
        put_size_args(ExArList, 0, TranHead, ArgNum, ArgPosDict).


% Transform a body using dynamic term size computation. 
dyn_transform_body(Pred, Body, ST, NT, GT, ArgPosDict, ASizeRel, TranBody):-
	mark_parallel_threads_0(Body, Threads),
	dyn_transform_threads(Threads, 1, _LitNum, ST, NT, GT, ASizeRel,
                              ArgPosDict, TranBody1),
        get_input_arglist_from_st(ST, Pred, InSizes),  
	get_needed_body_sizes(ArgPosDict, NeededBodySizes),
	seq(Body, SeqBody),
	flat_seq_body(SeqBody, FlatBody),
        % Check whether ASizeRel is in the right format.
        generate_initial_litnodes(NeededBodySizes, ASizeRel, InSizes, 
                                  FlatBody, GT, ST, LitNodes, IrTrans),
        transform_dyn_literals_0(LitNodes, FlatBody, TranBody1,
                                 TranBody, ArgPosDict),
        transform_dyn_predicate(IrTrans, GT).


dyn_transform_threads([Lit|Litrs], InLitNum, OutLitNum, ST, NT, GT, ASizeRel, 
                      ArgPosDict, TranThread):-
        !,
        % warning, check whether we can reverse this before.
        reverse([Lit|Litrs], RLiterals),
        dyn_transform_one_thread(RLiterals, InLitNum, OutLitNum, ST, NT, GT,
                                 ASizeRel, ArgPosDict, TranThread).
dyn_transform_threads((Lit1, Literals), InLitNum, OutLitNum, ST, NT, GT,
                      ASizeRel, ArgPosDict, (GraLit1, Literals2)):-!,
        dyn_transform_threads(Lit1, InLitNum, OutLitNum1, ST, NT, GT,
                              ASizeRel, ArgPosDict, GraLit1),
        dyn_transform_threads(Literals, OutLitNum1, OutLitNum, ST, NT, GT,
                              ASizeRel, ArgPosDict, Literals2).
dyn_transform_threads(Literal, InLitNum, OutLitNum, _ST, NT, GT, ASizeRel, 
                      ArgPosDict, AnnLiteral):-
        OutLitNum is InLitNum + 1,
        create_lit_granul_version(GT, NT, Literal, ASizeRel, AnnLiteral,
                                  ArgPosDict).


dyn_transform_one_thread([Lit1, Lit2], InLitNum, OutLitNum, ST, NT,
                         GT, ASizeRel, ArgPosDict, AnnThread):-
        translate_to_internal(Lit1, InternalLit1),
        get_pred_literal(InternalLit1, LitPred1),
        % get_pred_literal(Lit1, LitPred1),
        find_symbol_field(ST, LitPred1, time, TimeFunc1),
        (
	    var(TimeFunc1) -> 
	    warning_message("No time function available for: ~q.", [LitPred1])
	; 
	    create_lit_granul_version(GT, NT, Lit1, ASizeRel, GraLit1,
                                      ArgPosDict),
            create_lit_sequential_version(GT, Lit1, SeqLit1), 
	    dyn_obtain_test(TimeFunc1, ASizeRel, Lit1, InLitNum, ArgPosDict,
                            Test1),
	    LitNum2 is InLitNum + 1,
	    OutLitNum is LitNum2, 
	    translate_to_internal(Lit2, InternalLit2),
	    get_pred_literal(InternalLit2, LitPred2),
            % get_pred_literal(Lit2, LitPred2),
	    find_symbol_field(ST, LitPred2, time, TimeFunc2),
	    (
		var(TimeFunc2) -> 
		warning_message("No time function available for: ~q.",
                                [LitPred2])
	    ; 
		create_lit_granul_version(GT, NT, Lit2, ASizeRel, GraLit2,
                                          ArgPosDict),
                create_lit_sequential_version(GT, Lit2, SeqLit2), 
		dyn_obtain_test(TimeFunc2, ASizeRel, Lit2, LitNum2,
                                ArgPosDict, Test2),
		create_annotated_thread(Test1, Test2, SeqLit1, SeqLit2,
                                        GraLit1, GraLit2, AnnThread)
	    )
	).


create_annotated_thread(Test1, Test2, SeqLit1, SeqLit2, GraLit1, GraLit2,
                        AnnThread):-
	Test1 == Test2,
	!,
	AnnThread = (
			Test2 -> (GraLit1 & GraLit2)
		    ;
			(SeqLit1 , SeqLit2)
		    ).
create_annotated_thread(Test1, Test2, SeqLit1, SeqLit2, GraLit1, GraLit2,
                        AnnThread):-
	SeqLit2 == GraLit2,      
	SeqLit1 \== GraLit1,
	!,      
	AnnThread = (Test1 -> 
                        (Test2 -> (GraLit1 & GraLit2)
                                ; (GraLit1 , SeqLit2)
			)
		    ;
                        (SeqLit1 , SeqLit2)
		    ).
create_annotated_thread(Test1, Test2, SeqLit1, SeqLit2, GraLit1, GraLit2,
                        AnnThread):-
	SeqLit1 == GraLit1,      
	SeqLit2 \== GraLit2,
	!,      
	AnnThread = (Test2 -> 
                      (Test1 -> (GraLit1 & GraLit2)
                              ; (SeqLit1 , GraLit2)
		      )
		    ;
                        (SeqLit1 , SeqLit2)
		    ).
create_annotated_thread(Test1, Test2, SeqLit1, SeqLit2, GraLit1, GraLit2,
                        AnnThread):-
	AnnThread = (Test1 -> 
                      (Test2 -> (GraLit1 & GraLit2)
                              ; (GraLit1 , SeqLit2))
                      ;
                      (Test2 -> (SeqLit1 , GraLit2)
                              ; (SeqLit1 , SeqLit2)
		      )
		    ).


% Creates the sequential version of a literal (does not perform
% granularity control).
create_lit_sequential_version(GT, Lit, AnnLit):-
	rep_seq(Lit, GT, AnnLit).

% Creates the version of a literal that performs granularity control.

create_lit_granul_version(GT, NT, Lit, ASizeRel, AnnLit, ArgPosDict):-
	(
	    is_builtin_pred(Lit) -> AnnLit = Lit
	;
	    functor(Lit, Name, Arity),
	    find_gran_field(GT, Name/Arity, type, Type),
	    (
		Type == sequential -> AnnLit = Lit
	    ;
		create_lit_granul_version_1(NT, Lit, ASizeRel, AnnLit,
                                            ArgPosDict)
	    )
	).


create_lit_granul_version_1(NT, Lit, ASizeRel, AnnLit, ArgPosDict):-
	(
	    is_builtin_pred(Lit) -> AnnLit = Lit
	;
	    functor(Lit, Name, Arity),
	    find_name_entry(NT, Name/Arity, st(_, _, GCName/_Ari, InArList,
                            _, _)),
	    input_args_number(InArList, ExArList, _, 0, ExArgNum),
	    Ari is Arity + ExArgNum,
	    functor(NewLit, GCName, Ari),
	    put_Normal_Arguments(NewLit, Lit, Arity),
	    (
		ExArgNum > 0 ->
		InputArgNum is Arity + 1, 
		create_lit_granul_version_2(Lit, InputArgNum, ExArList,
                                            ASizeRel, 
		NewLit, AnnLit, ArgPosDict)
	    ;
		AnnLit = NewLit
	    )
	).
 

create_lit_granul_version_2(_, _, [], _, NewLit, NewLit, _ArgPosDict).
create_lit_granul_version_2(Lit, InputArgNum, [ArgNum|ANumList], ASizeRel,
		            NewLit, SizeExp, ArgPosDict):-
        find_size(ASizeRel, Lit/ArgNum, Size),
	decide_type_gen_eval_expresion(Size, Exp, SizeArg, ArgPosDict),
	(var(Exp)->
	      SizeExp = NewSizeExp
	;
              SizeExp = (Exp, NewSizeExp)
	),
	arg(InputArgNum, NewLit, SizeArg),
	NewInArgNum is InputArgNum + 1,
	create_lit_granul_version_2(Lit, NewInArgNum, ANumList,
                                    ASizeRel, NewLit, NewSizeExp, ArgPosDict).


% Warning!, only works if the TimeFunc is on one variable. 
dyn_obtain_test([TimeFunc], SizeRel, Literal, LitNum, ArgPosDict, 
                (ActVar > SizeThreshold)):-
        actual_time_function_parameters(TimeFunc, SizeRel, Literal,
                                LitNum, ActTimFunc, ArgPosDict, NeededVars),
        nonvar(NeededVars),
	NeededVars = [(SizeVar, _Var)| Rest],
	var(Rest), !, 
	current_pp_flag(granularity_threshold, Thres),
	find_size_threshold(ActTimFunc, [(SizeVar, 1)], Thres, SizeThreshold),
	insert_dyn_size_info(SizeVar, ArgPosDict, ActVar).
% Here create the expression that compute the cost and compare with
% the threshold. 
dyn_obtain_test(_, _SizeRel, _Literal, _LitNum, _, '_7634 < 10').


%%
%% Create the entry point clause of a predicate of type do_test or
%% supply_sizes.  Introduce calls to preicates that compute input data
%% sizes before calling the version taht performs gran. control.
%%

dyn_create_entry_point(Pred, ST, NT, (EntryHead :- EntryBody)):-
	find_symbol_entry(ST, Pred, st(Pred, _, _, Measure,_,_,_,_,_,_)),
	find_name_entry(NT,Pred,st(_,_,GranName/_,InArList,_,_)),
	Pred = Name/Arity,
	functor(EntryHead, Name, Arity),
	input_args_number(InArList, ExArList, _, 0, ExArgNum),
	Ari is Arity + ExArgNum,
	functor(GranulCall, GranName, Ari),
	put_Normal_Arguments(GranulCall, EntryHead, Arity),
	(
	    ExArgNum > 0 ->
	    InputArgNum is Arity + 1, 
	    dyn_create_entry_body(ExArList, InputArgNum, EntryHead,  
                                  Measure, GranulCall, EntryBody)
        ;
	    EntryBody = GranulCall
	).


put_Normal_Arguments(_,_,0).
put_Normal_Arguments(TranHead,Head,ArgNum):-
	ArgNum > 0,
	arg(ArgNum,Head,Arg),
	arg(ArgNum,TranHead,Arg),
	NewArgNum is ArgNum - 1,
	put_Normal_Arguments(TranHead,Head,NewArgNum).


dyn_create_entry_body([], _ArgNum, _EntryHead, _Measure,
                      GranulCall, GranulCall).
dyn_create_entry_body([NeedArgNum|ANumList], ArgNum,  
		       EntryHead, MeasureList, GranulCall, Body):-
        ith_element(MeasureList, NeedArgNum, Measure),
	arg(NeedArgNum, EntryHead, HeadArg), 
	create_size_comp_literal(Measure, HeadArg, Literal, ResVar),
	arg(ArgNum, GranulCall, ResVar),
	Body = (Literal, NewBody),
	NewArgNum is ArgNum + 1,
	dyn_create_entry_body(ANumList, NewArgNum, EntryHead,
                              MeasureList, GranulCall, NewBody).


create_size_comp_literal(length, InputArg, length(InputArg, ResVar), ResVar).
create_size_comp_literal(size, InputArg, term_size(InputArg, ResVar), ResVar).
create_size_comp_literal(depth([ChildList]), InputArg,
                         depth(InputArg, [ChildList], ResVar), ResVar).


create_output_ann_program(_ST,GT,_NT,AnnCla,AnnCla, Dicts, Dicts):-
	var(GT),!.
create_output_ann_program(ST,GT,NT,AnnotatedClauses,AnnClausesTail, Dicts,
                          DictsTail):-
	nonvar(GT),
	GT = [st(Pred,_,ParClauses,AnnClauses,_,_,Dict)|GTail],
	(
	    nonvar(AnnClauses)->
	    dyn_create_entry_point(Pred, ST, NT, EntryClause), !,
	    clause_to_internal_ciao_format(EntryClause, EntryClause1), 
	    AnnotatedClauses = [(EntryClause1,0)|AnnCl2],
	    create_dict(EntryClause,EntryDict1),
	    EntryDict1 = dic(L1,_),
	    length(L1,Size),
	    create_init_name_dicts(Size,L2),
	    EntryDict  = dic(L1,L2),
	    Dicts = [EntryDict|TemDic],
	    put_in_list(AnnClauses, Dict, AnnCl2, AnnCl3, TemDic, TemDic1),
	    find_symbol_field_clause(ST,Pred,SeqClauses), 
	    annotate_sequential_version(SeqClauses,Dict,GT,AnnCl3,
                                        TempClausesTail,TemDic1,TemDic2) 
        ;
	    put_in_list(ParClauses, Dict, AnnotatedClauses,TempClausesTail,
                        Dicts, TemDic2)
	),
	create_output_ann_program(ST,GTail,NT,TempClausesTail,
                                      AnnClausesTail,TemDic2, DictsTail).


% To improve!
create_init_name_dicts(N,L) :-
	create_init_name_dicts_(N,0,L).
create_init_name_dicts_(0,_,[]) :- !.
create_init_name_dicts_(N,M,[X|L]) :-
	M1 is M + 0'A,
	atom_codes(X,[M1]),
	NN is N - 1,
	MM is M + 1,
	create_init_name_dicts_(NN,MM,L).


% put_in_list(+AnnClauses, +Dict, -AnnCl, -AnnCl, -Dicts, -Dicts).
put_in_list(AnnClauses, Dict, AnnCl, AnnCl, Dicts, Dicts):-
	var(AnnClauses),
	!,
	var(Dict).
put_in_list(AnnClauses, Dicts, AnnCl1, AnnCl2, OuDicts, DictsTail):-
	nonvar(AnnClauses), !,
	nonvar(Dicts),
	AnnClauses = [Clause|Rest],
	Dicts = [Dict|RDic],
	complete_dict(Dict,Clause,NewDict), 
     % output
	clause_to_internal_ciao_format(Clause, Clause1), 
	AnnCl1 = [(Clause1,0)|Anncl3],
	OuDicts = [NewDict|TempDic],
	put_in_list(Rest, RDic, Anncl3, AnnCl2, TempDic, DictsTail).


clause_to_internal_ciao_format((H:-B), clause(H,B)):-!.
clause_to_internal_ciao_format((:-D), directive(D)):-!.
clause_to_internal_ciao_format((H), clause(H,true)).


annotate_sequential_version(Clauses,Dict,_,AnnClauses,AnnClauses,Dict,Dict) :-
	var(Clauses),
	!,
	var(Dict).
annotate_sequential_version(Clauses,Dict,GT,AnnClauses,AnnClausesTail,Dict1,
                            DictTail):-
	nonvar(Clauses),         
        nonvar(Dict),
	Clauses = [Clause|ResClauses],
        Dict = [Dic|DicList],
	create_dict(Clause,Dic1),
	Dic1    = dic(L1,_),
	Dic     = dic(_,L4),
	Diction = dic(L1,L4),
        clause_type(Clause,Type), 
        (
	    Type == 3 -> 
	    sequentialize_goals(Clause, GT, SeqClause)
	;
	    (Clause = (Head :- Body), 
	     sequentialize_goals(Head, GT, SHead), 
	     sequentialize_goals(Body, GT, SBody),
	     SeqClause = (SHead :- SBody))
	),
	clause_to_internal_ciao_format(SeqClause, Clause1),
	AnnClauses = [(Clause1,0)|AnnTail],
	Dict1 = [Diction|Dic2],
	annotate_sequential_version(ResClauses,DicList,GT,AnnTail,
	                            AnnClausesTail,Dic2,DictTail).


%% sequentialize_goals(Body, GT, NT, NewBody):-
%% 	seq(Body, SeqBody), flat_seq_body(SeqBody, FlatSeqBody),
%%         rep_seq_body(FlatSeqBody, GT, NT, NewBody).
sequentialize_goals(Body, GT, FlatSeqBody):-
	rep_seq(Body, GT, SeqBody),
	flat_seq_body(SeqBody, FlatSeqBody).

%%---------------------------------------------------------------------
% Sequentialize a body and replace literals of do_test or supply_sizes
% type by their sequential versions.
%%---------------------------------------------------------------------

rep_seq((GoalA,GoalB) , GT, (SeqGoalA,SeqGoalB) ):- !,
	rep_seq(GoalA, GT, SeqGoalA),
	rep_seq(GoalB, GT, SeqGoalB).
rep_seq(( _  => SubGoal ) , GT, SeqSubGoal ):- !,
	rep_seq( SubGoal , GT, SeqSubGoal).
rep_seq((GoalA & GoalB) ,  GT, (SeqGoalA,SeqGoalB) ):- !,
	rep_seq(GoalA,  GT,  SeqGoalA),
	rep_seq(GoalB,  GT, SeqGoalB).
rep_seq('andprolog_rt:&'(GoalA,GoalB) ,  GT, (SeqGoalA,SeqGoalB) ):- !,
	rep_seq(GoalA,  GT,  SeqGoalA),
	rep_seq(GoalB,  GT, SeqGoalB).
rep_seq(( _ -> _ ; ElseGoal ) , GT, SeqElseGoal ):- !,
	rep_seq(ElseGoal, GT, SeqElseGoal).
rep_seq(Lit, GT, NewLit ):- !,
        functor(Lit, Name, Arity),
        find_gran_field(GT, Name/Arity, type, Type),
	(
	    ((Type == do_test);(Type == supply_sizes)) ->
	      (%find_name_field(NT, Name/Arity, seq_name, SeqName),
		  (
		      get_mod_pred(Name,Mod,Pred1) ->
		      atom_concat('s_',Pred1,NewPred1), 
		      atom_concat(Mod,':',NewMod1),
		      atom_concat(NewMod1,NewPred1,NewName)
		  ;
		      atom_concat('s_', Name, NewName)
		  ),
		  functor(NewLit, NewName, Arity),
		  put_Normal_Arguments(NewLit,Lit,Arity)
	      )
 	;
	    NewLit = Lit
	).


 %% transform_dyn_literals_0(+LitNodes, +FlatBody, +InTranBody, -OuTranBody,
 %%                          ?ArgPosDict) 
 %% LitNodes: a list with literal nodes.
 %% FlatBody: the sequentialized flattened version of the body to be
 %%           annotated with granularity control. 
 %% InTranBody: Body with annotated threads.
 %% OuTranBody: Body with annotated threads and literals that perform dynamic
 %%             size computation.
 %% ArgPosDict: argument position dictionary. 

transform_dyn_literals_0(LitNodes,_FlatBody,TranBody,TranBody,_ArgPosDict):-
	var(LitNodes), !.
transform_dyn_literals_0(LitNodes,FlatBody,InTranBody,OuTranBody,ArgPosDict):-
	nonvar(LitNodes),
	LitNodes = [LitNode|Nodes], 
	LitNode = litnode(LitNum1, _Label, _CompSizes),
	ith_body_literal(LitNum1, FlatBody, DynLiteral),
	transform_dyn_lit_1(DynLiteral, LitNode, TrDynLiteral, ArgPosDict),
	replace_literal(InTranBody, DynLiteral, TrDynLiteral, TemTranBody),
	transform_dyn_literals_0(Nodes, FlatBody, TemTranBody, OuTranBody,
                                 ArgPosDict). 


% Process each tranformation node.
transform_dyn_predicate([], _GT).
transform_dyn_predicate([Node|Trans], GT):-
	Node = tn(Label,Graphs),
	Label = label(Pred, _InSizes, _OutSizes),
	find_gran_clauses_dicts(GT, Pred, Clauses, Dicts),
	create_dyn_pred_name(Label, TrPred), 
	transform_dyn_clauses(Clauses, TrPred, Label, Graphs, TrClauses),
	insert_gran_clauses_type_dicts(GT, TrPred, TrClauses, sequential,
	                               Dicts),
        transform_dyn_predicate(Trans, GT).


transform_dyn_clauses(Clauses, _TrPred, _Label, _Graphs, _TrClauses):-
	var(Clauses), !.
transform_dyn_clauses(Clauses, TrPred, Label, Graphs, [TrClaus|TrClauses]):-
	nonvar(Clauses), 
	Clauses = [Claus|RClauses],
	nonvar(Graphs),
	Graphs = [ClGraph|RGraphs],  
	transform_dyn_one_clause(Claus, TrPred, Label, ClGraph, TrClaus),
	transform_dyn_clauses(RClauses, TrPred, Label, RGraphs, TrClauses).


transform_dyn_one_clause(Clause, TrPred, Label, ClGraph, TrClause):-
	clause_type(Clause, Type),
	transform_dyn_one_clause(Type,TrPred,Label,Clause,ClGraph,TrClause).


% A fact
transform_dyn_one_clause(3, TrPred, Label, Claus, ClGraph, TrClaus):-
	ClGraph = sdg(CompSizes, _BodySizes),
	transform_dyn_atom(Label, 0, Claus, TrPred, TrHead, ArgPosDict), 
	create_required_size_comp_expressions(CompSizes, CompSizeExp,
                                              _Last, ArgPosDict),
        (
	    var(CompSizeExp) -> 
	    TrClaus = TrHead 
	; 
	    create_comp_size_exp_body(CompSizeExp, TrBody),
	    TrClaus = (TrHead :- TrBody)
	).
% A rule
transform_dyn_one_clause(2, TrPred, Label, (Head:-Body), ClGraph, 
                         (TrHead:-TrBody)):-
        ClGraph = sdg(CompSizes, BodySizes),
	transform_dyn_atom(Label, 0, Head, TrPred, TrHead, ArgPosDict), 
	create_required_size_comp_expressions(CompSizes, CompSizeExp,
                                              _Last, ArgPosDict),
        transform_dyn_body(Body, 1, BodySizes, TrBody1, ArgPosDict),
	create_comp_size_exp_body(CompSizeExp, ExpBody),
	TrBody = (TrBody1, ExpBody).


create_required_size_comp_expressions([],CompSizeExp,CompSizeExp,_ArgPosDict).
create_required_size_comp_expressions([(Pos,SizeExpre)|RSizeExps],
                                      CompSizeExp,Last,ArgPosDict):-
        gen_expresion(_ST, SizeExpre, _FNum, _NFNum, _FN, NSizeExp),
	gen_eval_expresion_using_argposdict(NSizeExp,Exp,SizeArg,ArgPosDict),
	(
	    var(Exp)->
	    CompSizeExp = NewCompSizeExp 
	;
	    CompSizeExp = (Exp,NewCompSizeExp)
	),
	insert_dyn_size_info(Pos, ArgPosDict, SizeArg),
	create_required_size_comp_expressions(RSizeExps,NewCompSizeExp,Last,
                                              ArgPosDict).


create_comp_size_exp_body(CompSizeExp, TrBody):-
	nonvar(CompSizeExp),
	CompSizeExp = (Exp, RExp),
	(
	    var(RExp) -> 
	    TrBody = Exp
	; 
	    TrBody = (Exp,TrBody1),
	    create_comp_size_exp_body(RExp, TrBody1)
	).


input_args_number([], [], [], R, R).
input_args_number([(h,ArNum)|InArList], ExtraArgList, 
                   [ArNum|NorArgList], IExArNum, OExArNum):-
        input_args_number(InArList, ExtraArgList, NorArgList, IExArNum, 
                          OExArNum).
input_args_number([(e,ArNum)|InArList], [ArNum|ExtraArgList], 
                  NorArgList, IExArNum, OExArNum):-
        ExArNum is IExArNum + 1,
	input_args_number(InArList, ExtraArgList, NorArgList, ExArNum, 
                          OExArNum).
%% input_args_number([(AType,ArNum)|InArList], ExtraArgList, IExArNum,
%%                   OExArNum):-
%%           (AType == h -> ExtraArgList = ExArList,
%%                          ExArNum is IExArNum
%%                       ;  ExtraArgList = [ArNum|ExArList],
%%                          ExArNum is IExArNum + 1),
%%   input_args_number(InArList, ExArList, ExArNum, OExArNum).


transform_dyn_atom(Label, LitNum, Atom, TrPred/TrA, TrAtom, ArgPosDict):-
	Label = label(_Pred/A, InSizes, OutSizes), 
	functor(TrAtom, TrPred, TrA),  
	put_Normal_Arguments(TrAtom, Atom, A),
	A1 is A + 1,
	put_size_args(InSizes, LitNum, TrAtom, A1, ArgPosDict),
	length(InSizes, L),
	A2 is A + L + 1,
	put_size_args(OutSizes, LitNum, TrAtom, A2, ArgPosDict).


transform_dyn_body((Lit,Body),LitNum,BodySizes,(TrLit,TrBody),ArgPosDict):-
        !,
	transform_dyn_lit(Lit, LitNum, BodySizes, TrLit, ArgPosDict),
	LitNum1 is LitNum + 1,
	transform_dyn_body(Body, LitNum1, BodySizes,  
                           TrBody, ArgPosDict).
transform_dyn_body(Lit, LitNum, BodySizes, TrLit, ArgPosDict):-
	transform_dyn_lit(Lit, LitNum, BodySizes, TrLit, ArgPosDict).


transform_dyn_lit(Lit, LitNum, BodySizes, TrLit, ArgPosDict):-
	(
	    find_lit_node(BodySizes, LitNum, LitNode) ->
	    transform_dyn_lit_1(Lit, LitNode, TrLit, ArgPosDict)
	;
	    TrLit = Lit
	).


find_lit_node([LitNode1|_BodySizes], LitNum, LitNode1):-
	LitNode1 = litnode(LitNum1, _Label, _CompSizes),
	LitNum1 =:= LitNum, !. 
find_lit_node([LitNode1|BodySizes], LitNum, OutLitNode):-
	LitNode1 = litnode(LitNum1, _Label, _CompSizes),
	LitNum1 =\= LitNum,  
	find_lit_node(BodySizes, LitNum, OutLitNode).


transform_dyn_lit_1(Lit, LitNode, TrLit, ArgPosDict):-
	LitNode = litnode(LitNum, Label, CompSizes),
	create_dyn_pred_name(Label, TrPred),
	transform_dyn_atom(Label, LitNum, Lit, TrPred, TrLit1, ArgPosDict), 
	create_required_size_comp_expressions(CompSizes, CompSizeExp,
                                              Last, ArgPosDict),
        Last = TrLit1, 
	TrLit = CompSizeExp.     

:- pop_prolog_flag(multi_arity_warnings).
