:- module(unfold_,[unfold/8],[]).

:- use_package(.(nomem_usage_)).

:- use_package(assertions).

:- entry unfold(SelRule,Unfold,Sg,Sv,Proj,AbsInt,NF,A) : 
	(ground(SelRule),ground(Unfold),ground(AbsInt),
	 var(NF),var(A),
	 indep(NF,A),indep(NF,Sg),indep(NF,Sv),indep(NF,Proj),
	 indep(A,Sg),indep(A,Sv),indep(A,Proj)
	 ).

:- include(unfold_local_).

:- use_module(engine(hiord_rt), ['$meta_call'/1]).

:- doc(title,"A Partial Evaluator Integrated with Abstract Interpretation").

:- doc(author, "Germ@'{a}n Puebla").
:- doc(author, "Elvira Albert").

:- doc(module," This module contains the implementation of local
     control for partial evaluation to be used in conjunction with
     PLAI.").

:- doc(bug,"does not seem to work if the program contains negation
      as failure (and possibly, meta-calls in general).").

:- doc(bug,"for exported predicates, if no clause remains after 
	specialization, a clause with 'fail' in its body should be added.").

:- use_module(sp_clauses_, 
	[ add_all_clauses/4,
	  literal_for_orig_pred/1,
	  collect_orig_clauses/2,
	  collect_one_orig_clause/2
	]).
:- use_module(unfold_builtins_, [can_be_evaluated/1]).
:- use_module(homeo_emb_, [homeomorphic_embedded/2]).
%:- use_module(abs_exec),[abs_exec/4]).
:- use_module(unfold_operations_, [replicate_atoms/4]).
%% :- use_module(useless_clauses),
%% 	[ decide_remove_useless_pre/6,
%% 	  decide_remove_useless_post/6
%% 	]).
:- use_module(mem_usage_, 
	[readjust_max_mem_usage/0, update_mem_usage/0]).
:- use_module(unfold_times_, 
	[global_time_ellapsed/3,
	 increment_unfold_time/1,
	 increment_transformation_time/1
	]).

:- use_module('..'(preprocess_flags_), [current_pp_flag/2]).
%:- use_module(plai(transform),[transform_clauses/5]).
:- use_module('..'(p_unit(p_unit_)), 
 	[ new_predicate/3,
 	  program/2]).
:- use_module('..'(p_unit(p_unit_)), [type_of_goal/2]).
:- use_module('..'(p_unit(clidlist_)), 
 	[ rewrite_source_all_clauses/2]).
%% 
:- use_module(library(vndict), [create_pretty_dict/2]).
:- use_module(library(aggregates), [findall/3]).
:- use_module(library(prolog_sys), [statistics/2]).
:- use_module(library(write), [write/1]).
:- use_module(library(lists), [append/3, reverse/2]).

unfold(SelRule,Unfold,Sg,Sv,Proj,AbsInt,NF,A):-
	statistics(runtime,[GT,_]),
	functor(Sg,F,A),
	new_predicate(F,A,NF),
	readjust_max_mem_usage,
	unfold__(SelRule,Unfold,Sg,Sv,Proj,AbsInt,F,A,UnfClauses0),
	statistics(runtime,[_,T_u]),
	inform_user(['{unfolded in ',T_u, ' msec.}']),
	increment_unfold_time(T_u),
%      	decide_remove_useless_post(UnfClauses0,AbsInt,Sg,Sv,Proj,UnfClauses),
	UnfClauses = UnfClauses0,
	add_all_clauses(UnfClauses,NF,A,NewClauses),
	create_dicts_and_recs(NewClauses,_Dicts,_Recs),
	rewrite_source_all_clauses(NewClauses,_NClauses),
%	transform_clauses(NClauses,Dicts,Recs,notarjan,AbsInt),
	statistics(runtime,[GT1,_]),
	global_time_ellapsed(GT1,GT,TT),
	increment_transformation_time(TT),
	inform_user(['{total transformation time ',TT, ' msec.}']).
	

unfold__(SelRule,df_tree_hom_emb,Sg,_Sv,_Proj,AbsInt,_F,_A,UnfClauses0):-!,
	perform_unfolding_depth_tree(SelRule,AbsInt,Sg,UnfClauses0).

unfold__(SelRule,df_hom_emb,Sg,_Sv,_Proj,AbsInt,_F,_A,UnfClauses0):-!,
	perform_unfolding_depth_hom_emb(SelRule,AbsInt,Sg,UnfClauses0).

unfold__(SelRule,df_hom_emb_as,Sg,_Sv,_Proj,AbsInt,_F,_A,UnfClauses0):-!,
	perform_unfolding_depth(SelRule,AbsInt,Sg,UnfClauses0).

unfold__(SelRule,Unfold,Sg,Sv,Proj,AbsInt,F,A,UnfClauses0):-
	collect_definition(Unfold,F,A,Sg,Clauses),
%	decide_remove_useless_pre(Clauses,AbsInt,Sg,Sv,Proj,Clauses0),
	Clauses0 = Clauses,
	perform_unfolding(SelRule,Unfold,AbsInt,Sg,Sv,Proj,Clauses0,UnfClauses0).

collect_definition(orig,F,A,_Sg,Clauses):-!,
	functor(Temp,F,A),
	collect_orig_clauses(Temp,Clauses).
collect_definition(_Unfold,_F,_A,Sg,Clauses):-
	collect_orig_clauses(Sg,Clauses).

perform_unfolding(_SelRule,orig,_,_,_,_,Clauses,UnfClauses):-
	UnfClauses = Clauses.
perform_unfolding(_SelRule,inst,_,_,_,_,Clauses,UnfClauses):-
	UnfClauses = Clauses.
perform_unfolding(SelRule,det,AbsInt,_,_,_,Clauses,UnfClauses):-
	unfold_while_deterministic(SelRule,Clauses,1,AbsInt,UnfClauses).
perform_unfolding(SelRule,det_la,AbsInt,_,_,_,Clauses,UnfClauses):-
	current_pp_flag(unf_depth,K),
	unfold_while_deterministic(SelRule,Clauses,K,AbsInt,UnfClauses).
perform_unfolding(SelRule,depth,AbsInt,_,_,_,Clauses,UnfClauses):-
	current_pp_flag(unf_depth,K),
	unfold_depthk(SelRule,Clauses,AbsInt,UnfClauses,K).
perform_unfolding(SelRule,first_sol,AbsInt,_,_,_,Clauses,UnfClauses):-
	unfold_first_sol(SelRule,Clauses,AbsInt,UnfClauses).
perform_unfolding(SelRule,first_sol_d,AbsInt,_,_,_,Clauses,UnfClauses):-
	current_pp_flag(unf_depth,K),
	unfold_first_sol_or_depth(SelRule,Clauses,AbsInt,UnfClauses,K).
perform_unfolding(SelRule,all_sol,AbsInt,_,_,_,Clauses,UnfClauses):-
	unfold_all_sol(SelRule,Clauses,AbsInt,UnfClauses).
perform_unfolding(SelRule,hom_emb,AbsInt,Sg,_,_,Clauses,UnfClauses):-
	copy_term(Sg,NSg),
	replicate_atoms(Clauses,[NSg],[],NAs),
	unfold_hom_emb(SelRule,Clauses,NAs,AbsInt,UnfClauses).
perform_unfolding(SelRule,hom_emb_anc,AbsInt,Sg,_,_,Clauses,UnfClauses):-
	copy_term(Sg,NSg),
	initial_ancestors(Clauses,NSg,AClauses),
	unfold_hom_emb_ancestors(SelRule,AClauses,AbsInt,AUnfClauses),
	remove_ancestors(AUnfClauses,UnfClauses).
perform_unfolding(SelRule,hom_emb_as,AbsInt,Sg,_Sv,_Proj,Clauses,UnfClauses):-
	copy_term(Sg,NSg),
	replicate_atoms(Clauses,[NSg],[],NAs),
	unfold_hom_emb_local_as(SelRule,Clauses,NAs,AbsInt,UnfClauses).
perform_unfolding_depth(SelRule,AbsInt,Sg,UnfClauses):-
	findall(UnfClause,depth_first_emb_local_0(SelRule,AbsInt,Sg,UnfClause),UnfClauses).
perform_unfolding_depth_tree(SelRule,AbsInt,Sg,UnfClauses):-
	findall(UnfClause,depth_first_emb_tree_0(SelRule,AbsInt,Sg,UnfClause),UnfClauses).
perform_unfolding_depth_hom_emb(SelRule,AbsInt,Sg,UnfClauses):-
	findall(UnfClause,depth_first_emb_hom_emb_0(SelRule,AbsInt,Sg,UnfClause),UnfClauses).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%               DETERMISTIC UNFOLDING               %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- doc(unfold_while_deterministic/5,"Implements an unfolding rule
     which expands derivations while they are deterministic and stops
     them otherwise.").

unfold_while_deterministic(_SelRule,[],_,_AbsInt,[]).
unfold_while_deterministic(SelRule,[clause(Sg,[])|Clauses],K,AbsInt,[clause(Sg,[])|NCls]):-!,
	unfold_while_deterministic(SelRule,Clauses,K,AbsInt,NCls).
unfold_while_deterministic(SelRule,[clause(Sg,Body)|Clauses],K,AbsInt,Result):-
	(unfold_deterministically_in_k_steps(SelRule,K,[clause(Sg,Body)],AbsInt,UnfClauses) ->
	    debug(yes),
	    (UnfClauses == [] ->
	        unfold_while_deterministic(SelRule,Clauses,K,AbsInt,Result)
	    ; 
		UnfClauses = [clause(Sg,OtherBody)],
		unfold_while_deterministic(SelRule,[clause(Sg,OtherBody)|Clauses],K,AbsInt,Result)
	    )
	;
	    debug(no),
	    Result = [clause(Sg,Body)|NClauses],
	    unfold_while_deterministic(SelRule,Clauses,K,AbsInt,NClauses)).
	

is_deterministic([]).
is_deterministic([residual(_)]):-!,fail.
is_deterministic([_]).

:- doc(unfold_deterministically_in_k_steps/5,"Implements an
unfolding rule which expands derivations while they are deterministic
and their length is less than @tt{k}.").

unfold_deterministically_in_k_steps(SelRule,K,Clauses,AbsInt,UnfClauses):-
	K > 0,
	unfold_all_clauses_one_step(SelRule,Clauses,AbsInt,TmpClauses),!,
	(is_deterministic(TmpClauses) ->
	    UnfClauses = TmpClauses
	;
	    K1 is K - 1,
	    unfold_deterministically_in_k_steps(SelRule,K1,TmpClauses,AbsInt,UnfClauses)
	).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%              FIRST SOLUTION UNFOLDING             %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

unfold_first_sol(_SelRule,[],_AbsInt,[]):-!.
unfold_first_sol(SelRule,Clauses,AbsInt,UnfClauses):-
	(there_is_solution_or_residual(Clauses) ->
	    peel_residual(Clauses,UnfClauses)
	;
	    unfold_all_clauses_one_step(SelRule,Clauses,AbsInt,UnfClauses1Step),
	    unfold_first_sol(SelRule,UnfClauses1Step,AbsInt,UnfClauses)).

unfold_first_sol_or_depth(_SelRule,[],_AbsInt,[],_):-!.
unfold_first_sol_or_depth(_SelRule,Clauses,_AbsInt,UnfClauses,0):-!,
	peel_residual(Clauses,UnfClauses).
unfold_first_sol_or_depth(SelRule,Clauses,AbsInt,UnfClauses,K):-
	(there_is_solution_or_residual(Clauses) ->
	    peel_residual(Clauses,UnfClauses)
	;
	    unfold_all_clauses_one_step(SelRule,Clauses,AbsInt,UnfClauses1Step),
	    K1 is K - 1,
	    unfold_first_sol_or_depth(SelRule,UnfClauses1Step,AbsInt,UnfClauses,K1)).
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%              ALL SOLUTIONS UNFOLDING              %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

unfold_all_sol(_SelRule,[],_AbsInt,[]):-!.
unfold_all_sol(SelRule,Clauses,AbsInt,UnfClauses):-
	(all_solutions_or_residual(Clauses) ->
	    peel_residual(Clauses,UnfClauses)
	;
	    unfold_all_clauses_one_step(SelRule,Clauses,AbsInt,UnfClauses1Step),
	    unfold_all_sol(SelRule,UnfClauses1Step,AbsInt,UnfClauses)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%                 DEPTH-K UNFOLDING                 %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

unfold_depthk(_SelRule,[],_AbsInt,[],_):-!.
unfold_depthk(_SelRule,Clauses,_AbsInt,UnfClauses,0):-!,
	peel_residual(Clauses,UnfClauses).
unfold_depthk(SelRule,Clauses,AbsInt,UnfClauses,K):-
	(all_solutions_or_residual(Clauses) ->
	    peel_residual(Clauses,UnfClauses)
	;
	    unfold_all_clauses_one_step(SelRule,Clauses,AbsInt,UnfClauses1Step),
	    K1 is K-1,
	    unfold_depthk(SelRule,UnfClauses1Step,AbsInt,UnfClauses,K1)
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%          COMMON CODE TO THE ABOVE STRATEGIES      %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


unfold_all_clauses_one_step(_SelRule,[],_AbsInt,[]).
unfold_all_clauses_one_step(SelRule,[Cl|Clauses],AbsInt,Result):-
	unfold_one_step(SelRule,Cl,AbsInt,Result,MoreClauses),
	unfold_all_clauses_one_step(SelRule,Clauses,AbsInt,MoreClauses).

unfold_one_step(_SelRule,residual(Cl),_AbsInt,NCls,Cont):-!,
	NCls = [residual(Cl)|Cont].
unfold_one_step(_SelRule,fact(Cl),_AbsInt,NCls,Cont):-!,
	NCls = [fact(Cl)|Cont].
unfold_one_step(SelRule,clause(Sg,Body),AbsInt,NCls,Cont):-
	(Body == [] ->
	    NCls = [fact(clause(Sg,Body))|Cont]
	;   
	    select_atom(SelRule,Body,NewBody,[],_Emb),
	    NewBody = [L|_],
	    copy_term(L,L1),
	    (unfold_literal_if_possible(L1,AbsInt,UnfClauses) ->
	        debug(yes),
		(UnfClauses == [] ->
		    NCls  = Cont
		; 
		    form_rules(UnfClauses,clause(Sg,NewBody),NCls,Cont)
		)
	    ;
		debug(no),
		NCls = [residual(clause(Sg,NewBody))|Cont]	    
	    )
	).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%     NAIVE UNFOLDING WITH EMBEDDING        %%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- doc(unfold_hom_emb(SelRule,Clauses,Atoms,AbsInt,UnfClauses),"This
    predicate implements an unfolding rule which stops a derivation when
    the selected atom subsumes a previously selected one according to the
    embedding ordering. An efficient implementation using ancestor stacks
    is done in predicate @tt{unfold_hom_emb_local_as}").

unfold_hom_emb(_SelRule,[],_,_AbsInt,[]):-!.
unfold_hom_emb(SelRule,Clauses,Atoms,AbsInt,UnfClauses):-
	(all_solutions_or_residual(Clauses) ->
	    peel_residual(Clauses,UnfClauses)
	;
	    unfold_all_clauses_with_atoms(SelRule,Clauses,Atoms,AbsInt,UnfClauses1Step,NewAtoms),
	    unfold_hom_emb(SelRule,UnfClauses1Step,NewAtoms,AbsInt,UnfClauses)).


unfold_all_clauses_with_atoms(_SelRule,[],[],_AbsInt,[],[]).
unfold_all_clauses_with_atoms(SelRule,[Cl|Clauses],[A|As],AbsInt,NClauses,NAtoms):-
	unfold_one_step_with_atoms(SelRule,Cl,A,AbsInt,NClauses,MoreClauses,NAtoms,MoreAtoms),
	unfold_all_clauses_with_atoms(SelRule,Clauses,As,AbsInt,MoreClauses,MoreAtoms).

unfold_one_step_with_atoms(_SelRule,residual(Cl),A,_AbsInt,NCls,Cl_Cont,NAs,As_Cont):-!,
	NCls = [residual(Cl)|Cl_Cont],
	NAs = [A|As_Cont].
unfold_one_step_with_atoms(_SelRule,fact(Cl),A,_AbsInt,NCls,Cl_Cont,NAs,As_Cont):-!,
	NCls = [fact(Cl)|Cl_Cont],
	NAs = [A|As_Cont].
unfold_one_step_with_atoms(_SelRule,clause(Sg,[]),A,_AbsInt,NCls,Cl_Cont,NAs,As_Cont):-!,
	NCls = [fact(clause(Sg,[]))|Cl_Cont],
	NAs = [A|As_Cont].
unfold_one_step_with_atoms(SelRule,clause(Sg,Body),A,_AbsInt,NCls,Cl_Cont,NAs,As_Cont):-
	select_atom(SelRule,Body,NewBody,A,_Emb),
	NewBody = [Lit|_],
	functor(Lit,F,Arity),
	functor(Atom,F,Arity),
 	member(Atom,A),
	debug(hom_emb),
 	homeomorphic_embedded(Atom,Lit),!,
	debug(embedded),
	NCls = [residual(clause(Sg,NewBody))|Cl_Cont],
	NAs = [A|As_Cont].

unfold_one_step_with_atoms(SelRule,clause(Sg,Body),A,AbsInt,NCls,Cl_Cont,NAs,As_Cont):-
	select_atom(SelRule,Body,NewBody,A,_Emb),
	NewBody = [L|_],
	copy_term(L,L1),
	(unfold_literal_if_possible(L1,AbsInt,UnfClauses) ->
	    debug(yes),
	    (UnfClauses == [] ->
	        NCls = Cl_Cont,
		NAs = As_Cont
	    ; 
		form_rules(UnfClauses,clause(Sg,NewBody),NCls,Cl_Cont),
		(UnfClauses = [clause(_,[])] ->
		    NA = A
		;
		    copy_term(L1,NL1),
		    NA = [NL1|A]
		),
		replicate_atoms(UnfClauses,NA,As_Cont,NAs)
	    )
	;
	    debug(no),
	    NCls = [residual(clause(Sg,NewBody))|Cl_Cont],
	    NAs = [A|As_Cont]
	).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%     NAIVE UNFOLDING WITH EMBEDDING AND ANCESTORS       %%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

unfold_hom_emb_ancestors(_SelRule,[],_AbsInt,[]):-!.
unfold_hom_emb_ancestors(SelRule,Clauses,AbsInt,UnfClauses):-
	(all_solutions_or_residual(Clauses) ->
	    peel_residual_A(Clauses,UnfClauses)
	;
	    unfold_all_clauses_with_atoms_A(SelRule,Clauses,AbsInt,UnfClauses1Step),
	    unfold_hom_emb_ancestors(SelRule,UnfClauses1Step,AbsInt,UnfClauses)).


unfold_all_clauses_with_atoms_A(_SelRule,[],_AbsInt,[]).
unfold_all_clauses_with_atoms_A(SelRule,[Cl|Clauses],AbsInt,NClauses):-
	unfold_one_step_with_atoms_A(SelRule,Cl,AbsInt,NClauses,MoreClauses),
	unfold_all_clauses_with_atoms_A(SelRule,Clauses,AbsInt,MoreClauses).

unfold_one_step_with_atoms_A(_SelRule,residual(Cl),_AbsInt,NCls,Cl_Cont):-!,
	NCls = [residual(Cl)|Cl_Cont].
unfold_one_step_with_atoms_A(_SelRule,fact(Cl),_AbsInt,NCls,Cl_Cont):-!,
	NCls = [fact(Cl)|Cl_Cont].
unfold_one_step_with_atoms_A(_SelRule,clause(Sg,[]),_AbsInt,NCls,Cl_Cont):-!,
	NCls = [fact(clause(Sg,[]))|Cl_Cont].
unfold_one_step_with_atoms_A(_SelRule,clause(Sg,Body),_AbsInt,NCls,Cl_Cont):-
	Body = [(Lit,A)|_],
	functor(Lit,F,Arity),
	functor(Atom,F,Arity),
 	member(Atom,A),
	debug(hom_emb),
 	\+ \+(homeomorphic_embedded(Atom,Lit)),!,
	debug(embedded),
	NCls = [residual(clause(Sg,Body))|Cl_Cont].

unfold_one_step_with_atoms_A(_SelRule,clause(Sg,Body),AbsInt,NCls,Cl_Cont):-
	Body = [(L,_)|_],
	copy_term(L,L1),
	(unfold_literal_if_possible(L1,AbsInt,UnfClauses) ->
	    debug(yes),
	    (UnfClauses == [] ->
	        NCls = Cl_Cont
	    ; 
		form_rules_A(UnfClauses,clause(Sg,Body),NewClauses),
		append(NewClauses,Cl_Cont,NCls)
	    )
	;
	    debug(no),
	    NCls = [residual(clause(Sg,Body))|Cl_Cont]
	).


form_rules_A([],_,[]).
form_rules_A([C2|Clauses],C,[C3|RClauses]):-
	copy_term(C,C1),
	form_one_rule_A(C1,C2,C3),
	form_rules_A(Clauses,C,RClauses).


form_one_rule_A(clause(Sg,[(L,L1)|R]),clause(L,Body), clause(Sg,NBody)):-
	update_ancestors(Body,(L,L1),ABody),
	append(ABody,R,NBody).

update_ancestors([],_L,[]).
update_ancestors([B|R],(L,A),[(B,[L1|A])|Rlist]):-
	copy_term(L,L1),
%	fresh_stack(A,A1),
	update_ancestors(R,(L,A),Rlist).

fresh_stack([],[]).
fresh_stack([L|R],[L1|R1]):-
	copy_term(L,L1),
	fresh_stack(R,R1).

initial_ancestors([],_L,[]).
initial_ancestors([clause(Sg,B)|R],L,[clause(Sg,NB)|Rlist]):-
	update_ancestors(B,(L,[]),NB),
	initial_ancestors(R,L,Rlist).

remove_ancestors([],[]).
remove_ancestors([clause(Sg,B)|R],[clause(Sg,NB)|Rlist]):-
	delete_ancestors(B,NB),
	remove_ancestors(R,Rlist).

delete_ancestors([],[]).
delete_ancestors([(B,_)|R],[B|R1]):-
	delete_ancestors(R,R1).

peel_residual_A([],[]).
peel_residual_A([residual(Cl)|Clauses],[NCl|NClauses]):-!,
	Cl = clause(Head,Body),
	NCl = clause(Head,Body),
	peel_residual_A(Clauses,NClauses).
peel_residual_A([fact(Cl)|Clauses],[NCl|NClauses]):-!,
	Cl = clause(Head,Body),
	NCl = clause(Head,Body),
	peel_residual_A(Clauses,NClauses).
peel_residual_A([Cl|Clauses],[NCl|NClauses]):-
	Cl = clause(Head,Body),
	NCl = clause(Head,Body),
	peel_residual_A(Clauses,NClauses).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%       EFFICIENT   UNFOLDING WITH EMBEDDING BASED ON STACKS       %% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


unfold_hom_emb_local_as(_SelRule,[],_,_AbsInt,[]):-!.
unfold_hom_emb_local_as(SelRule,Clauses,Atoms,AbsInt,UnfClauses):-
	(all_solutions_or_residual(Clauses) ->
%	(there_is_solution_or_residual(Clauses) ->
	    peel_residual_l(Clauses,UnfClauses)
	;
	    unfold_all_clauses_with_atoms_local(SelRule,Clauses,Atoms,AbsInt,UnfClauses1Step,NewAtoms),
	    unfold_hom_emb_local_as(SelRule,UnfClauses1Step,NewAtoms,AbsInt,UnfClauses)).

unfold_all_clauses_with_atoms_local(_SelRule,[],[],_AbsInt,[],[]).
unfold_all_clauses_with_atoms_local(SelRule,[Cl|Clauses],[A|As],AbsInt,NClauses,NAtoms):-
	unfold_one_step_with_atoms_local(SelRule,Cl,A,AbsInt,NClauses,MoreClauses,NAtoms,MoreAtoms),
	unfold_all_clauses_with_atoms_local(SelRule,Clauses,As,AbsInt,MoreClauses,MoreAtoms).

unfold_one_step_with_atoms_local(_SelRule,residual(Cl),A,_AbsInt,NCls,Cl_Cont,NAs,As_Cont):-!,
	NCls = [residual(Cl)|Cl_Cont],
	NAs = [A|As_Cont].
unfold_one_step_with_atoms_local(_SelRule,fact(Cl),A,_AbsInt,NCls,Cl_Cont,NAs,As_Cont):-!,
	NCls = [fact(Cl)|Cl_Cont],
	NAs = [A|As_Cont].
unfold_one_step_with_atoms_local(_SelRule,clause(Sg,[]),A,_AbsInt,NCls,Cl_Cont,NAs,As_Cont):-!,
	NCls = [fact(clause(Sg,[]))|Cl_Cont],
	NAs = [A|As_Cont].
unfold_one_step_with_atoms_local(SelRule,clause(Sg,['$pop$'|R]),A,AbsInt,NCls,Cl_Cont,NAs,As_Cont):-!,
	A = [_|A1s],
	unfold_one_step_with_atoms_local(SelRule,clause(Sg,R),A1s,AbsInt,NCls,Cl_Cont,NAs,As_Cont).
unfold_one_step_with_atoms_local(SelRule,clause(Sg,Body),A,AbsInt,NCls,Cl_Cont,NAs,As_Cont):-
	select_atom(SelRule,Body,NewBody,A,Flag),  
        NewBody = [L|R],    
	(L='$pop$' -> 
	    unfold_one_step_with_atoms_local(SelRule,clause(Sg,NewBody),A,AbsInt,NCls,Cl_Cont,NAs,As_Cont)
	   ;
	    debug(embed),
	    (is_embedded(Flag,L,A) ->
	     debug('EMBEDDED'),
	     NCls = [residual(clause(Sg,NewBody))|Cl_Cont],
	     NAs = [A|As_Cont]
	    ;
		copy_term(L,L1),
		(unfold_literal_if_possible(L1,AbsInt,UnfClauses) ->
		 debug(yes),
		 (UnfClauses == [] ->
		  NCls = Cl_Cont,
		  NAs = As_Cont
		 ; 
		     (UnfClauses = [clause(_,[])] ->
		      NA = A,
		      form_rules(UnfClauses,clause(Sg,NewBody),NCls,Cl_Cont)
		     
		     ;
			 copy_term(L1,NL1),
			 NA = [NL1|A],
			 form_rules(UnfClauses,clause(Sg,[L,'$pop$'|R]),NCls,Cl_Cont)
		     ),
		     replicate_atoms(UnfClauses,NA,As_Cont,NAs)
		 )
		;
		    NCls = [residual(clause(Sg,NewBody))|Cl_Cont],
		    NAs = [A|As_Cont]
		)
	    )
	).



is_embedded(true,L,A):- 
	functor(L,F,Arity),
	functor(Atom,F,Arity),
 	member(Atom,A),
	homeomorphic_embedded(Atom,L).

is_embedded_tree(true,L,Parent,A):- 
	functor(L,F,Arity),
	ancestor(Parent,A,Ancestor),
	functor(Ancestor,F,Arity),
	homeomorphic_embedded(Ancestor,L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%     COMMON CODE TO ALL ABOVE STRATEGIES   %%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


form_rules([],_,Cont,Cont).
form_rules([C2|Clauses],C,[C3|RClauses],Cont):-
	copy_term(C,C1),
	form_one_rule(C1,C2,C3),
	form_rules(Clauses,C,RClauses,Cont).


form_one_rule(clause(Sg,[L|R]),clause(L,Body), clause(Sg,NBody)):-
	append(Body,R,NBody).

form_one_rule_tree(clause(Sg,[(L,_)|R]),clause(L,Body), clause(Sg,NBody)):-
	append(Body,R,NBody).

:- doc(unfold_literal_if_possible(L,UnfClauses), "Unfolding steps
  are now always performed via this predicate. Requests to unfold a call
  to a predicate not defined in the current module and for which
  @pred{can_be_evaluated(L)} fails, finish with failure.").


%% unfold_literal_if_possible(L,AbsInt,_UnfClauses):-
%% 	update_mem_usage,
%% 	current_pp_flag(abs_exec,on),
%% 	functor(L,F,A),
%% 	abs_exec(AbsInt,F/A,_Sense,_Cond),
%% 	write('missed opportunity?'),
%% 	fail.
unfold_literal_if_possible(L,_AbsInt,UnfClauses):-
	literal_for_orig_pred(L),!,
	collect_orig_clauses(L,UnfClauses).

unfold_literal_if_possible(\+(L),_AbsInt,UnfClauses):-
%	type_of_goal(builtin(_TypeGoal),L),
	can_be_evaluated(L),!,
	findall(sol,'$meta_call'(L),Solutions),
	(Solutions = [] ->
	    UnfClauses = [clause(\+(L),[])]
	;
	    UnfClauses = []).
unfold_literal_if_possible(L,_AbsInt,UnfClauses):-
%	type_of_goal(builtin(_TypeGoal),L),!, % several successes ??
	(can_be_evaluated(L) ->
	    findall(clause(L,[]),'$meta_call'(L),UnfClauses),
	    debug('UNFOLDED builtin '),
	    debug(L)
	;
	    debug('NOT unfolded builtin '),
	    debug(L),
	    fail
	).





create_dicts_and_recs([],[],[]).
create_dicts_and_recs([Cl|Clauses],[D|Ds],[r|Rs]):-
	create_pretty_dict(Cl,D),
	create_dicts_and_recs(Clauses,Ds,Rs).

there_is_solution_or_residual([fact(_)|_]):-!.
there_is_solution_or_residual([residual(_)|_]):-!.
%there_is_solution_or_residual([clause(_,[])|_]):-!.
there_is_solution_or_residual([_|Clauses]):-
	there_is_solution_or_residual(Clauses).
	
%% there_is_solution([clause(_,true)|_]):-!.
%% there_is_solution([_|Clauses]):-
%% 	there_is_solution(Clauses).
%% 	
%% all_residual([]).
%% all_residual([residual(_)|Clauses]):-
%% 	all_residual(Clauses).

all_solutions_or_residual([]).
all_solutions_or_residual([residual(_)|Clauses]):-
	all_solutions_or_residual(Clauses).
all_solutions_or_residual([fact(_)|Clauses]):-
	all_solutions_or_residual(Clauses).
all_solutions_or_residual([clause(_,[])|Clauses]):-
	all_solutions_or_residual(Clauses).

peel_residual([],[]).
peel_residual([residual(Cl)|Clauses],[Cl|NClauses]):-!,
	peel_residual(Clauses,NClauses).
peel_residual([fact(Cl)|Clauses],[Cl|NClauses]):-!,
	peel_residual(Clauses,NClauses).
peel_residual([Cl|Clauses],[Cl|NClauses]):-
	peel_residual(Clauses,NClauses).

peel_residual_l([],[]).
peel_residual_l([residual(Cl)|Clauses],[NCl|NClauses]):-!,
	Cl = clause(Head,Body),
	NCl = clause(Head,NBody),
	filter_pops(Body,NBody),
	peel_residual_l(Clauses,NClauses).
peel_residual_l([fact(Cl)|Clauses],[NCl|NClauses]):-!,
	Cl = clause(Head,Body),
	NCl = clause(Head,NBody),
	filter_pops(Body,NBody),
	peel_residual_l(Clauses,NClauses).
peel_residual_l([Cl|Clauses],[NCl|NClauses]):-
	Cl = clause(Head,Body),
	NCl = clause(Head,NBody),
	filter_pops(Body,NBody),
	peel_residual_l(Clauses,NClauses).

filter_pops([],[]).
filter_pops(['$pop$'|Body],NBody):-!,
	filter_pops(Body,NBody).
filter_pops([L|Body],[L|NBody]):-!,
	filter_pops(Body,NBody).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%    DEPTH-FIRST UNFOLDING WITH EMBEDDING BASED ON STACKS  %%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

depth_first_emb_local_0(SelRule,AbsInt,Sg,UnfClause):-
	copy_term(Sg,NSg),
	collect_one_orig_clause(Sg,Clause),
	depth_first_emb_local(SelRule,Clause,[NSg],AbsInt,UnfClause).


depth_first_emb_local(SelRule,Clause,Atoms,AbsInt,UnfClause):-
	(fact_or_residual(Clause) ->
	    peel_fact_or_residual(Clause,UnfClause)
	;
	    unfold_one_step_one_clause(Clause,SelRule,Atoms,AbsInt,UnfClause1Step,NewAtoms),
	    depth_first_emb_local(SelRule,UnfClause1Step,NewAtoms,AbsInt,UnfClause)).

unfold_one_step_one_clause(residual(Cl),_SelRule,A,_AbsInt,Clause,A):-!,
	Clause = residual(Cl).
unfold_one_step_one_clause(fact(Cl),_SelRule,A,_AbsInt,Clause,A):-!,
	Clause = fact(Cl).
unfold_one_step_one_clause(clause(Sg,[]),_SelRule,A,_AbsInt,Clause,A):-!,
	Clause = fact(clause(Sg,[])).
unfold_one_step_one_clause(clause(Sg,['$pop$'|R]),SelRule,[_|A1s],AbsInt,Clause,NewAtoms):-!,
	unfold_one_step_one_clause(clause(Sg,R),SelRule,A1s,AbsInt,Clause,NewAtoms).
unfold_one_step_one_clause(clause(Sg,Body),SelRule,A,AbsInt,NCl,NAs):-
	select_atom(SelRule,Body,NewBody,A,Flag),  
        NewBody = [L|R],    
	(L='$pop$' -> 
	    A = [_|NA],
	    unfold_one_step_one_clause(SelRule,clause(Sg,R),NA,AbsInt,NCl,NAs)
	   ;
	    unfold_literal_or_residualize(clause(Sg,NewBody),R,L,A,AbsInt,NCl,NAs,Flag)
	).

unfold_literal_or_residualize(clause(Sg,NewBody),_R,L,A,_AbsInt,NCl,NAs,Flag):-
	debug(embed),
	is_embedded(Flag,L,A),!,
	debug('EMBEDDED'),
	NCl = residual(clause(Sg,NewBody)),
	NAs = A.
	

unfold_literal_or_residualize(clause(Sg,NewBody),R,L,A,AbsInt,NCl,NAs,_Flag):-
	copy_term(L,L2),
	can_continue(L,Case),!,
	unfold_literal_one_step(Case,L,AbsInt,UnfClause),
	(UnfClause = clause(H,[]) ->
	    form_one_rule(clause(Sg,NewBody),clause(H,[]),NCl),
	    NAs = A
	;
	    NAs = [L2|A],
	    form_one_rule(clause(Sg,[L,'$pop$'|R]),UnfClause,NCl)
	).
	
		 
unfold_literal_or_residualize(clause(Sg,NewBody),_R,_L,A,_AbsInt,NCl,NAs,_Flag):-
	NCl = residual(clause(Sg,NewBody)),
	NAs = A,
	debug('NOT unfolded builtin '),
	(NewBody = [L|_] ->
	    debug(L)
	;
	    true).

%% unfold_literal_one_step(L,AbsInt,_UnfClause):-
%% 	current_pp_flag(abs_exec,on),
%% 	functor(L,F,A),
%% 	abs_exec(AbsInt,F/A,_Sense,_Cond),
%% 	write('missed opportunity?'),
%% 	fail.
can_continue(L,Case):-
	literal_for_orig_pred(L),!,
	Case = internal.

can_continue(L,Case):-
	can_be_evaluated(L),
	Case = external.

unfold_literal_one_step(internal,L,_AbsInt,UnfClause):-
	update_mem_usage,
	collect_one_orig_clause(L,UnfClause).

unfold_literal_one_step(external,L,_AbsInt,UnfClause):-
	'$meta_call'(L),
	UnfClause=clause(L,[]),
	debug('UNFOLDED builtin '),
	debug(L).


fact_or_residual(residual(_)).
fact_or_residual(fact(_)).
fact_or_residual(clause(_,[])).


peel_fact_or_residual(residual(Cl),clause(Head,NewBody)):-!,
	Cl=clause(Head,Body),
	filter_pops(Body,NewBody).
peel_fact_or_residual(fact(Cl),clause(Head,NewBody)):-!,
	Cl=clause(Head,Body),
	filter_pops(Body,NewBody).
peel_fact_or_residual(Cl,clause(Head,NewBody)):-
	Cl=clause(Head,Body),
	filter_pops(Body,NewBody).





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  END DEPTH-FIRST UNFOLDING WITH EMBEDDING BASED ON STACKS  %%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  DEPTH-FIRST UNFOLDING WITH EMBEDDING BASED ON PROOF TREES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

depth_first_emb_tree_0(SelRule,AbsInt,Sg,UnfClause):-
	copy_term(Sg,NSg),
	collect_one_orig_clause(Sg,Clause),
	add_parents(Clause,1,NClause),
	depth_first_emb_tree(SelRule,NClause,[(1,NSg,0)],AbsInt,UnfClause).


depth_first_emb_tree(SelRule,Clause,Atoms,AbsInt,UnfClause):-
	(fact_or_residual(Clause) ->
	    peel_fact_or_residual_tree(Clause,UnfClause)
	;
	    unfold_one_step_one_clause_tree(Clause,SelRule,Atoms,AbsInt,UnfClause1Step,NewAtoms),
	    depth_first_emb_tree(SelRule,UnfClause1Step,NewAtoms,AbsInt,UnfClause)).

unfold_one_step_one_clause_tree(residual(Cl),_SelRule,A,_AbsInt,Clause,A):-!,
	Clause = residual(Cl).
unfold_one_step_one_clause_tree(fact(Cl),_SelRule,A,_AbsInt,Clause,A):-!,
	Clause = fact(Cl).
unfold_one_step_one_clause_tree(clause(Sg,[]),_SelRule,A,_AbsInt,Clause,A):-!,
	Clause = fact(clause(Sg,[])).
unfold_one_step_one_clause_tree(clause(Sg,['$pop$'|R]),SelRule,[_|A1s],AbsInt,Clause,NewAtoms):-!,
	unfold_one_step_one_clause_tree(clause(Sg,R),SelRule,A1s,AbsInt,Clause,NewAtoms).
unfold_one_step_one_clause_tree(clause(Sg,Body),SelRule,A,AbsInt,NCl,NAs):-
	select_atom(SelRule,Body,NewBody,A,Flag),  
        NewBody = [(L,Parent)|R],    
	(L='$pop$' -> 
	    A = [_|NA],
	    unfold_one_step_one_clause_tree(SelRule,clause(Sg,R),NA,AbsInt,NCl,NAs)
	   ;
	    unfold_literal_or_residualize_tree(clause(Sg,NewBody),R,L,Parent,A,AbsInt,NCl,NAs,Flag)
	).

unfold_literal_or_residualize_tree(clause(Sg,NewBody),_R,L,Parent,A,_AbsInt,NCl,NAs,Flag):-
	debug(embed),
	is_embedded_tree(Flag,L,Parent,A),!,
	debug('EMBEDDED'),
	NCl = residual(clause(Sg,NewBody)),
	NAs = A.
	

unfold_literal_or_residualize_tree(clause(Sg,NewBody),_R,L,Parent,A,AbsInt,NCl,NAs,_Flag):-
	copy_term(L,L2),
	can_continue(L,Case),!,
	unfold_literal_one_step(Case,L,AbsInt,UnfClause),
	parent_id(New_Parent),
	add_parents(UnfClause,New_Parent,UnfClauseP),
	(UnfClauseP = clause(H,[]) ->
	    form_one_rule_tree(clause(Sg,NewBody),clause(H,[]),NCl),
	    NAs = A
	;
	    NAs = [(New_Parent,L2,Parent)|A],
	    form_one_rule_tree(clause(Sg,NewBody),UnfClauseP,NCl)
	).
	
		 
unfold_literal_or_residualize_tree(clause(Sg,NewBody),_R,_L,_Parent,A,_AbsInt,NCl,NAs,_Flag):-
	NCl = residual(clause(Sg,NewBody)),
	NAs = A,
	debug('NOT unfolded builtin '),
	(NewBody = [L|_] ->
	    debug(L)
	;
	    true).


ancestor(Parent,A,Ancestor):-
	member((Parent,Ancestor,_),A).
ancestor(Parent,A,Ancestor):-
	member((Parent,_Ancestor0,Grand_Parent),A),
	ancestor(Grand_Parent,A,Ancestor).

:- data par_id/1.

par_id(1).

parent_id(Id):-
	retract_fact(par_id(Id0)),
	Id is Id0+1,
	asserta_fact(par_id(Id)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  END DEPTH-FIRST UNFOLDING WITH EMBEDDING BASED ON PROOF TREES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  DEPTH-FIRST UNFOLDING WITH EMBEDDING NOT ANCESTORS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

depth_first_emb_hom_emb_0(SelRule,AbsInt,Sg,UnfClause):-
	copy_term(Sg,NSg),
	collect_one_orig_clause(Sg,Clause),
	depth_first_emb_hom_emb(SelRule,Clause,[NSg],AbsInt,UnfClause).


depth_first_emb_hom_emb(SelRule,Clause,Atoms,AbsInt,UnfClause):-
	(fact_or_residual(Clause) ->
	    peel_fact_or_residual(Clause,UnfClause)
	;
	    unfold_one_step_one_clause_hom_emb(Clause,SelRule,Atoms,AbsInt,UnfClause1Step,NewAtoms),
	    depth_first_emb_hom_emb(SelRule,UnfClause1Step,NewAtoms,AbsInt,UnfClause)).

unfold_one_step_one_clause_hom_emb(residual(Cl),_SelRule,A,_AbsInt,Clause,A):-!,
	Clause = residual(Cl).
unfold_one_step_one_clause_hom_emb(fact(Cl),_SelRule,A,_AbsInt,Clause,A):-!,
	Clause = fact(Cl).
unfold_one_step_one_clause_hom_emb(clause(Sg,[]),_SelRule,A,_AbsInt,Clause,A):-!,
	Clause = fact(clause(Sg,[])).
unfold_one_step_one_clause_hom_emb(clause(Sg,Body),SelRule,A,AbsInt,NCl,NAs):-
	select_atom(SelRule,Body,NewBody,A,Flag),  
        NewBody = [L|R],    
	unfold_literal_or_residualize_hom_emb(clause(Sg,NewBody),R,L,A,AbsInt,NCl,NAs,Flag).

unfold_literal_or_residualize_hom_emb(clause(Sg,NewBody),_R,L,A,_AbsInt,NCl,NAs,Flag):-
	debug(embed),
	is_embedded(Flag,L,A),!,
	debug('EMBEDDED'),
	NCl = residual(clause(Sg,NewBody)),
	NAs = A.
	

unfold_literal_or_residualize_hom_emb(clause(Sg,NewBody),_R,L,A,AbsInt,NCl,NAs,_Flag):-
	copy_term(L,L2),
	can_continue(L,Case),!,
	unfold_literal_one_step(Case,L,AbsInt,UnfClause),
	(UnfClause = clause(H,[]) ->
	    form_one_rule(clause(Sg,NewBody),clause(H,[]),NCl),
	    NAs = A
	;
	    NAs = [L2|A],
	    form_one_rule(clause(Sg,NewBody),UnfClause,NCl)
	).
	
		 
unfold_literal_or_residualize_hom_emb(clause(Sg,NewBody),_R,_L,A,_AbsInt,NCl,NAs,_Flag):-
	NCl = residual(clause(Sg,NewBody)),
	NAs = A,
	debug('NOT unfolded builtin '),
	(NewBody = [L|_] ->
	    debug(L)
	;
	    true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  END DEPTH-FIRST UNFOLDING WITH EMBEDDING NO ANCESTORS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

peel_fact_or_residual_tree(residual(Cl),clause(Head,NewBody)):-!,
	Cl=clause(Head,Body),
	filter_parents(Body,NewBody).
peel_fact_or_residual_tree(fact(Cl),clause(Head,NewBody)):-!,
	Cl=clause(Head,Body),
	filter_parents(Body,NewBody).
peel_fact_or_residual_tree(Cl,clause(Head,NewBody)):-
	Cl=clause(Head,Body),
	filter_parents(Body,NewBody).

filter_parents([],[]).
%% filter_parents(['$pop$'|Body],NBody):-!,
%% 	filter_parents(Body,NBody).
filter_parents([(L,_Parent)|Body],[L|NBody]):-
	filter_parents(Body,NBody).

add_parents(clause(Head,Body),Parent,clause(Head,NBody)):-
	add_all_parents(Body,Parent,NBody).

add_all_parents([],_,[]).
add_all_parents([A|As],Parent,[(A,Parent)|NAs]):-
	add_all_parents(As,Parent,NAs).


:- doc(version_maintenance,dir('../version')).



:- doc(version(1*0+805,2004/11/03,18:47*44+'CET'), "Added
   unfolding rule @tt{df_hom_emb}, which is a depth-first unfolding
   rule based on embedding but without ancestors.  (German Puebla)").

:- doc(version(1*0+797,2004/11/02,11:44*22+'CET'), "uses the
   package nomem_usage to avoid the overhead of measuring memory
   consumption if we are not running experiments (German Puebla)").

:- doc(version(1*0+795,2004/10/30,22:24*05+'UTC'), "Adapted to the
   new code in mem_usage. This allows computing the memory required
   for unfolding even in programs which are not fully unfolded and
   where unfolding is calls several times from different memory
   prints.  (German Puebla)").

:- doc(version(1*0+792,2004/10/30,09:35*39+'UTC'), "Changes needed
   in order to measure unfolding times.  (German Puebla)").

:- doc(version(1*0+789,2004/10/27,18:42*17+'UTC'), "Added
   unfolding rule @tt{df_tree_hom_emb} which implements ancestors
   using a proof tree.  (German Puebla)").

:- doc(version(1*0+767,2004/10/20,12:25*34+'CEST'), "Added a new
   strategy @tt{hom_emb_anc} for local unfolding which implements an
   unfolding rule based on embedding without using efficient ancestor
   stacks but avoiding the restriction to perform only local
   unfolding.  (Elvira Albert)").

:- doc(version(1*0+706,2004/10/08,17:04*05+'CEST'), "Added a new
   depth-first implementation of the local unfolding rule with
   ancestors stacks in order to improve efficiency of partial
   evaluation in CiaoPP. It can be selected by setting the Local
   Control flag (i.e.,@tt{local_control}) to the value
   @tt{df_hom_emb_as}.  (Elvira Albert)").

:- doc(version(1*0+689,2004/10/04,18:29*00+'CEST'), "Existing
   unfolding strategies have been parameterized with the computation
   rule @tt{comp_rule} in order to allow local unfolding.  (Elvira
   Albert)").

:- doc(version(1*0+679,2004/09/30,23:20*21+'UTC'), "Fixed
   complicated bug in @pred{unfold_one_step}.  (German Puebla)").

:- doc(version(1*0+610,2004/08/30,16:00*04+'CEST'), "Reorganized
   and further modularized code. Now this module only exports
   @pred{unfold/7}. The rest of previously exported predicates are now
   in auxilary modules.  (German Puebla)").

:- doc(version(1*0+523,2004/07/05,12:22*08+'UTC'), "Important
   changes in order to take the abstract substitution into acccount
   during local control. Not finished yet though.  (German Puebla)").

:- doc(version(1*0+522,2004/07/05,12:20*55+'UTC'), "Useless
   clauses can now be removed during local control according to the
   value of flag @tt{rem_use_cls}.  (German Puebla)").

:- doc(version(1*0+451,2004/04/28,11:57*23+'CEST'), "Fail clauses
   are now added for predicates which are shown to fail during partial
   evaluation. This has several pros (side_effect analysis sees those
   predicates, run_time execution will not reach undefined predicates)
   and the cons that the final program can be a bit larger. The
   abstract specializer should be able to remove those fail clauses in
   the end.  (German Puebla)").

:- doc(version(1*0+434,2004/04/21,17:39*15+'CEST'), "Several
   predicates moved to module unfold_operations.pl (German Puebla)").

:- doc(version(1*0+431,2004/04/19,14:25*56+'CEST'), "Added the
   @tt{hom_emb_l} unfolding rule. It provides a much more efficient
   and aggresive behaviour than @tt{hom_emb} by using a pushdown
   automata for removing unneeded atoms from the set.  (German
   Puebla)").

:- doc(version(1*0+430,2004/04/19,14:23*59+'CEST'), "Major
   rewrital of unfolding rules by replacing internal representation of
   specialized definitions.  (German Puebla)").

:- doc(version(1*0+426,2004/04/13,15:32*36+'CEST'), "Added the
   @tt{hom_emb} unfolding rule which guarantees local termination by
   not unfolding atoms which are embedded in atoms already unfolded in
   the current derivation.  (German Puebla)").

:- doc(version(1*0+425,2004/04/13,15:30*41+'CEST'), "Modified the
   @tt{first_sol} unfolding rule to stop as soon as a residual
   derivation is found instead of all branches solution or residual.
   (German Puebla)").

:- doc(version(1*0+412,2004/04/04,16:05*18+'CEST'), "Implemented
   the new local_control strategy @tt{det_la} which stands for
   'deterministic with look ahead'. This flag uses the @tt{depth}
   argument such that the behaviour of the original deterministic rule
   is obtained with depth=1. By making depth=2 we can fully unfold
   qsort when called with a known input list.  (German Puebla)").

:- doc(version(1*0+411,2004/04/04,16:02*29+'CEST'), "Calls to
   predicates which can be evaluated are now much more efficiently
   performed using the engine predicate @pred{'$meta_call'/1}.
   (German Puebla)").

:- doc(version(1*0+388,2004/03/26,18:14*33+'CET'), "Negation as
   failure of calls to builtins can now be fully evaluated at
   specialization time.  (German Puebla)").

:- doc(version(1*0+378,2004/03/23,13:00*42+'CET'), "Added the
   @tt{all_sol} unfolding rule which computes all solutions. It is the
   most aggresive rule. However, it can only be used if the search
   space is finite.  (German Puebla)").

:- doc(version(1*0+377,2004/03/23,09:48*03+'CET'), "Improved
   handling of builtins. Now they can be evaluated even if they are
   non-deterministic, as long as they universally terminate since they
   are avaluated via findall.  (German Puebla)").

:- doc(version(1*0+369,2004/03/09,13:41*39+'CET'), "Added null def
   of peel_call/2.  (Francisco Bueno Carrillo)").

:- doc(version(1*0+302,2004/02/05,10:54*34+'CET'), "Two more
   unfolding strategies implemented: @tt{first_sol} and
   @tt{first_sol_d}. The first one unfolds until a first solution
   (body = true) is found. The second stops when a depth bound is
   reached.  (German Puebla)").


:- doc(version(1*0+297,2004/02/04,19:42*13+'CET'), "Corrected bug
   in the unfolding strategy @tt{det}.  (Elvira Albert)").
 
:- doc(version(1*0+296,2004/02/04,18:57*22+'CET'), "Implemented
   the unfolding strategy @tt{depthk(K)} which expands the unfolding
   tree (always choosing leftmost atoms) until its @tt{K}-th frontier
   is reached.  (Elvira Albert)").

:- doc(version(1*0+219,2004/01/18,20:43*39+'CET'), "The
   information about specialized definitions is now split into two
   tables. This is required for handling partial concretization
   correctly.  (German Puebla)").

:- doc(version(1*0+206,2004/01/08,11:00*45+'CET'), "Improved
   behaviour of deterministic unfolding rule.  (German Puebla)").

:- doc(version(1*0+202,2004/01/02,11:48*45+'CET'), "Usage of
   partial concretatization before unfolding already working.  (German
   Puebla)").

:- doc(version(1*0+198,2003/12/31,11:26*40+'CET'), "Atoms to be
   unfolded can now be partially concretized before actual unfolding.
   (German Puebla)").

:- doc(version(1*0+193,2003/12/30,20:34*01+'CET'), "Implemented
   homeomorphic embedding and used for improved (always terminating) global
   control.  (Elvira Albert)").

:- doc(version(1*0+186,2003/12/30,12:28*07+'CET'), "Added the
   possibility to select the global control and implemented @tt{id}
   and @tt{inst}. Other strategies soon to come.  (German Puebla)").

