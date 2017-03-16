:- module(tr_granul, 
	[annotate_granularity/3,
	 get_entry_point/1,
	 add_transformed_clauses/2
	], 
	[assertions, api(ciaopp_api)]). 

:- doc(author,"Pedro L@'{o}pez").  

% ciao library
:- use_module(library(lists), [append/3]). 

% ciaopp library
:- use_module(ciaopp(preprocess_flags), 
	[
	    current_pp_flag/2,  
	    set_pp_flag/2
	]). 
:- use_module(ciaopp(p_unit),              [entry_assertion/3]).
:- use_module(infercost(infercost),        [complexity_analysis/4]).
:- use_module(infercost(gran(gran_table)), [compute_exact_size_relations/3]). 
:- use_module(infer(gather_modes),         [gather_modes/4]). 
:- use_module(infer(vartypes),             [gather_vartypes/2]).  
:- use_module(program(clause_db),          [cleanup_clause_db/0]).
:- use_module(infer(infer_db),             [inferred/3]).   

% Own library
:- use_module(trans_dyn, 
	[
	    annotate_gran/7,
	    create_output_ann_program/7
	]). 


annotate_granularity(Cls,Ds,none) :-
	!,
	add_transformed_clauses(Cls,Ds).
annotate_granularity(Cls,Ds,CostAnalysis) :-
	gather_vartypes(Cls,_Trusts),
	gather_modes(Cls,Ds,NewCls,NewDs),
        current_pp_flag(cost_approximation,CurrentApprox),
	(
	    (CostAnalysis = both) ->
	     lower_cost_analysis(NewCls,NewDs,_,_,GT_Lb,_,_,_),
	     upper_cost_analysis(NewCls,NewDs,ST_Ub,SCCG_Ub,GT_Ub,GCG_Ub,
                                 Directs_Ub,DirDicts_Ub),
             annotate_granularity_rest(GT_Lb,GT_Ub,ST_Ub,SCCG_Ub,GCG_Ub,
                                       Directs_Ub,DirDicts_Ub)
	;
	    (CostAnalysis = upper) ->
	     upper_cost_analysis(NewCls,NewDs,ST,SCCG,GT,GCG,Dirs,DirDicts),
             annotate_granularity_rest(GT,GT,ST,SCCG,GCG,Dirs,DirDicts)
	;
	    (CostAnalysis = lower) ->
	     lower_cost_analysis(NewCls,NewDs,ST,SCCG,GT,GCG,Dirs,DirDicts),
             annotate_granularity_rest(GT,GT,ST,SCCG,GCG,Dirs,DirDicts)
	),
        set_pp_flag(cost_approximation,CurrentApprox).

annotate_granularity_rest(GT_Lb,GT_Ub,ST,SCCG,GCG,Directs,DirDicts) :-
	compute_exact_size_relations(GT_Lb,GT_Ub,ExactGT),
	get_entry_point(EP),
	!,
	annotate_gran(SCCG,ST,ExactGT,GCG,none,EP,NT), 
	create_output_ann_program(ST,ExactGT,NT,AnnCls,[],AnnDics,[]),
	append(Directs, AnnCls, OutCls),
	append(DirDicts, AnnDics, OutDics),
	add_transformed_clauses(OutCls,OutDics).

% TODO: complexity_analysis/4 must be replaced by equivalent
% TODO: resource-based method --EMM
lower_cost_analysis(Cls,Ds,ST,SCCG,GT,GCG,Dirs,DirDicts) :-
	set_pp_flag(cost_approximation,lower),
	complexity_analysis(steps_lb,gr,Cls,Ds),
	inferred(steps_lb,
	         '$current_punit',
                 comp_info(ST,SCCG,GT,GCG,Dirs,DirDicts)).

upper_cost_analysis(Cls,Ds,ST,SCCG,GT,GCG,Dirs,DirDicts) :-
	set_pp_flag(cost_approximation,upper),
	complexity_analysis(steps_ub,gr,Cls,Ds),
	inferred(steps_ub,
	         '$current_punit',
                 comp_info(ST,SCCG,GT,GCG,Dirs,DirDicts)).

get_entry_point(EP) :-
        entry_assertion(G, _Call, _Name),
        functor(G, F, A),
        EP = F/A.

add_transformed_clauses(Cls,Ds) :-
	cleanup_clause_db,
	add_transformed_clauses_(Cls,Ds).

add_transformed_clauses_([], []).
add_transformed_clauses_([(clause(H,B),_)|Cs], [D|Ds]) :-
	add_transformed_clauses__(H,B,D),
	add_transformed_clauses_(Cs,Ds).
add_transformed_clauses_([clause(H,B):_|Cs], [D|Ds]) :-
	add_transformed_clauses__(H,B,D),
	add_transformed_clauses_(Cs,Ds).
add_transformed_clauses__(H,B,D) :-
	loc_unknown(API_Loc),
	AC = cls${
		     head     => H,	
		     body     => B,
		     locator  => API_Loc,
		     dic      => D
		 },
	add_clause(AC).


