:- module(tr_granul_res, [annotate_granularity_res/2], [assertions, api(ciaopp_api)]). 

:- doc(author,"Pedro L@'{o}pez").  
:- doc(author,"Jorge Navas (adapted for resource analysis)").  


:- doc(bug,"1. To take into account the lower bounds in the granularity
           analysis, each resource must be defined for both lower and upper
           bounds. This feature is a limitation because a particular
           resource may be defined for both bounds but lower bounds could
           be not considered if the rest of resources are not also defined
           for both.").

% ciao library
:- use_module(library(lists),    [append/3]). 
:- use_module(library(messages), [warning_message/2]).
% ciaopp library
:- use_module(resources(resources), [complexity_analysis/5]).
:- use_module(resources(res_assrt_defs(resources_lib)), 
	[current_resources_by_approx/2,
         load_resources_modules/0,
	 cleanup_resources_db/0
        ]).
:- use_module(resources(gran_res(gran_table_res)), [compute_exact_size_relations/3]). 
:- use_module(infer(gather_modes), [gather_modes/4]). 
:- use_module(infer(vartypes), [gather_vartypes/2]).  
:- use_module(infer(infer_db), [cleanup_infer_db/1, inferred/3]).   
% Own library 
:- use_module(ciaopp(tr_parallel(tr_granul(tr_granul))), 
	[ get_entry_point/1,
	  add_transformed_clauses/2]).
:- use_module(trans_dyn_res, 
	[ annotate_gran/7,
	  create_output_ann_program/7
	]). 

annotate_granularity_res(Cls,Ds) :-
	cleanup_infer_db(resources),
	cleanup_infer_db(modes),
	cleanup_infer_db(vartypes),
	cleanup_resources_db,
	gather_vartypes(Cls,_Trusts),
	gather_modes(Cls,Ds,NewCls,NewDs),
	load_resources_modules,
	lower_resources_analysis(NewCls,NewDs,Ress_Lb,_,_,GT_Lb,_,_,_),
	upper_resources_analysis(NewCls,NewDs,Ress_Ub,ST,SCCG,GT_Ub,GCG,Dirs,DirDicts),
	( Ress_Lb == Ress_Ub ->
	  annotate_granularity_rest(GT_Lb,GT_Ub,ST,SCCG,GCG,Dirs,DirDicts)
	;
	  
          messages:warning_message("Lower-bound resources ~q different from
           	   upper-bound resources ~q. Considering only upper-bound
           	   resources for granularity analysis.", [Ress_Lb,Ress_Ub]),
	  annotate_granularity_rest(GT_Ub,GT_Ub,ST,SCCG,GCG,Dirs,DirDicts)  
	).

upper_resources_analysis(Cls,Ds,Resources,ST,SCCG,GT,GCG,Dirs,DirDicts) :-
	current_resources_by_approx(ub,Resources),
	resources_analysis_(Resources,ub,Cls,Ds,ST,SCCG,GT,GCG,Dirs,DirDicts).
lower_resources_analysis(Cls,Ds,Resources,ST,SCCG,GT,GCG,Dirs,DirDicts) :-
	current_resources_by_approx(lb,Resources),
	resources_analysis_(Resources,lb,Cls,Ds,ST,SCCG,GT,GCG,Dirs,DirDicts).
resources_analysis_(Resources,Approx,Cls,Ds,ST,SCCG,GT,GCG,Dirs,DirDicts) :-
	complexity_analysis(resources,Resources,Approx,Cls,Ds),
	inferred(resources,'$current_punit',
                 comp_info(ST,SCCG,GT,GCG,Dirs,DirDicts)).
% resources_analysis_(_,_,_,_,_,_,_,_,_,_) :-
% 	messages:error_message("Something failed during RUA. Make sure you
% 	         the program is appropriately annotated."),
% 	!, 
% 	fail.


annotate_granularity_rest(GT_Lb,GT_Ub,ST,SCCG,GCG,Directs,DirDicts) :-
	compute_exact_size_relations(GT_Lb,GT_Ub,ExactGT),
	get_entry_point(EP),
	!,
	annotate_gran(SCCG,ST,ExactGT,GCG,none,EP,NT), 
	create_output_ann_program(ST,ExactGT,NT,AnnCls,[],AnnDics,[]),
	append(Directs, AnnCls, OutCls),
	append(DirDicts, AnnDics, OutDics),
	add_transformed_clauses(OutCls,OutDics).



