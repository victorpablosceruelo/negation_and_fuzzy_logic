:- module(_,[main/0],[]).

:- use_module(ciaopp(ciaopp)).

main:-
        set_pp_flag(fixpoint,di),
        set_pp_flag(local_control,df_hom_emb_as),
	set_pp_flag(global_trees,on),
%        set_pp_flag(min_crit,isomorphic),
	module(numlist),
	analyze(pd),
	transform(codegen),
	output.
