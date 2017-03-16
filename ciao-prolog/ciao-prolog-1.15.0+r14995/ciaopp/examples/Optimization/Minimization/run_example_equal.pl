:- module(_,[main/0],[]).

:- use_module(ciaopp(ciaopp)).

main:-
        set_pp_flag(fixpoint,di),
        set_pp_flag(local_control,df_hom_emb_as),
        set_pp_flag(min_crit,equal),
	module(naive_reverse),
	analyze(pd),
	transform(codegen_min),
	output.
