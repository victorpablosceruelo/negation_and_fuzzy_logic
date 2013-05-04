:- module(_,[main/0],[]).

:- use_module(ciaopp(driver)).
:- use_module(ciaopp(preprocess_flags)).
:- use_module(ciaopp(printer)).

main:-
	% these three are the default values in auto_optimize
        set_pp_flag(fixpoint,di),
	set_pp_flag(local_control,df_hom_emb_as),
	set_pp_flag(dump_ai,off),
	% this one needs to be modified
	set_pp_flag(abs_spec_defs,all),
	module(example_sd),
        % we also need to use the shfr domain
	analyze(shfr),
	transform(codegen),
	transform(spec),
	output.
