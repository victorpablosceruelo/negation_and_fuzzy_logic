:- module(_,[main/0],[]).

:- use_module(ciaopp(ciaopp)).

main:-
	module(example_pcpd),
	push_pp_flag(fixpoint,poly_spec),
	analyze(pd),
	transform(codegen_poly),
	pop_pp_flag(fixpoint).
