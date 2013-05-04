:- module(_,[main/0],[]).

:- use_module(ciaopp(ciaopp)).

main:-
        set_pp_flag(fixpoint,di),
	module(slicing_ex),
	analyze(pd),
	transform(slicing),
	output.
