:- module(_, [f_res_steps/2, f_res_steps_1/2], [assertions]).

:- use_module(resources(resources_basic)).

f_res_steps(LitInfo, 1) :-
	litinfo_get_lit(LitInfo, Lit),
	member(Lit,
	    [
		'reverse_2:reverse'(_, _),
		'reverse_2:concat'(_, _, _)
	    ]),
	!.
f_res_steps(_LitInfo, 0).

f_res_steps_1(LitInfo, 1) :-
	litinfo_get_lit(LitInfo, Lit),
	member(Lit,
	    [
		'reverse_2:reverse'(_, _),
		'reverse_2:concat'(_, _, _)
	    ]),
	!.
f_res_steps_1(_LitInfo, 0).
