:- module(_, _, [assertions]).

:- use_module(resources(resources_basic)).

f_calls_to_r(LitInfo, 1) :-
	litinfo_get_lit(LitInfo, Lit),
	Lit = 'pqr_2:r'(_),
	!.
f_calls_to_r(_, 0).
