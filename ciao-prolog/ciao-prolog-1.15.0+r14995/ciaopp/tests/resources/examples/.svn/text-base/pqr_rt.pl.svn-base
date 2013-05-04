:- module(_, _, [assertions, fsyntax]).

:- use_module(resources(resources_basic)).

valid_lit_steps := 'pqr:q'(_) | 'pqr:r'(_).

f_res_steps_delta(LitInfo, inf) :-
	litinfo_get_lit(LitInfo, Lit),
	valid_lit_steps(Lit),
	!.
f_res_steps_delta(_, 1).

f_res_steps2_delta(LitInfo, inf) :-
	litinfo_get_lit(LitInfo, Lit),
	valid_lit_steps(Lit),
	!.
f_res_steps2_delta(_, 2).
