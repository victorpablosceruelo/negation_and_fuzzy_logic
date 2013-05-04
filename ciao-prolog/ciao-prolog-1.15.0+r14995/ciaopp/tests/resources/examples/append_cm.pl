:- module(_, [append/3,
		delta_res_steps/2,
		delta_calls_to_append/2,
		call_res_steps/2,
		call_calls_to_append/2], [assertions, nativeprops,
		regtypes, library(resdefs(resources_decl))]).
:- use_module(resources(resources_basic)).
:- redefining(append/3).

:- entry append(Xs, Ys, Zs) : (list(Xs, num), list(Ys, num)).

append([],     Y,  Y).
append([X|Xs], Ys, [X|Zs]) :-
	append(Xs, Ys, Zs).


%  RESOURCES
:- resource res_steps.
:- head_cost(ub, res_steps, delta_res_steps).
:- literal_cost(ub, res_steps, call_res_steps).

:- resource calls_to_append.
:- head_cost(ub, calls_to_append, delta_calls_to_append).
:- literal_cost(ub, calls_to_append, call_calls_to_append).

:- load_resource_module(resources(resources_tests(append_cm))).

delta_res_steps(_, 1) :- !.
delta_calls_to_append(LitInfo, 1) :-
	litinfo_get_lit(LitInfo, Head),
	Head = 'append_cm:append'(_, _, _),
	!.
delta_calls_to_append(_, 0) :- !.

call_res_steps(_, 0) :- !.
call_calls_to_append(_, 0) :- !.
