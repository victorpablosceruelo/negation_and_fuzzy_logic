:- module(reverse_cm,
	    [reverse/2,
		delta_res_steps/2,
		call_res_steps/2,
		delta_calls_to_reverse/2,
		call_calls_to_reverse/2],
	    [assertions, nativeprops, regtypes,
		library(resdefs(resources_decl))]).
:- use_module(resources(resources_basic)).

:- entry reverse(Xs, Ys) : (var(Ys), list(Xs, num)).

reverse([],    []).
reverse([X|T], L) :- reverse(T, L1), concat(L1, [X], L).

concat([],     Y,  Y).
concat([X|Xs], Ys, [X|Zs]) :-
	concat(Xs, Ys, Zs).

%  RESOURCES
:- resource res_steps.
:- head_cost(ub, res_steps, delta_res_steps).
:- literal_cost(ub, res_steps, call_res_steps).

:- resource calls_to_reverse.
:- head_cost(ub, calls_to_reverse, delta_calls_to_reverse).
:- literal_cost(ub, calls_to_reverse, call_calls_to_reverse).

:- load_resource_module(ciaopp(tests(resources(examples(reverse_cm))))).

delta_res_steps(_, 1) :- !.
delta_calls_to_reverse(LitInfo, 1) :-
	litinfo_get_lit(LitInfo, Head),
	Head = 'reverse_cm:reverse'(_, _),
	!.
delta_calls_to_reverse(_LitInfo, 0) :- !.

call_res_steps(_, 0) :- !.
call_calls_to_reverse(_, 0) :- !.
