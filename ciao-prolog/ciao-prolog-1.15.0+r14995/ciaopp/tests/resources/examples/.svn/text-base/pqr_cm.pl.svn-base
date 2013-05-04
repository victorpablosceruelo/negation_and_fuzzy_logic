:- module(pqr_cm, [p/2, delta_res_steps/2, call_res_steps/2], [
		assertions,
		basicmodes,
		regtypes,
		ciaopp(tests(resources)),
		library(resdefs(resources_decl))
	    ]).

:- entry p(+X, +Y).

:- trust comp p(X, Y) + ( head_cost(ub, res_steps, 1),
	    literal_cost(ub, res_steps, 0) ).

p(X, Y) :- q(X), r(Y).


:- trust comp q(+X) + (cost(ub, res_steps, 6)).

q(_).

:- trust comp r(+X) + (cost(ub, res_steps, 5)).

r(_).


%% RESOURCES 
:- resource res_steps.

:- literal_cost(ub, res_steps, call_res_steps).

delta_res_steps(_, 1) :- !.
call_res_steps(_, 0) :- !.

:- load_resource_module(ciaopp(tests(resources(examples(pqr_cm))))).
