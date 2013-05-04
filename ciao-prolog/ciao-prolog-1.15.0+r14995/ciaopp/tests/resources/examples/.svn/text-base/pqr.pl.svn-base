:- module(pqr, _, [assertions, regtypes, ciaopp(tests(resources)),
		library(resdefs(resources_decl))]).

:- load_resource_module(ciaopp(tests(resources(examples(pqr_rt))))).

:- resource res_steps.
:- resource res_steps2.

:- head_cost(ub, res_steps, f_res_steps_delta).
:- literal_cost(ub, res_steps, 0).

:- head_cost(ub, res_steps2, f_res_steps2_delta).
:- literal_cost(ub, res_steps2, 0).

:- entry p(X, Y) : (gnd(X), gnd(Y)).

p(X, Y) :- q(X), r(Y).

:- trust comp q(X) : gnd(X)
	+ (cost(ub, res_steps, 6), cost(ub, res_steps2, 12)).

q(_).

:- trust comp r(X) : gnd(X)
	+ (cost(ub, res_steps, 5), cost(ub, res_steps2, 10)).

r(_).
