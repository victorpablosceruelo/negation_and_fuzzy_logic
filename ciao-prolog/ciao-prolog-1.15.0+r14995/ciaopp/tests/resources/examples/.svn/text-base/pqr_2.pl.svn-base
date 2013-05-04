:- module(pqr_2, _, [assertions, basicmodes, regtypes,
		ciaopp(tests(resources)),
		library(resdefs(resources_decl))]).

:- load_resource_module(ciaopp(tests(resources(examples(pqr_2_rt))))).

:- resource calls_to_r.

:- head_cost(ub, calls_to_r, f_calls_to_r).
:- literal_cost(ub, calls_to_r, 0).

:- entry p(+X, +Y).

p(X, Y) :- q(X), r(Y), a(X).

q(_).

:- trust pred r(+X) + (cost(ub, calls_to_r, 1)).

r(_).

a(X) :- q(X), s(X).

s(_).
