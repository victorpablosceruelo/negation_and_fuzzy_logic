:- module(_, [eval_polynom/3], [assertions,
		ciaopp(tests(resources)),
		library(resdefs(resources_decl))]).

:- entry eval_polynom(A, B, C) : (list(A, num), num(B), var(C)).

eval_polynom([],     _X, 0).
eval_polynom([C|Cs], X,  Value) :-
	eval_polynom(Cs, X, Value0),
	Value is C + X * Value0.

:- load_resource_module(eval_polynom_res).
:- resource error_propagation.
:- resource fpu_time. % Floating point unit time usage.

:- head_cost(ub, fpu_time, 0).
:- literal_cost(ub, fpu_time, call_fpu_time).
:- trust_default + cost(ub, fpu_time, 0).
