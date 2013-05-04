:- module(_, [color_map/5], [assertions, regtypes, nativeprops,
		ciaopp(tests(resources)),
		predefres(res_all)]).

:- entry color_map/5 : var * var * var * var * var.

:- resource res_steps.

:- head_cost(ub, res_steps, 1).
:- literal_cost(ub, res_steps, 0).
:- head_cost(lb, res_steps, 1).
:- literal_cost(lb, res_steps, 0).

:- trust_default + cost(ub, res_steps, 0).
:- trust_default + cost(lb, res_steps, 0).

color_map(A, B, C, D, E) :-
	color(A), color(B), color(C), color(D), color(E),
	legal_coloring(A, B, C, D, E).

legal_coloring(A, B, C, D, E) :-
	A \== B,
	A \== C,
	A \== D,
	A \== E,
	c(B, C, D),
	C \== E.

c(X, Y, Z) :-
	X \== Y,
	X \== Z.

:- regtype color/1.

color(blue).
color(green).
color(orange).
color(red).
color(yellow).
