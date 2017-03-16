:- module(color_map, [color_map/5], [assertions, regtypes,
		ciaopp(tests(resources)),
		predefres(res_all)]).

:- entry color_map/5 : var * var * var * var * var.

color_map(A, B, C, D, E) :-
	color(A), color(B), color(C), color(D), color(E),
	legal_coloring(A, B, C, D, E).

legal_coloring(A, B, C, D, E) :-
	not_equal(A, B),
	not_equal(A, C),
	not_equal(A, D),
	not_equal(A, E),
	c(B, C, D),
	not_equal(C, E).

c(X, Y, Z) :-
	not_equal(X, Y),
	not_equal(X, Z).

:- regtype color/1.

color(blue).
color(green).
color(orange).
color(red).
color(yellow).

:- load_resource_module(color_map_res).

:- resource max_number_of_unifs.
:- head_cost(ub, max_number_of_unifs, delta_max_number_of_unifs).
:- literal_cost(ub, max_number_of_unifs, 0).
:- trust_default + cost(ub, max_number_of_unifs, 0).

:- resource calls_to_builtins.
:- head_cost(ub, calls_to_builtins, delta_calls_to_builtins).
:- literal_cost(ub, calls_to_builtins, 0).
:- trust_default + cost(ub, calls_to_builtins, 0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% :- impl_defined([not_equal/2]).
:- trust pred not_equal/2: color * color
	+ cost(ub, max_number_of_unifs, 3).

not_equal(X, Y) :- X \== Y.
