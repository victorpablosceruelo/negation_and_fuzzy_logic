:- module(reverse_2, [reverse/2], [assertions, regtypes,
		ciaopp(tests(resources)),
		predefres(res_all)]).

:- load_resource_module(ciaopp(tests(resources(examples(reverse_2_rt))))).

:- resource res_steps.

:- resource res_steps_1.

:- entry reverse(X, Y) : (var(Y), list(X, num)).

:- head_cost(ub, res_steps, f_res_steps).
:- literal_cost(ub, res_steps, 0).
:- head_cost(ub, res_steps_1, f_res_steps_1).
:- literal_cost(ub, res_steps_1, 0).

reverse([],    []).
reverse([X|T], L) :- reverse(T, L1), concat(L1, [X], L).

:- trust comp concat(X, Y, Z) +
	(cost(ub, res_steps, length(X) + 1)).
:- trust comp concat(X, Y, Z) +
	(cost(ub, res_steps_1, length(X) + 1)).

concat([],     Y,  Y).
concat([X|Xs], Ys, [X|Zs]) :-
	concat(Xs, Ys, Zs).
