:- module(partition_1, [dummy/3], [assertions, nativeprops,
		ciaopp(tests(resources)), predefres(res_all)]).

:- doc(author, "Edison Mera").

:- resource res_steps.

:- head_cost(ub, res_steps, 1).
:- literal_cost(ub, res_steps, 0).

:- head_cost(lb, res_steps, 1).
:- literal_cost(lb, res_steps, 0).

:- trust_default + cost(ub, res_steps, 0).
:- trust_default + cost(lb, res_steps, 0).

:- entry dummy(A, B, C) : list(num) * num * var.

dummy(A, B, R) :-
	partition(A, B, C, D),
	R = (C, D).

partition([],    _, [],        []).
partition([E|R], C, [E|Left1], Right) :-
	E < C, !,
	partition(R, C, Left1, Right).
partition([E|R], C, Left, [E|Right1]) :-
	E >= C,
	partition(R, C, Left, Right1).
