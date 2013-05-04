:- module(_, [append/3], [assertions, nativeprops, regtypes,
		ciaopp(tests(resources)),
		predefres(res_all)]).

:- redefining(append/3).

:- doc(author, "Edison Mera").

:- doc(module, "This program appends two lists.").

:- resource res_steps.
:- resource calls_to_append_by_3.
:- resource res_steps_2.

:- entry append(Xs, Ys, Zs) : (list(Xs, num), list(Ys, num), var(Zs)).

:- head_cost(ub, res_steps, 1).
:- literal_cost(ub, res_steps, 0).

:- head_cost(lb, res_steps, 1).
:- literal_cost(lb, res_steps, 0).

:- head_cost(ub, calls_to_append_by_3, 3).
:- literal_cost(ub, calls_to_append_by_3, 3).

:- head_cost(ub, res_steps_2, 1).
:- literal_cost(ub, res_steps_2, 0).

append([],     Y,  Y).
append([X|Xs], Ys, [X|Zs]) :-
	append(Xs, Ys, Zs).
