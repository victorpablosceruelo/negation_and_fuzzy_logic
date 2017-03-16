:- module(reverse_1, [reverse/2], [assertions, regtypes,
		ciaopp(tests(resources)),
		predefres(res_all)]).

:- resource res_steps.

:- entry reverse(Xs, Ys) : (var(Ys), list(Xs, num)).

:- trust comp reverse(X, Y) + (head_cost(ub, res_steps, 1)).
:- literal_cost(ub, res_steps, 0).


reverse([],    []).
reverse([X|T], L) :- reverse(T, L1), concat(L1, [X], L).

:- trust comp concat(X, Y, Z) + (head_cost(ub, res_steps, 1)).

concat([],     Y,  Y).
concat([X|Xs], Ys, [X|Zs]) :-
	concat(Xs, Ys, Zs).
