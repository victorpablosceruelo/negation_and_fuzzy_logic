:- module(_, [eight_queens/1], [assertions, regtypes,
		nativeprops,
		ciaopp(tests(resources)),
		predefres(res_all)]).

:- doc(author, "Nai-Wei Lin").
:- doc(author, "Edison Mera").

:- doc(module, "This program plays the 8-queens game.").

:- resource res_steps.

:- entry eight_queens/1 : var.

:- literal_cost(ub, res_steps, 0).
:- literal_cost(lb, res_steps, 0).

:- head_cost(ub, res_steps, 1).
:- head_cost(lb, res_steps, 1).

:- trust_default + cost(ub, res_steps, 0).
:- trust_default + cost(lb, res_steps, 0).

eight_queens([X1, X2, X3, X4, X5, X6, X7, X8]) :-
	generator(8, 8, [X1, X2, X3, X4, X5, X6, X7, X8]),
	queens(X1, X2, X3, X4, X5, X6, X7, X8).

queens(X1, X2, X3, X4, X5, X6, X7, X8) :-
	safe([X1, X2, X3, X4, X5, X6, X7, X8]).

safe([]).
safe([X|L]) :-
	noattacks(L, X, 1),
	safe(L).

:- trust comp noattacks(X, Y, Z) + (
	    size_metric(X, length),
	    size_metric(Y, int),
	    size_metric(Z, void)).

noattacks([],    _, _).
noattacks([Y|L], X, D) :-
	noattack(X, Y, D),
	D1 is D +1,
	noattacks(L, X, D1).

noattack(X, Y, D) :- X =\= Y, Y - X =\= D, Y - X =\= - D.

generator(0, _, []).
generator(M, N, [Q|L]) :-
	M > 0,
	choose(N, Q),
	M1 is M-1,
	generator(M1, N, L).

choose(N, N) :-
	N >= 1.
choose(N, M) :-
	N > 1,
	N1 is N-1,
	choose(N1, M).
