:- module(eight_queen, [eight_queens/1], [assertions, nativeprops,
		ciaopp(tests(resources)),
		library(resdefs(resources_decl))]).

:- doc(author, "Nai-Wei Lin").
:- doc(author, "Edison Mera").

:- doc(module, "This program plays the 8-queens game.").

:- load_resource_module(eight_queen_res).
:- resource movements.
:- head_cost(ub, movements, delta_qmovements).
:- literal_cost(ub, movements, 0).
:- trust_default + cost(ub, movements, 0).

:- entry eight_queens/1 : var.
eight_queens([X1, X2, X3, X4, X5, X6, X7, X8]) :-
	generator(8, 8, [X1, X2, X3, X4, X5, X6, X7, X8]),
	queens(X1, X2, X3, X4, X5, X6, X7, X8).

queens(X1, X2, X3, X4, X5, X6, X7, X8) :-
	safe([X1, X2, X3, X4, X5, X6, X7, X8]).

safe([]).
safe([X|L]) :-
	noattacks(L, X, 1),
	safe(L).

:- trust comp noattacks(X, Y, Z) +
	( size_metric(X, length), size_metric(Y, int),
	    size_metric(Z, void) ).

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
