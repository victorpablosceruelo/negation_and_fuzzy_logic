:- module(iqueen5, [iqueen/5], [assertions, nativeprops,
		ciaopp(tests(resources)),
		predefres(res_all)]).

:- doc(author, "Nai-Wei Lin").
:- doc(author, "Edison Mera").

:- doc(module, "This program plays the inverse n-queens game.").

:- resource res_steps.

:- head_cost(ub, res_steps, 1).
:- head_cost(lb, res_steps, 1).

:- literal_cost(ub, res_steps, 0).
:- literal_cost(lb, res_steps, 0).

:- trust_default + cost(ub, res_steps, 0).
:- trust_default + cost(ub, res_steps, 0).

:- entry iqueen/5 : int * int * int * int * int.

iqueen(X1, X2, X3, X4, X5) :-
	safe([X1, X2, X3, X4, X5]).

safe([]).
safe([X|L]) :-
	attacks(L, X, 1),
	safe(L).

:- trust comp attacks(A, B, C)
	+ (size_metric(A, length), size_metric(B, int), size_metric(C, void)).

attacks([],    _, _).
attacks([Y|L], X, D) :-
	attack(X, Y, D),
	D1 is D +1,
	attacks(L, X, D1).

:- trust comp attack(A, B, C)
	+ (size_metric(A, int), size_metric(B, int), size_metric(C, void)).

attack(X, Y, _) :-
	X =:= Y, !.
attack(X, Y, D) :-
	Y-X =:= D, !.
attack(X, Y, D) :-
	Y-X =:= -D.
