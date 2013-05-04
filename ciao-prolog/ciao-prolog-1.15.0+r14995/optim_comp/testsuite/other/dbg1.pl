:- module(_, _, []).

:- '$pragma'(insert_debug_info).

do_queens:-
        queens(5, Qs),
	display(Qs), nl,
	fail.
do_queens.

queens(N, Qs):-
        queens_list(N, Ns),
        queens_2(Ns, [], Qs).

queens_2([], Qs, Qs).
queens_2(Unplaced, Placed, Qs):-
        sel(Q, Unplaced, NewUnplaced),
        no_attack(Q, Placed),
        queens_2(NewUnplaced, [Q|Placed], Qs).

no_attack(Q, Safe):- no_attack_2(Safe, Q, 1).

no_attack_2([], _Queen, _Nb).
no_attack_2([Y|Ys], Queen, Nb) :-
        A is Y + Nb,
	Queen =\= A,
        B is Y - Nb,
	Queen =\= B,
        Nb1 is Nb + 1,
        no_attack_2(Ys, Queen, Nb1).

sel(X, [X|Ys], Ys).
sel(X, [Y|Ys], [Y|Zs]):-
        sel(X, Ys, Zs).

queens_list(0, []).
queens_list(N, [N|Ns]) :-
        N > 0,
        N1 is N - 1,
        queens_list(N1, Ns).
