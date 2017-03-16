:- module(_, [main/0], [hiord, assertions, hmtypes_check]).

%:- type_check_options([check(off),verbose(off)]).
:- runtime_hm_type_check(on).

main :-
	do_queens.

do_queens :-
        queens(5, Qs),
	display(Qs),nl,
        fail.
do_queens.

:- pred queens/2 :: integer * list(integer) + hmtyped.
queens(N, Qs):-
        queens_list(N, Ns),
        queens_2(Ns, [], Qs).

:- pred queens_2/3 :: list(integer) * list(integer) * list(integer) + hmtyped.
queens_2([], Qs, Qs).
queens_2(Unplaced, Placed, Qs):-
        sel(Q, Unplaced, NewUnplaced),
        no_attack(Q, Placed),
        queens_2(NewUnplaced, [Q|Placed], Qs).

:- pred no_attack/2 :: integer * list(integer) + hmtyped.
no_attack(Q, Safe):- no_attack_2(Safe, Q, 1).

% TODO: there should be type inference and this assertion should be redundant
:- pred no_attack_2/3 :: list(integer) * integer * integer + hmtyped.
no_attack_2([], _Queen, _Nb).
no_attack_2([Y|Ys], Queen, Nb) :-
        A is Y + Nb,
        Queen =\= A,
        B is Y - Nb,
        Queen =\= B,
        Nb1 is Nb + 1,
        no_attack_2(Ys, Queen, Nb1).

% TODO: sel(X, [a,1], Y) should be possible for T=any; but it does not
%       seem possible for type parameters to be 'any'
:- pred sel/3 :: call(T) * list(T) * list(T) + hmtyped.
sel(X, [X|Ys], Ys).
sel(X, [Y|Ys], [Y|Zs]):-
        sel(X, Ys, Zs).

:- pred queens_list/2 :: integer * list(integer) + hmtyped.
queens_list(0, []).
queens_list(N, [N|Ns]) :-
        N > 0,
        N1 is N - 1,
        queens_list(N1, Ns).
