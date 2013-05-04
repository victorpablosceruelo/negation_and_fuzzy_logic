:- module(queens, [do/0]).

:- use_package(fd).
:- use_module(library(aggregates)).


do :- do_queens(8).

do_queens(N):- 
	constrain_values(N, N, Qs),
	all_different(Qs),!,
	labeling(Qs).

constrain_values(0, _N, []).
constrain_values(N, Range, [X|Xs]):-
        N > 0, 
        X in 1 .. Range,
        N1 is N - 1,
        constrain_values(N1, Range, Xs),
        no_attack(Xs, X, 1).

no_attack([], _Queen, _Nb).
no_attack([Y|Ys], Queen, Nb):-
	Nb1 is Nb + 1,
	no_attack(Ys, Queen, Nb1),
	Queen .<>. Y + Nb,
	Queen .<>. Y - Nb.	
