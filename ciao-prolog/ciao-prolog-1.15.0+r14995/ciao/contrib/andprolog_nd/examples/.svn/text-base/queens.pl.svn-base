:- module(queens,
	[
	    seq/2,
	    par_nondet/2,
	    data/1
	],
	[andprolog_nd]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

size(10).
gc(10).

data(X) :- size(X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

seq(N,X) :- q_seq(N,X).
par_nondet(N,X) :- gc(GC), q_par(N,GC,X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

q_seq(N,Qs) :-
	queens_seq(N,Qs).

q_par(N,G,Qs) :-
	queens_par(N,G,Qs).

queens_seq(N, Qs):-
        queens_list(N, Ns),
        solve_queens_seq(Ns, [], Qs).    % To place, placed, result

solve_queens_seq([], Qs, Qs).
solve_queens_seq(Unplaced, Placed, Qs):-
        select_queen(Q, Unplaced, NewUnplaced),
        no_attack(Q, Placed),
        solve_queens_seq(NewUnplaced, [Q|Placed], Qs).

queens_par(N, Gran, Qs):-
        queens_list(N, Ns),
        solve_queens_par(Ns, N, Gran, [], Qs).    % To place, placed, result

solve_queens_par([], _, _, Qs, Qs).
solve_queens_par(Unplaced, N, Gran, Placed, Qs):-
	N1 is N - 1,
        select_queen(Q, Unplaced, NewUnplaced),
	(
	    N < Gran ->
	    no_attack(Q, Placed),
	    solve_queens_seq(NewUnplaced, [Q|Placed], Qs)
	;
	    no_attack(Q, Placed) &
            solve_queens_par(NewUnplaced, N1, Gran, [Q|Placed], Qs)
	).

no_attack(Q, Safe):- no_attack_acc(Safe, Q, 1).

no_attack_acc([], _Queen, _Nb).
no_attack_acc([Y|Ys], Queen, Nb):-
        Queen =\= Y + Nb,
        Queen =\= Y - Nb,
        Nb1 is Nb + 1,
        no_attack_acc(Ys, Queen, Nb1).

select_queen(X, [X|Ys], Ys).
select_queen(X, [Y|Ys], [Y|Zs]):-
        select_queen(X, Ys, Zs).

queens_list(0, []).
queens_list(N, [N|Ns]):-
        N > 0,
        N1 is N - 1,
        queens_list(N1, Ns).

