% 9-queens program
% Finds the first solution to the problem

#include "../mtsys_common.pl"

benchmark_data(queens, 50, [1,2,3,4,5,6,7,8,9]).

benchmark(Data, Out) :-
	queen(Data, Out).

queen(Data, Out) :-
	queen_2(Data, [], Out).

queen_2([], _, []).
queen_2([H|T], History, [Q|M]) :-
	qdelete(Q, H, T, L1),
	nodiag(History, Q, 1),
	queen_2(L1, [Q|History], M).

qperm([], []).
qperm([X|Y], [U|V]) :-
	qdelete(U, X, Y, Z),
	qperm(Z, V).

qdelete(A, A, L, L).
qdelete(X, A, [H|T], [A|R]) :-
	qdelete(X, H, T, R).

safe([]).
safe([N|L]) :-
	nodiag(L, N, 1),
	safe(L).

nodiag([], _, _).
nodiag([N|L], B, D) :-
	D =\= N - B,
	D =\= B - N,
	D1 is D + 1,
	nodiag(L, B, D1).
