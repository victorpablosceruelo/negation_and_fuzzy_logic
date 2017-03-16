#include "../mtsys_common.pl"

benchmark_data(nreverse, 100, Xs) :-
	mylist(500, Xs).

benchmark(Xs, Out) :-
	nreverse(Xs, Out).

#if OPTIMIZED
% :- '$props'(mylist/2, [imp=semidet]).
% :- '$props'(mylist/2, [argmodes=[in,out]]).
#endif
mylist(0, []) :- !.
mylist(N, [N|Xs]) :-
    N1 is N - 1,
    mylist(N1, Xs).

#if OPTIMIZED
% :- '$trust_entry'(nreverse/2, sht, [nonvar, var]).
% :- '$props'(nreverse/2, [imp=semidet]).
% :- '$props'(nreverse/2, [argmodes=[in,out]]).
#endif
nreverse([], []).
nreverse([X|Xs], Ys):-
    nreverse(Xs, Ys0),
    append(Ys0, [X], Ys).

#if OPTIMIZED
% :- '$trust_entry'(append/3, sht, [reclist(any), reclist(any), var]).
% :- '$props'(append/3, [imp=semidet]).
% :- '$props'(append/3, [argmodes=[in,in,out]]).
#endif
append([], Ys, Ys).
append([X|Xs], Ys, [X|Zs]) :-
    append(Xs, Ys, Zs).
