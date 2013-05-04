:- module(qsort_nd, [seq/2, par_nondet/2, data/1],[]).

:- use_package(andprolog_nd).
:- use_module(library(lists), [append/3,length/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%Size 30 y frec 4 are good for testing.
size(4000).  
gc(50).
frec(1000).

data([L,Size]) :- size(N), frec(F), gen_list(N,F,Lt), permutation(Lt,L), length(L,Size). 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

seq([L,_],X) :- qsort_seq(L,X).
par_nondet([L,Size],X) :- gc(GC), qsort_par_gc(L,Size,GC,X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

qsort_seq([], []) :- !.
qsort_seq([X|L], R) :-
	partition(L, X, L1, L2, max, min),
        qsort_seq(L1, R1), 
	qsort_seq(L2, R2), 
        append(R1, [X|R2], R).

qsort_par([], []) :- !.
qsort_par([X|L], R) :-
        partition(L, X, L1, L2, max, min),
        qsort_par(L1, R1) & qsort_par(L2, R2),
        append(R1, [X|R2], R).

partition([], _B, [], [], _, _) :- !.
partition([E|R], C, [E|Left1], Right, Max, Min) :- 
	less_than(E,C), !,
	check_min(E,Min),
	new_max(E,Max,NMax),
	partition(R, C, Left1, Right, NMax, Min).
partition([E|R], C, Left, [E|Right1], Max, Min) :-
	less_than(C,E), !,
	check_max(E,Max),
	new_min(E,Min,NMin),
	partition(R, C, Left, Right1, Max, NMin).
partition([E|R], C, [E|Left1], Right, Max, Min) :- 
	check_min(E,Min),
	new_max(E,Max,NMax),
	partition(R, C, Left1, Right, NMax, Min).
partition([E|R], C, Left, [E|Right1], Max, Min) :-
	check_max(E,Max),
	new_min(E,Min,NMin),
	partition(R, C, Left, Right1, Max, NMin).

new_min(E,min,E) :-  !.
new_min(E,Min,E) :- E < Min, !.
new_min(_,Min,Min).

new_max(E,max,E) :-  !.
new_max(E,Max,E) :- E > Max, !.
new_max(_,Max,Max).

check_min(_,min) :- !.
check_min(E,Min) :- E < Min.

check_max(_,max) :- !.
check_max(E,Max) :- E > Max.

less_than(N,M) :-
	1 is mod(N,2),
	1 is mod(M,2),
	N < M.

 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% %% Versions with granularity control
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

qsort_par_gc([], _InputLen, _GranLevel, []) :- !.
qsort_par_gc([X|L], InputLen, GLev, R) :-
        (
            InputLen > GLev ->
            partition_gc(L, X, L1, L2, N1, N2, max, min),
	    qsort_par_gc(L1, N1, GLev, R1) & qsort_par_gc(L2, N2, GLev, R2),
            append(R1, [X|R2], R)
        ;
            qsort_seq([X|L], R)
        ).

partition_gc([], _B, [], [], 0, 0, _, _) :- !.
partition_gc([E|R], C, [E|Left1], Right, NLL, LR, Max, Min) :- 
	less_than(E,C), !,
	check_min(E,Min),
	new_max(E,Max,NMax),
	partition_gc(R, C, Left1, Right, LL, LR, NMax, Min),
	NLL is LL + 1.
partition_gc([E|R], C, Left, [E|Right1], LL, NLR, Max, Min) :-
	less_than(C,E), !,
	check_max(E,Max),
	new_min(E,Min,NMin),
	partition_gc(R, C, Left, Right1, LL, LR, Max, NMin),
	NLR is LR + 1.
partition_gc([E|R], C, [E|Left1], Right, NLL, LR, Max, Min) :- 
	check_min(E,Min),
	new_max(E,Max,NMax),
	partition_gc(R, C, Left1, Right, LL, LR, NMax, Min),
	NLL is LL + 1.
partition_gc([E|R], C, Left, [E|Right1], LL, NLR, Max, Min) :-
	check_max(E,Max),
	new_min(E,Min,NMin),
	partition_gc(R, C, Left, Right1, LL, LR, Max, NMin),
	NLR is LR + 1.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

gen_list(0, _, []) :- !.
gen_list(N, 0, [N|L]) :-
	1 is mod(N,2), !,
	N1 is N - 1,
	gen_list(N1, 0, L).
gen_list(N, 0, [N|L]) :-
	0 is mod(N,2), !,
	N1 is N - 1,
	frec(F),
	gen_list(N1, F, L).
gen_list(N, F, L) :-
	0 is mod(N,2), !,
	N1 is N - 1,
	F1 is F - 1,
	gen_list(N1, F1, L).
gen_list(N, F, [N|L]) :-
	N1 is N - 1,
	gen_list(N1, F, L).

permutation(L1, L2) :-
	length(L1, N),
	permutation_(L1, N, L2).
permutation_(_, 0, []) :- !.
permutation_(Xs, N, [X|Zs]) :-
	N > 0,
        R is N*N*N*N*N*N*N*N*N*N*N*N*N mod 7919 mod N,
	I is R + 1,
	remove_at(X, Xs, I, Ys),
	N1 is N - 1,
	permutation_(Ys, N1, Zs).
remove_at(X, [X|Xs], 1, Xs).
remove_at(X, [Y|Xs], K, [Y|Ys]) :-
	K > 1,
	K1 is K - 1,
	remove_at(X, Xs, K1, Ys).
