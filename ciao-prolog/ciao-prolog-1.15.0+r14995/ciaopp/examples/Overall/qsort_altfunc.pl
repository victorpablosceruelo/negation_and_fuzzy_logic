
:- module( qsort_altfunc, [qsort/2], [assertions,regtypes] ).

:- entry qsort(As,Bs) : ( ground(As), var(Bs) ).

:- use_module(mytypes).

:- op(100,xfy,(&)).

qsort([],[]).
qsort(X & L, S):- 
	partition(L,X,L1,L2),
	qsort(L1,L1S),
	qsort(L2,L2S),
	append(L1S, X & L2S, S).

partition([],_,[],[]).
partition(E & R,C,E & Left1,Right):- E < C, !,
	partition(R,C,Left1,Right).
partition(E & R,C,Left,E & Right1):- E >= C,
	partition(R,C,Left,Right1).

append([],X,X).
append(H & X,Y,H & Z) :- append(X,Y,Z).
