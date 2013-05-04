:- module( qsort_altfunc_func, [qsort/2], [assertions,regtypes,fsyntax] ).

:- entry qsort(As,Bs) : ( ground(As), var(Bs) ).

:- use_module(mytypes_func).

:- op(100,xfy,(&)).

qsort([])    := [].
qsort(X & L) := ~append( ~qsort(L1), X & ~qsort(L2) ) :- partition(L,X,L1,L2).

append( []    , X) := X.
append( H & X , Y) := H & ~append(X,Y).

partition([],_,[],[]).
partition(E & R,C,E & Left1,Right):- E < C, !,
	partition(R,C,Left1,Right).
partition(E & R,C,Left,E & Right1):- E >= C,
	partition(R,C,Left,Right1).
