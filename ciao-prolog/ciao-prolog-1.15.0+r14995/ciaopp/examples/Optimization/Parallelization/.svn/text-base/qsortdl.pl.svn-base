:- module(qsortdl,[qsort/2],[assertions]).

:- entry qsort/2 : ground * var.

qsort(X,Y) :- qsort_(X,Y,[]).

qsort_([],X,X).
qsort_([First|L1],L2,T) :-
	partition(First,L1,Ls,Lg), 
	qsort_(Ls,L2,[First|Lg2]),
	qsort_(Lg,Lg2,T).

partition(_,[],[],[]).
partition(F,[X|Y],[X|Y1],Y2) :- 
	X @=< F, 
	partition(F,Y,Y1,Y2).
partition(F,[X|Y],Y1,[X|Y2]) :- 
	X @> F,
	partition(F,Y,Y1,Y2). 




