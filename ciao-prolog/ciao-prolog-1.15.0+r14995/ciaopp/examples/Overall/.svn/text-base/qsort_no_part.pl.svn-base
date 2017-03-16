
:- module( qsort_no_part, [qsort/2], [assertions] ).

:- use_module(partition, [partition/4]).
:- trust pred partition(A,B,C,D) => ( ground(C), ground(D) ).
:- entry qsort(As,Bs) : (ground(As), var(Bs), list(As,num)).

qsort([],[]).
qsort([X|L],S):- 
	partition(L,X,L1,L2),
	qsort(L1,L1S),
	qsort(L2,L2S),
	append(L1S,[X|L2S],S).


append([],X,X).
append([H|X],Y,[H|Z]) :- append(X,Y,Z).
