:- module(qsort_func, [qsort/2], [assertions,fsyntax]).

:- entry qsort(A,B) : (list(A,num), ground(A), var(B)).

qsort([])    := [].
qsort([X|L]) := ~append( ~qsort(L1), [X|~qsort(L2)] ) :- partition(L,X,L1,L2).

append( []    , X) := X.
append( [H|X] , Y) := [ H | ~append(X,Y) ].

partition([],_B,[],[]).
partition([E|R],C,Small,Large):- 
	( E < C   -> Small = [E|NSmall], Large = NLarge
	; E >= C,    Small = NSmall,     Large = [E|NLarge] ),
	partition(R,C,NSmall,NLarge).
