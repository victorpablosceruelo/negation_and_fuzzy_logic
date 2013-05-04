:- module(_, [qsort/2], [assertions,fsyntax]).

:- use_module(library(assertions(native_props))).

:- entry qsort/2 : {list(num), ground} * var.

qsort([])    := [].
qsort([X|L]) := ~append( ~qsort(L1), [X | ~qsort(L2)] )
	     :- partition(L,X,L1,L2).

append( []    , X) := X.
append( [H|X] , Y) := [ H | ~append(X,Y) ].

:- check comp partition/4 + terminates.
:- check comp partition/4 + fails.
:- check comp partition/4 + not_fails.
:- check comp partition/4 + is_det.

:- check success partition(A,B,C,D) => int(A).

partition([],_B,[],[]).
partition([E|R],C,[E|Left1], Right):- 
	E < C, !,
	partition(R,C,Left1,Right).
partition([E|R],C,Left,[E|Right1]):-
	E >= C,
	partition(R,C,Left,Right1).




%% Alt (but L1 unbound):
%% qsort([X|L]) := ~append( ~qsort(L1), [X | ~qsort(~partition(L,X,L1)) ] ).
%% Proper alt:
%% qsort([X|L]) := ~append( ~qsort(~partition(L,X,#,L2)), [X | ~qsort(L2) ] ).
