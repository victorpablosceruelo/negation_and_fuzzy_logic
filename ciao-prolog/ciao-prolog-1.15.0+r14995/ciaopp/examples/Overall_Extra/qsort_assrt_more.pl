:- module(qsort_assrt, [qsort/2], [assertions]).

:- use_module(library(assertions(native_props))).

:- entry qsort(A,B) : (list(A,num), ground(A), var(B)).

qsort([X|L],R) :-
	partition(L,X,L1,L2),
	qsort(L2,R2),
        qsort(L1,R1), 
        append(R1,[X|R2],R).
qsort([],[]).

:- check comp partition/4 + terminates.
:- check comp partition/4 + fails.
:- check comp partition/4 + not_fails.
:- check comp partition/4 + is_det.

:- check success partition(A,B,C,D) => int(A).

partition([],_B,[],[]).
partition([E|R],C,[E|Left1],Right):- 
	E < C,
	partition(R,C,Left1,Right).
partition([E|R],C,Left,[E|Right1]):-
	E >= C,
	partition(R,C,Left,Right1).

:- check comp append(A,B,C) + steps(length(A)+1).

append([],X,X).
append([H|X],Y,[H|Z]):- append(X,Y,Z).
