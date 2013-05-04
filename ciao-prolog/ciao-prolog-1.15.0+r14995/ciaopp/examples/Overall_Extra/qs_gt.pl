:- module(qs_gt, [qsort/2], [assertions]).

:- use_module(library(assertions(native_props))).

:- entry qsort(A,B) : (list(A,num), ground(A), var(B)).

qsort([X|L],R) :-
	partition(L,X,L1,L2),
	qsort(L2,R2),
        qsort(L1,R1), 
        append(R1,[X|R2],R).
qsort([],[]).

 %% :- check comp partition(A,B,C,D) + terminates.
 %% :- check comp partition/4 + fails.
 %% :- check success partition(A,B,C,D) => int(A).

 %% :- check comp partition(A,B,C,D) + terminates.
 %% :- check comp partition/4 + not_fails.
 %% :- check comp partition/4 + is_det.

partition([],_B,[],[]).
partition([E|R],C,[E|Left1],Right):- 
        lt(E, C),
	partition(R,C,Left1,Right).
partition([E|R],C,Left,[E|Right1]):-
	geq(E, C),
	partition(R,C,Left,Right1).

lt(E, C):- E < C.

geq(E, C):- E >= C.

append([],X,X).
append([H|X],Y,[H|Z]):- append(X,Y,Z).
