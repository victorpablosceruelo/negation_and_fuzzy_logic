:- module(_qsort, [qsort/2], [assertions]).

:- entry qsort(A,B) : list(A,num).

qsort([X|L],R) :-
        partition(L,X,L1,L2),
        qsort(L2,R2), qsort(L1,R1), 
        append(R2,[x|R1],R). 
qsort([],[]).

partition([],_B,[],[]).
partition([E|R],C,[E|Left1],Right):- 
        E < C, !, partition(R,C,Left1,Right).
partition([E|R],C,Left,[E|Right1]):-
        E >= C,   partition(R,C,Left,Right1).

append([],X,X).
append([H|X],Y,[H|Z]):- append(X,Y,Z).
