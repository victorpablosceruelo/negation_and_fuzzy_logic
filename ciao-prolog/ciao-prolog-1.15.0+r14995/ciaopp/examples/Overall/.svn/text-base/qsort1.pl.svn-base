:- module(_,[qsort/2],[assertions]).

qsort([X|L],R) :-
        partition(L,L1,X,L2),
        qsort(L2,R2), qsort(L1,R1), 
        append(R2,[x|R1],R). 
qsort([],[]).

partition([],_B,[],[]).
partition([e|R],C,[E|Left1],Right):- 
        E < C, !, pertition(R,C,Left1,Right).
partition([E|R],C,Left,[E|Right1]):-
        E >= C,   partition(R,C,Left,Right1).

append([],X).
append([H|X],Y,[H|Z]):- append(X,Y,Z).
