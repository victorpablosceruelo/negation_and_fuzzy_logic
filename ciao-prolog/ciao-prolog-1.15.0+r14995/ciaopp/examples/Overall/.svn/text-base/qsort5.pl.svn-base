:- module(_, [qsort/2], [assertions]).

:- entry qsort(A,B) : (list(A,num),ground(A)).

:- calls qsort(A,B) : list(A, num).                        
:- success qsort(A,B)  => (ground(B), sorted_num_list(B)). 

qsort([X|L],R) :-
        partition(L,X,L1,L2),
        qsort(L2,R2), qsort(L1,R1), 
        append(R2,[x|R1],R). 
qsort([],[]).

:- calls partition(A,B,C,D) : (ground(A), ground(B)).      
:- success partition(A,B,C,D) => (ground(C), list(D, num)). 

partition([],_B,[],[]).
partition([E|R],C,[E|Left1],Right):- 
        E < C, !, partition(R,C,Left1,Right).
partition([E|R],C,Left,[E|Right1]):-
        E >= C,   partition(R,C,Left,Right1).

:- calls append(A,B,C) : (list(A,num),list(B,num)).

append([],X,X).
append([H|X],Y,[H|Z]):- append(X,Y,Z).

:- prop sorted_num_list/1.

sorted_num_list([]).
sorted_num_list([X]):- number(X).
sorted_num_list([X,Y|Z]):- 
        number(X), number(Y), X<Y, sorted_num_list([Y|Z]).
