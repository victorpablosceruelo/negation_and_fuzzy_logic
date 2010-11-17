:- module(qsort2, _, [assertions]).

:- use_module(.(neg)).

:- entry qsort(A,B) : (list(A, num), var(B), ground(A)).
:- trust success neg(X) => true.

qsort([X|L],R) :-
	partition(L,X,L1,L2),
	qsort(L2,R2),
        qsort(L1,R1), 
        append(R1,[X|R2],R).
qsort([],[]).

partition([],_,[],[]).
partition([E|R],C,[E|Left1],Right):-
        less(E, C),
	partition(R,C,Left1,Right).
partition([E|R],C,Left,[E|Right1]):-
        greatereq(E, C),
	partition(R,C,Left,Right1).

append([],X,X).
append([H|X],Y,[H|Z]):- append(X,Y,Z).

less(E, C):- E < C.
greatereq(E, C):- E >= C.


prueba_2(A,B):- 
	neg(qsort(A,B)).
prueba_3(A,B,C):-
	neg(append(A,B,C)).
prueba_4(A,B,C,D):- 
	neg(partition(A,B,C,D)).



