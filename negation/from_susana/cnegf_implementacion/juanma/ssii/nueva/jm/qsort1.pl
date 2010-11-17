:- module(_qsort1, [eq_list/2, neq_list/2, qsort/2], [assertions]).

:- use_module(neg). %% Como dices que tu lo pones lo he comentado
                     %% pero en principio estaria mejor si esto
                     %% hubiera que ponerlo aqui porque si no el 
                     %% fichero qsort1.pl no funciona ni compila
                     %% por si solo.

 
:- entry eq_list(L1,L2) : (list(L1, num), list(L2, num), 
                            ground(L1), ground(L2)).
:- entry neq_list(L1,L2) : (list(L1, num), list(L2, num), 
                            ground(L1), ground(L2)).
:- entry qsort(A,B) : (list(A, num), var(B), ground(A)).

:- trust success neg(X) => true.

eq_list(L1,L2):-
	neg(neq_list(L1,L2)).  %% Creo que este lo deja como neg

neq_list(L1,L2):- 
	qsort(L1,L),
	neg(qsort(L2,L)).  %% Creo que este lo debe cambiar a cnegf

qsort([X|L],R) :-
	partition(L,X,L1,L2),
	qsort(L2,R2),
        qsort(L1,R1), 
        append(R1,[X|R2],R).
qsort([],[]).

partition([],_B,[],[]).

partition([E|R],C,[E|Left1],Right):- 
	E < C, !,
	partition(R,C,Left1,Right).
partition([E|R],C,Left,[E|Right1]):-
	E >= C,
	partition(R,C,Left,Right1).

append([],X,X).
append([H|X],Y,[H|Z]):- append(X,Y,Z).
