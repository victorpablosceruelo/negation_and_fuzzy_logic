:- module(qsort, [qsort/2], [assertions,regtypes,nativeprops]).

:- entry qsort(A,B) : (list(A,num), var(B)).
%
:- check comp qsort(A,_)  + steps_ub(exp(length(A),3)).
% :- false comp qsort(A,_1)
%          : intervals(length(A),[i(minusinf,1)])
%          + steps_ub(exp(length(A),3)).

% :- check comp qsort(A,_1)
%          : intervals(length(A),[i(1,plusinf)])
%          + steps_ub(exp(length(A),3)).

qsort([X|L],R) :-
	partition(L,X,L1,L2),
	qsort(L2,R2),
        qsort(L1,R1), 
        append(R1,[X|R2],R).
qsort([],[]).


partition([],_B,[],[]).
partition([E|R],C,[E|Left1],Right):- 
	E < C,
	partition(R,C,Left1,Right).
partition([E|R],C,Left,[E|Right1]):-
	E >= C,
	partition(R,C,Left,Right1).

append([],X,X).
append([H|X],Y,[H|Z]):- append(X,Y,Z).
