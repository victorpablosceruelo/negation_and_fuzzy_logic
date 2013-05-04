:- module(qsort, [qsort/2], [assertions]).

:- use_module(engine(arithmetic), [(<)/2, (>=)/2]).
%:- use_module(engine(basic_props),[list/2,num/1]).
%:- use_module(library(assertions(native_props)),[not_fails/1,var/1]).

:- entry qsort(A,B) : list(num) * var.

% :- comp qsort(A,B) + not_fails.

qsort([X|L],R) :-
	partition(L,X,L1,L2),
	qsort(L2,R2),
        qsort(L1,R1), 
        append(R1,[X|R2],R).
qsort([],[]).

partition([],_,[],[]).
partition([E|R],C,[E|Left1],Right):-
	E < C, !,
	partition(R,C,Left1,Right).
partition([E|R],C,Left,[E|Right1]):-
	E >= C,
	partition(R,C,Left,Right1).

append([],X,X).
append([H|X],Y,[H|Z]):- append(X,Y,Z).
