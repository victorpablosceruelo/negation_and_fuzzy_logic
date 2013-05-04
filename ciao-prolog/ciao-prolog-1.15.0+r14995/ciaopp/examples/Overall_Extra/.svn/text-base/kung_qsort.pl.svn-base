:- module(kung_qsort, [qsort/2], [assertions,regtypes]).

:- use_module(library(assertions(native_props))).

%% :- entry qsort(A,B) : (list(A,num), ground(A), var(B)).
%% :- entry qsort(A,B) : (num(A), ground(A), var(B)).
%% :- entry qsort(A,B) : (is_nil(A), ground(A), var(B)).

:- success qsort(A,B) => sorted(B).

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

:- regtype is_nil/1.

is_nil([]).

:- prop sorted/1.

sorted([]).
sorted([_]).
sorted([A,B|Rest]) :- A >= B, sorted([B|Rest]).
