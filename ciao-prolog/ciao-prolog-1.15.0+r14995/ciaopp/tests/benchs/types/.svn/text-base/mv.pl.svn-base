:- module(mv, [mv/3], [assertions]).

:- use_module(engine(arithmetic), [(<)/2, (>=)/2]).
:- use_module(engine(basic_props), [list/2]).
:- use_module(engine(term_basic), [(=)/2]).

:- entry mv(A,B,C) : list(num) * list(num) * var.

mv(A,B,C):- qsort(A,B), 
            !, 
            C = B.
mv(A,B,C):- append(A,B,D), qsort(D, C). 

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
