:- module(qsort2, [qsort/2], [assertions]).

% :- use_module(library(lists), [list/2, list1/2]).

% :- entry qsort(A,B) : (list(A, num), var(B)).

:- entry qsort(A,B) : (list(A, num), var(B), ground(A)).

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

%% Control version comment prompting for the file.
%% Local Variables: 
%% mode: CIAO
%% update-version-comments: "on"
%% End:


