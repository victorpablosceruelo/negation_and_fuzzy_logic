:- module(hanoi, [hanoi/5], [assertions]).

% :- use_module(library(lists), [list/2, list1/2]).

:- entry hanoi(A,B,C,D,E): (ground([A,B,C,D]), num(A), var(E)).

append([],L,L).
append([X|L1],L2,[X|L3]) :-
	append(L1,L2,L3).

hanoi(1,A,_B,C,[mv(A,C)]):-!.
hanoi(N,A,B,C,M) :-
	N1 is N - 1,
	hanoi(N1,A,C,B,M1),
	hanoi(N1,B,A,C,M2),
	append(M1,[mv(A,C)],T),
	append(T,M2,M).



%% Control version comment prompting for the file.
%% Local Variables: 
%% mode: CIAO
%% update-version-comments: "on"
%% End:

