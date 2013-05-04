:- module(_rev,[rev/2],[assertions]).

:- entry rev([1,2,3,4|L],R).

rev(A,B):-
	rev_ac(A,[],B).

rev_ac([],L,L).
rev_ac([X|Xs],Tmp,L):-
	rev_ac(Xs,[X|Tmp],L).
