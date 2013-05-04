/*
 The palindrome program from DhDW's thesis 
 exported: serialize0/2, with mode < ++, - >
*/

:- module(serialize, [serialize0/2], [ assertions ]).

:- entry serialize0(X,Y) : ( var(Y), ground(X) ).

goal(Y):- palin(X), serialize0(X,Y).

serialize0(L,R) :-
	pairlists(L,R,A),
	arrange0(A,T),
	numbered(T,1,_N).

pairlists([X|L],[Y|R],[pair(X,Y)|A]) :- pairlists(L,R,A).
pairlists([],[],[]).

arrange0([X|L],tree(T1,X,T2)) :-
	split0(L,X,L1,L2),
	arrange0(L1,T1),
	arrange0(L2,T2).
arrange0([],void).

split0([pair(X1,N1)|L],pair(X2,Y2),[pair(X1,N1)|L1],L2) :-
	X1 < X2, !,
	split0(L, pair(X2,Y2), L1,L2).
split0([pair(X1,N1)|L],pair(X2,N2),L1,L2) :-
	X1 =:= X2, N1 = N2, !,
	split0(L,pair(X2,N2),L1,L2).
split0([pair(X1,N1)|L],pair(X2,Y2),L1,[pair(X1,N1)|L2]) :-
	X1 > X2, !,
	split0(L, pair(X2,Y2), L1,L2).
split0([],_,[],[]).

numbered(tree(T1,pair(_X,N1),T2),N0,N) :-
	numbered(T1,N0,N1),
	N2 is N1 + 1,
	numbered(T2,N2,N).
numbered(void,N,N).

palin("ABLE WAS I ERE I SAW ELBA").
