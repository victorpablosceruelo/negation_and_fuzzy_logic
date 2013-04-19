:- module(ex_lists_ops,_,[.(cneg)]).
% :- module(ex_lists_ops,_,[.(cneg), .(debugger_pkg)]).

append1([],L,L).
append1([X|L],L1,[X|R]):-
	append1(L,L1,R).

prefix(P,L):- 
	append1(P,_S,L).
suffix(S,L):- 
	append1(_P,S,L).

sublist([],_List).
sublist([El|Sub],List):- 
	prefix(P,List),
	suffix([El|Sub],P).

no_append1(X,Y,Z):-cneg([], append1(X,Y,Z)).
no_prefix(P,L):-cneg([], prefix(P,L)).
no_suffix(S,L):-cneg([], suffix(S,L)).
no_sublist(L1,L2):-cneg([], sublist(L1,L2)).
