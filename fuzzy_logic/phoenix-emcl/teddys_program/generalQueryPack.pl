%:- module(housesQuery,_,[rfuzzy,clpr,expsearch,housesDBN]).
:- use_package(rfuzzy).
:- use_package(clpr).
:- use_package(.(qualifiersPack)).
:- use_module(engine(hiord_rt)).
:- use_module(library(lists)).

%GENERAL QUERIES
%--------------------------------------------------------------------------------------
%--------------------------------------------------------------------------------------
%For the Fuzzy part : for any list of fuzzy predicates.
gQueryFZ([],_,1).
gQueryFZ([(Q1,Q2,P)|T],X,Y):- solve(Q1,Q2,P,X,Y1),gQueryFZ(T,X,Y2),Y .=. Y1 * Y2.

%For the Crisp part : for a list of crisp predicates.
gQueryCR([],_).
gQueryCR([(Attr,C,V)|T],X):- get(Attr,X,Y),call(C,Y,V),gQueryCR(T,X).

%Combining the two we get the general query.
gQuery(Fl,Cl,X,Y):- gQueryCR(Cl,X),gQueryFZ(Fl,X,Y).
%--------------------------------------------------------------------------------------
%OPTIONS: Adding option for how to view the result of the general query.
%----------------------------------------------------------------------------------------
%getList generates the result as a list of pairs where each pair has an Identifier and the correponding value
getList(F,C,Pairs):-  findall(X,gQuery(F,C,X,Y),Ids),findall(Y,gQuery(F,C,X,Y),Vals),combineLists(Ids,Vals,Pairs).
combineLists([],_,[]).
combineLists([H1|T1],[H2|T2],[(H1,H2)|L]):- combineLists(T1,T2,L).

%Sorts the list based on the value descending
qsort([],[]).
qsort([(Id,Val)|T],L):- filter1(Val,T,L1,L2), qsort(L1,L3),qsort(L2,L4),append(L3,[(Id,Val)],L5),append(L5,L4,L).

filter1(_,[],[],[]).
filter1(A,[(Id,Val)|T],[(Id,Val)|L1],L2):- greaterthan(Val,A),!,filter1(A,T,L1,L2).
filter1(A,[H|T],L1,[H|L2]):- filter1(A,T,L1,L2).

%*This is the option to return all of the result in descending order.
allsorted(F,C,L):- getList(F,C,Lis),qsort(Lis,L).

%*This is the option to return only the best result.
thebest(F,C,H):- allsorted(F,C,[H|_]).

%*This is the option to return all results whose value is not 0.
allnonzeros(F,C,L1):-allsorted(F,C,L), removezeros(L,L1).

removezeros([],[]).
removezeros([(Id,Val)|T],[(Id,Val)|L]):- Val .>. 0,!, removezeros(T,L). 
removezeros([_H|T],L):- removezeros(T,L).

%------------------------------------------------------------------------------------------------
%GENERAL QUERY : This is the main query that adds option to the general query defined above
%-------------------------------------------------------------------------------------------------
%the options possible to be used are 'all','best', and 'nonzero' for the 3 cases
%------------------------------------------------------------------------------------------------
genQuery(F,C,all,L):- allsorted(F,C,L).
genQuery(F,C,best,L):- thebest(F,C,L).
genQuery(F,C,nonzero,L):- allnonzeros(F,C,L).
%------------------------------------------------------------------------------------------------