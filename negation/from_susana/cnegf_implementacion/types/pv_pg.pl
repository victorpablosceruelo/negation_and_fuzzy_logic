:- module(pv_pg, [pdsbm/2], [assertions]).

% :- use_module(library(lists), [list/2, list1/2]).

:- entry pdsbm(A,B): (num(A), var(B)).

% main(N,X):- integer(N), pdsbm(N,X).
 
pdsbm(N,X):- pds(N, [0,1|X]).

% changed -PL pdsbm(N,X):- pds(N, [0,1|X]).

ismember( X, [X|_Xs]).
ismember( X, [_|Xs]):- ismember(X,Xs).

iota(N,List):-
   iota1(0,N,List).  
iota1(K,K,[]).
iota1(K,N,[K|List]):- K1 is K+1, iota1(K1,N,List).

dif( [],_,_,[],[]).
dif( [S|Ss],Val,Mod,[D|Ds],[D2|D2s]):-
     D is Val - S, D2 is Mod - D,
     dif( Ss,Val,Mod,Ds,D2s).
    

rev( [],L,L).
rev( [X|Xs], Y,L):-
   rev( Xs,[X|Y],L).


mergedelete([],L,L).
mergedelete([D|Ds], [D|R], L2):-
        mergedelete( Ds, R, L2).
mergedelete( [D|Ds], [X|R], [X|L2]):-
        D > X,
        mergedelete( [D|Ds],R,L2).
       
check( [],_,L,L,_).
check( S, Choice, Old, L3,Modulus):-
      S = [_|_],
      dif( S, Choice,Modulus, Ds,Dds),
      mergedelete( Ds, Old,L2),
      rev( Dds,[],Rds),
      mergedelete(Rds, L2,L3).


pds1( [],_,[],_).
pds1( Unused, List, [Choice|Rest],Mod):-
     ismember(Choice,Unused),
     check( List, Choice, Unused,U3,Mod),
     pds1( U3, [Choice| List], Rest, Mod).
    
pds( Order, [0|Ans]):-
    N is Order * (Order + 1) + 1,
    iota( N, [0|List]),
    pds1( List, [0], Ans, N).

%% Control version comment prompting for the file.
%% Local Variables: 
%% mode: CIAO
%% update-version-comments: "on"
%% End:

