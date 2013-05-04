:- module(sorting,_).

sorted_list(in(L)) :- L = [].
sorted_list(in(L)) :- L = [_].
sorted_list(in(L)) :- L = [X,Y|R], X < Y, sorted_list(in([Y|R])).

%% isort([],[]).
%% isort([X|R],L_p) :- isort(R,R_p),insert(X,R_p,L_p).
%% 
%% insert(X,[],[X]).
%% insert(X,[Y|R],[X,Y|R]) :- X =< Y.
%% insert(X,[Y|R],[Y|R_p]) :- X > Y,insert(X,R,R_p).
%% 
%% qsort(L,Ls) :- qsort_(L,Ls,[]).
%% qsort_([X|L],R,R0) :-
%%         partition(L,X,L1,L2),
%%         qsort_(L2,R1,R0),
%%         qsort_(L1,R,[X|R1]).
%% qsort_([],R,R).
%% 
%% partition([X|L],Y,[X|L1],L2) :- X =< Y, % !
%%         partition(L,Y,L1,L2).
%% partition([X|L],Y,L1,[X|L2]) :- X > Y,
%%         partition(L,Y,L1,L2).
%% partition([],_,[],[]).
