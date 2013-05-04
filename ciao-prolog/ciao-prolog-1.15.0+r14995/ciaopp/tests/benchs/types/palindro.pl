:- module(palindro, [palindrome/2], [assertions]).

:- use_module(library(lists), [list1/2]).

:- entry palindrome(X,Y) : ( ground(X), list(X), var(Y) ).

palindrome([],[]).
palindrome([First|L1],L2) :-
        palindrome(L1,Ls2),
        palindrome(L1,Lg2),
        append(Ls2,[First|Lg2],L2).

append([],L,L).
append([H|L],L1,[H|R]) :-
        append(L,L1,R).
