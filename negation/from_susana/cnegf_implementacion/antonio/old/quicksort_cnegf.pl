:- module(quicksort_cnegf, _,[.(cnegf)]).

:- use_module(library(lists),[append/3,delete/3,list_insert/2,union/3]).
% quicksort( List, SortedList): sort List by the quicksort algorithm

no_quicksort(X,Y):-cnegf(quicksort(X,Y)).

quicksort( [], []).

quicksort( [X|Tail], Sorted)  :-
   split( X, Tail, Small, Big),
   quicksort( Small, SortedSmall),
   quicksort( Big, SortedBig),
   append( SortedSmall, [X|SortedBig], Sorted).

gt(X,Y):- X>Y.

split( _X, [], [], []).

split( X, [Y|Tail], [Y|Small], Big)  :-
   gt( X, Y), !,
   split( X, Tail, Small, Big).

split( X, [Y|Tail], Small, [Y|Big])  :-
   split( X, Tail, Small, Big).


