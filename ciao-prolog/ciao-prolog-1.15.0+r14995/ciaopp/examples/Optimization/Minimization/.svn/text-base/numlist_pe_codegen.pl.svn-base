:- module( _numlist, [main/2], [assertions] ).


:- use_module(library(write), [write/1]).

:- true pred main(A,B).

main(A,B) :-
        write('Hello world'),
        numlist_1([1,3|A]),
        numlist_3([2,4|B]) .

:- true pred numlist_1(A).

numlist_1([1,3]).
numlist_1([1,3,A|B]) :-
        num(A),
        numlist_2(B) .

:- true pred numlist_2(A).

numlist_2([]).
numlist_2([A|B]) :-
        num(A),
        numlist_2(B) .

:- true pred numlist_3(A).

numlist_3([2,4]).
numlist_3([2,4,A|B]) :-
        num(A),
        numlist_2(B) .
