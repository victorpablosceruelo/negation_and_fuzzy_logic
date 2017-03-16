:- module( _numlist, [main/2], [assertions] ).


:- use_module(library(write), [write/1]).

main(A,B) :-
        write('Hello world'),
        numlist_3([1,3|A]),
        numlist_3([2,4|B]) .

numlist_2([]).
numlist_2([A|B]) :-
        num(A),
        numlist_2(B) .

numlist_3([_1,_2]).
numlist_3([_1,_2,A|B]) :-
        num(A),
        numlist_2(B) .
