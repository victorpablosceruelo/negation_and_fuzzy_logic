:- module( _inclist_inst2, [main/4], [assertions] ).


:- use_module(library(write), [write/1]).

:- true pred main(A,B,C,D).

main(A,B,C,D) :-
        write('Hello world'),
        inclist_1([1,3|A],2,B),
        inclist_3([2,4|C],3,D) .

:- true pred inclist_1(A,B,C).

inclist_1([1,3],2,[3,5]).
inclist_1([1,3,A|B],2,[3,5,C|D]) :-
        C is 2+A,
        inclist_2(B,2,D) .

:- true pred inclist_2(A,B,C).

inclist_2([],2,[]).
inclist_2([A|B],2,[C|D]) :-
        C is 2+A,
        inclist_2(B,2,D) .

:- true pred inclist_3(A,B,C).

inclist_3([2,4],3,[5,7]).
inclist_3([2,4,A|B],3,[5,7,C|D]) :-
        C is 3+A,
        inclist_4(B,3,D) .

:- true pred inclist_4(A,B,C).

inclist_4([],3,[]).
inclist_4([A|B],3,[C|D]) :-
        C is 3+A,
        inclist_4(B,3,D) .
