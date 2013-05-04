:- module( _inclist_inst2, [main/4], [assertions] ).


:- use_module(library(write), [write/1]).

main(A,B,C,D) :-
        write('Hello world'),
        inclist_3([1,3|A],2,B),
        inclist_3([2,4|C],3,D) .

inclist_3([A,B],C,[D,E]) :-
        D is C+A,
        E is C+B .
inclist_3([A,B,F|G],C,[D,E,H|I]) :-
        D is C+A,
        E is C+B,
        H is C+F,
        inclist_4(G,C,I) .

inclist_4([],_1,[]).
inclist_4([B|C],A,[D|E]) :-
        D is A+B,
        inclist_4(C,A,E) .
