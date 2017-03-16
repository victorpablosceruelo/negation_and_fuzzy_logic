:- module( _naive_reverse, [rev/2], [assertions] ).


:- entry rev([1,2,3,4,5,6|L],R).

rev([1,2,3,4,5,6],[6,5,4,3,2,1]).
rev([1,2,3,4,5,6,B|C],A) :-
        rev_2(C,D),
        app_7(D,[B],E),
        app_7(E,[6],F),
        app_7(F,[5],G),
        app_7(G,[4],H),
        app_7(H,[3],I),
        app_7(I,[2],J),
        app_7(J,[1],A) .

rev_2([],[]).
rev_2([B|C],A) :-
        rev_2(C,D),
        app_7(D,[B],A) .

app_7([],[A],[A]).
app_7([A|B],[C],[A|D]) :-
        app_7(B,[C],D) .
