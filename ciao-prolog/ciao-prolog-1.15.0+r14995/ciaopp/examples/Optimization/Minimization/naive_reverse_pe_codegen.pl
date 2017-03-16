:- module( _naive_reverse, [rev/2], [assertions] ).


:- entry rev([1,2,3,4,5,6|L],R).

:- true pred rev(A,B).

rev([1,2,3,4,5,6],[6,5,4,3,2,1]).
rev([1,2,3,4,5,6,B|C],A) :-
        rev_2(C,D),
        app_1(D,[B],E),
        app_2(E,[6],F),
        app_3(F,[5],G),
        app_4(G,[4],H),
        app_5(H,[3],I),
        app_6(I,[2],J),
        app_7(J,[1],A) .

:- true pred rev_2(A,B).

rev_2([],[]).
rev_2([B|C],A) :-
        rev_2(C,D),
        app_1(D,[B],A) .

:- true pred app_1(A,B,C).

app_1([],[A],[A]).
app_1([A|B],[C],[A|D]) :-
        app_1(B,[C],D) .

:- true pred app_2(A,B,C).

app_2([],[6],[6]).
app_2([A|B],[6],[A|C]) :-
        app_2(B,[6],C) .

:- true pred app_3(A,B,C).

app_3([],[5],[5]).
app_3([A|B],[5],[A|C]) :-
        app_3(B,[5],C) .

:- true pred app_4(A,B,C).

app_4([],[4],[4]).
app_4([A|B],[4],[A|C]) :-
        app_4(B,[4],C) .

:- true pred app_5(A,B,C).

app_5([],[3],[3]).
app_5([A|B],[3],[A|C]) :-
        app_5(B,[3],C) .

:- true pred app_6(A,B,C).

app_6([],[2],[2]).
app_6([A|B],[2],[A|C]) :-
        app_6(B,[2],C) .

:- true pred app_7(A,B,C).

app_7([],[1],[1]).
app_7([A|B],[1],[A|C]) :-
        app_7(B,[1],C) .
