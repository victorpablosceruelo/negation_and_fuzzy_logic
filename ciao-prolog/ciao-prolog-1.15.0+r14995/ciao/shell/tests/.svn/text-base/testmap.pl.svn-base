:- module(_, [main/1],[]).

:- use_module(library(lists)).

main([]) :- display('Usage: testmap <size> [h|f|v]\n').
main([A,C]) :-
        atom_codes(A,S),
        number_codes(N,S),
        length(L,N),
        callmap(C,L,_).

callmap(v,_,_).
callmap(h,L,M) :-
        map(L,p,M).
callmap(f,L,M) :-
        mapp(L,M).

:- meta_predicate(map(?,pred(2),?)).

map([], _, []).
map([X|Xs], P, [Y|Ys]) :-
        P(X,Y),
        map(Xs, P, Ys).

mapp([], []).
mapp([X|Xs], [Y|Ys]) :-
        p(X,Y),
        mapp(Xs, Ys).


p(X,f(X)).
