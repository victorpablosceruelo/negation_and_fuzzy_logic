length(L, N) :- var(N), !, llength(L, 0, N).
length(L, N) :- dlength(L, 0, N).

llength([], I, I).
llength([_|L], I0, I) :- I1 is I0+1, llength(L, I1, I).

dlength([], I, I) :- !.
dlength([_|L], I0, I) :- I0<I, I1 is I0+1, dlength(L, I1, I).


nth(N, List, Elem) :-
        integer(N), !, N >= 1, nthfunc(N, List, Elem).
nth(N, List, Elem) :-
        var(N), !,
        findnth(List, Elem, 1, N).

nthfunc(1, [Elem|_], Elem) :- !.
nthfunc(N, [_|List], Elem) :-
        N1 is N-1,
        nthfunc(N1, List, Elem).

findnth([Elem|_], Elem, N, N).
findnth([_|List], Elem, N0, N) :-
        N1 is N0+1,
        findnth(List, Elem, N1, N).
