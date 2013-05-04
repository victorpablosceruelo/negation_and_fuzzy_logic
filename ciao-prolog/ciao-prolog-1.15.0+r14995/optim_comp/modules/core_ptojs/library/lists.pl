:- module(lists, [], [oo_syntax, hiord, assertions, fsyntax, mutables, string_type]).

% TODO: this is a reduced implementation of 'lists', merge

:- export(append/3).
append([], L, L).
append([E|Es], L, [E|R]) :- append(Es, L, R).

:- export(last/2).
last([E|L], X) :- last_aux(L, E, X).

last_aux([], E, E).
last_aux([E|L], _, X) :-
        last_aux(L, E, X).

:- export(length/2).
length(L, N) :- var(N), !, llength(L, 0, N).
length(L, N) :- dlength(L, 0, N).

llength([], I, I).
llength([_|L], I0, I) :- I1 is I0+1, llength(L, I1, I).

dlength([], I, I) :- !.
dlength([_|L], I0, I) :- I0<I, I1 is I0+1, dlength(L, I1, I).

:- export(reverse/2).
reverse(Xs,Ys):- reverse(Xs,[],Ys).

reverse([], L, L).
reverse([E|Es],L,R) :- reverse(Es,[E|L],R).

:- export(select/3).
select(E, [E|Es], Es).
select(E, [X|Es], [X|L]) :- select(E, Es, L).

