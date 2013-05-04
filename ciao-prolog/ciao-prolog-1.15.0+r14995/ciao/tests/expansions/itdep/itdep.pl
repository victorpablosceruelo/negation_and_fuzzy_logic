:- module(itdep, [main/0, create_atoms/2], [hiord]).

:- use_module(qsortid).
:- use_module(library(sort)).
:- use_module(library(format)).
:- use_module(library(lists)).
:- use_module(library(prolog_sys)).
:- use_module(library(hiordlib)).

list_size(50000).

main:-
        list_size(N),
        create_atoms(N, L),
        format("Testing it. deep.~n", []),
        sort(L, Ls1),
        format("Sorted first list (regular qs)~n", []),
        qsortid(L, Ls2),
        format("Sorted second list (i.d. qs)~n", []),
        (
            Ls1 = Ls2 ->
            format("Ok, both lists are equal~n", [])
        ;
            format("Uh, oh, the lists differ~n", [])
        ).

create_atoms(N, Atoms):-
        length(Atoms, N),
        map(Atoms, (_(_X,Y):- new_atom(Y)), Atoms).
