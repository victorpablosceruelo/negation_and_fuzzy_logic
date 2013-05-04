:- module( _example7_2_modified, [main/1], [assertions,nativeprops,regtypes,rtchecks] ).

:- use_module( library(sort) , [keylist/1 , keysort/2 , sort/2]).

main(L) :-
        tree(L),
        flatten_and_sort(L,_1).

flatten_and_sort(Struct,Sorted_List) :-
        sorted_int_list(Struct),
        fail.
flatten_and_sort(Struct,Sorted_List) :-
        int_list(Struct),
        fail.
flatten_and_sort(Struct,Sorted_List) :-
        list_of_int_lists(Struct),
        fail.
flatten_and_sort(Struct,Sorted_List) :-
        tree(Struct),
        flatten(Struct,Unsorted_List),
        sort:sort(Unsorted_List,Sorted_List).

sorted_int_list([]).
sorted_int_list([N]) :-
        basic_props:int(N),
        fail.
sorted_int_list([A,B|R]) :-
        basic_props:int(A),
        fail.

:- prop int_list(_74974)
         + ( basic_props:regtype ).

int_list([]).
int_list([H|L]) :-
        basic_props:int(H),
        fail.

:- prop list_of_int_lists(_75580)
         + ( basic_props:regtype ).

list_of_int_lists([]).
list_of_int_lists([H|L]) :-
        int_list(H),
        fail.

:- prop tree(_76113)
         + ( basic_props:regtype ).

tree(void).
tree(t(L,N,R)) :-
        basic_props:int(N),
        tree(L),
        tree(R).

flatten(X,[]) :-
        term_basic:(X=[]),
        fail.
flatten(X,[]) :-
        term_basic:(X=void).
flatten([N|Ns],L) :-
        term_typing:integer(N),
        fail.
flatten([L|Ls],Flat) :-
        flatten_element(L,Flat,Cont),
        fail.
flatten(t(L,N,R),Flat) :-
        flatten(L,FL),
        flatten(R,FR),
        append([N|FL],FR,Flat).

append([],L,L).
append([X|Xs],L,[X|Ys]) :-
        append(Xs,L,Ys).



