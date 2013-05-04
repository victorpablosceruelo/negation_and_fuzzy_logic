:- module( _example7_2_modified, [main/1], [assertions] ).

:- use_module( library( 'assertions/native_props.pl' ) ).

:- use_module( library(debugger) , [srcdbg_spy/6]).
:- use_module( library(sort) , [keylist/1 , keysort/2 , sort/2]).

main(L) :-
        tree(L),
        fail.

flatten_and_sort(Struct,Sorted_List) :-
        sorted_int_list(Struct),
        term_basic:(Sorted_List=Struct).
flatten_and_sort(Struct,Sorted_List) :-
        int_list(Struct),
        sort:sort(Struct,Sorted_List).
flatten_and_sort(Struct,Sorted_List) :-
        list_of_int_lists(Struct),
        flatten(Struct,Unsorted_List),
        sort:sort(Unsorted_List,Sorted_List).
flatten_and_sort(Struct,Sorted_List) :-
        tree(Struct),
        flatten(Struct,Unsorted_List),
        sort:sort(Unsorted_List,Sorted_List).

sorted_int_list([]).
sorted_int_list([N]) :-
        basic_props:int(N).
sorted_int_list([A,B|R]) :-
        basic_props:int(A),
        basic_props:int(B),
        arithmetic:(A=<B),
        sorted_int_list([B|R]).

:- check success int_list(_75364).

:- check calls int_list(_75364).

int_list([]).
int_list([H|L]) :-
        basic_props:int(H),
        int_list(L).

:- check success list_of_int_lists(_75998).

:- check calls list_of_int_lists(_75998).

list_of_int_lists([]).
list_of_int_lists([H|L]) :-
        int_list(H),
        list_of_int_lists(L).

:- check success tree(_76555).

:- check calls tree(_76555).

tree(void).
tree(t(L,N,R)) :-
        basic_props:int(N),
        tree(L),
        tree(R).

flatten(X,[]) :-
        term_basic:(X=[]).
flatten(X,[]) :-
        term_basic:(X=void).
flatten([N|Ns],L) :-
        term_typing:integer(N),
        !,
        term_basic:(L=[N|Ns]).
flatten([L|Ls],Flat) :-
        flatten_element(L,Flat,Cont),
        flatten(Ls,Cont).
flatten(t(L,N,R),Flat) :-
        flatten(L,FL),
        flatten(R,FR),
        append([N|FL],FR,Flat).

flatten_element([],Flat,Flat).
flatten_element([X|Xs],[X|Yx],Cont) :-
        flatten_element(Xs,Yx,Cont).

append([],L,L).
append([X|Xs],L,[X|Ys]) :-
        append(Xs,L,Ys).



