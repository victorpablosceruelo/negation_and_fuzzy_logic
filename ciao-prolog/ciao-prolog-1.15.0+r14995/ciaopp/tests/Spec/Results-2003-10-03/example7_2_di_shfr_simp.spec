:- module( _example7_2, [main/1], [assertions] ).

:- use_module( library( 'assertions/native_props.pl' ) ).

:- use_module( library(debugger) , [srcdbg_spy/6]).
:- use_module( library(sort) , [keylist/1 , keysort/2 , sort/2]).

main(L) :-
        int_list(L),
        fail.

flatten_and_sort(Struct,Sorted_List) :-
        sorted_int_list(Struct),
        term_basic:(Sorted_List=Struct).
flatten_and_sort(Struct,Sorted_List) :-
        int_list(Struct),
        sort:sort(Struct,Sorted_List).
flatten_and_sort(Struct,Sorted_List) :-
        list_of_int_lists(Struct),
        flatten_list(Struct,Unsorted_List),
        sort:sort(Unsorted_List,Sorted_List).
flatten_and_sort(Struct,Sorted_List) :-
        tree(Struct),
        flatten_tree(Struct,Unsorted_List),
        sort:sort(Unsorted_List,Sorted_List).

sorted_int_list([]).
sorted_int_list([N]) :-
        basic_props:int(N).
sorted_int_list([A,B|R]) :-
        basic_props:int(A),
        basic_props:int(B),
        arithmetic:(A=<B),
        sorted_int_list([B|R]).

:- check success int_list(_150481).

:- check calls int_list(_150481).

int_list([]).
int_list([H|L]) :-
        basic_props:int(H),
        int_list(L).

:- check success list_of_int_lists(_151039).

:- check calls list_of_int_lists(_151039).

list_of_int_lists([]).
list_of_int_lists([H|L]) :-
        int_list(H),
        list_of_int_lists(L).

:- check success tree(_151520).

:- check calls tree(_151520).

tree(void).
tree(t(L,N,R)) :-
        basic_props:int(N),
        tree(L),
        tree(R).

flatten_list([],[]).
flatten_list([L|Ls],Flat) :-
        flatten_element_list(L,Flat,Cont),
        flatten_list(Ls,Cont).

flatten_element_list([],Flat,Flat).
flatten_element_list([X|Xs],[X|Yx],Cont) :-
        flatten_element_list(Xs,Yx,Cont).

flatten_tree(X,[]) :-
        term_basic:(X=void).
flatten_tree(t(L,N,R),Flat) :-
        flatten_tree(L,FL),
        flatten_tree(R,FR),
        append([N|FL],FR,Flat).

append([],L,L).
append([X|Xs],L,[X|Ys]) :-
        append(Xs,L,Ys).



