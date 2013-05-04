:- module( _example7_2, [main/1], [assertions,nativeprops,regtypes,rtchecks] ).

:- use_module( library(sort) , [keylist/1 , keysort/2 , sort/2]).

main(L) :-
        int_list(L),
        flatten_and_sort(L,_1).

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
        fail.

sorted_int_list([]).
sorted_int_list([N]) :-
        basic_props:int(N).
sorted_int_list([A,B|R]) :-
        basic_props:int(A),
        basic_props:int(B),
        arithmetic:(A=<B),
        sorted_int_list([B|R]).

:- prop int_list(_36934)
         + ( basic_props:regtype ).

int_list([]).
int_list([H|L]) :-
        basic_props:int(H),
        int_list(L).

:- prop list_of_int_lists(_37476)
         + ( basic_props:regtype ).

list_of_int_lists([]).
list_of_int_lists([H|L]) :-
        int_list(H),
        list_of_int_lists(L).

:- prop tree(_37922)
         + ( basic_props:regtype ).

tree(void).
tree(t(L,N,R)) :-
        basic_props:int(N),
        fail.

flatten_list([],[]).
flatten_list([L|Ls],Flat) :-
        flatten_element_list(L,Flat,Cont),
        flatten_list(Ls,Cont).

flatten_element_list([],Flat,Flat).
flatten_element_list([X|Xs],[X|Yx],Cont) :-
        flatten_element_list(Xs,Yx,Cont).



