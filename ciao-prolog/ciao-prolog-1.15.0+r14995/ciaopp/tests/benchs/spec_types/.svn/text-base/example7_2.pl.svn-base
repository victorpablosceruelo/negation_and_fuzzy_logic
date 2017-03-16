:- module(_,[main/1],[assertions,regtypes]).

:- use_module(library(sort)).

main(L):-
	int_list(L), flatten_and_sort(L,_).
%	int_list(L), append(L,[3],L1),flatten_and_sort(L1,_).
% 	tree(L), flatten_and_sort(L,_).
%	list_of_int_lists(L), append(L,[[3]],L1),flatten_and_sort(L1,_).


flatten_and_sort(Struct,Sorted_List):-
	sorted_int_list(Struct),
	Sorted_List = Struct.

flatten_and_sort(Struct,Sorted_List):-
	int_list(Struct),
	sort(Struct,Sorted_List).

flatten_and_sort(Struct,Sorted_List):-
	list_of_int_lists(Struct),
	flatten_list(Struct,Unsorted_List),
	sort(Unsorted_List,Sorted_List).

flatten_and_sort(Struct,Sorted_List):-
	tree(Struct),
	flatten_tree(Struct,Unsorted_List),
	sort(Unsorted_List,Sorted_List).


sorted_int_list([]).
sorted_int_list([N]):- 	int(N).
sorted_int_list([A,B|R]):- int(A), int(B),
	A =< B,	sorted_int_list([B|R]).
	

:- regtype int_list/1.

int_list([]).
int_list([H|L]):- int(H), int_list(L).

:- regtype list_of_int_lists/1.

list_of_int_lists([]).
list_of_int_lists([H|L]):- 
	int_list(H), list_of_int_lists(L).

:- regtype tree/1.

tree(void).
tree(t(L,N,R)):- int(N), tree(L), tree(R).



flatten_list([],[]).
flatten_list([L|Ls],Flat):-
	flatten_element_list(L,Flat,Cont),
	flatten_list(Ls,Cont).
flatten_element_list([],Flat,Flat).
flatten_element_list([X|Xs],[X|Yx],Cont):-
	flatten_element_list(Xs,Yx,Cont).

flatten_tree(X,[]):- X = void.
flatten_tree(t(L,N,R),Flat):-
	flatten_tree(L,FL),
	flatten_tree(R,FR),
	append([N|FL],FR,Flat).

%% flatten(X,[]):- X = [].
%% flatten(X,[]):- X = void.
%% flatten([N|Ns],L):-
%% 	integer(N),!,L=[N|Ns].
%% flatten([L|Ls],Flat):-
%% 	flatten_element(L,Flat,Cont),
%% 	flatten(Ls,Cont).
%% flatten(t(L,N,R),Flat):-
%% 	flatten(L,FL),
%% 	flatten(R,FR),
%% 	append([N|FL],FR,Flat).
%% flatten_element([],Flat,Flat).
%% flatten_element([X|Xs],[X|Yx],Cont):-
%% 	flatten_element(Xs,Yx,Cont).


append([],L,L).
append([X|Xs],L,[X|Ys]):-
	append(Xs,L,Ys).
