:- module(ordered_lists,_,[assertions]).

:- doc(title,"Prolog implementation of ordered lists").

:- doc(author, "M. Zamalloa").

:- doc(module,"The module provides a set of operations to manipulate ordered lists. 
        Nodes are represented as a pair (Key,Element) where Key is a numerical key and
	Element its associated element").

:- use_module(library(lists), [append/3]).

is_empty([]).

get_first([(_,Elem)|_],Elem).

extract_first([(_,Elem)|R],Elem,R).

insert([],(Key,Elem),[(Key,Elem)]).
insert([(K,E)|R],(Key,Elem),[(Key,Elem),(K,E)|R]) :-
	Key =< K, !.
insert([(K,E)|R],(Key,Elem),[(K,E)|R_p]) :-
	insert(R,(Key,Elem),R_p).
	
insert_all(L,[],L).
insert_all(L,[X|R],L_pp) :-
	insert(L,X,L_p),
	insert_all(L_p,R,L_pp).
