:- module(g_utility,
	[
	    number_of_items/2, 
	    ith_element/3, 
	    is_builtin_pred/1, 
	    insert_in_open_list/2, 
	    ins_without_dup/2, 
	    is_in_open_list/2, 
	    open_union/2, 
	    open_union_changes/3, 
	    close_open_list/1, 
	    set_diff/3, 
	    set_union/3, 
	    set_inclusion/2,    
	    get_arg_num_list/2, 
	    combinations/2,     
	    find_value/3        
	], 
	[andprolog,assertions]). 

:- doc(author,"Pedro L@'{o}pez").  

:- use_module(library(lists), [append/3, select/3]). 
:- use_module(library(idlists), [member_0/2]). 

:- push_prolog_flag(multi_arity_warnings,off).

is_builtin_pred(Lit):-
	functor(Lit, F, A),
	member_0(F/A, 
	[
	    (pragma)/1,
	    (is)/2,
	    (write)/1, 
	    (writeq)/1,
	    (nl)/0,
	    (!)/0,
	    (true)/0, 
            (fail)/0, 
	    (false)/0,
            (get0)/1,
            (ttynl)/0,
            (ttyput)/1,
            (display)/1,
            (current_op)/3,
            (tab)/1,
            (print)/1,
            (functor)/3,
            (arg)/3,
            (findall)/3,
            (=:=)/2,     
            (=\=)/2,     
            (<)/2,      
            (>)/2,      
            (=<)/2,     
            (>=)/2,     
            (number)/1,
            (integer)/1,
            (atom)/1,
            (=)/2,      
            (==)/2,     
            (\==)/2
        ]).


combinations(_ItemList, []).
combinations(ItemList, [Item|Combs]):-
	select(Item, ItemList, Rest),
	combinations(Rest, Combs).


get_arg_num_list(A,L):-
	get_arg_num_list(A,[],L).


get_arg_num_list(0,InList,InList).
get_arg_num_list(A,InList,OuList):-
	A > 0,
	A1 is A - 1,
	get_arg_num_list(A1,[A|InList],OuList).


set_diff([],_,[]).
set_diff(Set1,Set2,[E|Set3]):-
	Set1 = [E|S1],
	\+ member_0(E,Set2),!,
	set_diff(S1,Set2,Set3).
set_diff(Set1,Set2,Set3):-
	Set1 = [_E|S1],
	set_diff(S1,Set2,Set3).


set_union(Set1,Set2,Set3):-
	set_diff(Set1,Set2,SetDiff),
	append(SetDiff,Set2,Set3).


%
%  Test if set Set1 is included in set Set2.
%
set_inclusion([],_).
set_inclusion(Set1,Set2):-
	Set1 = [E|S1],
	member_0(E,Set2),
	set_inclusion(S1,Set2).


%
% Close an open list.
%
close_open_list(L) :- 
	var(L),!,
	L = [].
close_open_list(L) :- 
	nonvar(L),
	L = [_|List],
        close_open_list(List).


%
% Calculates the number of items (or length) of an open list.
%
number_of_items(L, N):-
	number_of_items(L, 0, N).


number_of_items(L, I, I):-
	var(L).
number_of_items(L, I, N):-
	nonvar(L),
	L=[_|List],
        NI is I + 1, 
        number_of_items(List, NI, N).


%
% Insert an item in a open list.
%
insert_in_open_list(L,I) :- 
	var(L),!,
	L = [I|_].
insert_in_open_list(L,I) :- 
	nonvar(L),
	L = [_|List],
        insert_in_open_list(List,I).


%
% Insert a variable in a list if it isn't in yet.
%

ins_without_dup(L,I) :- 
	var(L),!,
	L = [I|_].
ins_without_dup(L,I) :- 
	nonvar(L),
	L = [Item|_],
	Item==I,!.
ins_without_dup(L,I) :- 
	nonvar(L),
	L = [Item|List],
	I \== Item,
	ins_without_dup(List,I).


%
%  Perform the union of two open lists and leave the result in the
%  second one.
%

open_union(L, _):-
	var(L),!.
open_union([Ele|L1], L2) :-
	ins_without_dup(L2, Ele),
	open_union(L1, L2).


%
%  Perform the union of two open lists and leave the result in the
%  second one and set the flag Changes to true if the seconf one has changed. 
%

open_union_changes(L, _, _Changes):-
	var(L),!.
open_union_changes([Ele|L1], L2, Changes) :-
	ins_without_dup_changes(L2, Ele, Changes),
	open_union_changes(L1, L2, Changes).

ins_without_dup_changes(L, I, true) :- 
	var(L), 
        !,
	L = [I|_].
ins_without_dup_changes(L, I, _Changes) :- 
	nonvar(L),
	L = [Item|_],
	Item == I,!.
ins_without_dup_changes(L, I, Changes) :- 
	nonvar(L),
	L = [Item|List],
	I \== Item,
	ins_without_dup_changes(List, I, Changes).

%
% Check if a item is in an open list .
%

is_in_open_list(L,_) :- 
	var(L),!,fail.
is_in_open_list(L,I) :- 
	nonvar(L),
	L = [Item|_],
        Item==I,!.
is_in_open_list(L,I) :- 
	nonvar(L),
	L = [Item|List],
	I \==Item,
	is_in_open_list(List,I).

%
%  Get the ith element in the list.
%
% (replace by that in ciao library)
ith_element(List, NumElem, Element):-
	ith_element(List, 1, NumElem, Element).
ith_element([Element|_List], Count, NumElem, Element):-
        Count =:= NumElem, !.
ith_element([_|List], Count, NumElem, Element):-
        Count < NumElem, 
        Count1 is Count + 1,
        ith_element(List, Count1, NumElem, Element).


%% 
%% Find the value associated to a Key in a list of pairs 
%%

%% find_value([],_,_).
%% 
%% find_value([(V ,Val)|_],VS,Val):-
%% 	VS == V,!. 
%% 
%% find_value([(V ,Val)|List],VS,Val):- 
%% 	VS \== V,
%% 	find_value(List,VS,Val).

find_value([],_,_):- fail.

find_value([(Var ,Value)|List],VS, OVal):-
	(VS == Var -> OVal = Value
	; find_value(List,VS,OVal)
	).
            
 %% %---------------------------------------------------------------------
 %% % (see ciao library and replace).
 %% concat(Y,X,Name):- 
 %%    name(X,L),append(Y,L,Z),name(Name,Z). 
 %% 
 %% %--------------------------------------------------------------------------


:- pop_prolog_flag(multi_arity_warnings).


