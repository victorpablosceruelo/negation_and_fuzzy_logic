:- module(cdic_, [], [compiler(complang)]).

:- use_module(library(assoc_yap)).

% Using 'assoc_yap', which seems to be a little bit faster:
%
%  assoc_ciao: real 0m13.399s, user 0m12.614s
%  assoc_yap:  real 0m11.861s, user 0m11.089s

% TODO: the module name cannot be 'cdic' here, fix
:- public class cdic {
    % A dictionary with a counter (number of elements)

    :- public attr d :: m_any.
    :- public attr n :: m_int.

    :- constructor new_/0.
    new_ :- ~d = ~empty_assoc, ~n = 0.

    % Insert a new element and increment the counter
    :- public insert/2.
    insert(K, V) :-
        d <- ~put_assoc(K, ~d, V),
	n.inc(1).

    % Replace the element at the given index
    % TODO: like insert/2, but does not increment
    :- public replace/2.
    replace(K, V) :-
        d <- ~put_assoc(K, ~d, V).

    % Get the element at index K
    :- public constant get/2.
    get(K) := V :-
        get_assoc(K, ~d, V).

    :- public map_insert/2.
    map_insert([], []).
    map_insert([X|Xs], [M|Ms]) :-
    	insert(X, M),
    	map_insert(Xs, Ms).

    :- public map_replace/2.
    map_replace([], []).
    map_replace([X|Xs], [M|Ms]) :-
    	replace(X, M),
    	map_replace(Xs, Ms).

    % List of key-values in the dictionary
    :- public constant to_list/1.
    to_list := L :-
        assoc_to_list(~d, L).
}.

%% % Version using 'library(dict)'
%% % 
%% % Time:
%% %  real 0m11.652s
%% %  user 0m10.854s
%% 
%% :- use_module(library(dict)).
%% 
%% :- public class cdic {
%%     % A dictionary with a counter (number of elements)
%%     :- attr d :: m_any.
%%     :- attr n :: m_int.
%% 
%%     :- constructor new_/0.
%%     new_ :- ~n = 0.
%% 
%%     % Insert a new element and return its index
%%     insert(K, V) :-
%%         dic_lookup(~d, K, V),
%% 	n.inc(1).
%% 
%%     % Replace the element at the given index
%%     replace(K, V) :-
%%         d <- ~dic_replace(~d, K, V).
%% 
%%     % Get the element at index K
%%     :- constant get/1.
%%     get(K) := V :-
%%         dic_get(~d, K, V).
%% 
%%     map_insert([], []).
%%     map_insert([X|Xs], [M|Ms]) :-
%%     	insert(X, M),
%%     	map_insert(Xs, Ms).
%% 
%%     map_replace([], []).
%%     map_replace([X|Xs], [M|Ms]) :-
%%     	replace(X, M),
%%     	map_replace(Xs, Ms).
%% 
%%     % List of key-values in the dictionary
%%     :- constant to_list/1.
%%     to_list := L :-
%%         dict_to_list(~d, L).
%% }.
%% 
%% % Dictionary to list
%% dict_to_list(Dic, L) :-
%% 	kvs :: accum(L), dict_to_list_(Dic).
%% {
%% :- fluid kvs :: accum.
%% dict_to_list_(Dic) :- var(Dic), !.
%% dict_to_list_(dic(K,V,L,R)) :-
%% 	dict_to_list_(L),
%% 	kvs.add(K-V),
%% 	dict_to_list_(R).
%% }.


