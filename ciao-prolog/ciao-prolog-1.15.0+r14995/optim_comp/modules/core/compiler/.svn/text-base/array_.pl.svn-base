:- module(array_, [], [compiler(complang)]).

:- use_module(library(assoc_yap)).

:- public class array {
    % An array (associative set from indexes to elements)
    :- attr d :: m_any.
    :- attr n :: m_int.

    :- constructor new_/0.
    new_ :- ~d = ~empty_assoc, ~n = 0.

    % Insert a new element and return its index
    :- public insert/2.
    insert(V) := K :-
        K = ~n,
        d <- ~put_assoc(K, ~d, V),
	n.inc(1).

    % Replace the element at the given index
    :- public replace/2.
    replace(K, V) :-
        d <- ~put_assoc(K, ~d, V).

    % Get the element at index K
    :- public get/2.
    get(K) := V :-
        get_assoc(K, ~d, V).

    % Get the list of keys
    :- public keys/1.
    keys := ~keys__2(0, ~n).
    :- static keys__2/3.
    keys__2(I, N) := [] :- I >= N, !.
    keys__2(I, N) := [I| ~keys__2(I1, N)] :- I1 is I + 1.
}.

%% :- use_module(library(dict)).
%% 
%% :- public class array {
%%     % An array (associative set from indexes to elements)
%%     :- attr d :: m_any.
%%     :- attr n :: m_int.
%% 
%%     :- constructor new_/0.
%%     new_ :- ~n = 0.
%% 
%%     % Insert a new element and return its index
%%     insert(V) := K :-
%%         K = ~n,
%%         dic_lookup(~d, K, V),
%% 	n.inc(1).
%% 
%%     % Replace the element at the given index
%%     replace(K, V) :-
%%         d <- ~dic_replace(~d, K, V).
%% 
%%     % Get the element at index K
%%     get(K) := V :-
%%         dic_get(~d, K, V).
%% 
%%     % Get the list of keys
%%     keys := ~keys__2(0, ~n).
%%     :- static keys__2/3.
%%     keys__2(I, N) := [] :- I >= N, !.
%%     keys__2(I, N) := [I| ~keys__2(I1, N)] :- I1 is I + 1.
%% }.
