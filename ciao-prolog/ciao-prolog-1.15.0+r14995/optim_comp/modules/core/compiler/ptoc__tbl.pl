:- module(_, [], [compiler(complang)]).

:- use_module(library(lists)).

% ---------------------------------------------------------------------------
% Table (not based on incomplete structures)
% TODO: reimplement or replace by AVL trees
% ---------------------------------------------------------------------------

:- public tbl_empty/1.
% an empty table
tbl_empty := empty.

:- public tbl_get/3.
% get the value of entry K
tbl_get(Table, K) := Val :-
	Table = node(K0, Val0, Table0),
	( K @< K0 ->
	    % not found
	    fail
	; K @> K0 ->
	    % continue search
	    Val = ~tbl_get(Table0, K)
	; % found
	  Val = Val0
	).

:- public tbl_values/2.
% its solutions are the values in the table
tbl_values(node(_, Val0, Table)) := Val :-
	( Val = Val0
	; Val = ~tbl_values(Table)
	).

:- public tbl_update/4.
% update the table entry K with value Val
tbl_update(empty, K, Val) := Table :- !,
	Table = node(K, Val, empty).
tbl_update(Table0, K, Val) := Table :-
	Table0 = node(K0, Val0, Table1),
	( K @< K0 ->
	    % put here value
            Table = node(K, Val, Table0)
	; K @> K0 ->
	    % continue search
	    Table = node(K0, Val0, Table2),
	    Table2 = ~tbl_update(Table1, K, Val)
	; % replace value
	  Table = node(K0, Val, Table1)
	).

:- public tbl_remove/3.
% remove the table entry K
tbl_remove(empty, _) := empty :- !.
tbl_remove(Table0, K) := Table :-
	Table0 = node(K0, Val, Table1),
	( K @< K0 ->
	    % nothing to remove
	    Table = Table0
	; K @> K0 ->
	    % continue search
	    Table = node(K0, Val, Table2),
	    Table2 = ~tbl_remove(Table1, K)
	; % remove value
	  Table = Table1
	).

:- public tbl_intersection/4.
:- meta_predicate tbl_intersection(_, _, pred(3), _).
% note: if Intersection fails, do not insert that element
tbl_intersection(empty, _, _) := empty :- !.
tbl_intersection(_, empty, _) := empty :- !.
tbl_intersection(TableA, TableB, Intersection) := Table :-
	TableA = node(Ka, Va, TableA0),
	TableB = node(Kb, Vb, TableB0),
	( Ka @< Kb ->
	    Table = ~tbl_intersection(TableA0, TableB, Intersection)
	; Kb @< Ka ->
	    Table = ~tbl_intersection(TableA, TableB0, Intersection)
	; ( V = ~Intersection(Va, Vb) ->
	      Table = node(Ka, V, Table0) % ok
	  ; Table = Table0 % no intersect
	  ),
	  Table0 = ~tbl_intersection(TableA0, TableB0, Intersection)
	).

:- public tbl_union/4.
:- meta_predicate tbl_union(_, _, pred(3), _).
tbl_union(empty, TableB, _) := TableB :- !.
tbl_union(TableA, empty, _) := TableA :- !.
tbl_union(TableA, TableB, Union) := Table :-
	TableA = node(Ka, Va, TableA0),
	TableB = node(Kb, Vb, TableB0),
	( Ka @< Kb ->
	    Table = node(Ka, Va, Table0),
	    Table0 = ~tbl_union(TableA0, TableB, Union)
	; Kb @< Ka ->
	    Table = node(Kb, Vb, Table0),
	    Table0 = ~tbl_union(TableA, TableB0, Union)
	; V = ~Union(Va, Vb),
	  Table = node(Ka, V, Table0),
	  Table0 = ~tbl_union(TableA0, TableB0, Union)
	).
