:- module(open_stack, 
	[
	    schedule/3,
	    goal/0
	], []).

:- use_package(library(tabling)).
:- const_table_module(library(difference_constraints(difference_constraints_tab))).
:- use_package(library(difference_constraints)).

:- use_module(library(sets)).
:- use_module(library(lists)).

schedule( _, [], _).
schedule(OpenCust, Products, Stacks) :-
        ord_delete(Products, P, RemProd),
        ord_union(OpenCust, P, Open),
	length(Open, N),
        Stacks #>= N,
        ord_union(RemProd, RemCust),
        ord_intersect(Open, RemCust, NewOpenCust),
	schedule(NewOpenCust, RemProd, Stacks).


ex1([1,2,3,4], [[1], [1,2], [1,3], [2,4], [3,4], [4]]).

goal :- iota(N), ex1(C, P),
        convert(P, PO),
        list_to_ord_set(PO, PS),
        schedule([], PS, N),  write(N), nl.

iota(0).
iota(N) :- iota(M), N is M + 1.

convert([], []).
convert([S|Ss], [O|Os]) :- list_to_ord_set(S,O), convert(Ss,Os).