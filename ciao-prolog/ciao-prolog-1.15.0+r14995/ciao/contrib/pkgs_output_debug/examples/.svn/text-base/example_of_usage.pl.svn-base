:- module(example_of_usage,_,[pkgs_output_debug]).

% :- use_module(library(write)).

test1("This is just a test").
test2(C) :- append([a], [b], C).

append([], L, L).
append([T:More], L1, [T:L2]) :-
	append(More, L1, L2).

:- define_pkgs_output_debug_file('debug_file_of_file_not_test.pl').