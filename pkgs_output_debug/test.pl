:- module(test,_,[pkgs_output_debug]).

test("This is just a test").

append([], L, L).
append([T:More], L1, [T:L2]) :-
	append(More, L1, L2).
	

:- define_pkgs_output_debug_file('debug_file_of_file_not_test.pl').