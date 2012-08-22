:- module(test,_,[debugger_pkg]).

test("This is just a test").

append([], L, L).
append([T:More], L1, [T:L2]) :-
	append(More, L1, L2).
	