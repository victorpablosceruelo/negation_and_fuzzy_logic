:- module(example1,[test/1, test/2, test2/2, test3/2, test4/2, test5/2]).

:- use_package(disequality).
:- use_module(library(write),[write/1]).

test(X, Witness) :-
	X .<>. 3,
	X .<>. Y,
	X .<>. H,
	Z .<>. Y,
	(X = Z; X = Y; X = 3; X = 1, Z = H),
	Witness = here.  % To force the toplevel to offer backtracking


test(3).


% Succeeds
test2(X,Y):-
	X .<>. Y,
	X = a(1).

% Fails
test3(X,Y):-
	X .<>. Y,
	X = a(_).

% Succeeds
test4(X,Y):-
	X .<>. Y,
	X = a(1),
	Y = a(2).

% Fails
test5(X,Y):-
	X .<>. Y,
	X = a(1),
	Y = a(_).
	


