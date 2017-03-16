:- module(quant_goals,[main/0],[]).
:- use_package(lprolog).
:- use_module(library(write)).

p(X,X).

main :-
	( sigma x \ (pi y \ p(x,y)) ->
		display('should have failed, but didnt'), nl
	;
		display('failed correctly'), nl
	).
