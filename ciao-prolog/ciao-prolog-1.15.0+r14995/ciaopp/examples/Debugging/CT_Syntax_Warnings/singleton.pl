:- module(singleton,[top/0],[]).

top:-
	input_data(X),
	compute(X,Y),
	show_results(Y).

input_data(5).

compute(X,Y):- 
	X1 is 3,
        X2 is X+X1,
	Y is X21+1.  % X21 should be X2

show_results(_X):-
	% ...
        true.
