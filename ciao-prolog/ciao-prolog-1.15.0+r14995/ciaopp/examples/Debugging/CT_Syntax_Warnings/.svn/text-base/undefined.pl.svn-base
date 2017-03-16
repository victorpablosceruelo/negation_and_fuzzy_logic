:- module(undefined,[top/0],[]).

top:-
	input_data(X),
	compute(X,Y,Y), % should be compute(X,Y)
	show_results(Y).

input_data(5).

compute(X,Y):- 
	X1 is 3,
        X2 is X+X1,
	Y is X2+1.  

show_results(_X):-
	% ...
        true.
