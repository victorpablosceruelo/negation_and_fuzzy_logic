:- module(discont,[top/0],[]).

top:-
	input_data(X),
	compute(X,Y),  
	show_results(Y).

input_data(5).

compute(0,1).
compute(X,Y):- 
	X1 is X-1,
	compute(X1,Y1),
	Y is Y1*X1.  

input_data(3). % this is old and should have been removed!!

show_results(_X):-
	% ...
        true.
