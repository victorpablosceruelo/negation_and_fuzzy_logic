:- module( lines_to_string , [lines_to_string/2] ).


lines_to_string( [] , [] ).

lines_to_string( [S|Ss] , T ) :-
	lines_to_string( Ss , Ts ),
	(
	    Ts = []
	->
	    T = S
	;
	    append( S , "\n"||Ts , T )
	).
