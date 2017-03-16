/* grips2_test.pl */


/*
These are test definitions for the GRIPS pre-processor.
To use them, consult grips2.pl, then consult this file.
*/


double( N ) <- N*2.


quadruple( N ) <- double( double(N) ).


factorial(N) <- 1 if N =< 0.
factorial(N) <- N * factorial(N-1) if N > 0.


factorial1(0) <- 1.
factorial1(N) <- N*factorial1(N-1).


count( [] ) <- 0.
count( [_|T] ) <- 1 + count(T).


join( [], L ) <- L.
join( [H|T], L ) <- [ H | join(T,L) ].


sum( [] ) <- 0.
sum( [H|T] ) <- H + sum(T).


sum1( L ) <- 0 if L = [].
sum1( [H|T] ) <- H + sum1(T).


sum_diff( A,B,C) <- A+B-C.
 

small( P ) if P < 24.


divides_by_4( N ) if ( N rem 4 ) = 0.


