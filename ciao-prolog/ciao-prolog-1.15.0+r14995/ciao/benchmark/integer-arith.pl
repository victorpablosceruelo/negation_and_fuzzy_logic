% bitwise conjunction of 10 random 50 bits integers (with most significant bit at 1)
test(conjunction_50):- 
	_ is 2071867179317428 /\
	1569935607408562 /\
	2007591249920969 /\
	2024863466152756 /\
	2152323582216973 /\
	1348322975126677 /\
	1503327176273545 /\
	1990849536075093 /\
	1438646427859169 /\
	1749614628466311.

% bitwise conjunction of 10 random 27 bits integers (with most significant bit at 1)
test(conjunction_27):-
    _ is 219541511 /\
    230491692 /\
    153223301 /\
    215683711 /\
    136405553 /\
    166817438 /\
    152636638 /\
    242152504 /\
    155246839 /\
    188031573.  
	
% do nothing.
test(dummy).

loop(N, Test, Time):-
	statistics(usertime, _),
	(
	    for(_, 0, N), 
	    (test(Test) -> fail; halt(1))
	;
	    true
	),
	statistics(usertime, [_, T]),
	Time is T / N.


for(I, I, _).
for(I, J, K):-
	J < K, J2 is J+1, 
	for(I, J2, K).


main([AN]):-
	atom_number(AN, N),
	loop(N, conjunction_27, Time1), 
	loop(N, dummy, Time2), 
	Time = Time1 - Time2, 
	format("Real Time:\t~f\nCorrected time:\t~f\n", [Time1, Time]),
        nl.

