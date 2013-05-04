:- module( app2, [main/6], [assertions] ).

main(T,L,R,T2,L2,R2):- 
	X is L +1,
	append([1,2|T],[L],R), 
	X is L +1,
	append([8,10|T2],L2,R2).

append([],X,X).
append([H|X],Y,[H|Z]):- append(X,Y,Z).


