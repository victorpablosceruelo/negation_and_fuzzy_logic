:- module(_ackermann,[ackermann/3],[assertions]).
%:- use_module(.(neg)).



:- entry ackermann(X,Y,Z) : (num(X), ground(X), num(Y), ground(Y), var(Z)).
%:- entry not_ackermann(X,Y,Z) : (num(X), ground(X), num(Y), ground(Y), var(Z)).

% R is the value of ackermann'sç function for the natural numbers N and M
ackermann(0,M,R):- R is M+1,!.
ackermann(N,0,R):- R1 is N-1,ackermann(R1,1,R),!.
ackermann(N,M,R):- N1 is N-1,M1 is M-1,ackermann(N,M1,R1),ackermann(N1,R1,R),!.

%not_ackermann(X,Y,Z):-neg(ackermann(X,Y,Z)). 

