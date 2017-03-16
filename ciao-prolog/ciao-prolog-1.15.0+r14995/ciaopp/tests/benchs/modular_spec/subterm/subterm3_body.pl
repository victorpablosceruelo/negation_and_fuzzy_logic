% :- entry subterm3(A,B,C) : ( ground(A), ground(B), var(C) ) .
% :- entry subterm3(A,B,C) : ( ground(A), var(B), ground(C) ) .
% :- entry subterm3(A,B,C) : ( var(A), ground(B), ground(C) ) .

subterm3(N,Sub,Term):-     
%	arg(N,Term,Arg),   
	nth(N,Term,Arg),    % also checks N > 0 (nth/3 fails otherwise!)
	subterm2(Sub,Arg,_).
subterm3(N,Sub,Term):- 
	N>1, 
	N1 is N-1, 
	subterm3(N1,Sub,Term).
