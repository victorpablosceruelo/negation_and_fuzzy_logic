:- module(pointers, [proc1/0], ciao(h)).
proc1 :-               % definition for procedure proc1
	X = a(1,W),    % create struct a(1,W), X points to it
	Y = 2,         % Y points to constant 2
	Z = c(3,L),    % create struct c(3,L), Z points to it
	f1(X,Y),       % call procedure f1 (X,Y passed as args)
	g1(Y,Z).       % call procedure g1 (Y,Z passed as args)

f1(X,Y) :- 
	X = a(_,K),    % a(_,K) matched w/input X: K is extracted
	Y = K.         % Y and K are now the same
g1(X,Y) :- 
	Y = c(M,N),    % extract arguments of struct in Y
	M = N.         % M and N are now the same
