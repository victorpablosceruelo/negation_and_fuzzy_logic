:- module(fib, [fibo/2], [assertions,regtypes,nativeprops]).

:- entry fibo(X,Y) : num * var.

% Assertion 1
 :- comp fibo(N,_) + steps_ub(exp(int(N),3)).

%Assertion 2
:- comp fibo(N,_) :intervals(int(N), [i(1,5)])
	+ (steps_ub(exp(2,int(N))-1000),steps_lb(exp(2,int(N))-10000)).

%Assertion 3
:- comp fibo(N,_) :intervals(int(N), [i(1,12)])
	+ (steps_ub(exp(2,int(N))-1000),steps_lb(exp(2,int(N))-10000)).

%Assertion 4
:- comp fibo(N,_) 
	+ (steps_ub(exp(2,int(N))-1000),steps_lb(exp(2,int(N))-10000)).

%----------------
fibo(0,0):- !.
fibo(1,1):- !.
fibo(M,N):- 
	M1 is M-1, M2 is M-2, 
	fibo(M1,N1),fibo(M2,N2),
	N is N1+N2.


