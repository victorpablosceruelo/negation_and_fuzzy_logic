fib(N,FN) :- fib_(N,0,1,FN).

fib_(0, F0, F1, F0).
fib_(N1, F0, F1, FN) :- N1>0, N is N1-1, F2 is F0+F1, fib_(N, F1, F2, FN).
