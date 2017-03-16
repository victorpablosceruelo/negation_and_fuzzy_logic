:- module(_1,[fib/2],[assertions,regtypes,ciaopp(examples(resources(exectimell))),nativeprops,basicmodes]).

:- doc(author,"Edison Mera").

:- doc(module,"This program calculate the N-th fibonacci number.").

:- entry fib(X,Y)
         : ( num(X), var(Y) ).

:- true pred fib(X,Y)
         : ( num(X), term(Y) )
        => ( num(X), num(Y) ).

:- true pred fib(X,Y)
         : ( mshare([[Y]]), var(Y), ground([X]) )
        => ground([X,Y]).

:- true pred fib(X,Y)
         : ( num(X), var(Y) )
        => ( num(X), num(Y) )
         + ( not_fails, covered ).

:- true pred fib(X,Y)
         : ( num(X), var(Y) )
        => ( num(X), num(Y), size(lb,X,int(X)), size(lb,Y,0.4472135954999579*exp(1.618033988749895,int(X))-0.4472135954999579*exp(-0.6180339887498949,int(X))) )
         + ( cost(lb,exectime,9833.331661679666*exp(1.618033988749895,int(X))+2962.220338320335*exp(-0.6180339887498949,int(X))-10223.808), cost(lb,exectime_me,9833.331661679666*exp(1.618033988749895,int(X))+2962.220338320335*exp(-0.6180339887498949,int(X))-10223.808), cost(lb,wamcount,26.66787870774914*exp(1.618033988749895,int(X))+8.332121292250864*exp(-0.6180339887498949,int(X))-27.0) ).

:- true pred fib(X,Y)
         : ( num(X), var(Y) )
        => ( num(X), num(Y), size(ub,X,int(X)), size(ub,Y,0.4472135954999579*exp(1.618033988749895,int(X))-0.4472135954999579*exp(-0.6180339887498949,int(X))) )
         + ( cost(ub,exectime,9833.331661679666*exp(1.618033988749895,int(X))+2962.220338320335*exp(-0.6180339887498949,int(X))-10223.808), cost(ub,exectime_me,9833.331661679666*exp(1.618033988749895,int(X))+2962.220338320335*exp(-0.6180339887498949,int(X))-10223.808), cost(ub,wamcount,26.66787870774914*exp(1.618033988749895,int(X))+8.332121292250864*exp(-0.6180339887498949,int(X))-27.0) ).

fib(0,0) :- !.
fib(1,1) :- !.
fib(M,N) :-
        M1 is M-1,
        M2 is M-2,
        fib(M1,N1),
        fib(M2,N2),
        N is N1+N2.


